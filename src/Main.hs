{-|
 Main module for this application. Defines the Snap application, handlers for the 
 application, and the main loop.
 
 The application itself forks off a 'tick' thread. This thread monitors all users and 
 autokicks inactive users. This combined with browser-side javascript lets us remove
 users who navigate away from the page.
-}
module Main where

import System
import Data.Text
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.Trans
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import Text.Templating.Heist
import Data.CIByteString (CIByteString(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
       (STM, writeTVar, readTVar, atomically, readTVarIO, newTVarIO, TVar)

import Chat
import Control.Concurrent (forkIO)
import Control.Monad (forever, mapM_)

type ChatState = TVar (Map.Map ByteString ChatRoom)

-- |Adds a field to the current header.
addHeaderField :: CIByteString -> [ByteString] -> Snap ()
addHeaderField key values = do
    modifyResponse $ (updateHeaders $ Map.insert key values)

-- |Indicates that the response generated by the snap context this is executed in 
--  should not be cached.
dontCache :: Snap () -> Snap ()
dontCache action = do
    action
    addHeaderField "Cache-Control" ["no-store"]

-- |Gets the indicated chat room. Creates a new room if the specified one doesn't
--  exist.
getRoom :: ChatState -> Snap ChatRoom
getRoom state = do
    roomParam <- getParam "room_name"
    let roomName = fromMaybe "default" roomParam
    -- Get the current room, or create a new one if it doesn't already exist.
    room <- liftIO $ atomically $ do
        roomMap <- readTVar state
        let addRoom = do 
                newRoom <- startingState
                writeTVar state (Map.insert roomName newRoom roomMap)
                return newRoom 
        case (Map.lookup roomName roomMap) of
            Just room -> do return room
            Nothing -> addRoom
    -- Return the room
    return room

-- |Top level configuration for the Snap application.
chatter :: ChatState -> Snap ()
chatter room = dontCache $ route all_routes 
    where 
        all_routes = [(":room_name/say",   sayHandler room),
                      (":room_name/room",  fileServeSingle "static/room.html"), 
                      (":room_name/entries", (roomHandler room)),
                      ("static",  fileServe "static/")]

-- |Handles the /say URL. Posts a message to the room.
sayHandler :: ChatState -> Snap ()
sayHandler state = do
    room <- getRoom state
    userParam <- getParam "user"
    let user = fromMaybe "system" userParam
    newEntry <- getParam "text"
    case newEntry of
        Just msg -> liftIO $ addEntry room (Message user msg)
        Nothing -> writeBS "say something!"

    roomHandler state

-- |Handles the /entries URL. Returns a list of all messages in the room.
roomHandler :: ChatState -> Snap ()
roomHandler state = do
    room <- getRoom state
    userParam <- getParam "user"
    case userParam of
        Nothing -> return ()
        Just "unknown" -> return ()
        Just userName -> liftIO $ addUser room userName

    currentEntries <- liftIO (getEntries room)
    writeText $ pack $ show currentEntries

-- |Updates every room by performing its "tick" action.
updateRooms :: ChatState -> IO ()
updateRooms state = do
    rooms <- readTVarIO state
    mapM_ tick (Map.elems rooms)

-- |Program entry point.
main :: IO ()
main = do
    state <- newTVarIO Map.empty
    _ <- forkIO (forever $ do updateRooms state; threadDelay 500000)
    let site = chatter state
    quickHttpServe site
