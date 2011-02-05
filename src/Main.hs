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
import Data.Maybe (fromMaybe)

import Chat --(ChatState, Entry, addEntry, addUser, entries, startingState)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

addHeaderField :: CIByteString -> [ByteString] -> Snap ()
addHeaderField key values = do
    modifyResponse $ (updateHeaders $ Map.insert key values)

dontCache :: Snap () -> Snap ()
dontCache action = do
    action
    addHeaderField "Cache-Control" ["no-store"]

chatter :: ChatRoom -> Snap ()
chatter room = dontCache $ route [("say",     sayHandler room),
                                  ("room",    fileServeSingle "static/room.html"),
                                  ("static",  fileServe "static/"),
                                  ("entries", (roomHandler room))]

sayHandler :: ChatRoom -> Snap ()
sayHandler state = do
    userParam <- getParam "user"
    let user = fromMaybe "system" userParam
    newEntry <- getParam "text"
    case newEntry of
        Just msg -> liftIO $ addEntry state (Message user msg)
        Nothing -> writeBS "say something!"

    roomHandler state

roomHandler :: ChatRoom -> Snap ()
roomHandler room = do
    userParam <- getParam "user"
    case userParam of
        Nothing -> return ()
        Just "unknown" -> return ()
        Just userName -> liftIO $ addUser room userName

    currentEntries <- liftIO (getEntries room)
    writeText $ pack $ show currentEntries

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                   []  -> 8000
                   p:_ -> read p
    room <- startingState

    _ <- forkIO (forever $ tick room)
    let site = chatter room
    quickHttpServe site
