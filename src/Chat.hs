{-|

Implements chat room functionality. Allows the application to post messages 
('addEntry'), add users ('addUser'), get a list of entries ('getEntries') and 
kick inactive users ('tick'). All of this is done in a thread-safe way through the
magic of "Control.Concurrent.STM".

-}
module Chat where

import Data.Map (Map)
import Data.CIByteString (CIByteString(..))
import Data.ByteString (ByteString)
import qualified Data.Map as Map
       (size, update, toList, partition, member, map, empty, insert)
import qualified Data.ByteString as ByteString (concat)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
       (STM, writeTVar, readTVar, atomically, readTVarIO, newTVarIO, TVar)
import Control.Monad (liftM, forM_)
import Data.Sequence ((|>), Seq)
import qualified Data.Sequence as Seq (empty)
import qualified Data.Foldable as Foldable (toList)

-- |An active user in a chat room.
type User = ByteString
-- |Something someone has said.
data Entry = Message ByteString ByteString
-- |A chat room with a list of active users and what they have said.
data ChatRoom = Room { entries :: TVar (Seq Entry),
                       users :: TVar (Map User Integer) }

instance Show Entry where
    show (Message user msg) = show [user, msg]

-- |Creates an empty chat room.
startingState :: IO ChatRoom
startingState = do currentEntries <- newTVarIO Seq.empty
                   currentUsers <- newTVarIO (Map.empty :: Map User Integer)
                   return (Room currentEntries currentUsers)

-- |Gets everything that has been said in the given chat room.
getEntries :: ChatRoom -> IO [Entry]
getEntries s = liftM Foldable.toList $ readTVarIO (entries s)

-- |Updates the given variable in place.
updateTVar :: TVar a -> (a -> a) -> STM a
updateTVar var f = do
    value <- readTVar var
    let result = f value
    writeTVar var result
    return result

-- |Adds an entry to the room.
addEntry :: ChatRoom -> Entry -> IO ()
addEntry room newEntry = do
    atomically $ do _ <- updateTVar (entries room) (|> newEntry); return ()
    print newEntry

-- |Number of ticks to wait for a user before kicking them.
ticksBeforeExit :: Integer
ticksBeforeExit = 4

-- |Adds a user to the chat room. Posts a system message indicating the user has
--  joined.
addUser :: ChatRoom -> ByteString -> IO ()
addUser room name = do
    currentUsers <- atomically $ do
        currentUsers <- readTVar (users room)
        writeTVar (users room) (Map.insert name ticksBeforeExit currentUsers)
        return currentUsers
    if not (Map.member name currentUsers) then
        addEntry room (Message "system" (ByteString.concat [name, " joined"]))
        else checkIn room name

-- |Checks the user in to the room, preventing the feared auto-kick.
checkIn :: ChatRoom -> ByteString -> IO ()
checkIn room name = do
    atomically $ do
        currentUsers <- readTVar (users room)
        writeTVar (users room) (Map.insert name ticksBeforeExit currentUsers)

-- |Kicks all inactive users from the room. A user is considered inactive if 
--  'ticksBeforeExit' ticks have passed and they have not checked in to the room.
tick :: ChatRoom -> IO ()
tick room = do
    usersGone <- atomically $ do
        currentUsers <- readTVar (users room)
        let updatedUsers = Map.map (\ x -> x - 1) currentUsers
        let (usersLeft, usersGone) = Map.partition (/=0) updatedUsers         
        writeTVar (users room) usersLeft
        return usersGone
    
    let exitMsg name = (Message "system" (ByteString.concat [name, " left"]))
        keys m = map fst (Map.toList m)
    forM_ (keys usersGone) (\ name -> addEntry room (exitMsg name))

    threadDelay 500000
