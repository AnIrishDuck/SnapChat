-----------------------------------------------------------------------------
--
-- Module      :  Chat
-- Copyright   :  Frank Murphy, 2010
-- License     :  BSD
--
-- Maintainer  :  fpmurphy@mtu.edu
-- Stability   :  Experimental
-- Portability :  Unknown
--
-- |
--
-----------------------------------------------------------------------------

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

data Entry = Message ByteString ByteString
type User = ByteString
data ChatRoom = Room { entries :: TVar (Seq Entry),
                       users :: TVar (Map User Integer) }

instance Show Entry where
    show (Message user msg) = show [user, msg]

startingState :: IO ChatRoom
startingState = do currentEntries <- newTVarIO Seq.empty
                   currentUsers <- newTVarIO (Map.empty :: Map User Integer)
                   return (Room currentEntries currentUsers)

getEntries :: ChatRoom -> IO [Entry]
getEntries s = liftM Foldable.toList $ readTVarIO (entries s)

updateTVar :: TVar a -> (a -> a) -> STM a
updateTVar var f = do
    value <- readTVar var
    let result = f value
    writeTVar var result
    return result

addEntry :: ChatRoom -> Entry -> IO ()
addEntry room newEntry = do
    atomically $ do _ <- updateTVar (entries room) (|> newEntry); return ()
    print newEntry

ticksBeforeExit :: Integer
ticksBeforeExit = 4

addUser :: ChatRoom -> ByteString -> IO ()
addUser room name = do
    currentUsers <- atomically $ do
        currentUsers <- readTVar (users room)
        writeTVar (users room) (Map.insert name ticksBeforeExit currentUsers)
        return currentUsers
    if not (Map.member name currentUsers) then
        addEntry room (Message "system" (ByteString.concat [name, " joined"]))
        else checkIn room name

checkIn :: ChatRoom -> ByteString -> IO ()
checkIn room name = do
    atomically $ do
        currentUsers <- readTVar (users room)
        writeTVar (users room) (Map.insert name ticksBeforeExit currentUsers)

tick :: ChatRoom -> IO ()
tick room = do
    usersGone <- atomically $ do
        currentUsers <- readTVar (users room)
        let (usersLeft, usersGone) = Map.partition (/=0) (Map.map (\ x -> x - 1) currentUsers)
        writeTVar (users room) usersLeft
        return usersGone

    let exitMsg name = (Message "system" (ByteString.concat [name, " left"]))
        keys m = map fst (Map.toList m)
    forM_ (keys usersGone) (\ name -> addEntry room (exitMsg name))

    threadDelay 500000
