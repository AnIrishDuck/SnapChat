module Main where

import           System
import           Data.Text
import           Data.IORef
import           Control.Applicative
import           Control.Monad.Trans
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist

type Entry = String
type ChatState = IORef [Entry]

text :: Entry -> String
text e = e

entries :: ChatState -> IO [Entry]
entries s = readIORef s

-- site :: Snap ()
-- site =
--     ifTop (writeBS "hello world") <|>
--     fileServe "."

chatter :: ChatState -> Snap ()
chatter room = route [("say",     sayHandler),
                      ("static",  fileServe "static/"),
                      ("entries", (roomHandler room))]

sayHandler :: Snap ()
sayHandler = writeBS "say something!"

x :: Snap()
x = do 
  r <- getRequest; 
  writeBS $ rqPathInfo r

roomHandler :: ChatState -> Snap ()
roomHandler room = do 
  currentEntries <- liftIO (entries room)
  writeBS "abc"
  writeText $ pack $ show currentEntries

increment :: IORef Int -> IO Int
increment var = do
  x <- readIORef var
  writeIORef var (x + 1)
  return (x)

countUp :: IORef Int -> Snap ()
countUp var = do
  current <- liftIO $ increment var
  writeText $ pack $ show current

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                   []  -> 8000
                   p:_ -> read p
    roomState <- newIORef ["something something dark side",
                           "something something destiny"]
    let site = chatter roomState
    httpServe "*" port "myserver"
        (Just "access.log")
        (Just "error.log")
        site
