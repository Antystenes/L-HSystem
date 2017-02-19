{-# LANGUAGE OverloadedStrings #-}
module Server where

import LHsys
import Control.Monad (unless)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C


interaction :: Socket -> IO ()
interaction cli = do
  msg <- recv cli 1024
  unless (S.null msg) $ if msg == "quit"
    then return ()
    else do
      C.putStrLn msg
      sendAll cli msg
      interaction cli

main :: IO ()
main = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
  sock@(MkSocket _ fam stype _ _) <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1
  (client, cliaddr) <- accept sock
  interaction client
  close client
  close sock
