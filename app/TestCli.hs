{-# LANGUAGE OverloadedStrings #-}
module TestClient where

import LHsys
import Control.Monad (unless)
import System.IO (isEOF)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

loop sock = do
  iseof <- isEOF
  unless iseof $ do
    msg <- C.getLine
    if msg == "quit"
      then return ()
      else do
        sendAll sock msg
        resp <- recv sock 1024
        loop sock


testcli :: IO ()
testcli = do
  serveraddr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "5000")
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  loop sock
  close sock
