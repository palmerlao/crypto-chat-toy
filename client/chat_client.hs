module Main( main ) where

import Control.Concurrent
import Control.Monad (forever, when)
import Network
import System.Environment
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = withSocketsDo $
  do
    args <- getArgs
    when ((length args) /= 3) $ do
      putStrLn "Incorrect number of arguments.\n Usage: ./chat_client [server_ip] [server_port] [prime bit-length]"
      exitFailure
    let server_ip   = args !! 0
        server_port = read (args !! 1 ) :: Int
        p_bit_len   = read (args !! 2) :: Integer
    chat_handle <- connectTo server_ip (PortNumber $ fromIntegral server_port)
    forkIO $ forever (read_messages chat_handle)
    send_messages chat_handle
    return ()

read_messages :: Handle -> IO ()
read_messages handle = do
  output <- hGetContents handle
  putStrLn output

send_messages :: Handle -> IO ()
send_messages handle = do
  input <- getContents
  sequence_ $ map (\a -> do
    hPutStr handle $ a ++ "\n"
    hFlush handle) $ lines input
