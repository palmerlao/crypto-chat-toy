import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Network
import System.Environment
import System.IO

main :: IO ()
main = withSocketsDo $
  do
    chan <- newChan
    args <- getArgs
    let listen_port = read $ args !! 0 :: Int
    listen_socket <- listenOn (PortNumber $ fromIntegral 2048)
    putStrLn $ "Listening on port " ++ show listen_port
    sequence_ $ repeat $ init_conns listen_socket chan
        where
          init_conns :: Socket -> Chan ((String,String),String) -> IO ()
          init_conns sock ch = do
            connection <- accept sock
            let (client_handle, hostname, portnumber) = connection
            hSetBuffering client_handle NoBuffering
            hPutStrLn client_handle "You've successfully connected."
            hPutStrLn client_handle "What's your name?"
            client_name <- hGetLine client_handle
            hPutStrLn client_handle "Who do you want to talk to?"
            recipient_name <- hGetLine client_handle
            putStrLn $ client_name ++ "@" ++ hostname ++ ":" ++ (show portnumber) ++ " joined to talk to " ++ recipient_name
            forkIO $ handle_client_conn connection (client_name,recipient_name) ch
            return ()

handle_client_conn :: (Handle, String, PortNumber) -> (String, String) -> Chan ((String,String),String) -> IO ()
handle_client_conn (handle, hostname, portnumber) (client,recipient) ch = do
  iseof <- hIsEOF handle
  ch' <- dupChan ch
  when (not iseof) $ do
    forkIO $ send_msg_to_client client handle ch'
    receive_msg handle ch' client recipient
  putStrLn $ client ++ "@" ++ hostname ++ ":" ++ (show portnumber) ++ " left."
  where
    send_msg_to_client :: String -> Handle -> Chan ((String,String),String) -> IO ()
    send_msg_to_client c h ch = do
      iseof <- hIsEOF h
      hSetBuffering h NoBuffering
      ((sender,receiver),msg) <- readChan ch
      when ((c == receiver)&&(not iseof)) $ do
        hPutStrLn h msg
        send_msg_to_client c h ch
    receive_msg :: Handle -> Chan ((String,String),String) -> String -> String -> IO ()
    receive_msg h ch c r = do
      iseof <- hIsEOF h
      hSetBuffering h NoBuffering
      when (not iseof) $ do
        msg <- hGetLine h
        putStrLn $ c ++ "-->" ++ r ++ ": " ++ msg
        writeChan ch ((c,r),msg)
        receive_msg h ch c r
