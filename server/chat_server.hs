import Network.Socket
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    runConn conn
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hi!\n"
    sClose sock
