module Server where
import System.Network
import Secret

main :: IO ()
main = do
  sfd <- socket AF_INET SOCK_STREAM
  setsocketopt sfd SO_REUSEADDR 1
  bind sfd (mkSockAddr 9900 Nothing)
  listen sfd 2
  serve sfd

serve :: Socket -> IO ()
serve sfd = do
  putStrLn "accepting"
  (fd, addr) <- accept sfd
  putStrLn $ "got connection " ++ show addr
  sec <- recvString fd (length secret)
  if (sec == secret) then do
    str <- recvString fd 1000
    putStrLn $ "echoing"
    sendString fd (reverse str)
    return ()
   else do
    putStrLn "bad secret"
  close fd
  serve sfd
