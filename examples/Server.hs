module Server where
import System.Network

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
  str <- recvString fd 100
  putStrLn $ "echoing"
  sendString fd (reverse str)
  close fd
  serve sfd
