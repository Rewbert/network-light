module Client where
import System.Network

main :: IO ()
main = do
  fd <- socket AF_INET SOCK_STREAM
  connect fd (mkSockAddr 9900 (Just "127.0.0.1"))
  putStrLn "Sending"
  sendString fd "Hello"
  str <- recvString fd 1000
  putStrLn $ "Got: " ++ show str
  close fd
