module Client where
import System.Network
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let msg | null args = "Hello"
          | otherwise = unwords args
  fd <- socket AF_INET SOCK_STREAM
  connect fd (mkSockAddr 9900 (Just "127.0.0.1"))
  putStrLn $ "Sending: " ++ msg
  sendString fd msg
  str <- recvString fd 1000
  putStrLn $ "Got: " ++ show str
  close fd
