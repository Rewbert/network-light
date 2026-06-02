module ClientApply where
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.Network
import System.IO.Serialize
import System.IO.TimeMilli
import Text.Printf
import Packet
import Secret

secretBS :: BS.ByteString
secretBS = BS.pack secret

nfib :: Int -> Int
nfib n =
  case n < 2 of
    False -> nfib (n - 1) + nfib (n - 2) + 1
    True  -> 1

compute :: (a->b) -> a -> IO b
compute fcn arg = do
  fd <- socket AF_INET SOCK_STREAM
  connect fd (mkSockAddr 9900 (Just "127.0.0.1"))
  sendByteString fd secretBS
  outbs <- fcn `seq` arg `seq`
           writeSerializedCompressedBS (fcn, arg)
  putStrLn $ "send: " ++ show (BS.length outbs)
  sendBS fd outbs
  inbs <- recvBS fd
  putStrLn $ "recv: " ++ show (BS.length inbs)
  res <- readSerializedBS inbs
  close fd
  return res

main :: IO ()
main = do
  args <- getArgs
  let n = case args of s:_ -> read s; _ -> 10
  putStrLn $ "arg=" ++ show n
  t1 <- getTimeMilli
  print (nfib n)
  t2 <- getTimeMilli
  compute nfib n >>= print
  t3 <- getTimeMilli
  printf "local %f\nremote %f\n" (fromIntegral (t2-t1) / 1000) (fromIntegral (t3-t2) / 1000)
