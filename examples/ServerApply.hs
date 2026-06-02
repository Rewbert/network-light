module ServerApply where
import qualified Data.ByteString.Char8 as BS
import Data.Any
import System.IO.Serialize
import System.Network
import Packet
import Secret

secretBS :: BS.ByteString
secretBS = BS.pack secret

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
  sec <- recvByteString fd (length secret)
  if (sec == secretBS) then do
    inbs <- recvBS fd
    putStrLn $ "recv " ++ show (BS.length inbs)
    (fcn, arg) <- readSerializedBS inbs :: IO (Any -> Any, Any)
    --cprint arg
    outbs <- writeSerializedCompressedBS $! fcn arg
    putStrLn $ "send " ++ show (BS.length outbs)
    sendBS fd outbs
    return ()
   else do
    putStrLn "bad secret"
  close fd
  serve sfd
