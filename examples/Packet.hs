module Packet(sendBS, recvBS) where
import qualified Data.ByteString as BS
import Data.Word
import System.Network

-- Send a ByteString, preceded by its length in 32 bits, network byte order.
sendBS :: Socket -> BS.ByteString -> IO ()
sendBS s bs = do
  let l = BS.length bs
      (q3, r3) = quotRem  l 256
      (q2, r2) = quotRem q3 256
      (r0, r1) = quotRem q2 256
      len = BS.pack $ map fromIntegral [r0, r1, r2, r3]
  sendByteString s len
  sendByteString s bs

-- Receive a ByteString, preceded by its length in 32 bits, network byte order.
recvBS :: Socket -> IO (BS.ByteString)
recvBS s = do
  len <- recvByteStringFull s 4
  let [r0, r1, r2, r3] = map fromIntegral $ BS.unpack len
      l = (((r0 * 256) + r1) * 256 + r2) * 256 + r3
  recvByteStringFull s l
