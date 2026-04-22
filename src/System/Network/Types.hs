-- | Shared types, instances, and pure helpers used by both
-- "Network.Blocking" and "Network.NonBlocking".
module System.Network.Types
    ( Socket(..)
    , Domain(..)
    , StreamType(..)
    , SockAddr(..)
    , SockOpt(..)
    , mkSockAddr
    , cSizeOf
    , cFromEnum
    ) where

import Data.Bits
import Data.List
import Data.Word

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | A socket file descriptor.
newtype Socket = Socket CInt

-- | Addressing family.
data Domain
    = AF_INET -- ^ IPv4
    -- more families can be added here

instance Enum Domain where
    toEnum 2 = AF_INET
    toEnum _ = error "Domain.toEnum: unrecognised value"
    fromEnum AF_INET = 2

-- | Socket stream type.
data StreamType
    = SOCK_STREAM -- ^ TCP
    | SOCK_DGRAM  -- ^ UDP

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum 2 = SOCK_DGRAM
    toEnum _ = error "StreamType.toEnum: unrecognised value"
    fromEnum SOCK_STREAM = 1
    fromEnum SOCK_DGRAM  = 2

-- | Socket address.  Internals are intentionally opaque to callers.
data SockAddr = SockAddrInet Int String deriving (Show, Eq)

-- | Construct a 'SockAddr'.  Pass 'Nothing' for the address to get
-- @INADDR_ANY@ (@0.0.0.0@).
mkSockAddr :: Int -> Maybe String -> SockAddr
mkSockAddr port (Just address) = SockAddrInet port address
mkSockAddr port Nothing        = SockAddrInet port "0.0.0.0"

-- | Socket options for use with @setsocketopt@.
data SockOpt
    = SO_REUSEADDR
    | SO_DEBUG
    | SO_TYPE
    | O_NONBLOCK

-- ---------------------------------------------------------------------------
-- Storable SockAddr
-- ---------------------------------------------------------------------------

instance Storable SockAddr where
    sizeOf    _ = 16
    alignment _ = 16
    peek        = peekSockAddr
    poke        = pokeSockAddr

-- | Serialise a 'SockAddr' into a @struct sockaddr_in@ laid out in memory.
-- Only 'AF_INET' is supported; the @sin_family@ field is hard-coded to @2@.
pokeSockAddr :: Ptr SockAddr -> SockAddr -> IO ()
pokeSockAddr p (SockAddrInet port address) =
    pokeArray (castPtr p) (sin_family ++ sin_port ++ sin_addr)
  where
    sin_family :: [Word8]
    sin_family = [0x02, 0x00]

    sin_port :: [Word8]
    sin_port =
        let high = fromIntegral ((port `shiftR` 8) .&. 0xFF)
            low  = fromIntegral  (port             .&. 0xFF)
        in [high, low]

    sin_addr :: [Word8]
    sin_addr = take 4 $ map read $ splitOn '.' address
      where
        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn sep xs =
            let pref = takeWhile (/= sep) xs
                suff = dropWhile (/= sep) xs
            in case suff of
                 []     -> [pref]
                 (_:t)  -> pref : splitOn sep t

-- | Deserialise a @struct sockaddr_in@ from memory into a 'SockAddr'.
-- Reads the port from bytes 2–3 (network byte order) and the IPv4 address
-- from bytes 4–7.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
    xs <- peekArray 8 (castPtr p :: Ptr Word8)
    case xs of
        _:_:high:low:sin_addr ->
            let port    = (fromIntegral high `shiftL` 8) .|. fromIntegral low
                address = intercalate "." $ map show sin_addr
            in  return $ SockAddrInet port address
        _ -> error "peekSockAddr: unexpected buffer layout"

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

-- | Return the 'sizeOf' of a value as a 'CInt', for passing to C functions.
cSizeOf :: Storable a => a -> CInt
cSizeOf = CInt . fromIntegral . sizeOf

-- | Convert a Haskell 'Enum' value to a 'CInt', for passing to C functions.
cFromEnum :: Enum a => a -> CInt
cFromEnum = CInt . fromIntegral . fromEnum
