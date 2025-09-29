module Main where

import Data.List
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import System.Exit

import Network.Network

import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary SockAddr where
    arbitrary = do
        address <- (intercalate "." . map show) <$> vectorOf 4 (arbitrary :: Gen Word8)
        port <- chooseInt (1, 9999)
        return $ SockAddrInet port address

prop_pokepeek :: SockAddr -> Property
prop_pokepeek sockaddr = monadicIO $ do
    sockaddr' <- run $ with sockaddr $ \p -> do
        peek p
    
    monitor $ whenFail $ do
        putStrLn "==== input ===="
        putStrLn $ show sockaddr
        putStrLn "==== output ===="
        putStrLn $ show sockaddr'

    assert $ sockaddr == sockaddr'

main :: IO ()
main = do
    r <- quickCheckResult $ withMaxSuccess 10000 prop_pokepeek
    if isSuccess r
        then exitSuccess
        else exitFailure