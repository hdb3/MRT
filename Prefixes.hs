{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Prefixes where
import Data.Word
import Data.Hashable
import GHC.Generics(Generic)
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)


-- this is a fork from the 'bgplib' file of the same name (Prefixes.hs)
-- it is made standalone and removes the Data.Binary encoding functions, arguably a proper sepration of concern
-- the immediate purpose is to support seprate compilation of the 'Overlap' functions

newtype Prefix = Prefix (Word8,Word32) deriving (Eq,Generic)
newtype IPrefix = IPrefix Int deriving Eq
toPrefix :: IPrefix -> Prefix
toPrefix (IPrefix w64) = Prefix (fromIntegral $ unsafeShiftR w64 32, fromIntegral $ 0xffffffff .&. w64)
fromPrefix :: Prefix -> IPrefix
fromPrefix (Prefix (!l,!v)) = let l' = fromIntegral l :: Int
                                  v' = fromIntegral v :: Int
                              in IPrefix $! unsafeShiftL l' 32 .|. v'
fromPrefixes :: [Prefix] -> [IPrefix]
fromPrefixes = map fromPrefix

toPrefixes :: [IPrefix] -> [Prefix]
toPrefixes = map toPrefix

instance IsString Prefix where
    fromString = read

instance IsString IPrefix where
    fromString = read

instance Read IPrefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromPrefix $ fromAddrRange a,s')]

instance Read Prefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromAddrRange a,s')]

instance Hashable Prefix

instance {-# INCOHERENT #-} Show [Prefix] where
    show = shorten

instance {-# INCOHERENT #-} Show [IPrefix] where
    show = shorten . map toPrefix

instance Show Prefix where
    show = show.toAddrRange

instance Show IPrefix where
    show = show.toAddrRange.toPrefix

shorten :: [Prefix] -> String
shorten = shortenLim 4
    where
    shortenLim :: Int -> [Prefix] -> String
    shortenLim l pfxs = if length pfxs < (l+1) then realShow pfxs else show (take l pfxs) ++ "(+" ++ show (length pfxs - l) ++ ")"
    realShow = show . map toAddrRange

subnetOf :: Prefix -> Word8
subnetOf (Prefix (s,_)) = s

ipOf :: Prefix -> Word32
ipOf (Prefix (_,i)) = i
 
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)

fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar
