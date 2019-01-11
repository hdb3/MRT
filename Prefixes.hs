{-# LANGUAGE FlexibleInstances #-}

module Prefixes where
import Data.Word
import Data.IP
import Data.String(IsString,fromString)


-- derived from the 'bgplib' file of the same name (Prefixes.hs)
-- it is made standalone and removes the Data.Binary encoding functions, arguably a proper sepration of concern
-- the immediate purpose is to support seprate compilation of the 'Overlap' functions

type Prefix = (Word8,Word32)
--newtype Prefix = Prefix (Word8,Word32) deriving Eq

instance {-# INCOHERENT #-} IsString Prefix where
    fromString = read

instance {-# INCOHERENT #-} Read Prefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(toPrefix a,s')]

instance {-# INCOHERENT #-} Show Prefix where
    show = show.fromPrefix

--subnetOf :: Prefix -> Word8
--subnetOf (Prefix (s,_)) = s

--ipOf :: Prefix -> Word32
--ipOf (Prefix (_,i)) = i
 
fromPrefix :: Prefix -> AddrRange IPv4
fromPrefix (subnet,ip) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)
--fromPrefix (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)

toPrefix :: AddrRange IPv4 -> Prefix
--toPrefix ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
toPrefix ar = (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar
