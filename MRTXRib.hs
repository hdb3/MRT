{-# LANGUAGE RecordWildCards #-}
module MRTXRib where

{-
MRTXRib -s actually more primituve than MRTrib
MRTXRib creates a 'raw' RIB from a MRT file, hodling preciesly the RIBEntry objects presnt in the file, indexed on the prefix.
Two Ribs are generated - IPv4 and IPv6.
Useful derived RIBs can contain e.g. a bit map represnting the peer indices which refernce a given prefix
-}
import Data.Maybe(fromJust)
import Data.IP
import Data.Bits
import Data.Word 
import qualified Data.IntMap.Strict as Map

import MRTlib
import MRTrib

type RawRIB = Map.IntMap [RIBEntry] -- either IPv4/6 - as long as there is a defined mapping from a prefix to a word64 (int)
type BiRIB = (RawRIB,RawRIB) -- (IPv4,IPv6)

v4hash :: Word8 -> IPv4 -> Int
v4hash l ip = let w64 x = fromIntegral x :: Word64 in fromIntegral $ unsafeShiftL (w64 l) 32 .|. w64 ( toHostAddress ip)

fromV4hash :: Int -> (Word8, IPv4)
fromV4hash h = (fromIntegral $ unsafeShiftR h' 32, fromHostAddress $ fromIntegral $ 0xffffffff .&. h') where h' = fromIntegral h :: Word64

fromV6hash :: Int -> (Word8, IPv6)
fromV6hash h = (fromIntegral $ unsafeShiftR h' 56, fromHostAddress64 $ 0xffffffffffffff .&. h')
    where
    h' = fromIntegral h :: Word64
    fromHostAddress64 x = fromHostAddress6 (w0,w1,0,0) where
        w0 = fromIntegral $ unsafeShiftR x 32
        w1 = fromIntegral $ x .&. 0xffffffff

v6hash :: Word8 -> IPv6 -> Int
v6hash l ip | l < 57 = fromIntegral $ unsafeShiftL (w64 l) 56 .|. toHostAddress64 ip
            -- | otherwise = fromIntegral $ asWord64 $ hash64WithSeed (w64 l) (toHostAddress64 ip) 
            | otherwise = error $ "cant' handle IPv6 > /56: " ++ show l ++ "/" ++ show ip
    where
    w64 x = fromIntegral x :: Word64
    toHostAddress64 x = let (w0,w1,_,_) = toHostAddress6 x in unsafeShiftL (w64 w0) 32 .|. w64 w1

mrtToRIB :: [MRTRecord] -> BiRIB
mrtToRIB mrts = (Map.fromList $ map rib4entry l4, Map.fromList $ map rib6entry l6) where
    (l4,l6) = buildBiList mrts
    rib4entry RIBIPV4Unicast{..} = (v4hash re4Length re4Address,re4RIB)
    rib4entry mrt = error $ "rib4entry only defined on RIBIPV4Unicast: " ++ show mrt
    rib6entry RIBIPV6Unicast{..} = (v6hash re6Length re6Address,re6RIB)
    rib6entry mrt = error $ "rib6entry only defined on RIBIPV6Unicast: " ++ show mrt


    buildBiList :: [MRTRecord] -> ([MRTRecord],[MRTRecord])
    buildBiList = foldl insertBiList ([],[]) where
        insertBiList (ip4s,ip6s) mrt4@RIBIPV4Unicast{} = (mrt4:ip4s,ip6s)
        insertBiList (ip4s,ip6s) mrt6@RIBIPV6Unicast{} = (ip4s,mrt6:ip6s)
        insertBiList x _ = x

type PeerRepRIB = Map.IntMap Word64
peerRepRIB :: RawRIB -> PeerRepRIB
peerRepRIB = Map.map getPeerRep
    where
    getPeerRep :: [RIBEntry] -> Word64
    getPeerRep = foldl f 0 where
        f bitmap RIBEntry{..} | 64 > rePeerIndex = bitmap `setBit` fromIntegral rePeerIndex
                              | otherwise = error $ "getPeerRep only defined for peerIdices < 64: " ++ show rePeerIndex

checkPeerRepRIB :: PeerRepRIB -> IP4Prefix -> PeerIndex -> Bool
checkPeerRepRIB rib (ip,l) px | 64 > px = testBit (fromJust $ Map.lookup (v4hash l ip) rib) (fromIntegral px) 
