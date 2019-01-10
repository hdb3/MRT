{-# LANGUAGE RecordWildCards #-}
module FilterMoreSpecifics where

{-

the purpose of this function is to filter a list of entries for overlapped prefixes
it does so by excluding and replacing longer prefixes for shorter ones on the same underlying address
this requires a 'least specific lookup' of some form
a naive implmentation would require lookups over every possible length, which is quite expensive (and complex)
-}
import Data.Maybe(fromJust)
import Data.Bits
import Data.Word 
import qualified Data.IntMap.Strict as Map
import Text.Printf

import MRTlib

type RawRIB = Map.IntMap (Word8,MRTRecord)


mrtFromRIB :: RawRIB -> [MRTRecord]
mrtFromRIB = map snd . Map.elems

process :: [MRTRecord] -> [MRTRecord]
process = mrtFromRIB . mrtToRIB

mrtToRIB :: [MRTRecord] -> RawRIB
mrtToRIB (mrt:mrts) = (mrt,Map.fromList $ map rib4entry (filter notSlash24 l4), Map.fromList $ map rib6entry ( filter notSlash56 l6)) where
    (l4,l6) = buildBiList mrts
    notSlash56 RIBIPV6Unicast{..} = 57 > re6Length
    notSlash24 RIBIPV4Unicast{..} = 25 > re4Length
    rib4entry RIBIPV4Unicast{..} = (v4hash (re4Address, re4Length) ,re4RIB)
    rib4entry mrt = error $ "rib4entry only defined on RIBIPV4Unicast: " ++ show mrt
    rib6entry RIBIPV6Unicast{..} = (v6hash (re6Address, re6Length), re6RIB)
    rib6entry mrt = error $ "rib6entry only defined on RIBIPV6Unicast: " ++ show mrt


    buildBiList :: [MRTRecord] -> ([MRTRecord],[MRTRecord])
    buildBiList = foldl insertBiList ([],[]) where
        insertBiList (ip4s,ip6s) mrt4@RIBIPV4Unicast{} = (mrt4:ip4s,ip6s)
        insertBiList (ip4s,ip6s) mrt6@RIBIPV6Unicast{} = (ip4s,mrt6:ip6s)
        insertBiList x _ = x
