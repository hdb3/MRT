{-# LANGUAGE RecordWildCards #-}
module FilterMoreSpecifics where

{-

the purpose of this function is to filter a list of entries for overlapped prefixes
it does so by excluding and replacing longer prefixes for shorter ones on the same underlying address
this requires a 'least specific lookup' of some form
a naive implmentation would require lookups over every possible length, which is quite expensive (and complex)
-}
import Data.Maybe(catMaybes)
import Data.IP
import MRTlib
import Overlap

filter :: [MRTRecord] -> [MRTRecord]
filter = mrtFromTree . mrtToTree

mrtToTree :: [MRTRecord] -> Tree [RIBEntry]
mrtToTree = Overlap.fromList . catMaybes . map mrtToLeaf where
    mrtToLeaf RIBIPV4Unicast{..} = Just $ ((re4Length, toHostAddress re4Address) , re4RIB)
    mrtToLeaf _ = Nothing

mrtFromTree = map mrtFromLeaf . zip [0..] . Overlap.toList where
    mrtFromLeaf (n,((l,v),ribs)) = RIBIPV4Unicast (fromIntegral n) l (fromHostAddress v) ribs
