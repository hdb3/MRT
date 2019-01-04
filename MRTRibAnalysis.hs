{-# LANGUAGE DataKinds,FlexibleInstances #-}
module MRTRibAnalysis where

--import Data.IP
--import Data.Word 
import qualified Data.IntMap.Strict as Map
import Data.Array.IArray
import qualified Data.Hashable
import Data.List((\\),union,intersect)
--import FarmHash(hash64)

import MRTlib
import MRTrib

-- Prefix Analysis

{-
This module compares pairs of peers based on presence of distinct prefix groups
'reportDistance :: PeerMap -> String' applies 'show distance' to every candidate peer pair
'distance' represents a metric over groups of prefix lists - it is the number of prefix groups which are present in only one of a pair of group lists
'distance' is the count of the set A // B where '//' is the (symmetric) set operation (A ∩ B) - (A ∪ B)
a custom calculation over an ordered list can probably be done more efficiently that simply using the Data>List functions, and a calculation over sorted lists is probably faster than unsorted..... (TBC...)

-}

prefixListHash :: PrefixList -> PrefixListHash
prefixListHash = Data.Hashable.hash

distance :: PrefixListHashList-> PrefixListHashList -> Int
distance l1 l2 = length (diff l1 l2) where
    diff a b = union a b \\ intersect a b

sortedDiff :: Ord a => [a] -> [a] -> [a] -> [a]
sortedDiff acc [] [] = acc
sortedDiff acc (a:ax) [] = sortedDiff (a:acc) ax []
sortedDiff acc [] (b:bx) = sortedDiff (b:acc) bx []
sortedDiff acc (a:ax) (b:bx) | a == b = sortedDiff acc ax bx
                             | a < b  = sortedDiff (a:acc) ax (b:bx)
                             | a > b  = sortedDiff (b:acc) (a:ax) bx
sortedDiff _ _ _ = error "not posible?!"

type PeerPrefixGroupHashTable = Array PeerIndex PrefixListHashList
getPeerPrefixGroupHashTable :: IPv4PeerTable -> PeerPrefixGroupHashTable
getPeerPrefixGroupHashTable = amap hashPeerTableEntry where
    hashPeerTableEntry :: (MRTPeer,RouteMapv4) -> PrefixListHashList
    hashPeerTableEntry (_,rm) = map (Data.Hashable.hash . snd) $ Map.elems rm 

--reportDistance :: PeerMap -> String
--showDistance t = unlines $ map show peerPairs
showMetrics :: IPv4PeerTable -> String
showMetrics = unlines . map show . getMetrics
getMetrics :: IPv4PeerTable -> [((PeerIndex,PeerIndex),Int)]
getMetrics t = map calcMetric peerPairs
    where
    peers = getPeerPrefixGroupHashTable t
    l = fromIntegral $ size peers
    peerPairs = [(i,j) | i <- [0 .. l-2], j <- [1 .. l-1],i<j]
    calcMetric (i,j) = ((i,j), distance (peers ! i) (peers ! j) )

{- the histogram has too much data - we need only presence not count - and 'nub' can do that...
countDistinct' :: [Int] -> Int
countDistinct' ix = length $ foldl f Map.empty ix where
    --type M = Map.IntMap Int
    --f :: M -> Int -> M
    -- f generates an (unordered) histogram from the input list (count of each distinct element)
    f :: Map.IntMap Int -> Int -> Map.IntMap Int
    f m i = Map.alter g i m
    g Nothing = Just 1
    g (Just n) = Just (n+1)
-}
--countDistinctPrefixGroups :: PeerMap -> Int
--countDistinctPrefixGroups = countDistinct . getPrefixListHashes where
--    getPrefixListHashes :: PeerMap -> [PrefixListHash]
--    getPrefixListHashes = concatMap ( map ( prefixListHash . snd ) . Map.elems ) . Map.elems

--getPrefixLists :: PeerMap -> [PrefixList]
--getPrefixLists = concatMap getRouteMapPrefixLists . Map.elems
--    where
--    getRouteMapPrefixLists :: RouteMap -> [PrefixList]
--    getRouteMapPrefixLists  = map snd . Map.elems

