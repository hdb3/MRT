{-# LANGUAGE DataKinds,FlexibleInstances,RecordWildCards #-}
module MRTrib where

import Data.IP
import Data.Word 
import qualified Data.IntMap.Strict as Map
import qualified Data.Hashable
import FarmHash(hash64)

import MRTlib
import MRTrib

-- Prefix Analysis

instance Data.Hashable.Hashable IP
instance Data.Hashable.Hashable IPv4
instance Data.Hashable.Hashable IPv6

prefixListHash :: PrefixList -> PrefixListHash
prefixListHash = Data.Hashable.hash

getGroupedPrefixListHashes :: PeerMap -> [[PrefixListHash]]
getGroupedPrefixListHashes = map ( map ( prefixListHash . snd ) . Map.elems ) . Map.elems

absoluteDistance :: [PrefixListHash] -> [PrefixListHash] -> Int
absoluteDistance l1 l2 = countDistinct (l1 ++ l2) - max (length l1) (length l2)


getPrefixListHashes :: PeerMap -> [PrefixListHash]
getPrefixListHashes = concatMap ( map ( prefixListHash . snd ) . Map.elems ) . Map.elems

getPrefixLists :: PeerMap -> [PrefixList]
getPrefixLists = concatMap getRouteMapPrefixLists . Map.elems
    where
    getRouteMapPrefixLists :: RouteMap -> [PrefixList]
    getRouteMapPrefixLists  = map snd . Map.elems

countDistinctPrefixGroups :: PeerMap -> Int
countDistinctPrefixGroups = countDistinct . getPrefixListHashes

reportDistance :: PeerMap -> String
reportDistance m = unlines $ map report peerPairs
    where
    peerList = getGroupedPrefixListHashes m
    l = length peerList
    peerPairs = [(i,j) | i <- [0 .. l-2], j <- [1 .. l-1],i<j]
    report (i,j) = show (i,j) ++ ": " ++ show ( absoluteDistance (peerList !! i) (peerList !! j) )

countDistinct :: [Int] -> Int
countDistinct ix = length $ foldl f Map.empty ix where
    --type M = Map.IntMap Int
    --f :: M -> Int -> M
    f :: Map.IntMap Int -> Int -> Map.IntMap Int
    f m i = Map.alter g i m
    g Nothing = Just 1
    g (Just n) = Just (n+1)
