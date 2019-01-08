module PrefixGroupMetrics where

import qualified Data.IntMap.Strict as Map
import Data.Maybe(isJust)
import MRTrib
import MRTrib

newtype PrefixRib = PR (Map.IntMap PrefixListHash)
newtype PrefixCache = PC (Map.IntMap IP4PrefixList)

lookup :: (PrefixRib, PrefixCache) -> (IP4PrefixList,PrefixListHash) -> [Int]
lookup (PR pr, PC pc) (pl,plh) = if isJust $ Map.lookup plh pc then [length pl] else []

buildRIB :: Map.IntMap (a, IP4PrefixList) -> (PrefixRib, PrefixCache)
buildRIB m = (buildPrefixRib prefixRibItems, buildPrefixCache prefixCacheItems)
    where
    prefixLists = map snd $ Map.elems m
    prefixRibItems :: [(IP4PrefixHash,PrefixListHash)]
    prefixRibItems = concatMap (\prefixList -> map (\prefix -> (hashP prefix, hashL prefixList)) prefixList) prefixLists
    ---prefixRibItems = concatMap f prefixLists
    ---f :: IP4PrefixList -> [(IP4PrefixHash,PrefixListHash)]
    ---f pl = map (\prefix -> (hashP prefix, hashL pl)) pl
    buildPrefixRib :: [(IP4PrefixHash,PrefixListHash)] -> PrefixRib
    buildPrefixRib = PR . Map.fromList

    prefixCacheItems :: [(PrefixListHash,IP4PrefixList)]
    prefixCacheItems = map (\prefixList -> (hashL prefixList, prefixList)) prefixLists
    buildPrefixCache :: [(PrefixListHash,IP4PrefixList)] -> PrefixCache
    buildPrefixCache = PC . Map.fromList

    hashP = v4hash
    hashL :: IP4PrefixList -> PrefixListHash
    hashL = prefixHash
