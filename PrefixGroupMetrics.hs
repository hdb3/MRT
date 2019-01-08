module PrefixGroupMetrics where

import qualified Data.IntMap.Strict as Map
import Data.Maybe(isJust,catMaybes)
import Data.List(sort,group)
import MRTrib
import MRTrib

newtype PrefixRib = PR (Map.IntMap PrefixListHash)
newtype PrefixCache = PC (Map.IntMap IP4PrefixList)

lookup :: (PrefixRib, PrefixCache) -> (IP4PrefixList,PrefixListHash) -> Maybe [Int]
lookup (PR pr, PC pc) (pl,plh) = if isJust $ Map.lookup plh pc then Nothing else Just $ reduce $ map ((flip Map.lookup) pr . v4hash) pl where
    reduce :: [Maybe Int] -> [Int]
    reduce = sort . map length . group . sort . catMaybes

buildRIB :: Map.IntMap (a, IP4PrefixList) -> (PrefixRib, PrefixCache)
buildRIB m = (buildPrefixRib prefixRibItems, buildPrefixCache prefixCacheItems)
    where
    prefixLists = map snd $ Map.elems m
    prefixRibItems :: [(IP4PrefixHash,PrefixListHash)]
    prefixRibItems = concatMap (\prefixList -> map (\prefix -> (v4hash prefix, prefixHash prefixList)) prefixList) prefixLists
    ---prefixRibItems = concatMap f prefixLists
    ---f :: IP4PrefixList -> [(IP4PrefixHash,PrefixListHash)]
    ---f pl = map (\prefix -> (v4hash prefix, prefixHash pl)) pl
    buildPrefixRib :: [(IP4PrefixHash,PrefixListHash)] -> PrefixRib
    buildPrefixRib = PR . Map.fromList

    prefixCacheItems :: [(PrefixListHash,IP4PrefixList)]
    prefixCacheItems = map (\prefixList -> (prefixHash prefixList, prefixList)) prefixLists
    buildPrefixCache :: [(PrefixListHash,IP4PrefixList)] -> PrefixCache
    buildPrefixCache = PC . Map.fromList
