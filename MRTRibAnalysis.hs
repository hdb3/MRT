module MRTRibAnalysis ( showMetrics
                      , getMetrics
                      , comparePeerStats
                      , maxPathCount
                      , maxPrefixCount
                      , preFilterTable
                      ) where

import qualified Data.IntMap.Strict as Map
import qualified Data.Hashable
import Data.List(sort,maximum)
import Text.Printf

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



--class PrefixHash a where
    --prefixHash :: a -> Int

--instance PrefixHash [IP4Prefix] where
    --prefixHash = Data.Hashable.hash

--instance PrefixHash [IP6Prefix] where
    --prefixHash = Data.Hashable.hash

--type MRTRibV4 = [(PeerIndex,MRTPeer,RouteMapv4)]
--type RouteMapv4 = Map.IntMap (BGPAttributes,IP4PrefixList)
--showMetrics :: MRTRibV4 -> String
showMetrics :: PrefixHash a =>  [(PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,a))] -> String
showMetrics = unlines . map show . getMetrics

--getMetrics :: MRTRibV4 -> [((Int,Int),Int)]
getMetrics t = map calcMetric peerPairs

    where

    peers = getPeerPrefixGroupHashTable t
    l  = fromIntegral $ length peers
    peerPairs = [(i,j) | i <- [0 .. l-2], j <- [1 .. l-1],i<j]
    calcMetric (i,j) = ((i,j), distance (peers !! i) (peers !! j) )

    --getPeerPrefixGroupHashTable :: MRTRibV4 -> [PrefixListHashList]
    getPeerPrefixGroupHashTable = map hashPeerTableEntry where
        hashPeerTableEntry :: PrefixHash a => (PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,a)) -> PrefixListHashList
        --hashPeerTableEntry :: (PeerIndex,MRTPeer,RouteMapv4) -> PrefixListHashList
        --hashPeerTableEntry (_,_,rm) = sort $ map (Data.Hashable.hash . snd) $ Map.elems rm 
        hashPeerTableEntry (_,_,rm) = sort $ map (prefixHash . snd) $ Map.elems rm 

    distance :: PrefixListHashList-> PrefixListHashList -> Int
    distance l1 l2 = length (sortedDiff l1 l2)

    sortedDiff :: Ord a => [a] -> [a] -> [a]
    sortedDiff = sd [] where
        sd acc [] [] = acc
        sd acc (a:ax) [] = sd (a:acc) ax []
        sd acc [] (b:bx) = sd (b:acc) bx []
        sd acc (a:ax) (b:bx) | a == b = sd acc ax bx
                             | a < b  = sd (a:acc) ax (b:bx)
                             | a > b  = sd (b:acc) (a:ax) bx
        sd _ _ _ = error "not posible?!"

-- Peer pre-selection
-- Peers which have reasonably full route tables are of interest
-- Reasonably full can be defined as being within a defined delta size of the largest
--
-- We will need to prepare a summary table, which should contain the numbers of paths and prefixes in each peer RIB...
-- 'statsRouteMap' from MRTrib does this for each peer routemap
-- so, getStats :: IPv4PeerTable -> Array PeerIndex (Int,Int), where (Int,Int) is the count of paths and routes respectiveley....:

type Stats = [(PeerIndex, (Int,Int))]

-- filtering the peer table for full size, requires a defintion of eta which is the permissible route count deficit
-- so, interesting to look at the candidates in a sample, i.e. report the per peer counts as a perecntage of the respective maxima...
-- a function to do this over a list of (Int,Int) give a 100% value of (Int,Int) looks like this:
-- maxCompare :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Float,Float))]

--comparePeerStats :: MRTRibV4 -> IO ()
comparePeerStats rib  = do
    let stats = getStats rib
    putStrLn $ "max paths/prefixes is: " ++ show (maxima stats)
    putStrLn $ showMaxCompare $ maxCompare (maxima stats) (map snd stats)

    where

    maxima :: Stats -> (Int,Int)
    maxima a = (ma,mb) where
        ma = maximum $ map (fst .snd) a
        mb = maximum $ map (snd . snd) a

    --getStats :: MRTRibV4 -> Stats
    getStats = map (\(px,_,x) -> (px,statsRouteMap x ))

    maxCompare :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Float,Float))]
    maxCompare (ma,mb) = map f where
        f :: (Int,Int) -> ((Int,Int),(Float,Float))
        f (a,b) = ((a,b),(1.0-fromIntegral a/fromIntegral ma,1.0-fromIntegral b/fromIntegral mb))

    showMaxCompare :: [((Int,Int),(Float,Float))] -> String
    showMaxCompare = unlines . map s where
        s ((a,b),(da,db)) = printf "%6d %6d %4f %4f" a b (100*da) (100*db)


--  running this on my recent AMS RIPE IPv4 dataset, there are 26/32 within 6%, the next is at -57%.  Only 2 are within 1%. 14 fall within 3%. 5 are with 2%.
-- so, depending on taste, an eta of 6, 3 or 2% would be sensible but different.  Hard coding 5% seems sensible.  But it will be interesting to study the differences!!!!!
-- as an aside, we can reconstitute IPv4PeerTables with restricted components because the MRT peer data is hel as a value in the array alongside the RIBs....
-- So,  building 'reFilterTable :: Float -> IPv4PeerTable -> IPv4PeerTable' is quite simple
--maxPathCount :: MRTRibV4 -> Int
maxPathCount = maximum . map ( pathCountRouteMap . third ) where third (_,_,x) = x

--maxPrefixCount :: MRTRibV4 -> Int
maxPrefixCount = maximum . map ( prefixCountRouteMap . third ) where third (_,_,x) = x

--preFilterTable :: Float -> MRTRibV4 -> MRTRibV4
preFilterTable eta m = filter ( \(_,_,pfxs) -> prefixCountRouteMap pfxs > l) m
    where
    l = ceiling $ (1.0 - eta) * fromIntegral ( maxPrefixCount m )
