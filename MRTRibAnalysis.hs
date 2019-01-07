module MRTRibAnalysis where

import qualified Data.IntMap.Strict as Map
import Data.Array.IArray
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

prefixListHash :: PrefixList -> PrefixListHash
prefixListHash = Data.Hashable.hash

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

getPeerPrefixGroupHashTable :: MRTRibV4 -> [PrefixListHashList]
getPeerPrefixGroupHashTable = map hashPeerTableEntry where
    hashPeerTableEntry :: (PeerIndex,MRTPeer,RouteMapv4) -> PrefixListHashList
    hashPeerTableEntry (_,_,rm) = sort $ map (Data.Hashable.hash . snd) $ Map.elems rm 

showMetrics :: MRTRibV4 -> String
showMetrics = unlines . map show . getMetrics
getMetrics :: MRTRibV4 -> [((Int,Int),Int)]
getMetrics t = map calcMetric peerPairs
    where
    peers = getPeerPrefixGroupHashTable t
    l  = fromIntegral $ length peers
    peerPairs = [(i,j) | i <- [0 .. l-2], j <- [1 .. l-1],i<j]
    calcMetric (i,j) = ((i,j), distance (peers !! i) (peers !! j) )

-- Peer pre-selection
-- Peers which have reasonably full route tables are of interest
-- Reasonably full can be defined as being within a defined delta size of the largest
--
-- We will need to prepare a summary table, which should contain the numbers of paths and prefixes in each peer RIB...
-- 'statsRouteMap' from MRTrib does this for each peer routemap
-- so, getStats :: IPv4PeerTable -> Array PeerIndex (Int,Int), where (Int,Int) is the count of paths and routes respectiveley....:

type Stats = [(PeerIndex, (Int,Int))]
getStats :: MRTRibV4 -> Stats
getStats = map (\(px,_,x) -> (px,statsRouteMap x ))

maxima :: Stats -> (Int,Int)
maxima a = (ma,mb) where
    ma = maximum $ map (fst .snd) a
    mb = maximum $ map (snd . snd) a

-- filtering the peer table for full size, requires a defintion of eta which is the permissible route count deficit
-- so, interesting to look at the candidates in a sample, i.e. report the per peer counts as a perecntage of the respective maxima...
-- a function to do this over a list of (Int,Int) give a 100% value of (Int,Int) looks like this:
-- maxCompare :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Float,Float))]

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

<<<<<<< HEAD
maxPrefixCount :: MRTRibV4 -> Int
maxPrefixCount = maximum . map ( prefixCountRouteMap . third ) where third (_,_,x) = x

preFilterTable :: Float -> MRTRibV4 -> MRTRibV4
preFilterTable eta m = filter ( \(_,_,pfxs) -> (prefixCountRouteMap pfxs) > l) m
    where
    l = ceiling $ (1.0 - eta) * ( fromIntegral $ maxPrefixCount m )
=======
-- first, we will need to define a selection vector to apply to the array.....
type Selector = [Bool]
select :: Selector -> IPv4PeerTable -> IPv4PeerTable
select s t = makePeerTable $ foldr f [] (zip s (elems t)) where
    f (p,a) l = if p then a:l else l
  
tableSizeSelector :: Float -> [((Int,Int),(Float,Float))] -> Selector
tableSizeSelector eta = map (\((_,_),(_,x)) -> eta > x)

peerCountLimit :: Int -> [a] -> Selector
peerCountLimit n l = take (length l) ( replicate n True ++ repeat False ) 

preFilterTable :: Float -> IPv4PeerTable -> IPv4PeerTable
preFilterTable eta t = select s t
    where
    s = tableSizeSelector eta (maxCompare (maxima stats) (elems stats))
    stats = getStats t
    e = elems t

capTable :: Int -> IPv4PeerTable -> IPv4PeerTable
capTable n t = select (peerCountLimit n (indices t)) t
>>>>>>> 3e1799ec04ae93402c0fff7f83238185fa6ea6b1
