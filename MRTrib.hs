{-# LANGUAGE FlexibleInstances,RecordWildCards #-}
module MRTrib where

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as Char8
--import qualified Data.ByteString.Base16
--import qualified Data.Attoparsec.ByteString as DAB
--import Data.Attoparsec.ByteString(Parser,anyWord8,count)
--import Data.Attoparsec.Binary
--import Control.Monad(unless)
import Data.IP
import qualified Data.Hashable
--import Data.Bits
import Data.Word 
import qualified Data.IntMap.Strict as Map

import FarmHash(hash64)
import MRTlib

-- MRTPeerIndexTable { tdBGPID :: BGPid , tdViewName :: String, peerTable :: [MRTPeer] }
-- RIBIPV4Unicast { re4SequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: IPv4 , re4RIB :: [RIBEntry] }

-- MRTPeer { mrtPeerBGPID :: BGPid, mrtPeerASN :: AS4 , mrtPeerIPAddress :: IP } deriving Show
-- RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Timestamp , reAttributes :: BGPAttributes } deriving Show


type IPv4Prefix = (IPv4,Word8)
type BGPAttributeHash = Int
type PrefixListHash = Int
type Peer = Word16
type PeerMapInput = (Peer, BGPAttributeHash,BGPAttributes,IPv4Prefix)
data RIBrecord = RIBrecord { rrPrefix :: IPv4Prefix, rrPeerIndex :: Word16 , rrOriginatedTime :: Timestamp , rrAttributes :: BGPAttributes, rrAttributeHash :: BGPAttributeHash } deriving Show

extractRIBrecords :: MRTRecord -> [RIBrecord]
extractRIBrecords rib@RIBIPV4Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = (re4Address,re4Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re4RIB
    where myHash (BGPAttributes bs) = fromIntegral $ FarmHash.hash64 bs
extractRIBrecords _ = []

extractPeerMapInput :: MRTRecord -> [PeerMapInput]
extractPeerMapInput = (map ribRecordToPeerMapInput) . extractRIBrecords where
    ribRecordToPeerMapInput :: RIBrecord -> PeerMapInput
    ribRecordToPeerMapInput RIBrecord{..} = (rrPeerIndex,rrAttributeHash,rrAttributes,rrPrefix)

buildPeerMap :: [PeerMapInput] -> PeerMap
buildPeerMap = foldl insertPeerMap Map.empty

mrtToPeerMap :: [MRTRecord] -> PeerMap
mrtToPeerMap = buildPeerMap . mrtToPeerMapInput

mrtToPeerMapInput :: [MRTRecord] -> [PeerMapInput]
mrtToPeerMapInput = concatMap extractPeerMapInput

type PeerMap = Map.IntMap RouteMap
type RouteMap = Map.IntMap (BGPAttributes,[IPv4Prefix])

insertPeerMap :: PeerMap -> (Peer, BGPAttributeHash,BGPAttributes,IPv4Prefix) -> PeerMap
insertPeerMap m (peer,hash,attrs,prefix) = Map.alter (insertRouteMap (hash,attrs,prefix)) (fromIntegral peer) m

insertRouteMap :: (BGPAttributeHash,BGPAttributes,IPv4Prefix) -> Maybe RouteMap -> Maybe RouteMap
insertRouteMap (hash,attrs,prefix) Nothing = Just $ Map.singleton hash (attrs,[prefix])
insertRouteMap (hash,attrs,prefix) (Just routeMap) = Just $ Map.alter (alterRouteMap (attrs,prefix)) hash routeMap

alterRouteMap :: (BGPAttributes,IPv4Prefix) -> Maybe (BGPAttributes,[IPv4Prefix]) -> Maybe (BGPAttributes,[IPv4Prefix])
alterRouteMap (attrs,prefix) Nothing = Just (attrs,[prefix])
alterRouteMap (_,prefix) (Just (attrs, prefixes)) = Just (attrs,prefix:prefixes)

reportPeerMap :: PeerMap -> String
reportPeerMap m = -- "Report PeerMap\n" ++
                show (length m) ++ " peers, " ++
                show (maxr,maxp) ++ " = (max routes, max prefixes), " ++
                show distinctPrefixGroups ++ " DistinctPrefixGroups, " ++
                show pratio ++ " pratio\n"
                where (maxr,maxp) = statsPeerMap m
                      distinctPrefixGroups = countDistinctPrefixGroups m
                      pratio = (fromIntegral maxp) / (fromIntegral distinctPrefixGroups)
                -- ++ concatMap reportRouteMap (Map.elems m)

reportRouteMap :: RouteMap -> String
reportRouteMap m = "\nReport RouteMap " ++
                show rc ++ " routes in map " ++
                show pc ++ " prefixes in map" where
                (rc,pc) = statsRouteMap m

statsPeerMap :: PeerMap -> (Int,Int)
statsPeerMap m = foldl (\(a1,b1) (a2,b2) -> (max a1 a2, max b1 b2)) (0,0) (map statsRouteMap (Map.elems m))

statsRouteMap :: RouteMap -> (Int,Int)
statsRouteMap m = (length m, prefixCount m) where prefixCount = sum . map ( length . snd ) . Map.elems
-- Prefix Analysis

instance Data.Hashable.Hashable IPv4

prefixListHash :: [IPv4Prefix] -> PrefixListHash
prefixListHash = Data.Hashable.hash

getGroupedPrefixListHashes :: PeerMap -> [[PrefixListHash]]
getGroupedPrefixListHashes = map ( map ( prefixListHash . snd ) . Map.elems ) . Map.elems

absoluteDistance :: [PrefixListHash] -> [PrefixListHash] -> Int
absoluteDistance l1 l2 = countDistinct (l1 ++ l2) - max (length l1) (length l2)


getPrefixListHashes :: PeerMap -> [PrefixListHash]
getPrefixListHashes = concatMap ( map ( prefixListHash . snd ) . Map.elems ) . Map.elems

getPrefixLists :: PeerMap -> [[IPv4Prefix]]
getPrefixLists = concatMap getRouteMapPrefixLists . Map.elems
    where
    getRouteMapPrefixLists :: RouteMap -> [[IPv4Prefix]]
    getRouteMapPrefixLists  = map snd . Map.elems

countDistinctPrefixGroups :: PeerMap -> Int
countDistinctPrefixGroups = countDistinct . getPrefixListHashes

reportDistance :: PeerMap -> String
reportDistance m = unlines $ map report peerPairs
    where peerList = getGroupedPrefixListHashes m
          l = length peerList
          peerPairs = [(i,j) | i <- [0 .. l-2], j <- [1 .. l-1],i<j]
          report (i,j) = show ((i,j)) ++ ": " ++ show ( absoluteDistance (peerList !! i) (peerList !! j) )

type M = Map.IntMap Int
countDistinct :: [Int] -> Int
countDistinct ix = length m where
    m = foldl f Map.empty ix
    f :: M -> Int -> M
    f m i = Map.alter f' i m
    f' Nothing = Just 1
    f' (Just n) = Just (n+1)