{-# LANGUAGE DataKinds,FlexibleInstances,RecordWildCards #-}
module MRTrib where

import Data.IP
import Data.Word 
import qualified Data.IntMap.Strict as Map
import qualified Data.Hashable
import FarmHash(hash64)

import MRTlib

type IPPrefix = (IP,Word8)
type PrefixList = [IPPrefix]
--type IPv4Prefix = (IPv4,Word8)
type BGPAttributeHash = Int
type PrefixListHash = Int
type PeerIndex = Word16
type PeerMapInput = (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix)
type RouteMap = Map.IntMap (BGPAttributes,PrefixList)
type PeerMap = Map.IntMap RouteMap

data RIBrecord = RIBrecord { rrPrefix :: IPPrefix, rrPeerIndex :: PeerIndex , rrOriginatedTime :: Timestamp , rrAttributes :: BGPAttributes, rrAttributeHash :: BGPAttributeHash } deriving Show


mrtToPeerMap :: [MRTRecord] -> PeerMap
mrtToPeerMap = buildPeerMap . mrtToPeerMapInput
    where

    mrtToPeerMapInput :: [MRTRecord] -> [PeerMapInput]
    mrtToPeerMapInput = concatMap extractPeerMapInput

    extractPeerMapInput :: MRTRecord -> [PeerMapInput]
    extractPeerMapInput = map ribRecordToPeerMapInput . extractRIBrecords
    extractRIBrecords :: MRTRecord -> [RIBrecord]
    extractRIBrecords RIBIPV4Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = (Data.IP.IPv4 re4Address,re4Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re4RIB
    extractRIBrecords RIBIPV6Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = (Data.IP.IPv6 re6Address,re6Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re6RIB
    extractRIBrecords _ = []
    myHash (BGPAttributes bs) = fromIntegral $ FarmHash.hash64 bs

    ribRecordToPeerMapInput :: RIBrecord -> PeerMapInput
    ribRecordToPeerMapInput RIBrecord{..} = (rrPeerIndex,rrAttributeHash,rrAttributes,rrPrefix)


    buildPeerMap :: [PeerMapInput] -> PeerMap
    buildPeerMap = foldl insertPeerMap Map.empty

    insertPeerMap :: PeerMap -> (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix) -> PeerMap
    insertPeerMap m (peer,hash,attrs,prefix) = Map.alter (insertRouteMap (hash,attrs,prefix)) (fromIntegral peer) m

    insertRouteMap :: (BGPAttributeHash,BGPAttributes,IPPrefix) -> Maybe RouteMap -> Maybe RouteMap
    insertRouteMap (hash,attrs,prefix) Nothing = Just $ Map.singleton hash (attrs,[prefix])
    insertRouteMap (hash,attrs,prefix) (Just routeMap) = Just $ Map.alter (alterRouteMap (attrs,prefix)) hash routeMap

    alterRouteMap :: (BGPAttributes,IPPrefix) -> Maybe (BGPAttributes,PrefixList) -> Maybe (BGPAttributes,PrefixList)
    alterRouteMap (attrs,prefix) Nothing = Just (attrs,[prefix])
    alterRouteMap (_,prefix) (Just (attrs, prefixes)) = Just (attrs,prefix:prefixes)

-- building intermediate form (het list -> tuple hom list)
data DiscRouteMap = DiscRouteMap (Map.IntMap (BGPAttributes,[(IPv4,Word8)])) (Map.IntMap (BGPAttributes,[(IPv6,Word8)]))
type DiscPeerMap = Map.IntMap DiscRouteMap

getDiscRouteMap :: RouteMap -> DiscRouteMap
getDiscRouteMap m = DiscRouteMap m4 m6
    where
    (m4,m6) = Map.mapEither f (discriminateRouteMap m)
    f (attr,IP4PrefixList l4) = Left (attr, l4)
    f (attr,IP6PrefixList l6) = Right (attr, l6)

getDiscPeerMap :: PeerMap -> DiscPeerMap
getDiscPeerMap = Map.map getDiscRouteMap

-- building final disc hom list, from tuple hom list

data DiscPrefixList = IP4PrefixList [(IPv4,Word8)] | IP6PrefixList [(IPv6,Word8)] | Empty
type RouteMap' = Map.IntMap (BGPAttributes,DiscPrefixList)
type PeerMap' = Map.IntMap RouteMap'

keys = Map.keys
{-
getPeerIndexMap :: [MRTRecord] -> PeerIndex -> DiscRouteMap
getPeerIndexMap mrts px =
    lookup' px (mrtToDiscPeerMap mrts)
    where 
    lookup' k m = fromMaybe 
-}
discriminatePeerMap :: PeerMap -> PeerMap'
discriminatePeerMap = Map.map discriminateRouteMap
discriminateRouteMap :: RouteMap -> RouteMap'
discriminateRouteMap = Map.map (\(attr,pfxs) -> (attr,discriminate pfxs))
    where
    discriminate :: PrefixList -> DiscPrefixList
    discriminate l4@((IPv4 _,_) : ipx ) = IP4PrefixList $ map (\(ip4,l) -> (Data.IP.ipv4 ip4,l)) l4 
    discriminate l6@((IPv6 _,_) : ipx ) = IP6PrefixList $ map (\(ip6,l) -> (Data.IP.ipv6 ip6,l)) l6

--
-- show/report
--

reportPeerTable :: MRTRecord -> String
reportPeerTable MRTlib.MRTPeerIndexTable{..} = "MRTPeerIndexTable { tdBGPID = " ++ show tdBGPID ++ " tdViewName = " ++ show tdViewName ++ "\n" ++
                                               unlines (map show peerTable) ++ "\n}"
reportPeerTable _ = error "reportPeerTable only defined on MRTlib.MRTPeerIndexTable"

reportPeerMap :: PeerMap -> String
reportPeerMap m = -- "Report PeerMap\n" ++
                show (length m) ++ " peers, " ++
                show (maxr,maxp) ++ " = (max routes, max prefixes), " ++
                show distinctPrefixGroups ++ " DistinctPrefixGroups, " ++
                show pratio ++ " pratio\n"
                ++ concatMap reportRouteMap (Map.elems m)
                ++ concatMap reportDiscRouteMap (Map.elems m)
                where (maxr,maxp) = statsPeerMap m
                      distinctPrefixGroups = countDistinctPrefixGroups m
                      pratio = fromIntegral maxp / fromIntegral distinctPrefixGroups :: Float

reportDiscRouteMap :: RouteMap -> String
reportDiscRouteMap m = "\nReport DiscRouteMap " ++
                show (rc4,pc4) ++ " IP4 routes/prefixes " ++
                show (rc6,pc6) ++ " IP6 routes/prefixes "
                where
                (rc4,pc4) = statsRouteMap m4
                (rc6,pc6) = statsRouteMap m6
                (DiscRouteMap m4 m6) = getDiscRouteMap m

reportRouteMap :: RouteMap -> String
reportRouteMap m = "\nReport RouteMap " ++
                show (rc,pc) ++ " routes/prefixes "
                where
                (rc,pc) = statsRouteMap m

statsPeerMap :: PeerMap -> (Int,Int)
statsPeerMap m = foldl (\(a1,b1) (a2,b2) -> (max a1 a2, max b1 b2)) (0,0) (map statsRouteMap (Map.elems m))

--statsRouteMap :: RouteMap -> (Int,Int)
statsRouteMap m = (length m, prefixCount m) where prefixCount = sum . map ( length . snd ) . Map.elems
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
