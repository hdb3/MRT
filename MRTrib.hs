{-# LANGUAGE DeriveGeneric,DataKinds,FlexibleInstances,RecordWildCards #-}
module MRTrib where

import Data.IP
import Data.Word 
import GHC.Generics (Generic)
import qualified Data.IntMap.Strict as Map
import qualified Data.Hashable
import FarmHash(hash64)
import Data.Array.IArray
import Data.Maybe(fromMaybe)

import MRTlib

data IPPrefix = IP4Prefix IP4Prefix | IP6Prefix IP6Prefix deriving (Show,Generic)
instance Data.Hashable.Hashable IPv4
instance Data.Hashable.Hashable IPv6
instance Data.Hashable.Hashable IPPrefix

type PrefixList = [IPPrefix]
type IP4Prefix = (IPv4,Word8)
type IP4PrefixList = [IP4Prefix]
type IP6Prefix = (IPv6,Word8)
type IP6PrefixList = [IP6Prefix]
type BGPAttributeHash = Int
type PrefixListHash = Int
type PrefixListHashList = [Int]
type PeerIndex = Word16
type PeerMapInput = (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix)
type RouteMapv4 = Map.IntMap (BGPAttributes,IP4PrefixList)
type RouteMapv6 = Map.IntMap (BGPAttributes,IP6PrefixList)
type RouteMap = (RouteMapv4, RouteMapv6)
emptyRouteMap :: RouteMap
emptyRouteMap = (Map.empty,Map.empty)
type PeerMap = Map.IntMap RouteMap
data PeerTableEntry = PT { ptPeer :: MRTPeer, ptRibV4 :: RouteMapv4, ptRibV6 :: RouteMapv6 }
type PeerTable = Array PeerIndex PeerTableEntry
type IPv4PeerTable = Array PeerIndex (MRTPeer,RouteMapv4)
type IPv6PeerTable = Array PeerIndex (MRTPeer,RouteMapv6)

data RIBrecord = RIBrecord { rrPrefix :: IPPrefix, rrPeerIndex :: PeerIndex , rrOriginatedTime :: Timestamp , rrAttributes :: BGPAttributes, rrAttributeHash :: BGPAttributeHash } deriving Show

mrtToPeerMap :: [MRTRecord] -> PeerMap
mrtToPeerMap = buildPeerMap . mrtToPeerMapInput
    where

    mrtToPeerMapInput :: [MRTRecord] -> [PeerMapInput]
    mrtToPeerMapInput = concatMap extractPeerMapInput

    extractPeerMapInput :: MRTRecord -> [PeerMapInput]
    extractPeerMapInput = map ribRecordToPeerMapInput . extractRIBrecords
    extractRIBrecords :: MRTRecord -> [RIBrecord]
    extractRIBrecords RIBIPV4Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = IP4Prefix (re4Address,re4Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re4RIB
    extractRIBrecords RIBIPV6Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = IP6Prefix (re6Address,re6Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re6RIB
    extractRIBrecords _ = []
    myHash (BGPAttributes bs) = fromIntegral $ FarmHash.hash64 bs

    ribRecordToPeerMapInput :: RIBrecord -> PeerMapInput
    ribRecordToPeerMapInput RIBrecord{..} = (rrPeerIndex,rrAttributeHash,rrAttributes,rrPrefix)


    buildPeerMap :: [PeerMapInput] -> PeerMap
    buildPeerMap = foldl insertPeerMap Map.empty

    insertPeerMap :: PeerMap -> (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix) -> PeerMap
    insertPeerMap m (peer,hash,attrs,prefix) = Map.alter (insertRouteMap (hash,attrs,prefix)) (fromIntegral peer) m

    insertRouteMap :: (BGPAttributeHash,BGPAttributes,IPPrefix) -> Maybe RouteMap -> Maybe RouteMap
    insertRouteMap (hash,attrs,prefix) Nothing = insertRouteMap (hash,attrs,prefix) (Just emptyRouteMap)
    insertRouteMap (hash,attrs,IP4Prefix prefix) (Just (rm4,rm6)) = Just (Map.alter (alterRouteMap (attrs,prefix)) hash rm4,rm6)
    insertRouteMap (hash,attrs,IP6Prefix prefix) (Just (rm4,rm6)) = Just (rm4,Map.alter (alterRouteMap (attrs,prefix)) hash rm6)

    alterRouteMap (attrs,prefix) Nothing = Just (attrs,[prefix])
    alterRouteMap (_,prefix) (Just (attrs, prefixes)) = Just (attrs,prefix:prefixes)

--keys = Map.keys

--
-- show/report
--

reportPeerTable :: MRTRecord -> String
reportPeerTable MRTlib.MRTPeerIndexTable{..} = "MRTPeerIndexTable { tdBGPID = " ++ show tdBGPID ++ " tdViewName = " ++ show tdViewName ++ "\n"
                                               ++ show (length peerTable) ++ " peers in peer index table\n"
                                               ++ unlines (map show peerTable) ++ "\n}"
reportPeerTable _ = error "reportPeerTable only defined on MRTlib.MRTPeerIndexTable"

reportPeerMap :: PeerMap -> String
reportPeerMap m = show (length m) ++ " peers found in RIB\n"
                  ++ concatMap reportRouteMap (Map.elems m)

reportRouteMap :: RouteMap -> String
reportRouteMap (m4,m6) = "\nReport RouteMap " ++
                show (rc4,pc4) ++ " IPv4 routes/prefixes " ++
                show (rc6,pc6) ++ " IPv6 routes/prefixes "
                where
                (rc4,pc4) = statsRouteMap m4
                (rc6,pc6) = statsRouteMap m6

                -- statsPeerMap m = foldl (\(a1,b1) (a2,b2) -> (max a1 a2, max b1 b2)) (0,0) (map statsRouteMap (Map.elems m))
                statsRouteMap m = (length m, prefixCount m) where prefixCount = sum . map ( length . snd ) . Map.elems

getPeerTable :: [MRTRecord] -> PeerTable
getPeerTable (mrt0:mrtx) = buildPeerTable mrt0 (mrtToPeerMap mrtx)
getPeerTable [] = error "getPeerTable requires at least a MRT Peer Table Record"

buildPeerTable :: MRTRecord -> PeerMap -> PeerTable
buildPeerTable MRTlib.MRTPeerIndexTable{..} peerMap =
    array (0, fromIntegral al)
          [ (fromIntegral i, PT (peerTable !! i) (fst $ peerLookup i) (snd $ peerLookup i)) | i <- [0..al]]
    where
    al = length peerTable - 1
    peerLookup i = fromMaybe emptyRouteMap (Map.lookup (fromIntegral i) peerMap)
buildPeerTable _ _ = error "buildPeerTable only valid on MRT Peer Index Table records"

showPeerTable :: PeerTable -> String
showPeerTable = unlines . map (\(ix,pte) -> show ix ++ ": " ++ showPeerTableEntry pte) . assocs
    where
    showPeerTableEntry PT{..} = "IPv4: " ++ show (statsRouteMap ptRibV4) ++ "  IPv6: " ++ show (statsRouteMap ptRibV6) ++ " : " ++ show ptPeer
    statsRouteMap m = (length m, prefixCount m) where prefixCount = sum . map ( length . snd ) . Map.elems

showStatsRouteMap :: Map.IntMap (a, [b]) -> String
showStatsRouteMap m = show (length m, prefixCount m) where prefixCount = sum . map ( length . snd ) . Map.elems

size :: (Ix a1, IArray a e, Num a1) => a a1 e -> a1
size a = h -l + 1 where (l,h) = bounds a

getIPv4PeerTable :: PeerTable -> IPv4PeerTable
getIPv4PeerTable pt = listArray (0,fromIntegral $ length l - 1) l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p r4 _) -> (p,r4)) $ elems pt

showIPv4PeerTable :: IPv4PeerTable -> String
showIPv4PeerTable a = "IPv4 peers ("
                      ++ show ( size a )
                      ++ ")\n"
                      ++ unlines ( map showIPv4PeerTableEntry $ assocs a)
    where
    showIPv4PeerTableEntry (i,(p,r)) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r
 
getIPv6PeerTable :: PeerTable -> IPv6PeerTable
getIPv6PeerTable pt = listArray (0,fromIntegral $ length l - 1) l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p _ r6) -> (p,r6)) $ elems pt

showIPv6PeerTable :: IPv6PeerTable -> String
showIPv6PeerTable a = "IPv6 peers ("
                      ++ show ( size a )
                      ++ ")\n"
                      ++ unlines ( map showIPv6PeerTableEntry $ assocs a)
    where
    showIPv6PeerTableEntry (i,(p,r)) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r
