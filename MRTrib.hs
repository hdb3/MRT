{-# LANGUAGE LiberalTypeSynonyms,FlexibleInstances,DeriveGeneric,RecordWildCards #-}
module MRTrib ( IP4Prefix,IP6Prefix
              , IP4PrefixList, IP6PrefixList
              , PrefixListHash, PrefixListHashList
              , prefixCountRouteMap,pathCountRouteMap,statsRouteMap,showStatsRouteMap
              , getMRTRibV4,getMRTRibV6,showMRTRibV4,showMRTRibV6
              , Peer,MRTRib,Rib,PrefixHash(..),PeerIndex
              , RouteMapv4,RouteMapv6
              , MRTRibV4,MRTRibV6
              , getMRTTableDumpV2
              , v4hash, v4unhash, v6hash, v6unhash, IP4PrefixHash, IP6PrefixHash ) where

import Data.Bits
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

type IP4PrefixHash = Int
type IP6PrefixHash = Int

v4hash :: IP4Prefix -> IP4PrefixHash
v4hash (ip,l) = let w64 x = fromIntegral x :: Word64 in fromIntegral $ unsafeShiftL (w64 l) 32 .|. w64 ( byteSwap32 $ toHostAddress ip)

v4unhash :: IP4PrefixHash -> IP4Prefix
v4unhash h = ( fromHostAddress $ byteSwap32 $ fromIntegral $ 0xffffffff .&. h' , fromIntegral $ unsafeShiftR h' 32 ) where h' = fromIntegral h :: Word64

v6unhash :: IP6PrefixHash -> IP6Prefix
v6unhash h = ( fromHostAddress64 $ 0xffffffffffffff .&. h' , fromIntegral $ unsafeShiftR h' 56)
    where
    h' = fromIntegral h :: Word64
    fromHostAddress64 x = fromHostAddress6 (w0,w1,0,0) where
        w0 = fromIntegral $ unsafeShiftR x 32
        w1 = fromIntegral $ x .&. 0xffffffff

v6hash :: IP6Prefix -> IP6PrefixHash
v6hash (ip,l) | l < 57 = fromIntegral $ unsafeShiftL (w64 l) 56 .|. toHostAddress64 ip
            -- | otherwise = fromIntegral $ asWord64 $ hash64WithSeed (w64 l) (toHostAddress64 ip) 
              | otherwise = error $ "cant' handle IPv6 > /56: " ++ show l ++ "/" ++ show ip
    where
    w64 x = fromIntegral x :: Word64
    toHostAddress64 x = let (w0,w1,_,_) = toHostAddress6 x in unsafeShiftL (w64 w0) 32 .|. w64 w1

class PrefixHash a where
    prefixHash :: a -> Int
    prefixShow :: a -> String

instance PrefixHash [IP4Prefix] where
    prefixHash = Data.Hashable.hash
    prefixShow = show

instance PrefixHash [IP6Prefix] where
    prefixHash = Data.Hashable.hash
    prefixShow = show

-- these are really RIB sets, not RIBs - should redfine to allow 'Rib' to be used in a more meaningful way
--     the two variants are required for reasons i do not understand - in some cases the type signature of Rib is rejected on some functions
type Peer a = (PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,[a]))
type Rib a = [(PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,a))]
type MRTRib a = [(PeerIndex, MRTPeer, Map.IntMap (BGPAttributes, [a]))]

type IP4Prefix = (IPv4,Word8)
type IP4PrefixList = [IP4Prefix]
type IP6Prefix = (IPv6,Word8)
type IP6PrefixList = [IP6Prefix]
type BGPAttributeHash = Int
type PrefixListHash = Int
type PrefixListHashList = [PrefixListHash]
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

type MRTRibV4 = [(PeerIndex,MRTPeer,RouteMapv4)]
type MRTRibV6 = [(PeerIndex,MRTPeer,RouteMapv6)]
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

getPeerTable :: [MRTRecord] -> PeerTable
getPeerTable [] = error "getPeerTable requires at least a MRT Peer Table Record" 
getPeerTable (mrt0:mrtx) = buildPeerTable mrt0 (mrtToPeerMap mrtx)
    where
    buildPeerTable :: MRTRecord -> PeerMap -> PeerTable
    buildPeerTable MRTlib.MRTPeerIndexTable{..} peerMap =
        array (0, fromIntegral al)
              [ (fromIntegral i, PT (peerTable !! i) (fst $ peerLookup i) (snd $ peerLookup i)) | i <- [0..al]]
        where
        al = length peerTable - 1
        peerLookup i = fromMaybe emptyRouteMap (Map.lookup (fromIntegral i) peerMap)
    buildPeerTable _ _ = error "buildPeerTable only valid on MRT Peer Index Table records"

prefixCountRouteMap :: Map.IntMap (a, [b]) -> Int
prefixCountRouteMap = sum . map ( length . snd ) . Map.elems

pathCountRouteMap :: Map.IntMap (a, [b]) -> Int
pathCountRouteMap = length

statsRouteMap :: Map.IntMap (a, [b]) -> (Int,Int)
statsRouteMap m = (pathCountRouteMap m, prefixCountRouteMap m)

showStatsRouteMap :: Map.IntMap (a, [b]) -> String
showStatsRouteMap = show . statsRouteMap

makePeerTable :: [a] -> Array PeerIndex a
makePeerTable l = listArray (0,fromIntegral $ length l - 1) l

getMRTRibV4 :: [MRTRecord] -> MRTRibV4
getMRTRibV4 = map (\(a,(b,c))->(a,b,c)) . assocs . getIPv4PeerTable . getPeerTable

getMRTRibV6 :: [MRTRecord] -> MRTRibV6
getMRTRibV6 = map (\(a,(b,c))->(a,b,c)) . assocs . getIPv6PeerTable . getPeerTable

getIPv4PeerTable :: PeerTable -> IPv4PeerTable
getIPv4PeerTable pt = makePeerTable l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p r4 _) -> (p,r4)) $ elems pt

showMRTRibV4 :: MRTRibV4 -> String
showMRTRibV4 a = "IPv4 peers ("
                      ++ show ( length a )
                      ++ ")\n"
                      ++ unlines ( map showMRTRibV4Entry a)
    where
    showMRTRibV4Entry (i,p,r) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r
 
getIPv6PeerTable :: PeerTable -> IPv6PeerTable
getIPv6PeerTable pt = makePeerTable l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p _ r6) -> (p,r6)) $ elems pt

showMRTRibV6 :: MRTRibV6 -> String
showMRTRibV6 a = "IPv6 peers ("
                      ++ show ( length a )
                      ++ ")\n"
                      ++ unlines ( map showMRTRibV6Entry a)
    where
    showMRTRibV6Entry (i,p,r) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r
