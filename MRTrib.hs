{-# LANGUAGE RecordWildCards #-}
module MRTrib where

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as Char8
--import qualified Data.ByteString.Base16
--import qualified Data.Attoparsec.ByteString as DAB
--import Data.Attoparsec.ByteString(Parser,anyWord8,count)
--import Data.Attoparsec.Binary
--import Control.Monad(unless)
import Data.IP
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
type Peer = Word16
data RIBrecord = RIBrecord { rrPrefix :: IPv4Prefix, rrPeerIndex :: Word16 , rrOriginatedTime :: Timestamp , rrAttributes :: BGPAttributes, rrAttributeHash :: BGPAttributeHash } deriving Show

extractRIBrecords :: MRTRecord -> [RIBrecord]
extractRIBrecords rib@RIBIPV4Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = (re4Address,re4Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re4RIB
    where myHash (BGPAttributes bs) = fromIntegral $ FarmHash.hash64 bs

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

