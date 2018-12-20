{-# LANGUAGE RecordWildCards #-}
module MRTquest where
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as Map
import qualified MRTlib

getGroupedMRT = fmap group getMRT
    where
    group recs = fmap (\(a,b) -> (unidentify a,b)) $ Map.toList $ foldl f Map.empty recs
    f m v = Map.insertWith f' (identify' v) [v] m
    f' [u] ux = u:ux 

getMRT = fmap MRTlib.mrtParse BS.getContents


getMRTTableDumpV2 = fmap ( mrtFilterN [MRTPeerIndexTable, RIBIPV4Unicast, RIBIPV6Unicast] ) getMRT

getMRTUpdates = fmap ( mrtFilter BGP4MPMessageAS4 ) getMRT

identify MRTlib.MRTPeerIndexTable{..} = MRTPeerIndexTable
identify MRTlib.RIBIPV4Unicast{..} = RIBIPV4Unicast
identify MRTlib.RIBIPV6Unicast{..} = RIBIPV6Unicast
identify MRTlib.MRTUnimplemented{..} = MRTUnimplemented
identify MRTlib.BGP4MPMessageAS4{..} = BGP4MPMessageAS4
identify MRTlib.BGP4MPStateChangeAS4{..} = BGP4MPStateChangeAS4
identify MRTlib.RIBv1IPv4{..} = RIBv1IPv4
identify MRTlib.RIBv1IPv6{..} = RIBv1IPv6
identify' = fromEnum . identify

unidentify :: Int -> MRTTypes
unidentify = toEnum

data MRTTypes = MRTPeerIndexTable|RIBIPV4Unicast|RIBIPV6Unicast|MRTUnimplemented|BGP4MPMessageAS4|BGP4MPStateChangeAS4|RIBv1IPv4|RIBv1IPv6 deriving (Show,Enum,Eq,Ord)

mrtFilter t = filter ( (t ==) . identify)

mrtFilterN types = filter p where
    p mrtrec = elem (identify mrtrec) types
