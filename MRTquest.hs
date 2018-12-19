{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as Map
import qualified MRTlib


main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = MRTlib.mrtParse f
    putStrLn $ show (length mrtMsgs) ++ " read"
    let collection = collect mrtMsgs
        sizes = Map.toList $ fmap length collection
        sizes' = fmap (\(a,b) -> (unidentify a,b)) sizes
    print sizes
    print sizes'
    putStrLn "done"

collect = foldl f Map.empty 
    where
    f m v = Map.insertWith f' (identify' v) [v] m
    f' [u] ux = u:ux 

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

data MRTTypes = MRTPeerIndexTable|RIBIPV4Unicast|RIBIPV6Unicast|MRTUnimplemented|BGP4MPMessageAS4|BGP4MPStateChangeAS4|RIBv1IPv4|RIBv1IPv6 deriving (Show,Enum)
-- mrtFilter types = 
