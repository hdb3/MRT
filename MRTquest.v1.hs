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
    f m v = Map.insertWith f' (identify v) [v] m
    f' [u] ux = u:ux 

identify MRTlib.MRTPeerIndexTable{..} = 0
identify MRTlib.RIBIPV4Unicast{..} = 1
identify MRTlib.RIBIPV6Unicast{..} = 2
identify MRTlib.MRTUnimplemented{..} = 3
identify MRTlib.BGP4MPMessageAS4{..} = 4
identify MRTlib.BGP4MPStateChangeAS4{..} = 5
identify MRTlib.RIBv1IPv4{..} = 6
identify MRTlib.RIBv1IPv6{..} = 7
unidentify n = mrtNames !! n

mrtNames = [ "MRTPeerIndexTable"
           ,"RIBIPV4Unicast"
           ,"RIBIPV6Unicast"
           ,"MRTUnimplemented"
           ,"BGP4MPMessageAS4"
           ,"BGP4MPStateChangeAS4"
           ,"RIBv1IPv4"
           ,"RIBv1IPv6"
           ]

data MRTTypes = MRTPeerIndexTable|RIBIPV4Unicast|RIBIPV6Unicast|MRTUnimplemented|BGP4MPMessageAS4|BGP4MPStateChangeAS4|RIBv1IPv4|RIBv1IPv6 deriving (Show,Enum)
-- mrtFilter types = 
