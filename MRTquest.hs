{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified Data.ByteString as BS
import Control.Monad(mapM_)
import qualified Data.IntMap.Strict as Map
import MRTlib


main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = mrtParse f
    putStrLn $ show (length mrtMsgs) ++ " read"
    -- mapM_ print mrtMsgs 
    let collection = collect mrtMsgs
        sizes = Map.toList $ fmap length collection
        sizes' = fmap (\(a,b) -> (mrtNames !! a,b)) sizes
    print sizes'
    putStrLn "done"

collect = foldl f Map.empty 
    where
    f m v = Map.insertWith f' (identify v) [v] m
    f' [u] ux = u:ux 
{-
identify MRTPeerIndexTable{..} = (0,"MRTPeerIndexTable")
identify RIBIPV4Unicast{..} = (1,"RIBIPV4Unicast")
identify RIBIPV6Unicast{..} = (2,"RIBIPV6Unicast")
identify MRTUnimplemented{..} = (3,"MRTUnimplemented")
identify BGP4MPMessageAS4{..} = (4,"BGP4MPMessageAS4")
identify BGP4MPStateChangeAS4{..} = (5,"BGP4MPStateChangeAS4")
identify RIBv1IPv4{..} = (6,"RIBv1IPv4")
identify RIBv1IPv6{..} = (7,"RIBv1IPv6")
-}

identify MRTPeerIndexTable{..} = 0
identify RIBIPV4Unicast{..} = 1
identify RIBIPV6Unicast{..} = 2
identify MRTUnimplemented{..} = 3
identify BGP4MPMessageAS4{..} = 4
identify BGP4MPStateChangeAS4{..} = 5
identify RIBv1IPv4{..} = 6
identify RIBv1IPv6{..} = 7

mrtNames = [ "MRTPeerIndexTable"
           ,"RIBIPV4Unicast"
           ,"RIBIPV6Unicast"
           ,"MRTUnimplemented"
           ,"BGP4MPMessageAS4"
           ,"BGP4MPStateChangeAS4"
           ,"RIBv1IPv4"
           ,"RIBv1IPv6"
           ]
