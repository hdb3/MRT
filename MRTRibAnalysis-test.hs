module Main where
import MRTquest
import MRTrib
import MRTRibAnalysis
import Data.Array.IArray(elems)

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        let ribDB = getMRTRibV4 (peerTable:rib)
            stats = getStats ribDB
        putStrLn $ showIPv4PeerTable ribDB
        putStrLn $ "max paths/prefixes is: " ++ show (maxima stats)
        putStrLn $ showMaxCompare $ maxCompare (maxima stats) (map snd stats)
        --let ipv6PeerTable = getIPv6PeerTable pt
        --putStrLn $ showIPv6PeerTable ipv6PeerTable
        putStrLn $ showMetrics ribDB
        let v4table = take 99 $ preFilterTable 0.04 ribDB
        putStrLn $ showIPv4PeerTable v4table
        putStrLn $ showMetrics v4table
