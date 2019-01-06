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
        let pt = getPeerTable (peerTable:rib)
        let ipv4PeerTable = getIPv4PeerTable pt
            stats = getStats ipv4PeerTable
        putStrLn $ showIPv4PeerTable ipv4PeerTable
        putStrLn $ "max paths/prefixes is: " ++ show (maxima stats)
        putStrLn $ showMaxCompare $ maxCompare (maxima stats) (elems stats)
        --let ipv6PeerTable = getIPv6PeerTable pt
        --putStrLn $ showIPv6PeerTable ipv6PeerTable
        --putStrLn $ showMetrics ipv4PeerTable
        let v4table = capTable 99 $ preFilterTable 0.04 ipv4PeerTable
        putStrLn $ showIPv4PeerTable v4table
        putStrLn $ showMetrics v4table
