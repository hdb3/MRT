module Main where
import MRTquest
import MRTrib
import MRTRibAnalysis

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        let pt = getPeerTable (peerTable:rib)
        let ipv4PeerTable = getIPv4PeerTable pt
        putStrLn $ showIPv4PeerTable ipv4PeerTable
        --let ipv6PeerTable = getIPv6PeerTable pt
        --putStrLn $ showIPv6PeerTable ipv6PeerTable
        putStrLn $ showMetrics ipv4PeerTable
