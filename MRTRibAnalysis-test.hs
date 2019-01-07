module Main where
import MRTrib
import MRTRibAnalysis

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        let ribDB = getMRTRibV4 (peerTable:rib)
        comparePeerStats ribDB
        putStrLn $ showIPv4PeerTable ribDB
        putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB)
        putStrLn $ "max path count is: " ++ show (maxPathCount ribDB)
        --let ipv6PeerTable = getIPv6PeerTable pt
        --putStrLn $ showIPv6PeerTable ipv6PeerTable
        --putStrLn $ showMetrics ribDB
        let v4table = take 10 $ preFilterTable 0.02 ribDB
        putStrLn $ showIPv4PeerTable v4table
        putStrLn $ showMetrics v4table
