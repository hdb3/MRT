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
        putStrLn $ showMRTRibV4 ribDB
        putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB)
        putStrLn $ "max path count is: " ++ show (maxPathCount ribDB)
        let v4table = take 10 $ preFilterTable 0.02 ribDB
        putStrLn $ showMRTRibV4 v4table
        putStrLn $ showMetrics v4table

        let ribDB6 = getMRTRibV6 (peerTable:rib)
        comparePeerStats ribDB6
        putStrLn $ showMRTRibV6 ribDB6
        putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB6)
        putStrLn $ "max path count is: " ++ show (maxPathCount ribDB6)
        let v6table = take 10 $ preFilterTable 0.02 ribDB6
        putStrLn $ showMRTRibV6 v6table
        putStrLn $ showMetrics v6table
