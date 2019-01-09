module Main where
import Control.Monad(mapM_)
import MRTrib
import MRTRibAnalysis
import PrefixGroupMetrics

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        --doRIBv4 (getMRTRibV4 (peerTable:rib))
        --doRIBv6 (getMRTRibV6 (peerTable:rib))
        extendedMetrics (getMRTRibV4 (peerTable:rib))

doRIBv4 ribDB = do
        comparePeerStats ribDB
        putStrLn $ showMRTRibV4 ribDB
        putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB)
        putStrLn $ "max path count is: " ++ show (maxPathCount ribDB)
        let v4table = take 10 $ preFilterTable 0.02 ribDB
        putStrLn $ showMRTRibV4 v4table
        putStrLn $ showMetrics v4table

doRIBv6 ribDB6 = do
        comparePeerStats ribDB6
        putStrLn $ showMRTRibV6 ribDB6
        putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB6)
        putStrLn $ "max path count is: " ++ show (maxPathCount ribDB6)
        let v6table = take 10 $ preFilterTable 0.02 ribDB6
        putStrLn $ showMRTRibV6 v6table
        putStrLn $ showMetrics v6table

extendedMetrics ribDB = do
    putStrLn "extendedMetrics"
    putStrLn "(n , empty , subset , superset , multiple , multiple/partial)"
    let abx = pairs $ take 99 $ preFilterTable 0.02 ribDB
    mapM_ pairExtendedMetrics abx
    where
    pairExtendedMetrics ((pi0,p0,m0),(pi1,p1,m1)) = do
    putStrLn $ show (pi0,pi1) ++ " - " ++ compareRouteMapv4 m0 m1
