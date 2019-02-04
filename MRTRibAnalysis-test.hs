module Main where
import Text.Printf
import Control.Monad(mapM_)
import MRTrib
import MRTRibAnalysis
import PrefixGroupMetrics
import FilterMoreSpecifics
import qualified ClusterMetrics


main :: IO ()
main = do
    mrtss <- getMRTTableDumps
    if null mrtss then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (sum $ map length mrtss) ++ " raw records read "
        let filteredMrtss = map (filter filterSlash25) mrtss
        putStr $ show (sum $ map length filteredMrtss) ++ " filtered records read "
        let ipv4PeerTable = getMRTRibs filteredMrtss
        putStrLn $ showMRTRibV4 ipv4PeerTable
        simplePeerMetrics ipv4PeerTable
        simpleGroupMetrics ipv4PeerTable

main' :: IO ()
main' = do
    (peerTable:rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        --doRIBv4 (getMRTRibV4 (peerTable:rib))
        --doRIBv6 (getMRTRibV6 (peerTable:rib))
        simpleGroupMetrics (getMRTRibV4 (peerTable:rib))
        simplePeerMetrics (getMRTRibV4 (peerTable:rib))
        --groupMetrics (getMRTRibV4 (peerTable:rib))
        --extendedMetrics (getMRTRibV4 (peerTable:rib))

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
    let validTables = preFilterTable 0.03 ribDB
    putStrLn $ "extendedMetrics (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn "(n , empty , subset , superset , multiple , multiple/partial)"
    mapM_ showExtendedMetrics (pairs validTables)
    where
    showExtendedMetrics ((pi0,p0,m0),(pi1,p1,m1)) = putStrLn $ printf "(%2d,%2d) " pi0 pi1  ++ compareRouteMapv4 m0 m1
    -- showExtendedMetrics ((pi0,p0,m0),(pi1,p1,m1)) = putStrLn $ printf "(%2d,%2d) " pi0 pi1  ++ ClusterMetrics.compareRouteMapv4 m0 m1

simplePeerMetrics ribDB = do
    let validTables = preFilterTable 0.03 ribDB
    putStrLn $ "simplePeerMetrics (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn "(peer index , peer , routes , prefixes"
    mapM_ showPeerMetrics validTables
    where
    showPeerMetrics (pi,p,m) = putStrLn $ show pi ++ " - " ++ show p ++ " - " ++ showStatsRouteMap m

simpleGroupMetrics ribDB = do
    let validTables = sortOnLength $ preFilterTable 0.05 ribDB
    putStrLn $ "simpleGroupMetrics - full tables only - (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn $ "using RIs from : " ++ show ( map (\(pi,_,_) -> pi) validTables )
    putStrLn $ ClusterMetrics.simpleCompareRouteMaps ( map (\(_,_,peer) -> peer) validTables)

    let validTables = sortOnLength ribDB
    putStrLn $ "simpleGroupMetrics - all tables - (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn $ "using RIs from : " ++ show ( map (\(pi,_,_) -> pi) validTables )
    putStrLn $ ClusterMetrics.simpleCompareRouteMaps ( map (\(_,_,peer) -> peer) validTables)

groupMetrics ribDB = do
    let validTables = sortOnLength $ preFilterTable 0.03 ribDB
    putStrLn $ "groupMetrics (sample set size " ++ show ( length validTables) ++ ")"
    mapM_ (showGroupMetrics validTables) [2.. length validTables]
    where
    showGroupMetrics t l = do
        let peerList = take l t
        putStrLn $ "using RIs from " ++ show l ++ " : " ++ show ( map (\(pi,_,_) -> pi) peerList )
        putStrLn $ ClusterMetrics.compareRouteMaps ( map (\(_,_,peer) -> peer) peerList)
