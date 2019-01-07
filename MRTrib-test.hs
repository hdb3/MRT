module Main where
import MRTrib

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        let ipv4PeerTable = getMRTRibV4 (peerTable:rib)
        putStrLn $ showMRTRibV4 ipv4PeerTable
        let ipv6PeerTable = getMRTRibV6 (peerTable:rib)
        putStrLn $ showMRTRibV6 ipv6PeerTable
