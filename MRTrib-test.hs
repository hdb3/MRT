module Main where
import MRTquest
import MRTrib

oldcode = False
main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        --putStrLn $ reportPeerTable peerTable
        let map = mrtToPeerMap rib
        --putStrLn $ reportPeerMap map
        let pt = getPeerTable (peerTable:rib)
        -- putStrLn $ showPeerTable pt
        let ipv4PeerTable = getIPv4PeerTable pt
        putStrLn $ showIPv4PeerTable ipv4PeerTable
        let ipv6PeerTable = getIPv6PeerTable pt
        putStrLn $ showIPv6PeerTable ipv6PeerTable
