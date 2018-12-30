{-# LANGUAGE RecordWildCards #-}
module Main where
import MRTquest
import MRTrib

main :: IO ()
main = do
    --putStrLn "getting RIB records"
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        putStrLn $ reportPeerTable peerTable
        let map = mrtToPeerMap $ take 999999999999 rib
        putStrLn $ reportPeerMap map
        putStr $ reportDistance map
        --print map
