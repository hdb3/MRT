{-# LANGUAGE RecordWildCards #-}
module Main where
import MRTquest
import MRTrib

main :: IO ()
main = do
    (peerTable,rib) <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (length rib) ++ " records read "
        putStrLn $ reportPeerTable peerTable
        let map = mrtToPeerMap rib
            discMap = getDiscPeerMap map
        putStrLn $ "keys: " ++ show (keys discMap)
        putStrLn $ reportPeerMap map
        putStr $ reportDistance map
