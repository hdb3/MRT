{-# LANGUAGE RecordWildCards #-}
module Main where
import MRTquest
import MRTrib

main :: IO ()
main = do
    putStrLn "getting RIB records"
    rib <- getMRTTableDumpV2
    if null rib then
        putStrLn "no RIB records found in file"
    else do
        putStrLn $ show (length rib) ++ " records read"
        let map = mrtToPeerMap $ take 10 rib
        print map
