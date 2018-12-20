{-# LANGUAGE RecordWildCards #-}
module Main where
import MRTquest

main :: IO ()
main = mainUpdateRecords

mainGroupedRecords = do
    putStrLn "get grouped records"
    groups <- getGroupedMRT
    print $ fmap (\(a,b) -> (a, length b)) groups

mainAllRecords = do
    putStrLn "get all records"
    mrt <- getMRT
    putStrLn $ show (length mrt) ++ " records read"

mainRibRecords = do
    putStrLn "get RIB records"
    rib <- getMRTTableDumpV2
    putStrLn $ show (length rib) ++ " records read"

mainUpdateRecords = do
    putStrLn "get Update records"
    updates <- getMRTUpdates
    putStrLn $ show (length updates) ++ " records read"
