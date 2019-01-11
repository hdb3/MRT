module Main where
import qualified Data.ByteString as BS -- Strict verion import
-- import qualified Data.ByteString.Lazy as BS -- Lazy version import
import Control.Monad(mapM_)
import MRTformat
import FilterMoreSpecifics

main :: IO ()
main = do
    putStrLn "FMS-test"
    (header:mrts) <- fmap mrtParse BS.getContents
    putStrLn $ show (length mrts) ++ " MRT messages read"
    let filteredMRTMessages = FilterMoreSpecifics.filter mrts
    putStrLn $ show (length filteredMRTMessages) ++ " MRT messages after filter"
    putStrLn "done"
