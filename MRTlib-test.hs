module Main where
import qualified Data.ByteString as BS
import Control.Monad(mapM_)
import MRTlib


main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = mrtParse f
    mapM_ print mrtMsgs 
    putStrLn "done"
