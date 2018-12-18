module Main where
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import MRTlib


main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = parse' f
    print mrtMsgs 
    putStrLn "done"
    where
    parse' bs = feed (parse rawMRTParse bs) BS.empty
