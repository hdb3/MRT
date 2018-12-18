module Main where
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Control.Monad(mapM_)
import MRTlib


main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = mrtParse f
    -- print mrtMsgs 
    mapM_ print ( mrtMsgs ) 
    putStrLn "done"
