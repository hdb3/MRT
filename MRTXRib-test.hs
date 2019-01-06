module Main where
import qualified Data.ByteString as BS
import MRTlib
import MRTXRib


main :: IO ()
main = do
    putStrLn "MRTXRib-test"
    f <- BS.getContents
    let mrtMsgs = mrtParse f
    putStrLn $ show (length mrtMsgs) ++ " MRT messages loaded"
    let (v4rib,v6rib) = mrtToRIB mrtMsgs
    putStrLn $ show (length v4rib) ++ " IPv4 prefixes loaded"
    putStrLn $ show (length v6rib) ++ " IPv6 prefixes loaded"
    let repRib4 = peerRepRIB v4rib
    print $ take 100 repRib4
    putStrLn "done"
