module MRTlib where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
--import Control.Applicative
--import Control.Monad(when)
--import Data.IP
--import Data.Bits
import Data.Word 

-- Records which the parser will generate......

data MRTPeerIndexTable = MRTPeerIndexTable { tdBGPID :: Word32 , tdViewName :: String, peerTable :: [MRTPeer] } 
data MRTPeer = MRTPeer { mrtPeerBGPIP , mrtPeerASN , mrtPeerIPAddress :: Word32 }
data RIBIPV4Unicast = RIBIPV4Unicast { reSequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: Word32 , reRIB :: [RIBEntry] }
data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Word32 , reAttributes :: BS.ByteString }
data MRTUnimplmented = MRTUnimplmented { xTimestamp :: Word32 , xType, xSubtype :: Word16 , xMessage :: BS.ByteString }
instance Show MRTUnimplmented where
    show (MRTUnimplmented ts t st m) = "MRTUnimplmented { ts: " ++ show ts ++ " type: " ++ show t ++ " subtype: " ++ show st ++ " length: " ++ show (BS.length m)

rawMRTParse :: Parser [MRTUnimplmented]
rawMRTParse = many1 rawMRTParser

rawMRTParser :: Parser MRTUnimplmented
rawMRTParser = do
    ts <- anyWord32be
    t  <- anyWord16be
    st <- anyWord16be
    l  <- anyWord32be
    m  <- DAB.take (fromIntegral l )
    return $ MRTUnimplmented ts t st m
