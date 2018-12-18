{-# LANGUAGE MultiWayIf,RecordWildCards #-}
module MRTlib where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base16
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
--import Control.Applicative
--import Control.Monad(when)
--import Data.IP
import Data.Bits
import Data.Word 

-- Records which the parser will generate......
newtype HexByteString = HexByteString BS.ByteString
instance Show HexByteString where
    show (HexByteString bs) = toHex bs

toHex = Char8.unpack . Data.ByteString.Base16.encode
--toHex' = toHex . BS.toStrict


data MRTRecord = MRTPeerIndexTable { tdBGPID :: Word32 , tdViewName :: String, peerTable :: [MRTPeer] } 
                 | RIBIPV4Unicast { reSequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: Word32 , reRIB :: [RIBEntry] }
                 | MRTUnimplmented { xTimestamp :: Word32 , xType, xSubtype :: Word16 , xMessage :: HexByteString }
                 | BGP4MPMessageAS4 { msgAS4PeerAS,msgAS4LocalAS,msgAS4PeerIP,msgAS4LocalIP :: Word32, msgAS4Message :: BS.ByteString } -- this is an IPv4 only object
                 | BGP4MPStateChangeAS4 { scAS4PeerAS,scAS4LocalAS,scAS4PeerIP,scAS4LocalIP :: Word32, scOldState, scNewState:: BGP4MPState } -- this is an IPv4 only object
                 deriving Show

data MRTPeer = MRTPeer { mrtPeerBGPID , mrtPeerASN , mrtPeerIPAddress :: Word32 } deriving Show
data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Word32 , reAttributes :: BS.ByteString } deriving Show
data BGP4MPState = BGP4MPIdle | BGP4MPConnect | BGP4MPActive | BGP4MPOpenSent | BGP4MPOpenConfirm | BGP4MPEstablished deriving Show

mrtParse :: BS.ByteString -> [MRTRecord]
mrtParse bs = mrtParse' (parse' bs) where
    parse' bs = feed (parse rawMRTParse bs) BS.empty
    mrtParse' (Done _ r) = r
    mrtParse' (Fail _ sx s) = fail $ show (s,sx)

rawMRTParse :: Parser [MRTRecord]
rawMRTParse = many1 rawMRTParser

rawMRTParser :: Parser MRTRecord
rawMRTParser = do
    ts <- anyWord32be
    t  <- anyWord16be
    st <- anyWord16be
    l  <- anyWord32be
    -- m  <- take (fromIntegral l )
    case (t,st) of
        (13,1) -> parseMRTPeerIndexTable
        (13,2) -> parseRIBIPV4Unicast
        (16,4) -> parseBGP4MPMessageAS4 l
        (16,5) -> parseBGP4MPStateChangeAS4
        (_,_)  -> do m  <- DAB.take (fromIntegral l )
                     return $ MRTUnimplmented ts t st (HexByteString m)

parseMRTPeerIndexTable :: Parser MRTRecord
--parseMRTPeerIndexTable m = return $ MRTPeerIndexTable 0 "" []
parseMRTPeerIndexTable = do
    bgpid <- anyWord32be
    l <- anyWord16be
    name <- DAB.take (fromIntegral l )
    c <- anyWord16be
    px <- count (fromIntegral c) parseMRTPeer
    return $ MRTPeerIndexTable bgpid (Char8.unpack name) px
    where
    parseMRTPeer = do
        word8 0x2 -- IPv4 + AS4
        bgpid <- anyWord32be
        ip    <- anyWord32be
        as   <- anyWord32be
        return $ MRTPeer bgpid ip as
    
--parseRIBIPV4Unicast = return $ RIBIPV4Unicast 0 0 0 []
parseRIBIPV4Unicast :: Parser MRTRecord
parseRIBIPV4Unicast = do
    sn <- anyWord32be
    (plen,pfx) <- parsePrefix
    c <- anyWord16be
    peers <- count (fromIntegral c) parseRIBEntry
    return $ RIBIPV4Unicast sn plen pfx peers 
    where
    parseRIBEntry = do
        peerIndex <- anyWord16be
        originatedTime <- anyWord32be
        l <- anyWord16be
        bgpAttributes <- DAB.take (fromIntegral l )
        return $ RIBEntry peerIndex originatedTime bgpAttributes


-- shamelessly copied from my zserv code for zvPrefixIPv4Parser
parsePrefix :: Parser (Word8,Word32) 
parsePrefix = do
    plen <- anyWord8
    pfx <- if | plen == 0  -> return 0
              | plen < 9   -> readPrefix1Byte
              | plen < 17  -> readPrefix2Byte
              | plen < 25  -> readPrefix3Byte
              | plen < 33  -> readPrefix4Byte
    return (plen,pfx)
    where
        readPrefix1Byte = do
            b0 <- anyWord8
            return (unsafeShiftL (fromIntegral b0) 24)
        readPrefix2Byte = do
            b0 <- anyWord16be
            return (unsafeShiftL (fromIntegral b0) 16)
        readPrefix3Byte = do
            b0 <- anyWord16be
            b1 <- anyWord8
            return (unsafeShiftL (fromIntegral b1) 8 .|. unsafeShiftL (fromIntegral b0) 16)
        readPrefix4Byte = anyWord32be

--parseBGP4MPMessageAS4 = return $ BGP4MPMessageAS4 0 0 0 0 BS.empty
parseBGP4MPMessageAS4 l = do
        msgAS4PeerAS <- anyWord32be
        msgAS4LocalAS <- anyWord32be
        ifIndex <- anyWord16be
        word16be 0x1 -- IPv4 (IPv6 == 2)
        msgAS4PeerIP <- anyWord32be
        msgAS4LocalIP <- anyWord32be
        msgAS4Message <- DAB.take (fromIntegral l - 20 )
        return $ BGP4MPMessageAS4{..}  


-- parseBGP4MPStateChangeAS4 = return $ BGP4MPStateChangeAS4 0 0 0 0 BGP4MPIdle BGP4MPIdle
parseBGP4MPStateChangeAS4 = do
        scAS4PeerAS <- anyWord32be
        scAS4LocalAS <- anyWord32be
        ifIndex <- anyWord16be
        word16be 0x1 -- IPv4 (IPv6 == 2)
        scAS4PeerIP <- anyWord32be
        scAS4LocalIP <- anyWord32be
        scOldState <- fmap toBGP4MPState anyWord16be
        scNewState <- fmap toBGP4MPState anyWord16be
        return $ BGP4MPStateChangeAS4{..}  
                 -- BGP4MPStateChangeAS4 { scAS4PeerAS , scAS4LocalAS , scAS4PeerIP , scAS4LocalIP :: Word32 ,  scOldState ,  scNewState:: BGP4MPState } -- this is an IPv4 only object
toBGP4MPState :: Word16 -> BGP4MPState
toBGP4MPState 1 = BGP4MPIdle
toBGP4MPState 2 = BGP4MPConnect
toBGP4MPState 3 = BGP4MPActive
toBGP4MPState 4 = BGP4MPOpenSent
toBGP4MPState 5 = BGP4MPOpenConfirm
toBGP4MPState 6 = BGP4MPEstablished

