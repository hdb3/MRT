{-# LANGUAGE MultiWayIf,RecordWildCards #-}
module MRTlib where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base16
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import Control.Monad(unless)
import Data.IP hiding(ipv4,ipv6)
import Data.Bits
import Data.Word 

newtype HexByteString = HexByteString BS.ByteString
instance Show HexByteString where
    show (HexByteString bs) = toHex bs

newtype AS4 = AS4 Word32
instance Show AS4 where
    show (AS4 w32) = "AS" ++ show w32

newtype Timestamp = Timestamp Word32
instance Show Timestamp where
    show (Timestamp w32) = show w32 ++ "UTC"

newtype BGPid = BGPid IPv4
instance Show BGPid where
    show (BGPid ip) = show ip

toHex = Char8.unpack . Data.ByteString.Base16.encode

data MRTRecord = MRTPeerIndexTable { tdBGPID :: BGPid , tdViewName :: String, peerTable :: [MRTPeer] } 
                 | RIBIPV4Unicast { reSequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: IPv4 , reRIB :: [RIBEntry] }
                 | MRTUnimplmented { xTimestamp :: Timestamp , xType, xSubtype :: Word16 , xMessage :: HexByteString }
                 | BGP4MPMessageAS4 { msgAS4PeerAS,msgAS4LocalAS :: AS4 ,msgAS4PeerIP,msgAS4LocalIP :: IP, msgAS4Message :: BS.ByteString }
                 | BGP4MPStateChangeAS4 { scAS4PeerAS,scAS4LocalAS :: AS4 ,scAS4PeerIP,scAS4LocalIP :: IP, scOldState, scNewState:: BGP4MPState }
                 deriving Show

data MRTPeer = MRTPeer { mrtPeerBGPID :: BGPid, mrtPeerASN :: AS4 , mrtPeerIPAddress :: IP } deriving Show
data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Timestamp , reAttributes :: BS.ByteString } deriving Show
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
    ts <- timestamp
    t  <- anyWord16be
    st <- anyWord16be
    l  <- anyWord32be
    case (t,st) of
        (13,1) -> parseMRTPeerIndexTable
        --(13,2) -> parseRIBIPV4Unicast
        (16,4) -> parseBGP4MPMessageAS4 l
        (16,5) -> parseBGP4MPStateChangeAS4
        (_,_)  -> do m  <- DAB.take (fromIntegral l )
                     return $ MRTUnimplmented ts t st (HexByteString m)

timestamp = fmap Timestamp anyWord32be
as4 = fmap AS4 anyWord32be
as2 = fmap (AS4 . fromIntegral) anyWord16be
bgpid = fmap BGPid ipv4
ipv4 = fmap fromHostAddress anyWord32le
ipv6 = do
    b1 <- anyWord32be
    b2 <- anyWord32be
    b3 <- anyWord32be
    b4 <- anyWord32be
    return $ fromHostAddress6 (b1,b2,b3,b4)

parseIPv4 :: Parser IP
parseIPv4 = fmap IPv4 ipv4

parseIPv6 :: Parser IP
parseIPv6 = fmap IPv6 ipv6

parseMRTPeerIndexTable :: Parser MRTRecord
parseMRTPeerIndexTable = do
    tdBGPID <- bgpid
    l <- anyWord16be
    tdViewName <- fmap Char8.unpack $ DAB.take (fromIntegral l )
    c <- anyWord16be
    peerTable <- count (fromIntegral c) parseMRTPeer
    return $ MRTPeerIndexTable{..}
    --return $ MRTPeerIndexTable bgpid (Char8.unpack name) px
    where
    parseMRTPeer = do
        peerType <- anyWord8
        let isV6 = testBit peerType 0
            isAS4 = testBit peerType 1
        mrtPeerBGPID <- bgpid
        mrtPeerIPAddress <- if isV6 then parseIPv6 else parseIPv4
        mrtPeerASN <- if isAS4 then as4 else as2
        return $ MRTPeer{..}
    
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
        rePeerIndex <- anyWord16be
        reOriginatedTime <- timestamp
        l <- anyWord16be
        reAttributes <- DAB.take (fromIntegral l )
        return $ RIBEntry{..}
        -- return $ RIBEntry peerIndex originatedTime bgpAttributes
        -- data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Timestamp , reAttributes :: BS.ByteString } deriving Show


-- shamelessly copied from my zserv code for zvPrefixIPv4Parser
parsePrefix :: Parser (Word8,IPv4) 
parsePrefix = do
    plen <- anyWord8
    pfx <- if | plen == 0  -> return 0
              | plen < 9   -> readPrefix1Byte
              | plen < 17  -> readPrefix2Byte
              | plen < 25  -> readPrefix3Byte
              | plen < 33  -> readPrefix4Byte
    return (plen, fromHostAddress pfx)
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
        msgAS4PeerAS <- as4
        msgAS4LocalAS <- as4
        ifIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = (afi == 2)
        msgAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        msgAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        bgpMarker <- DAB.take 16
        unless ( bgpMarker == (BS.replicate 16 0xff) ) (fail "BGP marker synchronisation error")
        bgpLength <- anyWord16be
        -- unless ( fromIntegral l == 38 + bgpLength ) (fail $ "BGP length error " ++ show l ++ " " ++ show bgpLength)
        msgAS4Message <- DAB.take (fromIntegral bgpLength - 18 )
        return $ BGP4MPMessageAS4{..}  


-- parseBGP4MPStateChangeAS4 = return $ BGP4MPStateChangeAS4 0 0 0 0 BGP4MPIdle BGP4MPIdle
parseBGP4MPStateChangeAS4 = do
        scAS4PeerAS <- as4
        scAS4LocalAS <- as4
        ifIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = (afi == 2)
        scAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        scAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        scOldState <- fmap toBGP4MPState anyWord16be
        scNewState <- fmap toBGP4MPState anyWord16be
        return $ BGP4MPStateChangeAS4{..}  

toBGP4MPState :: Word16 -> BGP4MPState
toBGP4MPState 1 = BGP4MPIdle
toBGP4MPState 2 = BGP4MPConnect
toBGP4MPState 3 = BGP4MPActive
toBGP4MPState 4 = BGP4MPOpenSent
toBGP4MPState 5 = BGP4MPOpenConfirm
toBGP4MPState 6 = BGP4MPEstablished
--toBGP4MPState _ = fail "invalid BGP FSM state"

