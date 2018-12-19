{-# LANGUAGE MultiWayIf,RecordWildCards #-}
module MRTlib where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base16
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString(Parser,anyWord8,count)
import Data.Attoparsec.Binary
import Control.Monad(unless)
import Data.IP hiding(ipv4,ipv6)
import Data.Bits
import Data.Word 

newtype BGPMessage = BGPMessage BS.ByteString
instance Show BGPMessage where
    show (BGPMessage bs) = "BGPMessage: " ++ toHex bs

newtype BGPAttributes = BGPAttributes BS.ByteString
instance Show BGPAttributes where
    show (BGPAttributes bs) = "BGPAttributes: " ++ toHex bs

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
fromHex = fst . Data.ByteString.Base16.decode

data MRTRecord = MRTPeerIndexTable { tdBGPID :: BGPid , tdViewName :: String, peerTable :: [MRTPeer] } 
                 | RIBIPV4Unicast { re4SequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: IPv4 , re4RIB :: [RIBEntry] }
                 | RIBIPV6Unicast { re6SequenceNumber :: Word32 , re6Length :: Word8 , re6Address :: IPv6 , re6RIB :: [RIBEntry] }
                 | MRTUnimplemented { xTimestamp :: Timestamp , xType, xSubtype :: Word16 , xMessage :: HexByteString }
                 | BGP4MPMessageAS4 { msgAS4PeerAS,msgAS4LocalAS :: AS4 ,msgAS4PeerIP,msgAS4LocalIP :: IP, msgAS4Message :: BGPMessage }
                 | BGP4MPStateChangeAS4 { scAS4PeerAS,scAS4LocalAS :: AS4 ,scAS4PeerIP,scAS4LocalIP :: IP, scOldState, scNewState:: BGP4MPState }
                 deriving Show

data MRTPeer = MRTPeer { mrtPeerBGPID :: BGPid, mrtPeerASN :: AS4 , mrtPeerIPAddress :: IP } deriving Show
data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Timestamp , reAttributes :: BGPAttributes } deriving Show
data BGP4MPState = BGP4MPIdle | BGP4MPConnect | BGP4MPActive | BGP4MPOpenSent | BGP4MPOpenConfirm | BGP4MPEstablished deriving Show

mrtParse :: BS.ByteString -> [MRTRecord]
mrtParse bs = mrtParse' (parse' bs) where
    parse' bs' = DAB.feed (DAB.parse rawMRTParse bs') BS.empty
    mrtParse' (DAB.Done _ r) = r
    mrtParse' (DAB.Fail _ sx s) = error $ show (s,sx)
    mrtParse' (DAB.Partial _ ) = error "Partial unexpected!"

rawMRTParse :: Parser [MRTRecord]
rawMRTParse = DAB.many1 rawMRTParser

rawMRTParser :: Parser MRTRecord
rawMRTParser = do
    ts <- timestamp
    t  <- anyWord16be
    st <- anyWord16be
    l  <- anyWord32be
    case (t,st) of
        (13,1) -> parseMRTPeerIndexTable
        (13,2) -> parseRIBIPV4Unicast
        (13,4) -> parseRIBIPV6Unicast
        (16,4) -> parseBGP4MPMessageAS4
        (16,5) -> parseBGP4MPStateChangeAS4
        (_,_)  -> do m  <- DAB.take (fromIntegral l )
                     return $ MRTUnimplemented ts t st (HexByteString m)

bs16 = do
    l <- anyWord16be
    DAB.take (fromIntegral l )

bgpAttributes = fmap BGPAttributes bs16
bgpMessage = do
    marker <- DAB.take 16
    unless ( marker == BS.replicate 16 0xff ) (fail "BGP marker synchronisation error")
    l <- anyWord16be
    fmap BGPMessage $ DAB.take (fromIntegral l - 18)

string16 = fmap Char8.unpack bs16

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
    tdViewName <- string16
    c <- anyWord16be
    peerTable <- count (fromIntegral c) parseMRTPeer
    return MRTPeerIndexTable{..}
    where
    parseMRTPeer = do
        peerType <- anyWord8
        let isV6 = testBit peerType 0
            isAS4 = testBit peerType 1
        mrtPeerBGPID <- bgpid
        mrtPeerIPAddress <- if isV6 then parseIPv6 else parseIPv4
        mrtPeerASN <- if isAS4 then as4 else as2
        return MRTPeer{..}

parseRIBIPV4Unicast :: Parser MRTRecord
parseRIBIPV4Unicast = do
    re4SequenceNumber <- anyWord32be
    (re4Length,re4Address) <- parsePrefixV4
    re4RIB <- parseRIB
    return RIBIPV4Unicast{..}

parseRIBIPV6Unicast :: Parser MRTRecord
parseRIBIPV6Unicast = do
    re6SequenceNumber <- anyWord32be
    (re6Length,re6Address) <- parsePrefixV6
    re6RIB <- parseRIB
    return RIBIPV6Unicast{..}

parseRIB = do
    c <- fmap fromIntegral anyWord16be
    count c parseRIBEntry
    where
    parseRIBEntry = do
        rePeerIndex <- anyWord16be
        reOriginatedTime <- timestamp
        reAttributes <- bgpAttributes
        return RIBEntry{..}

-- shamelessly copied from my zserv code for zvPrefixIPv4Parser
parsePrefixV4 :: Parser (Word8,IPv4) 
parsePrefixV4 = do
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

parsePrefixV6 :: Parser (Word8,IPv6)
parsePrefixV6 = do
    pLen <- anyWord8
    let bytePLen = ((fromIntegral pLen - 1) `div` 8) + 1 -- DANGER Will Robinson (-ve arithmetic on unsigned words will wrap around badly, so rearrange at your peril...)
    bytes <- count bytePLen anyWord8
    let extendedBytes = bytes ++ replicate (16-bytePLen) 0
    return (pLen,toIPv6b $ map fromIntegral extendedBytes)

parseBGP4MPMessageAS4 = do
        msgAS4PeerAS <- as4
        msgAS4LocalAS <- as4
        _ <- anyWord16be
        -- ifIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = afi == 2
        msgAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        msgAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        msgAS4Message <- bgpMessage
        return BGP4MPMessageAS4{..}  

parseBGP4MPStateChangeAS4 = do
        scAS4PeerAS <- as4
        scAS4LocalAS <- as4
        _ <- anyWord16be
        -- ifIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = afi == 2
        scAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        scAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        scOldState <- fmap toBGP4MPState anyWord16be
        scNewState <- fmap toBGP4MPState anyWord16be
        return BGP4MPStateChangeAS4{..}  

toBGP4MPState :: Word16 -> BGP4MPState
toBGP4MPState 1 = BGP4MPIdle
toBGP4MPState 2 = BGP4MPConnect
toBGP4MPState 3 = BGP4MPActive
toBGP4MPState 4 = BGP4MPOpenSent
toBGP4MPState 5 = BGP4MPOpenConfirm
toBGP4MPState 6 = BGP4MPEstablished
toBGP4MPState _ = error "invalid BGP FSM state"

