module PrefixTree where

import Data.IP
import MRTPrefixes
import qualified Overlap

newtype PrefixTree a = PrefixTree (Overlap.Tree a) deriving Eq

insert :: AddrRange IPv4 -> a -> PrefixTree a -> PrefixTree a
insert p a (PrefixTree t) = PrefixTree (Overlap.insert (toPrefix p) a t) 

toList :: PrefixTree a -> [(AddrRange IPv4,a)]
toList (PrefixTree t) = map (\(p,a) -> (fromPrefix p,a)) $ Overlap.toList t

fromList :: [(AddrRange IPv4,a)] -> PrefixTree a
fromList = PrefixTree . Overlap.fromList . map (\(p,a) -> (toPrefix p,a))

fromListLS :: [(AddrRange IPv4,a)] -> PrefixTree a
fromListLS = PrefixTree . Overlap.fromListLS . map (\(p,a) -> (toPrefix p,a))

instance Show a => Show (PrefixTree a) where
    show = show . toList
