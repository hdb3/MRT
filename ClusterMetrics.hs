module ClusterMetrics where

import Text.Printf
import qualified Data.IntMap.Strict as Map
import Data.Maybe(isJust,catMaybes)
import Data.List(sort,group,uncons)
import MRTrib
import GroupRIBReport

fromRouteMapv4 :: RouteMapv4 -> [[IP4PrefixHash]]
fromRouteMapv4 = map (map v4hash) . Map.elems . Map.map snd

compareRouteMaps :: [RouteMapv4] -> String
compareRouteMaps = groupRIBReport . take 100 . interlace . map fromRouteMapv4
    where
    interlace :: [[a]] -> [a]
    interlace [] = []
    interlace ax = heads ++ interlace tails
        where
        (heads,tails) = unzip $ catMaybes $ map uncons ax

compareRouteMapv4 :: RouteMapv4 -> RouteMapv4 -> String
compareRouteMapv4 a b = compareRouteMaps [ a , b ]
