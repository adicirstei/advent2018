module AOC2A where

import qualified Data.Map as Map


solution :: [String] -> Integer
solution l = (sum $ map two l)  * (sum $ map three l)

counts :: [Char] -> Map.Map Char Integer
counts = loop Map.empty 
  where 
    loop m [] = m
    loop m (h:t) = loop (Map.insert h (1 + Map.findWithDefault 0 h m) m) t




two :: String -> Integer
two s = if Map.size twos > 0 then 1
        else 0
    where 
      twos =  Map.filter (==2) $ counts s



three :: String -> Integer
three s = if Map.size threes > 0 then 1
        else 0
    where 
      threes =  Map.filter (==3) $ counts s