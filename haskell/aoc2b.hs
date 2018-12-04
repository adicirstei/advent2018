module AOC2B where

import qualified Data.Map as Map


solution :: [String] -> String
solution l = common $ head [(s1,s2) | s1 <- l, s2 <-l, diff s1 s2 == 1 ]


common (s1,s2) = map fst $ filter (\(c1,c2) -> c1 == c2) $ zip s1 s2


diff :: String -> String -> Int
diff s1 s2 = loop 0 s1 s2
  where 
    loop a [] x = a + length x
    loop a x [] = a + length x
    loop a (h1:t1) (h2:t2) = loop (a + (if h1==h2 then 0 else 1)) t1 t2


getData = lines <$> readFile "../AOC2.txt"