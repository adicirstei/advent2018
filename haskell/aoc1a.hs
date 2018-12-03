module AOC1A {-  (main) -} 
where 

main = 
  solution <$> readFile "AOC1A.txt"
  >>= print


parse :: String -> Integer
parse ('+':t) = read t
parse x = read x

solution :: String -> Integer
solution = sum . (map parse) . lines 