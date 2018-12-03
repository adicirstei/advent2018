module AOC1A {-  (main) -} 
where 

main = 
  (solution . (map parse) . lines ) <$> readFile "AOC1A.txt"
  >>= print


parse :: String -> Integer
parse ('+':t) = read t
parse x = read x

solution :: [Integer] -> Integer
solution = sum 