module AOC1B 
where 

main = 
  (solution . (map parse) . lines ) <$> readFile "AOC1A.txt"
  >>= print


parse :: String -> Integer
parse ('+':t) = read t
parse x = read x

twice :: [Integer] -> [Integer] -> Maybe Integer
twice vis [] = Nothing
twice vis (h:t) = 
  if h `elem` vis then Just h
  else twice (h:vis) t



solution :: [Integer] -> Maybe Integer
solution  = (twice []) . (scanl (+) 0) . cycle