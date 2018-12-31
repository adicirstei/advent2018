module AOC4A where

import Data.List(sort)
import Parser

type Minute = Int
type GuardId = Int


data LogEntry 
  = Shift Minute GuardId 
  | Sleep Minute
  | Wake Minute


getData = sort . lines <$> readFile "../AOC4.txt"

-- entry :: Parser LogEntry
-- entry = shiftP <|> sleepP <|> wakeP
--   where 
--     shiftP = Shift <$> 

-- many1 = many


