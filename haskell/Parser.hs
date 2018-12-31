module Parser where

import Control.Applicative

newtype Parser a = P { unP :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (P fn) = P $ \s -> do
    (s', x') <- fn s
    Just (s', f x')

instance Applicative Parser where
  pure x = P $ \s -> Just (s, x)
  (P f ) <*> (P x) = P $ \s -> do
    (s', f') <- f s
    (s'', x') <- x s'
    Just (s'', f' x')

instance Alternative Parser where
  empty = P $ const Nothing
  P a <|> P b = P $ \s -> 
    case a s of 
      result@(Just _) -> result
      _ -> b s


char :: (Char -> Bool) -> Parser Char
char p = P $ \s ->
  case s of
    (c:cs) | p c -> Just (cs, c)
    _ -> Nothing


anyChar = char $ const True
thisChar c = char (== c)

parse p s = snd <$> unP p s


many1 :: Parser a -> Parser [a]
many1 p =  P $ \s -> do
  let (P fn) = many p
  (s', xs) <- fn s
  case xs of
    [] -> Nothing
    _ -> Just (s', xs)
