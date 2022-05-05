module Cipher where
import Data.Char

shift_pos :: Char -> Int
shift_pos c
  | isUpper c = (ord c) - 65
  | otherwise = (ord c) - 97

caesar :: Int -> [Char] -> [Char]
caesar shift str = map f str
  where
    f :: Char -> Char
    f c = chr ((mod ((shift_pos c) + shift) 26) + ((-(shift_pos c)) + (ord c)))

uncaesar :: Int -> [Char] -> [Char]
uncaesar shift str = map f str
  where
    f :: Char -> Char
    f c = chr ((mod ((shift_pos c) - shift) 26) + ((-(shift_pos c)) + (ord c)))
