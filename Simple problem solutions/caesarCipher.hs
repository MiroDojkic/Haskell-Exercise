module CaesarCipher
(encrypt)
where

encrypt :: [Char] -> Int -> [Char]
encrypt [] _ = []
encrypt (s:ss) n = (rotate s n):(encrypt ss n)

rotate :: Char -> Int -> Char
rotate s' 0 = s'
rotate s' n = rotate (next s') (n-1)

next :: Char -> Char
next 'z' = 'a'
next s' = succ s'