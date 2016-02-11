caesarCipher :: [Char] -> Int -> [Char]
caesarCipher [] _ = []
caesarCipher (s:ss) n = (rotate s n) : (caesarCipher ss n)

rotate :: Char -> Int -> Char
rotate s' 0 = s'
rotate s' n = rotate (next s') (n-1)

next :: Char -> Char
next 'z' = 'a'
next s' = succ s'