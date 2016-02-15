--Primitive password entropy calculator - entropy is calculated on the basis of normal probability distribution, not that of human-generated passwords.
--Number of available characters depends on the password itself, e.g. if only lower case letters are used, N = 26, where N is the number of available characters,
--if combination of both, lower case letters and numbers is used, N = 26 + 10.
--It's unconventional and works only for rare, target-specific cases, but the code can easily be tweaked to become useful in general.

module PasswordEntropyCalculator
(calculateEntropy,
    calculatePossibleSymbols)
where

calculateEntropy :: String -> Double
calculateEntropy "" = 0
calculateEntropy s = fromIntegral(length s) * logBase 2 (fromIntegral(calculatePossibleSymbols s) * 1.0)

calculatePossibleSymbols :: String -> Int
calculatePossibleSymbols s = foldl (\acc f -> acc + f s) 0 [addNumberFactor, addUpperFactor, addLowerFactor, addSymbolFactor]
        where
    addNumberFactor s
                | any (\c -> c `elem` s) ['0' .. '9'] = 10
                | otherwise = 0
    addLowerFactor s
                | any (\c -> c `elem` s) ['a' .. 'z'] = 26
                | otherwise = 0
    addUpperFactor s
                | any (\c -> c `elem` s) ['A' .. 'Z'] = 26
                | otherwise = 0
    addSymbolFactor s
                | any (\c -> c `elem` s) punctuationList = length punctuationList
                | otherwise = 0
                where punctuationList = ['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']