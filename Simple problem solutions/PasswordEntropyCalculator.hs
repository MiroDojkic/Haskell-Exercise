module PasswordEntropyCalculator
(calculateEntropy,
    calculatePossibleSymbols,
    addNumberFactor,
    addLowerFactor,
    addUpperFactor,
    addSymbolFactor)
where

calculateEntropy :: String -> Double
calculateEntropy "" = error "Entry missing."
calculateEntropy s = fromIntegral(length s) * logBase 2 (fromIntegral(calculatePossibleSymbols s) * 1.0)

calculatePossibleSymbols :: String -> Int
calculatePossibleSymbols s = sum $ map ($ s) [addNumberFactor, addUpperFactor, addLowerFactor, addSymbolFactor]

addNumberFactor :: String -> Int
addNumberFactor s
            | any (\c -> c `elem` s) ['0' .. '9'] = 10
            | otherwise = 0

addLowerFactor :: String -> Int
addLowerFactor s
            | any (\c -> c `elem` s) ['a' .. 'z'] = 26
            | otherwise = 0

addUpperFactor :: String -> Int
addUpperFactor s
            | any (\c -> c `elem` s) ['A' .. 'Z'] = 26
            | otherwise = 0

addSymbolFactor :: String -> Int
addSymbolFactor s
            | any (\c -> c `elem` s) punctuationList = length punctuationList
            | otherwise = 0
            where punctuationList = ['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']