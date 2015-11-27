{-# OPTIONS_GHC -Wall #-}

toDigits    :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    | x < 0 = []
    | x `mod` 10 == x = [x]
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev rev
    | rev < 0 = []
    | rev `div` 10 == 0 = [rev]
    | otherwise = (rev `mod` 10 : toDigitsRev (rev `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
    | length(zs) `mod` 2 == 0 = x*2:y:doubleEveryOther(zs)
    | otherwise = x:2*y:doubleEveryOther(zs)

--sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:zs) = (sum $ toDigits(x)) + sumDigits(zs)

--validate only when sum of digits is multiple of 10
validate :: Integer -> Bool
validate x
    | sumDigits(doubleEveryOther(toDigits $ x)) `mod` 10 == 0 = True
    | otherwise = False

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a