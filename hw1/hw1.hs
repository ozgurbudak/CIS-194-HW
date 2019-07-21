-- Validate credit card numbers

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0= [] 
toDigitsRev n = if n < 10 then (if n < 0 then [] else [n]) else (n `mod` 10): toDigitsRev (n `div`  10) 

toDigits :: Integer -> [Integer]
toDigits 0= []
toDigits n = if n < 10  then (if n < 0 then [] else [n]) else (toDigits (n `div`  10))  ++ [(n `mod` 10)]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:[])= (2*x):[y]
doubleEveryOther (x:y) = (if odd (length y) then (2*x) else x):(doubleEveryOther y)


sumDigits :: [Integer] -> Integer
sumDigits []=0
sumDigits (x:[]) = if x < 10 then x else sum (toDigits x)
sumDigits (x:y) = sumDigits (toDigits x) + sumDigits y


validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0



-- The Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 2 x y z = [(x,z), (x,y), (z,y)] 
hanoi n x y z = (hanoi (n-1) x z y) ++ [(x,y)] ++ (hanoi (n-1) z y x)