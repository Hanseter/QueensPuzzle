module QueensPuzzle
(findPlacements, placeNext) where 
import Data.List

findPlacements :: Integer -> [[Integer]]
findPlacements n = placeNext n n [[]]

placeNext :: Integer -> Integer -> [[Integer]] -> [[Integer]]
placeNext _ 0 xs = xs
placeNext length n xs = placeNext length (n-1) [pos:x | x <- xs, pos <- [((n-1)*length)..(n*length)-1], isValid length pos x]

isValid :: Integer -> Integer -> [Integer] -> Bool
isValid length n xs = (not $ lineBlocked length (lineOf length n) xs) && (not $ columnBlocked length (columnOf length n) xs) && (null $ intersect (findDiagonalFields length n) xs)

lineOf :: Integer -> Integer -> Integer
lineOf length n = n `div` length

columnOf :: Integer -> Integer -> Integer
columnOf length n = n `mod` length

lineBlocked :: Integer -> Integer -> [Integer] -> Bool
lineBlocked length line xs = any (\x -> (lineOf length x) == line) xs

columnBlocked :: Integer -> Integer -> [Integer] -> Bool
columnBlocked length column xs = any (\x -> (columnOf length x) == column) xs

diagonalBlocked :: Integer -> Integer -> [Integer] -> Bool
diagonalBlocked length n xs = False

findDiagonalFields :: Integer -> Integer -> [Integer]
findDiagonalFields length n = addDownLeft length n (addDownRight length n (addUpLeft length n (addUpRight length n [])))

addUpRight :: Integer -> Integer -> [Integer] -> [Integer]
addUpRight length n xs
            | n < 0 = xs
            | ((columnOf length n) == (lastColumn length)) = n:xs
            | otherwise = addUpRight length (n - length + 1) (n:xs)

addUpLeft :: Integer -> Integer -> [Integer] -> [Integer]
addUpLeft length n xs
            | n < 0 = xs
            | ((columnOf length n) == 0) = n:xs
            | otherwise = addUpLeft length (n - length - 1) (n:xs)

addDownRight :: Integer -> Integer -> [Integer] -> [Integer]
addDownRight length n xs
            | n > length*length = xs
            | ((columnOf length n) == (lastColumn length)) = n:xs
            | otherwise = addDownRight length (n + length + 1) (n:xs)

addDownLeft :: Integer -> Integer -> [Integer] -> [Integer]
addDownLeft length n xs
            | n > length*length = xs
            | ((columnOf length n) == 0) = n:xs
            | otherwise = addDownLeft length (n + length - 1) (n:xs)

lastColumn :: Integer -> Integer
lastColumn n = n-1