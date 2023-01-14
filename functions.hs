module SampleFunctions (
    addMe, whatAge, whatAgeCase, 
    factorial, isOdd, showListItems,
    getFirstChar, areEqualStrings, getFnAddOne, 
    doubleEvenNumbers
) where

addMe :: Int -> Int -> Int
addMe x y = x + y

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _ = ":)" -- could replace _ with any other names

-- Or use case statement
whatAgeCase :: Int -> String
whatAgeCase n = case n of
    16 -> "You can drive"
    18 -> "You can vote"
    21 -> "You're an adult"
    _ -> ":)"

-- Recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

-- x:y
-- x:xs if we don't know how many elements are in the list. 
showListItems :: [Int] -> String
showListItems [] = "Your list is empty"
showListItems (x: []) = "Your list starts with " ++ show x
showListItems (x: y: []) = "Your list contains " ++ show x ++ " followed by " ++ show y
showListItems (x: xs) = "First item in your list is " ++ show x ++ " and then " ++ show xs

-- The as pattern
getFirstChar :: String -> String
getFirstChar [] = "The string is empty"
getFirstChar all@(x: xs) = "The first char of " ++ show all ++ " is " ++ [x]

-- Recursion
areEqualStrings :: String -> String -> Bool
areEqualStrings [] [] = True
areEqualStrings (x:xs) (y:ys) = x == y && areEqualStrings xs ys

-- Function as parameter / return value
getFnAddOne :: (Int -> Int) -> (Int -> Int)
getFnAddOne fn x = fn x + 1

-- Lambda
dblOneToTen = map (\x -> x * 2 + 2) [1..10]

-- If statement
doubleEvenNumbers :: Int -> Int
doubleEvenNumbers x = 
    if (x `mod` 2 /= 0)
        then x 
        else x * 2

-- Where
getGrade :: Double -> Double -> String
getGrade score total
    | ratio < 0.6 = "Fail"
    | ratio < 0.8 = "Pass"
    | ratio < 0.9 = "Good"
    | otherwise = "A+"
    where ratio = score / total
