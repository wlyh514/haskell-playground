addMe :: Int -> Int -> Int
addMe x y = x + y

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _ = ":)" -- could replace _ with any other names

-- Recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

-- x:y
showListItems :: [Int] -> String
showListItems [] = "Your list is empty"
showListItems (x: []) = "Your list starts with " ++ show x
showListItems (x: y: []) = "Your list contains " ++ show x ++ " followed by " ++ show y
showListItems (x: xs) = "First item in your list is " ++ show x ++ " and then " ++ show xs

-- The as pattern
getFirstChar :: String -> String
getFirstChar [] = "The string is empty"
getFirstChar all@(x: xs) = "The first char of " ++ show all ++ " is " ++ [x]