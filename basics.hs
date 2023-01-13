-- Comments
{-
Multiline comments

To load a module into ghci: 
ghci
:l <filename>
:r to refresh
-}

import Data.List
import System.IO

---- Data types

-- Int -2^63 to 2^63
maxInt = maxBound :: Int
minInt = minBound :: Int 

-- Integer unbounded
myInteger = 114514 :: Integer 
-- Float / Double : accurate up to 11 decimal points
myDouble = 114.514 :: Double
-- Bool True False
myBool = True :: Bool 

-- Char
myChar = 'c' :: Char

-- Tuple

---- Math functions
sumOfNums = sum [1..100]

-- Results in a double
divEx = 5 / 4
modEx = mod 5 4
modExMid = 5 `mod` 4

int9 = 9 :: Int 
sqrt9 = sqrt( fromIntegral int9 )

piVal = pi
ePow9 = exp 9
logOf9 = log 9
cubeOf9 = 9 ** 3
truncateVal = truncate 9.99 
roundVal = round 9.99 
ceilVal = ceiling 9.99 
floorVal = floor 9.99 

-- and sin, cos, tan, sinh etc. 
notVal = not myBool 

---- Lists
primeNumbers = [2, 3, 5, 7]
morePrimes = primeNumbers ++ [11, 13, 17, 19]
favNums = 5 : 1 : 4 : myInteger : []
twoDList = [[1, 1, 4], [5, 1, 4]]
pushFront = 2 : favNums
lenFavNums = length favNums
revFavnums = reverse favNums
isListEmpty = null favNums
-- indexing
firstPrime = head morePrimes
secondPrime = morePrimes !! 1
lastPrime = tail morePrimes
exceptLastPrime = init morePrimes

firstThreePrimes = take 3 morePrimes
otherPrimes = drop 3 morePrimes

sevenIsPrime = 7 `elem` morePrimes
maxPrime = maximum morePrimes
minPrime = minimum morePrimes
prodPrimes = product morePrimes
evenList = [2, 4 .. 20]
charList = ['A', 'C' .. 'Z']
infList = [10, 20 ..]
lenInf = length infList
tenThrees = replicate 10 3
cycleList = take 10 (cycle [0,1,2,3,4])
listTimesTwo = [x * 2 | x <- favNums]
filtered = [x | x <- favNums, x < 10]
sorted = sort favNums
sumOfLists = zipWith (+) [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]
favLessThan10 = filter (<10) favNums
evensupTo20 = takeWhile (<= 20) [2, 4..]
fold = foldr (-) 1 [2, 3, 4, 5]

---- Tuples
stanley = ("Stanley", 427) -- Tuple pair
stanleyName = fst stanley
stanleyRoom = snd stanley
names = ["A", "B", "Egg"]
rooms = [5, 1, 4]
namesNRooms = zip names rooms

---- Custom types
-- Enums
data Campus = UTSG | UTSC | UTM 

-- Type Classes

instance Eq Campus where 
    UTSG == UTSG = True
    UTSC == UTSC = True
    UTM == UTM = True
    _ == _ = False

instance Show Campus where
    show UTSC = "University of Toronto Scarborough"
    show UTSG = "University of Toronto St.George"
    show UTM = "University of Toronto Mississauga"

class EqAndShow a where
    areEqual :: a -> a -> Bool
    toString :: a -> String

data Employee = Employee String Int
    deriving Show
e427 = Employee "Stanley" 427

data Shape = Circle Double Double Double | Rectangle Double Double Double Double
    deriving Show

area :: Shape -> Double
area ((Circle _ _ r)) = pi * r ^ 2
area ((Rectangle x y x2 y2)) = abs (x2 - x) * abs (y2 - y)

-- Type classes
data Student = Student {
    name :: String,
    campus :: Campus,
    studentNumber :: Int
} deriving (Eq, Show)

student1 = Student {
    name = "a",
    campus = UTSC, 
    studentNumber = 514
}

student2 = Student {
    name = "a",
    campus = UTSC, 
    studentNumber = 514
}

---- . and $
sumVal = putStrLn (show(1 + 2))
sumVal2 = putStrLn . show $ 1 + 2 -- equv