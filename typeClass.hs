-- Set of types s.t. every one of the types will implement a (set of) functions
-- Structured function overloading
data MyPair = Pair Int Int deriving Eq
data MyEqPair a = Pair' a a
instance (Eq a) => Eq(MyEqPair a) where 
    (Pair' x _) == (Pair' y _) = x == y

class YesNo a where
    yesno, notme :: a -> Bool
    notme = not . yesno
    yesno = not . notme

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True
