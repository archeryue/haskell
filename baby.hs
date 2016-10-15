import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import GeometryCal
import qualified Geometry.Sphere as Sphere

import Shapes

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100 then x else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
{-addVectors a b = (fst a + fst b, snd a + snd b)-}
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = smallPart ++ [x] ++ bigPart
    where smallPart = quickSort' (filter (<=x) xs)
          bigPart   = quickSort' (filter (>x) xs)

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

collatzChain :: (Integral a) => a -> [a]
collatzChain 1= [1]
collatzChain x
    | odd x = x : collatzChain (3 * x + 1)
    | even x = x : collatzChain (div x 2)

encode :: Int -> String -> String
encode shift msg = map chr shifted
    where ords = map ord msg
          shifted = map (+shift) ords

encode' :: Int -> String -> String
encode' shift msg =
    let ords = map ord msg
        shifted = map (+shift) ords
    in map chr shifted

{-findKey :: (Eq k) => k -> [(k,v)] -> v-}
{-findKey key xs = snd . head . filter(\(k,v) -> key == k) $ xs-}

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

{-data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)-}
data Car a b c = Car {company :: a, model :: b, year :: c} deriving (Show)
{-Car "Archer" "X1" 1992-}

{-type String = [Char]-}
type PhoneBook = [(String, String)]

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
