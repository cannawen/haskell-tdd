module RotatedDigits788 where
import Data.Function

rotateChar :: Char -> Maybe Char
rotateChar '0' = Just '0'
rotateChar '1' = Just '1'
rotateChar '2' = Just '5'
rotateChar '3' = Nothing
rotateChar '4' = Nothing
rotateChar '5' = Just '2'
rotateChar '6' = Just '9'
rotateChar '7' = Nothing
rotateChar '8' = Just '8'
rotateChar '9' = Just '6'

isValid :: Int -> Bool
isValid int = rotatedInt /= Nothing && rotatedInt /= Just integerString
    where 
        integerString = show int
        rotatedInt = map rotateChar integerString & sequence

rotateUntil :: Int -> Int
rotateUntil n = map isValid [0..n] & filter id & length

isSame :: Char -> Bool
isSame '0' = True
isSame '1' = True
isSame '2' = False
isSame '3' = False
isSame '4' = False
isSame '5' = False
isSame '6' = False
isSame '7' = False
isSame '8' = True
isSame '9' = False

isRotatable :: Char -> Bool
isRotatable '0' = True
isRotatable '1' = True
isRotatable '2' = True
isRotatable '3' = False
isRotatable '4' = False
isRotatable '5' = True
isRotatable '6' = True
isRotatable '7' = False
isRotatable '8' = True
isRotatable '9' = True

isValid' :: Int -> Bool
isValid' int = rotatable && not same
    where 
        intString = show int
        rotatable = map isRotatable intString & and
        same = map isSame intString & and
