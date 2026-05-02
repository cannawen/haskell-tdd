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