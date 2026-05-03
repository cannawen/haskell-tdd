module RotateString796 where
import Lib

rotateString :: String -> String -> Bool
rotateString inString goalString = or [goalString == rotateBy n inString | n <- [0.. length inString]]
