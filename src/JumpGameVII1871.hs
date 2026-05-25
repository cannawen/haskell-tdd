module JumpGameVII1871
  (canReachEnd) where

-- canReachEnd string minJump maxJump = validJump string minJump

canReachEnd :: [Char] -> Int -> Int -> Bool
canReachEnd string minJump maxJump = 
  if string == ""
    then True
    else if validJump string minJump
    then canReachEnd (drop (succ minJump) string) minJump maxJump
    else False

validJump :: [Char] -> Int -> Bool
validJump string jump = string !! jump == '0'

