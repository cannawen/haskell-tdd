module JumpGameVII1871
  (canReachEnd) where

canReachEnd :: [Char] -> Int -> Int -> Bool
canReachEnd string minJump maxJump = or $ do
  jump <- [minJump .. maxJump]
  if string == "0" 
    then return True 
    else if validJump string jump
    then return $ canReachEnd (drop jump string) minJump maxJump
    else return False

validJump :: [Char] -> Int -> Bool
validJump string jump = 
  if length string > jump
    then string !! jump == '0'
    else False
