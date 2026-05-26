module JumpGameVII1871
  (canReachEnd) where


canReachEnd :: [Char] -> Int -> Int -> Bool
canReachEnd string minJump maxJump = 
  if string == "0" then True else or $ map jumpFn [minJump .. maxJump]

  where 
    jumpFn jump = if validJump string jump
                    then canReachEnd (drop jump string) minJump maxJump
                    else False

validJump :: [Char] -> Int -> Bool
validJump string jump = 
  if length string > jump
    then string !! jump == '0'
    else False
