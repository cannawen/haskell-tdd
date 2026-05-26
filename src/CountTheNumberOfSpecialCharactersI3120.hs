module CountTheNumberOfSpecialCharactersI3120
    (count) where

import qualified Data.Set as Set
import Data.Char (isUpper, isLower, toUpper)

count word = 
  Set.size $ Set.intersection lower upper
  
  where 
    (lower, upper) = 
      foldl 
      (\(lower, upper) c -> 
        if isUpper c 
          then (lower, Set.insert c upper)
          else (Set.insert (toUpper c) lower, upper)) 
      (Set.empty, Set.empty) 
      word
