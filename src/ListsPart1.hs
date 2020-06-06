module ListsPart1 where

import Data.Vector hiding (foldr1, foldr, foldl, splitAt, head, zip, reverse, (++), concatMap, map, span, length)
import Data.List (group, groupBy)
import Prelude hiding (last, take)
import Control.Applicative
import Control.Monad

-- P1 (*) Find the last element of a list
myLastUnsafe :: [a] -> a
myLastUnsafe [] = error "Empty list"
myLastUnsafe [a] = a
myLastUnsafe (x : xs) = myLastUnsafe xs

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [el] = Just el
myLast (h : t) = myLast t

myLast' :: [a] -> a
myLast' = last . fromList

myLast'' :: [a] -> a
myLast'' = foldr1 (const id)

