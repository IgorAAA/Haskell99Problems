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

-- P2 (*) Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne [] = error "Empty List"
lastButOne [a] = error "List length < 2"
lastButOne [x, _] = x
lastButOne (_ : xs) = lastButOne xs

lastButOne' :: [a] -> a
lastButOne' = fst . foldl (\(a, b) x -> (b, x)) (err1, err2)
  where
    err1 = error "Empty list"
    err2 = error "list length < 2"

-- P3 (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Int -> Vector a -> a
elementAt n = last . take n

elementAt' :: Int -> [a] -> a
elementAt' _ [] = error "Empty list"
elementAt' n (x : xs)  = fst $ foldl (\(acc, counter) x -> if counter == n then (x, counter + 1) else (acc, counter + 1) ) (x, 2) xs

-- P4 (*) Find the number of elements of a list
findSize :: [a] -> Int
findSize = foldr (\_ acc -> acc + 1) 0

findSize' :: [a] -> Int
findSize' xs = fst $ last $ fromList $ zip [1 .. ] xs

-- P5 (*) Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\acc el -> el : acc) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- P6 (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = (==) <*> reverse

isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' = liftM2 (==) id reverse

-- P7 (**) Flatten a nested list structure. Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = concatMap flatten' xs

-- P8 (**) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = fst $ foldr (\el (acc, lastEl) -> if el == lastEl then (acc, el) else (el : acc, el)) ([x], x) xs

compress' :: Eq a => [a] -> [a]
compress' = map head . group

-- P9 (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack = group

pack' :: Eq a => [a] -> [[a]]
pack' = groupBy (==)

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x : xs) = (x : first) : pack'' rest
                  where (first, rest) = span (== x) xs

-- P10 (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\xs -> (length xs, head xs)) $ pack xs
