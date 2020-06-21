module ListsPart1Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ListsPart1
import Data.Vector hiding (last, (++), length, reverse, concat, concatMap)
import Control.Monad
                         
main' :: IO ()
main' = hspec spec

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

genN :: Int -> Gen Int
genN n = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0 && x <= n)

genListAndPos :: Gen ([Int], Int) 
genListAndPos = do
  xs <- listOf1 genPos
  n <- genN $ length xs
  return (xs, n)
  
genList :: Gen [Int]
genList = listOf ((arbitrary :: Gen Int) `suchThat` (>= 0))

spec :: Spec
spec = do
  describe "myLastUnsafe" $ do
    it "finds the last element of a list" $ do
      myLastUnsafe [1,2,3,4,5] `shouldBe` 5
      myLast'' [1,2,3,4,5] `shouldBe` 5
    it "throws an exception if used with an empty list" $ do
      evaluate (myLastUnsafe []) `shouldThrow` anyException
      evaluate (myLast'' []) `shouldThrow` anyException

  describe "myLast" $ do
    it "finds the last element of a list" $
      myLast [1,2,3,4,5] `shouldBe` Just 5
    it "return Nothigh if used with an empty list" $ 
      (myLast [] :: Maybe Int) `shouldBe` Nothing
    it "finds the last element of a list from props" $ 
      forAll (listOf1 (arbitrary :: Gen Int)) (\xs -> myLast xs == Just (last xs))  

  describe "lastButOne" $ do
    it "find the last but one element of a list" $ do
      lastButOne [1,2,3,4,5] `shouldBe` 4
      lastButOne' [1,2,3,4,5] `shouldBe` 4
    it "throws an exception if used with an empty list" $ do
      evaluate (lastButOne []) `shouldThrow` anyException
      evaluate (lastButOne' []) `shouldThrow` anyException
      
  describe "elementAt" $ 
    it "finds k-th element of a list" $ 
      forAll genListAndPos (\(s, n) -> elementAt n (fromList s) == unsafeIndex (fromList s) (n - 1)) 
   
  describe "elementAt'" $ do 
    it "finds k-th element of a list with elementAt'" $   
       forAll genListAndPos (\(s, n) -> elementAt' n s == unsafeIndex (fromList s) (n - 1)) 
    it "throws an exeption if used with an empty list" $ 
       evaluate (elementAt' 1 []) `shouldThrow` anyException   
       
  describe "findSize" $ 
    it "finds the number of elements of a list" $
      forAll genList (\xs -> findSize xs == length xs)
  
  describe "findSize'" $ 
    it "finds the number of elements of a list" $
      forAll genList (\xs -> findSize' xs == length xs) 
      
  describe "myReverse" $
    it "reverses a list" $
      forAll genList (\xs -> myReverse xs == reverse xs) 
      
  describe "myReverse'" $
    it "reverses a list" $
      forAll genList (\xs -> myReverse' xs == reverse xs)    
      
  describe "isPalindrome" $ do
    it "finds a palindrome" $ do
      isPalindrome "abcba" `shouldBe` True 
      isPalindrome' "abcba" `shouldBe` True  
      isPalindrome'' "abcba" `shouldBe` True
    it "not a palindrome" $ do
      isPalindrome "abcbaa" `shouldBe` False 
      isPalindrome' "abcbda" `shouldBe` False  
      isPalindrome'' "abcbafa" `shouldBe` False 
      
  describe "flatten" $
    it "flattens a list of lists" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten (List [] :: NestedList Int) `shouldBe` []
      flatten' (List [] :: NestedList Int) `shouldBe` []
      flatten (Elem 5) `shouldBe` [5]
      flatten' (Elem 5) `shouldBe` [5]
      
  describe "compress" $
    it "eliminates consecutive duplicates of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"    
      compress' "aaaabccaadeeee" `shouldBe` "abcade"
      
  describe "pack" $
    it "packs consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]        
      pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]        
      pack'' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]        

  describe "encode" $
    it "run-length encoding of a list" $
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
