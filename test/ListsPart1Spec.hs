module ListsPart1Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ListsPart1
import Data.Vector
import Control.Monad
                         
main' :: IO ()
main' = hspec spec

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
      ((myLast []) :: Maybe Int) `shouldBe` Nothing

  describe "lastButOne" $ do
    it "find the last but one element of a list" $ do
      lastButOne [1,2,3,4,5] `shouldBe` 4
      lastButOne' [1,2,3,4,5] `shouldBe` 4
    it "throws an exception if used with an empty list" $ do
      evaluate (lastButOne []) `shouldThrow` anyException
      evaluate (lastButOne' []) `shouldThrow` anyException



