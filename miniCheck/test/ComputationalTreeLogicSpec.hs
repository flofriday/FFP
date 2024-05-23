{-# LANGUAGE QuasiQuotes #-}

module ComputationalTreeLogicSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import ComputationalTreeLogic
import ComputationalTreeLogic (parseComputationalTreeLogic)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "parse parseComputationalTreeLogic" $ do
    it "correct input" $ do
        let src = "FORALL (U (AND (AP ap1) (NOT (AP ap2))) (OR (AP ap3) (EXISTS (A (AP ap4)))))"
        let result = parse parseComputationalTreeLogic "internal.txt" src
        isRight result `shouldBe` True

    it "correct input with newlines" $ do
        let src = [r|
            FORALL (U 
                (AND 
                    (AP ap1) 
                    (NOT (AP ap2))) 
                (OR 
                    (AP ap3) 
                    (EXISTS 
                        (A (AP ap4))))) 
        |] 
        let result = parse parseComputationalTreeLogic "internal.txt" src
        isRight result `shouldBe` True


    it "empty input" $ do
        let src =  ""
        let result = parse parseComputationalTreeLogic "internal.txt" src
        isLeft result `shouldBe` True
    