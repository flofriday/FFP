{-# LANGUAGE QuasiQuotes #-}

module ComputationalTreeLogicSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import ComputationalTreeLogic
import ComputationalTreeLogic (parseComputationalTreeLogic)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)

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
    
    it "correct input with comments" $ do
        let src = [r|
            -- Just a simple example
            FORALL (U 
                (AND 
                    (AP ap1) -- Sometimes we have them trailing
                    (NOT (AP ap2))) 

                -- Let's try or here
                --(AND 
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

    it "desugar: or" $ do
        let src = "OR (AP x) (AP y)"
        let result = parse parseComputationalTreeLogic "internal.txt" src
        result `shouldBe` Right (StateCtl (Not (And (Not (AtomicP "x")) (Not (AtomicP "y")))))

    it "desugar: Exists eventually" $ do
        let src = "EXISTS (E (AP x))"
        let result = parse parseComputationalTreeLogic "internal.txt" src
        result `shouldBe` Right (StateCtl (Exists (U (State_True) (AtomicP "x"))))

    it "desugar: Forall eventually" $ do
        let src = "FORALL (E (AP x))"
        let result = parse parseComputationalTreeLogic "internal.txt" src
        result `shouldBe` Right (StateCtl (Forall (U (State_True) (AtomicP "x"))))
        
    