{-# LANGUAGE QuasiQuotes #-}

module ComputationalTreeLogicSpec (spec) where

{- ORMOLU_DISABLE -}
import Text.RawString.QQ
import Test.Hspec
import ComputationalTreeLogic
import ComputationalTreeLogic (parseComputationalTreeLogic)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)
{- ORMOLU_ENABLE -}

spec :: Spec
spec = do
  describe "parse parseComputationalTreeLogic" $ do
    it "correct input" $ do
      let src = "FORALL (U (AND (AP ap1) (NOT (AP ap2))) (OR (AP ap3) (EXISTS (A (AP ap4)))))"
      let result = parse parseComputationalTreeLogic "internal.txt" src
      isRight result `shouldBe` True

    it "correct input with newlines" $ do
      let src =
            [r|
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
      let src =
            [r|
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
      let src = ""
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
      result `shouldBe` Right (StateCtl (Not (Exists (A (Not (AtomicP "x"))))))

    it "desugar: Forall always" $ do
      let src = "FORALL (A (AP x))"
      let result = parse parseComputationalTreeLogic "internal.txt" src
      result `shouldBe` Right (StateCtl (Not (Exists (U State_True (Not (AtomicP "x"))))))

    it "desugar: implies" $ do
      let src = "IMPLIES (AP x) (AP y)"
      let result = parse parseComputationalTreeLogic "internal.txt" src
      result `shouldBe` Right (StateCtl (Not (And (Not (Not (AtomicP "x"))) (Not (AtomicP "y")))))

    it "desugar: equivalent" $ do
      let src = "EQUIVALENT (AP x) (AP y)"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      let first_implies = (Not (And (Not (Not (AtomicP "x"))) (Not (AtomicP "y"))))
      let second_implies = (Not (And (Not (Not (AtomicP "y"))) (Not (AtomicP "x"))))
      result `shouldBe` Right (StateCtl (And (first_implies) (second_implies)))

    it "desugar: xor" $ do
      let src = "XOR (AP x) (AP y)"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      let phi_1 = AtomicP "x"
      let phi_2 = AtomicP "y"
      let first_and = (And (phi_1) (Not (phi_2)))
      let second_and = (And (phi_2) (Not (phi_1)))

      result `shouldBe` Right (StateCtl (Not (And (Not (first_and)) (Not (second_and)))))

    it "desugar: forall next" $ do
      let src = "FORALL (O (AP x))"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Not (Exists (O (Not (AtomicP "x"))))))

    it "NO desugar: true" $ do
      let src = "TRUE"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl State_True)

    it "NO desugar: atomic proposition" $ do
      let src = "AP abcdefg_abc"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (AtomicP "abcdefg_abc"))

    it "NO desugar: and" $ do
      let src = "AND (AP a) (AP b)"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (And (AtomicP "a") (AtomicP "b")))

    it "NO desugar: not" $ do
      let src = "NOT (AP a)"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Not (AtomicP "a")))

    it "NO desugar: exists next" $ do
      let src = "EXISTS (O (AP a))"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Exists (O (AtomicP "a"))))

    it "NO desugar: exists until" $ do
      let src = "EXISTS (U (AP a) (AP b))"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Exists (U (AtomicP "a") (AtomicP "b"))))

    it "NO desugar: Exists always" $ do
      let src = "EXISTS (A (AP a))"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Exists (A (AtomicP "a"))))

    it "desugar: implies forall" $ do
      let src = "IMPLIES (FORALL (E (AP pay))) (FORALL (E (AP select)))"
      let result = parse parseComputationalTreeLogic "internal.txt" src

      result `shouldBe` Right (StateCtl (Not (And (Not (Not (Not (Exists (A (Not (AtomicP "pay"))))))) (Not (Not (Exists (A (Not (AtomicP "select")))))))))