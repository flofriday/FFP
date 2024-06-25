{-# LANGUAGE QuasiQuotes #-}

module LinearTemporalLogicSpec (spec) where

{- ORMOLU_DISABLE -}
import Text.RawString.QQ
import Test.Hspec
import LinearTemporalLogic
import LinearTemporalLogic (parseLinearTemporalLogic)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)
{- ORMOLU_ENABLE -}

spec :: Spec
spec = do
  describe "parse parseLinearTemporalLogic" $ do
    it "parse true" $ do
      let src = "TRUE"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right State_True
    it "parse atomic prop" $ do
      let src = "AP asdsd_dsdasd"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (AtomicP "asdsd_dsdasd")
    it "parse and" $ do
      let src = "AND (AP ab) (TRUE)"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (And (AtomicP "ab") State_True)
    it "parse not" $ do
      let src = "NOT (AP abc)"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (Not (AtomicP "abc"))
    it "parse next" $ do
      let src = "O (AP abc)"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (O (AtomicP "abc"))
    it "parse until" $ do
      let src = "U (AP abc) (AP cba)"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (U (AtomicP "abc") (AtomicP "cba"))
    it "parse eventually" $ do
      let src = "E (AND (AP a) (AP b))"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (U State_True (And (AtomicP "a") (AtomicP "b")))
    it "parse always" $ do
      let src = "A (AP a)"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (Not (U State_True (Not (AtomicP "a"))))
    it "parse implies eventually" $ do
      let src = "IMPLIES (E (AP pay)) (E (AP select))"
      let result = parse parseLinearTemporalLogic "internal.txt" src
      result `shouldBe` Right (Not (And (Not (Not (U State_True (AtomicP "pay")))) (Not (U State_True (AtomicP "select")))))