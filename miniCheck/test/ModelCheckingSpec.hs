{-# LANGUAGE QuasiQuotes #-}

module ModelCheckingSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import TransitionSystem
import ComputationalTreeLogic
import ModelChecking
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft)
import TransitionSystem (parseTransitionSystem)

spec :: Spec
spec = do
  describe "verify modelCheck" $ do
    it "simplest model - Set Check" $ do
        let ts = TransitionSystem (Set.fromList ["pay"])  (Set.fromList ["pay", "select"]) (Set.fromList []) [] (Map.fromList [])
        let ctl = StateCtl (State_True)
        let satStates = satFun ts ctl
        satStates `shouldBe` Set.fromList ["pay", "select"]
    it "simplest model - Full Check" $ do
        let ts = TransitionSystem (Set.fromList ["pay"])  (Set.fromList ["pay", "select"]) (Set.fromList []) [] (Map.fromList [])
        let ctl = StateCtl (State_True)
        let result = modelCheck ts ctl
        result `shouldBe` True
        
