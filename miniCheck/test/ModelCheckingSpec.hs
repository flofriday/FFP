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
    it "set check single atomic proposition - single AP" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("select", [("select", True), ("soda", False), ("beer", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", False)]), 
                ("beer", [("select", False), ("soda", False), ("beer", True)])
            ])
        }
        let ctl = StateCtl ((AtomicP "select"))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["select"]
    it "set check single atomic proposition - multiple AP" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("select", [("select", True), ("soda", False), ("beer", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", False)]), 
                ("beer", [("select", False), ("soda", True), ("beer", True)])
            ])
        }
        let ctl = StateCtl ((AtomicP "soda"))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["soda", "beer"]
    it "set check and with empty result" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("select", [("select", True), ("soda", False), ("beer", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", False)]), 
                ("beer", [("select", True), ("soda", False), ("beer", True)])
            ])
        }
        let ctl = StateCtl (And (AtomicP "select") (AtomicP "soda"))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList []
    it "set check and with non empty result" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("select", [("select", True), ("soda", False), ("beer", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", True)]), 
                ("beer", [("select", True), ("soda", False), ("beer", True)])
            ])
        }
        let ctl = StateCtl (And (AtomicP "select") (AtomicP "beer"))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["beer"]
    it "set check - negation - basic" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("select", [("select", True), ("soda", False), ("beer", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", True)]), 
                ("beer", [("select", True), ("soda", False), ("beer", True)])
            ])
        }
        let ctl = StateCtl (Not (AtomicP "soda"))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["select", "beer", "pay"]
    it "set check - exists next - basic" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("pay", [("select", True), ("soda", False), ("beer", False), ("pay", True)]), 
                ("select", [("select", True), ("soda", False), ("beer", False), ("pay", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", True), ("pay", False)]), 
                ("beer", [("select", True), ("soda", False), ("beer", True), ("pay", False)])
            ])
        }
        let ctl = StateCtl (Exists (O (AtomicP "pay")))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["soda", "beer"]
    it "set check - exists until - basic" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("pay", [("select", False), ("soda", False), ("beer", False), ("pay", True)]), 
                ("select", [("select", True), ("soda", False), ("beer", False), ("pay", False)]), 
                ("soda", [("select", False), ("soda", True), ("beer", True), ("pay", False)]), 
                ("beer", [("select", True), ("soda", False), ("beer", True), ("pay", False)])
            ])
        }
        let ctl = StateCtl (Exists (U (AtomicP "select") (AtomicP "soda")))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["select", "soda"]
    it "set check - exists until - basic 2" $ do
        let ts = TransitionSystem { 
            initial_states=(Set.fromList ["pay"]),
            states= (Set.fromList ["select", "soda", "pay", "beer"]),
            actions=(Set.fromList ["get_soda", "get_beer", "insert_coin"]),
            transition=[
                ("pay", "insert_coin", "select"), 
                ("beer", "get_beer", "pay"), 
                ("select", "TRUE", "beer"), 
                ("select", "TRUE", "soda"), 
                ("soda", "get_soda", "pay")
            ],
            label_functions=(Map.fromList [
                ("pay", [("select", False), ("soda", False), ("beer", True), ("pay", False)]), 
                ("select", [("select", False), ("soda", True), ("beer", True), ("pay", False)]), 
                ("soda", [("select", False), ("soda", False), ("beer", False), ("pay", False)]), 
                ("beer", [("select", False), ("soda", False), ("beer", False), ("pay", True)])
            ])
        }
        let ctl = StateCtl (Exists (U (AtomicP "beer") (AtomicP "pay")))
        let result = satFun ts ctl
        result `shouldBe` Set.fromList ["pay", "select", "beer"]

{-
## simple example:
        
let src = [r|
    initial states pay
    states select, soda, beer
    actions insert_coin, get_beer, get_soda
    trans soda get_soda pay
    trans beer get_beer pay
    trans pay insert_coin select
    trans select TRUE beer
    trans select TRUE soda
    labels select: select, -soda, -beer
    labels soda: -select, soda, -beer
    labels beer: -select, -soda, beer
    |]
-}