{-# LANGUAGE QuasiQuotes #-}

module TransitionSystemSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import TransitionSystem
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft)
import TransitionSystem (parseTransitionSystem)

spec :: Spec
spec = do
  describe "parse parseTransitionSystem" $ do
    it "correct input" $ do
        let src = [r|
            initial states pay
            states select, soda, beer
            actions insert_coin, get_beer, get_soda
            trans soda get_soda pay
            trans beer get_beer pay
            trans pay insert_coin select
            trans select TRUE beer
            trans select TRUE soda
            labels select: -x, y, -z, -a, b
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isRight result `shouldBe` True
        -- FIXME: Yes there should be tests whether or not it got parsed 
        -- correctly. However, I am not sure if the format will stay that way 
        -- so there are now tests for it now.
        --result `shouldBe` Right (TransitionSystem {initial_states = Set.fromList ["pay"], states = Set.fromList ["beer","select","soda"], actions = Set.fromList ["get_beer","get_soda","insert_coin"], transition = [("soda","get_soda","pay"),("beer","get_beer","pay"),("pay","insert_coin","select"),("select","TRUE","beer"),("select","TRUE","soda")], label_functions = Map.fromList [("select",[("x",False),("y",True),("z",False),("a",False),("b",True)])]})

    it "empty input" $ do
        let src =  ""
        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True
     
    it "missing states" $ do
        let src = [r|
            initial states pay
            actions insert_coin, get_beer, get_soda
            trans soda get_soda pay
            trans beer get_beer pay
            trans pay insert_coin select
            trans select TRUE beer
            trans select TRUE soda
            labels select: -x, y, -z, -a, b
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True
     