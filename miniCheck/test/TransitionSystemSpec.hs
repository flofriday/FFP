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
            states select, soda, beer, pay
            actions insert_coin, get_beer, get_soda
            trans soda get_soda pay
            trans beer get_beer pay
            trans pay insert_coin select
            trans select TRUE beer
            trans select TRUE soda
            labels select: select, -x
            labels soda: soda
            labels beer: beer
            labels pay: pay
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isRight result `shouldBe` True
        -- FIXME: Yes there should be tests whether or not it got parsed 
        -- correctly. However, I am not sure if the format will stay that way 
        -- so there are now tests for it now.
        --result `shouldBe` Right (TransitionSystem {initial_states = Set.fromList ["pay"], states = Set.fromList ["beer","select","soda"], actions = Set.fromList ["get_beer","get_soda","insert_coin"], transition = [("soda","get_soda","pay"),("beer","get_beer","pay"),("pay","insert_coin","select"),("select","TRUE","beer"),("select","TRUE","soda")], label_functions = Map.fromList [("select",[("x",False),("y",True),("z",False),("a",False),("b",True)])]})

    it "correct input with comments" $ do
        let src = [r|
            --  Just an example
            initial states pay
            states select, soda, beer, pay
            actions insert_coin, get_beer, get_soda

            -- EXAMPLE-FIXME: Reinsert in the future
            -- trans soda get_soda pay
            trans soda get_soda pay
            trans beer get_beer pay
            trans pay insert_coin select
            trans select TRUE beer
            trans select TRUE soda
            labels select: select, -x
            labels pay: pay
            labels soda: soda
            labels beer: beer
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isRight result `shouldBe` True

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
     
    it "trailing whitespaces" $ do
        let src = [r|
            initial states a 
            states a 
            actions x 
            trans a TRUE a 
            labels a: a 
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isRight result `shouldBe` True
 
    it "initial states exists" $ do
        let src = [r|
            initial states notARealState
            states a 
            actions x 
            trans a TRUE a 
            labels a: a 
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True

    it "left transition states exists" $ do
        let src = [r|
            initial states a
            states a
            actions x 
            trans doesNotExist TRUE a 
            labels a: a 
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True

    it "right transition states exists" $ do
        let src = [r|
            initial states a
            states a 
            actions x 
            trans a TRUE doesNotExist 
            labels a: a 
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True

    it "transition action exists" $ do
        let src = [r|
            initial states a
            states a
            actions x 
            trans a doesNotExist a 
            labels a: a 
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True

    it "every state labels itself" $ do
        let src = [r|
            initial states a
            states a, b 
            actions x 
            trans a TRUE a 
            labels a: b 
            labels b: b
            |]

        let result = parse parseTransitionSystem "internal.txt" src
        isLeft result `shouldBe` True
