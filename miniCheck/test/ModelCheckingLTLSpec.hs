{-# LANGUAGE QuasiQuotes #-}

module ModelCheckingLTLSpec (spec) where

{- ORMOLU_DISABLE -}
import Text.RawString.QQ
import Test.Hspec
import TransitionSystem
import LinearTemporalLogic
import ModelCheckingLTL
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec (parse)
import Data.Either (isRight, isLeft)
import TransitionSystem (parseTransitionSystem)
{- ORMOLU_ENABLE -}

errorOnLeft :: (Show a) => Either a b -> IO b
errorOnLeft (Left err) = error $ "Encountered Left: " ++ show err
errorOnLeft (Right result) = return result

basic_ts :: TransitionSystem
basic_ts = TransitionSystem
              { initial_states = (Set.fromList ["pay"]),
                states = (Set.fromList ["select", "soda", "pay", "beer"]),
                actions = (Set.fromList ["get_soda", "get_beer", "insert_coin"]),
                transition =
                  [ ("pay", "insert_coin", "select"),
                    ("beer", "get_beer", "pay"),
                    ("select", "TRUE", "beer"),
                    ("select", "TRUE", "soda"),
                    ("soda", "get_soda", "pay")
                  ],
                label_functions =
                  ( Map.fromList
                      [ ("pay", [("select", False), ("soda", False), ("beer", False), ("pay", True)]),
                        ("select", [("select", True), ("soda", False), ("beer", False), ("pay", False)]),
                        ("soda", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("beer", [("select", False), ("soda", True), ("beer", False), ("pay", False)])
                      ]
                  )
              }

basic_ts_with_multiple_init_state :: TransitionSystem
basic_ts_with_multiple_init_state = TransitionSystem
              { initial_states = (Set.fromList ["pay", "select"]),
                states = (Set.fromList ["select", "soda", "pay", "beer"]),
                actions = (Set.fromList ["get_soda", "get_beer", "insert_coin"]),
                transition =
                  [ ("pay", "insert_coin", "select"),
                    ("beer", "get_beer", "pay"),
                    ("select", "TRUE", "beer"),
                    ("select", "TRUE", "soda"),
                    ("soda", "get_soda", "pay")
                  ],
                label_functions =
                  ( Map.fromList
                      [ ("pay", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("select", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("soda", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("beer", [("select", False), ("soda", False), ("beer", False), ("pay", False)])
                      ]
                  )
              }

spec :: Spec
spec = do
  describe "verify modelCheckLTL" $ do
    it "path with 0 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 0
      result `shouldBe` [["pay"]]
    it "path with 1 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 1
      result `shouldBe` [["pay", "select"]]
    it "path with 2 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 2
      result `shouldBe` [["pay", "select", "beer"], ["pay", "select", "soda"]]
    it "path with 3 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 3
      result `shouldBe` [["pay", "select", "beer", "pay"], ["pay", "select", "soda", "pay"]]
    it "path with 4 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 4
      result `shouldBe` [["pay", "select", "beer", "pay", "select"], ["pay", "select", "soda", "pay", "select"]]
    it "path with 5 length (with start state)" $ do
      let result = generatePathsFromState basic_ts "pay" 5
      result `shouldBe` [["pay", "select", "beer", "pay", "select", "beer"], ["pay", "select", "beer", "pay", "select", "soda"], ["pay", "select", "soda", "pay", "select", "beer"], ["pay", "select", "soda", "pay", "select", "soda"]]
    it "path with 0 length" $ do
      let result = generatePaths basic_ts 0
      result `shouldBe` Set.fromList [["pay"]]
    it "path with 3 length" $ do
      let result = generatePaths basic_ts 3
      result `shouldBe` Set.fromList [["pay", "select", "beer", "pay"], ["pay", "select", "soda", "pay"]]
    it "path with 1 length and 2 init states" $ do
      let result = generatePaths basic_ts_with_multiple_init_state 1
      result `shouldBe` Set.fromList [["pay","select"],["select","beer"],["select","soda"]]
    it "path with 2 length and 2 init states" $ do
      let result = generatePaths basic_ts_with_multiple_init_state 2
      result `shouldBe` Set.fromList [["pay","select","beer"],["pay","select","soda"],["select","beer","pay"],["select","soda","pay"]]
    it "modelCheckLTL - pay holds in init state" $ do
      let ltl = (AtomicP "pay")
      let result = modelCheckLTL basic_ts ltl 0
      result `shouldBe` True
    it "modelCheckLTL - no other atomic proposition holds in init state" $ do
      let ltlContent = "OR (AP select) (OR (AP beer) (AP soda))"
      ltl <- errorOnLeft $ parse parseLinearTemporalLogic "intenal.txt" ltlContent
      let result = modelCheckLTL basic_ts ltl 0
      result `shouldBe` False
    it "modelCheckLTL - pay holds until soda holds" $ do
      let ts = TransitionSystem
              { initial_states = (Set.fromList ["pay"]),
                states = (Set.fromList ["select", "soda", "pay", "beer"]),
                actions = (Set.fromList ["get_soda", "get_beer", "insert_coin"]),
                transition =
                  [ ("pay", "insert_coin", "select"),
                    ("beer", "get_beer", "pay"),
                    ("select", "TRUE", "beer"),
                    ("select", "TRUE", "soda"),
                    ("soda", "get_soda", "pay")
                  ],
                label_functions =
                  ( Map.fromList
                      [ ("pay", [("select", False), ("soda", False), ("beer", False), ("pay", True)]),
                        ("select", [("select", True), ("soda", False), ("beer", False), ("pay", True)]),
                        ("soda", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("beer", [("select", False), ("soda", True), ("beer", True), ("pay", False)])
                      ]
                  )
              }
      let ltlContent = "U (AP pay) (AP soda)"
      ltl <- errorOnLeft $ parse parseLinearTemporalLogic "intenal.txt" ltlContent
      let result = modelCheckLTL ts ltl 3
      result `shouldBe` True
    it "modelCheckLTL - pay does not hold until soda holds" $ do
      let ts = TransitionSystem
              { initial_states = (Set.fromList ["pay"]),
                states = (Set.fromList ["select", "soda", "pay", "beer"]),
                actions = (Set.fromList ["get_soda", "get_beer", "insert_coin"]),
                transition =
                  [ ("pay", "insert_coin", "select"),
                    ("beer", "get_beer", "pay"),
                    ("select", "TRUE", "beer"),
                    ("select", "TRUE", "soda"),
                    ("soda", "get_soda", "pay")
                  ],
                label_functions =
                  ( Map.fromList
                      [ ("pay", [("select", False), ("soda", False), ("beer", False), ("pay", True)]),
                        ("select", [("select", True), ("soda", False), ("beer", False), ("pay", False)]),
                        ("soda", [("select", False), ("soda", True), ("beer", False), ("pay", False)]),
                        ("beer", [("select", False), ("soda", True), ("beer", True), ("pay", False)])
                      ]
                  )
              }
      let ltlContent = "U (AP pay) (AP soda)"
      ltl <- errorOnLeft $ parse parseLinearTemporalLogic "intenal.txt" ltlContent
      let result = modelCheckLTL ts ltl 3
      result `shouldBe` False