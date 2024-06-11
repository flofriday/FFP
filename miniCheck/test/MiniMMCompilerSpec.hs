{-# LANGUAGE QuasiQuotes #-}

module MiniMMCompilerSpec (spec) where

{- ORMOLU_DISABLE -}
import Text.RawString.QQ
import Test.Hspec
import MiniMM
import MiniMMCompiler
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)
import Data.Maybe (fromJust)
import qualified Data.Either as Either
import MiniMMCompiler (compileMiniMM)
import TransitionSystem
import TransitionSystem (TransitionSystem)
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
{- ORMOLU_ENABLE -}

-- | Counts the number of nodes/states in the transition system
nodeCount :: TransitionSystem -> Int
nodeCount ts = length $ states ts

-- | Counts the number of transitions in the transition system
transCount :: TransitionSystem -> Int
transCount ts = length $ transition ts

-- | Tests wether a atomic proposition is set in all return nodes
isInAllReturns :: AP -> TransitionSystem -> Bool
isInAllReturns target ts = all (elem target) returnAps
  where
    returnAps = map (\r -> fromJust (Map.lookup r (label_functions ts))) returnNodes
    returnNodes = filter (\s -> "return" `isPrefixOf` s) (Set.toList (states ts))

spec :: Spec
spec = do
  describe "compile without checking ts" $ do
    it "minimal example" $ do
      let src =
            [r|
            procedure main(a) {
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "invalid variable not defined" $ do
      let src =
            [r|
            procedure main(a, b) {
                return c;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isLeft (compileMiniMM ast) `shouldBe` True

    it "print" $ do
      let src =
            [r|
            procedure main(a) {
                print_bool(a);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "print and" $ do
      let src =
            [r|
            procedure main(a, b) {
                print_bool(a & b);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "print or" $ do
      let src =
            [r|
            procedure main(a, b) {
                print_bool(a | b);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "print xor" $ do
      let src =
            [r|
            procedure main(a, b) {
                print_bool(a ^ b);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "print implies" $ do
      let src =
            [r|
            procedure main(a, b) {
                print_bool(a => b);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "print equal" $ do
      let src =
            [r|
            procedure main(a, b) {
                print_bool(a == b);
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "assign" $ do
      let src =
            [r|
            procedure main(a) {
                x = a;
                print_bool(x);
                return x;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "read" $ do
      let src =
            [r|
            procedure main(a) {
                x = read_bool();
                return x;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "if" $ do
      let src =
            [r|
            // This only works because variables are only resolved if they are
            // needed and therfore this never fails.
            procedure main(a) {
                if (a) {
                    x = true;
                }
                if (a) {
                    print_bool(x);
                }
                return a;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "if else" $ do
      let src =
            [r|
            procedure main(a) {
                if (a) {
                    x = true;
                } else {
                    x = true;
                }
                return x;
            }
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

    it "assignmentExample" $ do
      let src =
            [r|
           procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      isRight (compileMiniMM ast) `shouldBe` True

  describe "compile and verify nodes" $ do
    it "minimal" $ do
      let src =
            [r|
           procedure main(a) {
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 5
      transCount ts `shouldBe` 5

    it "if" $ do
      let src =
            [r|
           procedure main(a) {
                if (a) {
                    a = false;
                }
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 8
      transCount ts `shouldBe` 8

    it "readbool" $ do
      let src =
            [r|
           procedure main(a) {
                b = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 9
      transCount ts `shouldBe` 11

    it "2x readbool" $ do
      let src =
            [r|
           procedure main(a) {
                b = read_bool();
                c = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 17
      transCount ts `shouldBe` 23

    it "3x readbool" $ do
      let src =
            [r|
           procedure main(a) {
                b = read_bool();
                c = read_bool();
                d = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 33
      transCount ts `shouldBe` 47

    it "4x readbool" $ do
      let src =
            [r|
           procedure main(a) {
                b = read_bool();
                c = read_bool();
                d = read_bool();
                e = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 65
      transCount ts `shouldBe` 95

    it "5x readbool" $ do
      let src =
            [r|
           procedure main(a) {
                b = read_bool();
                c = read_bool();
                d = read_bool();
                e = read_bool();
                f = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 129
      transCount ts `shouldBe` 191

    it "assignmentExample" $ do
      let src =
            [r|
           procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      nodeCount ts `shouldBe` 21
      transCount ts `shouldBe` 21

  {- During compilaion all expressions are evaluated and in these tests we
     verify that the evaluation is correct, by looking into all returns in the
     end.
  -}
  describe "compile and verify evaluated expressions" $ do
    it "simple assign" $ do
      let src =
            [r|
           procedure main(a) {
                x = false;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("x", False) ts `shouldBe` True

    it "if then branch" $ do
      let src =
            [r|
           procedure main(a) {
                cond = true;
                if (cond) {
                    x = true;
                }
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("x", True) ts `shouldBe` True

    it "if no branch" $ do
      let src =
            [r|
           procedure main(a) {
                cond = false;
                x = false;
                if (cond) {
                    x = true;
                }
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("x", False) ts `shouldBe` True

    it "if else then branch" $ do
      let src =
            [r|
           procedure main(a) {
                if (true) {
                    x = true;
                } else {
                    x = false;
                }
                return x;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("x", True) ts `shouldBe` True

    it "if else else branch" $ do
      let src =
            [r|
           procedure main(a) {
                if (false) {
                    x = true;
                } else {
                    x = false;
                }
                return x;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("x", False) ts `shouldBe` True

    it "and" $ do
      let src =
            [r|
           procedure main(a) {
                ff = false & false;
                ft = false & true;
                tf = true & false;
                tt = true & true;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("ff", False) ts `shouldBe` True
      isInAllReturns ("ft", False) ts `shouldBe` True
      isInAllReturns ("tf", False) ts `shouldBe` True
      isInAllReturns ("tt", True) ts `shouldBe` True

    it "or" $ do
      let src =
            [r|
           procedure main(a) {
                ff = false | false;
                ft = false | true;
                tf = true | false;
                tt = true | true;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("ff", False) ts `shouldBe` True
      isInAllReturns ("ft", True) ts `shouldBe` True
      isInAllReturns ("tf", True) ts `shouldBe` True
      isInAllReturns ("tt", True) ts `shouldBe` True

    it "xor" $ do
      let src =
            [r|
           procedure main(a) {
                ff = false ^ false;
                ft = false ^ true;
                tf = true ^ false;
                tt = true ^ true;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("ff", False) ts `shouldBe` True
      isInAllReturns ("ft", True) ts `shouldBe` True
      isInAllReturns ("tf", True) ts `shouldBe` True
      isInAllReturns ("tt", False) ts `shouldBe` True

    it "implies" $ do
      let src =
            [r|
           procedure main(a) {
                ff = false => false;
                ft = false => true;
                tf = true => false;
                tt = true => true;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("ff", True) ts `shouldBe` True
      isInAllReturns ("ft", True) ts `shouldBe` True
      isInAllReturns ("tf", False) ts `shouldBe` True
      isInAllReturns ("tt", True) ts `shouldBe` True

    it "equal" $ do
      let src =
            [r|
           procedure main(a) {
                ff = false == false;
                ft = false == true;
                tf = true == false;
                tt = true == true;
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("ff", True) ts `shouldBe` True
      isInAllReturns ("ft", False) ts `shouldBe` True
      isInAllReturns ("tf", False) ts `shouldBe` True
      isInAllReturns ("tt", True) ts `shouldBe` True

    it "readbool" $ do
      let src =
            [r|
           procedure main(a) {
                car = true;
                other = read_bool();
                yet_another = read_bool();
                yet_another_again = read_bool();
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("car", True) ts `shouldBe` True

    it "printbool" $ do
      let src =
            [r|
           procedure main(a) {
                car = true;
                print_bool(true);
                print_bool(false);
                return a;
            } 
        |]
      let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
      let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)
      isInAllReturns ("car", True) ts `shouldBe` True