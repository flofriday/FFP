{-# LANGUAGE QuasiQuotes #-}

module MiniMMCompilerSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import MiniMM
import MiniMMCompiler
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)
import qualified Data.Either as Either
import MiniMMCompiler (compileMiniMM)
import TransitionSystem

nodeCount :: TransitionSystem -> Int
nodeCount ts = length $ states ts

transCount :: TransitionSystem -> Int
transCount ts = length $ transition ts


spec :: Spec
spec = do
  describe "compile without check" $ do
    it "minimal example" $ do
        let src = [r|
            procedure main(a) {
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "invalid variable not defined" $ do
        let src = [r|
            procedure main(a, b) {
                return c;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isLeft (compileMiniMM ast) `shouldBe` True

    it "print" $ do
        let src = [r|
            procedure main(a) {
                print_bool(a);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "print and" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a & b);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True


    it "print or" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a | b);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "print xor" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a ^ b);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "print implies" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a => b);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "print equal" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a == b);
                return a;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "assign" $ do
        let src = [r|
            procedure main(a) {
                x = a;
                print_bool(x);
                return x;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "read" $ do
        let src = [r|
            procedure main(a) {
                x = read_bool();
                return x;
            }
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        isRight (compileMiniMM ast) `shouldBe` True

    it "if" $ do
        let src = [r|
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
        let src = [r|
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
        let src = [r|
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
        let src = [r|
           procedure main(a) {
                return a;
            } 
        |]
        let ast = fromRight (error "Expected Right but got Left") $ parse parseMiniMM "internal.txt" src
        let ts = fromRight (error "Expected Right but got Left") (compileMiniMM ast)  
        nodeCount ts `shouldBe` 5
        transCount ts `shouldBe` 5

    it "if" $ do
        let src = [r|
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

    it "assignmentExample" $ do
        let src = [r|
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
    
