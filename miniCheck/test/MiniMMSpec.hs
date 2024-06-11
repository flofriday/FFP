{-# LANGUAGE QuasiQuotes #-}

module MiniMMSpec (spec) where

import Text.RawString.QQ
import Test.Hspec
import MiniMM
import Text.Parsec (parse)
import Data.Either (isRight, isLeft, fromRight)
import qualified Data.Either as Either
import ComputationalTreeLogic
import ModelChecking
import MiniMMCompiler


errorOnLeft :: Show a => Either a b -> IO b
errorOnLeft (Left err) = error $ "Encountered Left: " ++ show err
errorOnLeft (Right result) = return result


spec :: Spec
spec = do
  describe "parse parseMiniMM" $ do
    it "minimal example" $ do
        let src = [r|
            procedure main(a, b) {
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "comments" $ do
        let src = [r|
            procedure main(a, b) {
                // This is not valid mini--
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print not" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(!a);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print true" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(true);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print false" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(false);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print and" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a & b);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print or" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a | b);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print xor" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a ^ b);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print implies" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a => b);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print equal" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(a == b);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print false" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(false);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "print true" $ do
        let src = [r|
            procedure main(a, b) {
                print_bool(true);
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True



    it "read" $ do
        let src = [r|
            procedure main(a, b) {
                a = read_bool();
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "read read" $ do
        -- There once was a parsing bug here so here is an extra test
        let src = [r|
            procedure main(a, b) {
                a = read_bool();
                b = read_bool();
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "assign" $ do
        let src = [r|
            procedure main(a, b) {
                c = a & b;
                return c;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "if" $ do
        let src = [r|
            procedure main(a, b) {
                c = a;
                if (!a) {
                    c = b;
                }
                return c;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "if else" $ do
        let src = [r|
            procedure main(a, b) {
                if (a) {
                    c = a;
                } else {
                    c = b;
                }
                return c;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "example from assignment" $ do
        let src = [r|
            // Yes some symbols are different here cause we switched them as
            // they are way easier to type.
            // Also in the assignment there are no comments.
            procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True


    it "variable names that start with same letter as keyword" $ do
        let src = [r|
            procedure main(a) {
                paul = true;
                richard = false;
                paul = richard;
                retina = true;
                ifinia = false;
                return a;
            }
        |]
        let result = parse parseMiniMM "internal.txt" src
        isRight result `shouldBe` True

    it "Mini-- a does not hold always for all paths" $ do
        let miniProgram = [r|
            procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            }
        |]
        miniParsed <- errorOnLeft $ parse parseMiniMM "internal.txt" miniProgram
        ts <- errorOnLeft $ compileMiniMM miniParsed

        let ctlFormula = "FORALL (A (AP a))"
        ctlParsed <- errorOnLeft $ parse parseComputationalTreeLogic "internal.txt" ctlFormula

        let result = modelCheck ts ctlParsed
        result `shouldBe` False


    it "Mini-- there exists a path so that eventually a or terminal holds" $ do
        let miniProgram = [r|
            procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            }
        |]
        miniParsed <- errorOnLeft $ parse parseMiniMM "internal.txt" miniProgram
        ts <- errorOnLeft $ compileMiniMM miniParsed

        let ctlFormula = "EXISTS (E (XOR (AP a) (AP terminal)))" 
        ctlParsed <- errorOnLeft $ parse parseComputationalTreeLogic "internal.txt" ctlFormula

        let result = modelCheck ts ctlParsed
        result `shouldBe` True

    it "Mini-- a does not hold for all states until terminal holds" $ do
        let miniProgram = [r|
            procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                return d;
            }
        |]
        miniParsed <- errorOnLeft $ parse parseMiniMM "internal.txt" miniProgram
        ts <- errorOnLeft $ compileMiniMM miniParsed

        let ctlFormula = "FORALL (U (AP a) (AP terminal))" 
        ctlParsed <- errorOnLeft $ parse parseComputationalTreeLogic "internal.txt" ctlFormula

        let result = modelCheck ts ctlParsed
        result `shouldBe` False

    it "Mini-- for all paths eventually car or terminal holds" $ do
        let miniProgram = [r|
            procedure main(a) {
                car = true;
                print_bool(true);
                print_bool(false);
                return a;
            } 
        |]
        miniParsed <- errorOnLeft $ parse parseMiniMM "internal.txt" miniProgram
        ts <- errorOnLeft $ compileMiniMM miniParsed

        let ctlFormula = "FORALL (E (XOR (AP car) (AP terminal)))"
        ctlParsed <- errorOnLeft $ parse parseComputationalTreeLogic "internal.txt" ctlFormula

        let result = modelCheck ts ctlParsed
        result `shouldBe` True

    it "Mini-- there exists a path where c is eventually true and the next state is terminal" $ do
        let miniProgram = [r|
            procedure main(a, b) {
                if (a) { c = !(b); 
                } else { c = b; } 
                d = c ^ true;
                c = true;
                return d;
            }
        |]
        miniParsed <- errorOnLeft $ parse parseMiniMM "internal.txt" miniProgram
        ts <- errorOnLeft $ compileMiniMM miniParsed

        let ctlFormula = "EXISTS (E (AND (AP c) (FORALL (O (AP terminal)))))"
        ctlParsed <- errorOnLeft $ parse parseComputationalTreeLogic "internal.txt" ctlFormula

        let result = modelCheck ts ctlParsed
        result `shouldBe` True
