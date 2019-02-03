module Test.Data.JSON.ParseForeignSpec where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Foreign (F, ForeignError(..))
import Data.List.NonEmpty (NonEmptyList, singleton, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.JSON.ParseForeign (class ParseForeign, readJSON')
import Test.Spec (describe, it, Spec, pending')
import Test.Spec.Assertions (shouldEqual)


-- Helper function to ease comparison
multipleErrors :: ∀ a. Array ForeignError -> Either (NonEmptyList ForeignError) a
multipleErrors errs = case fromFoldable errs of
  Just list -> Left list
  Nothing   -> Left $ singleton $ ForeignError "The test data needs to have at least one error"

-----------------
-- Test Types   -
-----------------

-- I have to define a new type around Record because I want to Show the results in the spec runner and I have to check for Equality
-- to see that the correct record was parsed
newtype RecordA = RecordA {a :: String}
derive newtype instance parseForeignRecordA :: ParseForeign RecordA

instance showRecordA :: Show RecordA where
  show (RecordA e) = "{a: "<> show e.a <> "}"

instance eqRecordA :: Eq RecordA where
  eq :: RecordA -> RecordA -> Boolean
  eq (RecordA first) (RecordA second) = eq first.a second.a


newtype RecordABC = RecordABC {a :: String, b :: Boolean, c :: Int}
derive newtype instance parseForeignRecordABC :: ParseForeign RecordABC

instance showRecordABC :: Show RecordABC where
  show (RecordABC e) = "{a: "<> show e.a <> ", b: "<> show e.b <> ", c: " <> show e.c <> "}"

instance eqRecordABC :: Eq RecordABC where
  eq :: RecordABC -> RecordABC -> Boolean
  eq (RecordABC first) (RecordABC second) = first.a == second.a && first.b == second.b && first.c == second.c



parseForeignSpec :: Spec Unit
parseForeignSpec =
    describe "class ParseForeign" do
      -----------------
      -- Record Specs -
      -----------------
      describe "instance parseRecord" do
        it "should parse a record of one element"
          let
            parsed :: F RecordA
            parsed = readJSON' """ {"a": "valueA"} """

          in
            runExcept parsed
              `shouldEqual`
                Right (RecordA {a: "valueA"})

        it "should parse a record of multiple elements"
          let
            parsed :: F RecordABC
            parsed = readJSON' """ {"a": "valueA", "b": true, "c": 9} """

          in
            runExcept parsed
              `shouldEqual`
                Right (RecordABC {a: "valueA", b: true, c: 9})

        pending' "should fail with TypeError if given another type"
          let
            parsed :: F RecordA
            parsed = readJSON' """ 9 """

          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [TypeMismatch "Object" "Number"]

        it "should fail with a PropertyError TypeMismatch if an inside property mismatch types"
          let
            parsed :: F RecordA
            parsed = readJSON' """ {"a": 9} """

          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [(ErrorAtProperty "a" (TypeMismatch "String" "Number"))]

        it "should fail with a PropertyError TypeMismatch if an inside property is missing"
          let
            parsed :: F RecordA
            parsed = readJSON' """ {"b": 9} """

          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [(ErrorAtProperty "a" (TypeMismatch "String" "Undefined"))]

        it "should fail with multiple errors"
          let
            parsed :: F RecordABC
            parsed = readJSON' """ {"a": "valueA", "b": "wrong", "c": "wrong"} """

          in
            runExcept parsed
              `shouldEqual`
                multipleErrors
                  [ (ErrorAtProperty "b" (TypeMismatch "Boolean" "String"))
                  , (ErrorAtProperty "c" (TypeMismatch "Int" "String"))
                  ]

      -----------------
      -- parseArray   -
      -----------------
      describe "instance parseArray" do
        it "should parse multiple values"
          let
            parsed :: F (Array String)
            parsed = readJSON' """ ["foo", "bar", "zoo"] """
          in
            runExcept parsed
              `shouldEqual`
                Right ["foo", "bar", "zoo"]

        it "should fail with type mismatch if it's not even an array"
          let
            parsed :: F (Array String)
            parsed = readJSON' """ 9 """
          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [TypeMismatch "array" "Number"]

        it "should fail with ErrorAtIndex 0 if we have an array with an error in the first position"
          let
            parsed :: F (Array String)
            parsed = readJSON' """ [9, "foo"] """
          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [ErrorAtIndex 0 (TypeMismatch "String" "Number")]


        it "should fail with ErrorAtIndex 1 if we have an array with an error in the second position"
          let
            parsed :: F (Array String)
            parsed = readJSON' """ ["foo", 9] """
          in
            runExcept parsed
              `shouldEqual`
                multipleErrors [ErrorAtIndex 1 (TypeMismatch "String" "Number")]


        it "should return multiple errors"
          let
            parsed :: F (Array String)
            parsed = readJSON' """ ["foo", 9, 9] """
          in
            runExcept parsed
              `shouldEqual`
                multipleErrors
                  [ ErrorAtIndex 1 (TypeMismatch "String" "Number")
                  , ErrorAtIndex 2 (TypeMismatch "String" "Number")
                  ]

      -----------------
      -- parseString  -
      -----------------

      describe "instance parseString" do
        it "should parse the right string"
          let
            parsed :: F String
            parsed = readJSON' """ "some string" """
          in
            runExcept parsed `shouldEqual` Right "some string"

        it "should fail if given another type"
          let
            parsed :: F String
            parsed = readJSON' """ 9 """

          in
            runExcept parsed `shouldEqual` multipleErrors [TypeMismatch "String" "Number"]

      -----------------
      -- parseMaybe   -
      -----------------

      describe "instance parseMaybe" do
        it "should parse Just the value if present string"
          let
            parsed :: F (Maybe String)
            parsed = readJSON' """ "some string" """
          in
            runExcept parsed `shouldEqual` Right (Just "some string")

        it "should parse Nothing if null is parsed"
          let
            parsed :: F (Maybe String)
            parsed = readJSON' "null"
          in
            runExcept parsed `shouldEqual` Right Nothing

        it "should fail if the inner value is from another type"
          let
            parsed :: F (Maybe String)
            parsed = readJSON' """ 9 """

          in
            runExcept parsed `shouldEqual` multipleErrors [TypeMismatch "String" "Number"]

      -----------------
      -- parseInt   -
      -----------------

      describe "instance parseInt" do
        it "should parse some ints"
          let
            parsed :: F Int
            parsed = readJSON' """ 9 """
          in
            (runExcept parsed) `shouldEqual` (Right 9)

        it "should fail if given another type"
          let
            parsed :: F Int
            parsed = readJSON' """ "some string" """
          in
            runExcept parsed `shouldEqual` multipleErrors [TypeMismatch "Int" "String"]

