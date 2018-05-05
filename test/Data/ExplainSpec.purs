module Test.Data.ExplainSpec where

import Prelude

import Data.Explain (explain)
import Data.Foreign (ForeignError(..))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.String (trim)

explainSpec :: forall eff. Spec eff Unit
explainSpec =
  describe "ForeignError" do
    describe "Type Mismatch" do
      it "type mismatch"
        let
          error = TypeMismatch "String" "Number"
          explination = """ i was expecting "String", but got "Number" """
        in
          explain error `shouldEqual` trim explination
    describe "ErrorAtProperty" do
      it "simple property"
        let
          error = ErrorAtProperty "id" $ TypeMismatch "String" "Number"
          explination = """ property "id" was expected to be "String", but is "Number" """
        in
          explain error `shouldEqual` trim explination

      it "anidated property"
        let
          error = ErrorAtProperty "user" $ ErrorAtProperty "address" $ ErrorAtProperty "number" $ TypeMismatch "String" "Number"
          explination = """ property "user.address.number" was expected to be "String", but is "Number" """
        in
          explain error `shouldEqual` trim explination

      it "anidated property and index"
        let
          error = ErrorAtProperty "user" $ ErrorAtProperty "address" $ ErrorAtIndex 0 $ TypeMismatch "String" "Number"
          explination = """ property "user.address[0]" was expected to be "String", but is "Number" """
        in
          explain error `shouldEqual` trim explination

      it "undefined property"
        let
          error = ErrorAtProperty "user" $ TypeMismatch "String" "Undefined"
          explination = """ property "user" is undefined """
        in
          explain error `shouldEqual` trim explination

      it "undefined anidated property"
        let
          error = ErrorAtProperty "user" $ ErrorAtProperty "address" $ ErrorAtProperty "number" $ TypeMismatch "String" "Undefined"
          explination = """ property "user.address.number" is undefined """
        in
          explain error `shouldEqual` trim explination

    describe "ErrorAtIndex" do
      it "simple index"
        let
          error = ErrorAtIndex 0 $ TypeMismatch "String" "Number"
          explination = """ at position [0] was expected to be "String", but is "Number" """
        in
          explain error `shouldEqual` trim explination

      it "anidated index"
        let
          error = ErrorAtIndex 2 $ ErrorAtIndex 2 $ TypeMismatch "String" "Number"
          explination = """ at position [2][2] was expected to be "String", but is "Number" """
        in
          explain error `shouldEqual` trim explination

