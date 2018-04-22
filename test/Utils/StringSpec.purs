module Test.Utils.StringSpec where

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Utils.String (capitalize)


stringSpec :: forall eff. Spec eff Unit
stringSpec =
    describe "String" do
      describe "capitilize" do

        it "should return empty string if it receives an empty string" do
          (capitalize "") `shouldEqual` ""

        it "should capitilize the first letter of a string of length 1" do
          (capitalize "a") `shouldEqual` "A"

        it "should capitilize the first letter of any string " do
          (capitalize "hello world") `shouldEqual` "Hello world"