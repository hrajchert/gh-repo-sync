module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.ExplainSpec (explainSpec)
import Test.Data.JSON.ReadForeignSpec (parseForeignSpec)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utils.StringSpec (stringSpec)
import Effect.Aff (launchAff_)
main :: Effect Unit
main = launchAff_ $ runSpec  [consoleReporter] do
  describe "Utils" do
    stringSpec
  describe "SimpleJSON" do
    parseForeignSpec
  describe "Explain" do
    explainSpec