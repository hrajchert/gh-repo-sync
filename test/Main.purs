module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Data.ExplainSpec (explainSpec)
import Test.Data.JSON.ParseForeignSpec (parseForeignSpec)
import Test.Spec (describe)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Utils.StringSpec (stringSpec)
main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Utils" do
    stringSpec
  describe "SimpleJSON" do
    parseForeignSpec
  describe "Explain" do
    explainSpec