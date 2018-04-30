module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Utils.StringSpec (stringSpec)
import Test.Data.JSON.ParseForeignSpec (parseForeignSpec)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Utils" do
    stringSpec
  describe "SimpleJSON" do
    parseForeignSpec
