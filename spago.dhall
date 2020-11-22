{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "gh-repo-sync"
, dependencies =
  [ "affjax"
  , "checked-exceptions"
  , "console"
  , "effect"
  , "halogen"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec-quickcheck"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
