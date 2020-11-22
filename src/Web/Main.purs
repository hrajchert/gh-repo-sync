module Web.Main where

import Prelude
import Effect (Effect)
import Halogen.HTML
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Home.page) unit body
