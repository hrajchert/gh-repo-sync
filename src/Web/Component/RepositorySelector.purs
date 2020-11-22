module Web.Component.RepositorySelector where

import Prelude (Unit, bind, pure, ($), (<<<))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Github.Entities (BranchObject, canonicalBranch)
import Halogen as H
import Halogen.HTML (HTML, div_, input, label_, text)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties as HP

type State
  = BranchObject

data Action
  = SelectOwner String
  | SelectRepository String
  | SelectBranch String

data Query a
  = GetBranchObject (BranchObject -> a)

component :: forall i o m. H.Component HTML Query i o m
component =
  H.mkComponent
    { initialState: \_ -> canonicalBranch "" "" ""
    , render: render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  div_
    [ label_
        [ text "@" ]
    , input
        [ HP.value $ unwrap state.owner
        , onValueInput (Just <<< SelectOwner)
        ]
    , label_
        [ text "/" ]
    , input
        [ HP.value $ unwrap state.repository
        , onValueInput (Just <<< SelectRepository)
        ]
    , label_
        [ text "#" ]
    , input
        [ HP.value $ unwrap state.branch
        , onValueInput
            ( Just <<< SelectBranch
            )
        ]
    ]

handleQuery :: forall output a m. Query a -> H.HalogenM State Action () output m (Maybe a)
handleQuery = case _ of
  GetBranchObject reply -> do
    branchObject <- H.get
    pure (Just (reply branchObject))

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  SelectOwner owner' -> H.modify_ \st -> st { owner = wrap owner' }
  SelectRepository repository' -> H.modify_ \st -> st { repository = wrap repository' }
  SelectBranch branch' -> H.modify_ \st -> st { branch = wrap branch' }
