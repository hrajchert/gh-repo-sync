module Web.Page.Home where

import Halogen.HTML (HTML, button, div_, input, p_, slot, text)
import Halogen.HTML.Events (onClick, onValueInput)
import Prelude (Unit, Void, absurd, bind, discard, unit, void, ($), (<<<), (<>))
import Control.Async (runAsync)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Explain (explain)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Github.Api.Api (AccessToken(..))
import Github.Entities (BranchObject, branchURI, canonicalBranch)
import Github.Settings.BranchProtection (syncBranchProtectionSettings)
import Halogen as H
import Halogen.HTML.Properties as HP
import Web.Component.RepositorySelector as RepositorySelector

type Slots
  = ( from :: H.Slot RepositorySelector.Query Void Int
    , to :: H.Slot RepositorySelector.Query Void Int
    )

_from = SProxy :: SProxy "from"

_to = SProxy :: SProxy "to"

type State
  = { from :: BranchObject
    , to :: BranchObject
    , accessToken :: String
    }

data Action
  = SyncRepository
  | SetAccessToken String

page :: forall q i o m. MonadEffect m => H.Component HTML q i o m
page =
  H.mkComponent
    { initialState:
        \_ ->
          { accessToken: ""
          , from: canonicalBranch "" "" ""
          , to: canonicalBranch "" "" ""
          }
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  div_
    [ p_ [ text $ "Access token" ]
    , input
        [ HP.value $ state.accessToken
        , HP.type_ HP.InputPassword
        , onValueInput (Just <<< SetAccessToken)
        ]
    , p_ [ text $ "from" ]
    , slot _from 0 RepositorySelector.component unit absurd
    , p_ [ text $ "to" ]
    , slot _to 0 RepositorySelector.component unit absurd
    , button
        [ onClick \_ -> Just $ SyncRepository ]
        [ text "Sync repo" ]
    , p_ [ text $ (branchURI state.from) <> " -> " <> (branchURI state.to) ]
    ]

handleAction :: forall output m. MonadEffect m => Action → H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SyncRepository ->
    void
      $ runMaybeT do
          accessToken <- H.gets _.accessToken
          from <- MaybeT $ H.query _from 0 $ H.request RepositorySelector.GetBranchObject
          to <- MaybeT $ H.query _to 0 $ H.request RepositorySelector.GetBranchObject
          H.modify_ \state -> state { from = from, to = to }
          H.liftEffect $ Console.log $ "Syncing stuff " <> accessToken
          let
            asyncAction = syncBranchProtectionSettings (AccessToken accessToken) from to

            -- TODO: refactor Async into Aff and put this result in the state
            resultCb result = case result of
              Left err -> H.liftEffect $ Console.log $ "Some error " -- <> explain err
              Right val -> H.liftEffect $ Console.log $ "Yeay: " <> explain val
          H.liftEffect $ runAsync asyncAction resultCb
  SetAccessToken token -> H.modify_ \state -> state { accessToken = token }
