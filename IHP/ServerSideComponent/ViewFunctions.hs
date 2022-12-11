{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.ServerSideComponent.ViewFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.ViewFunctions where

import IHP.Prelude
import IHP.ViewSupport
import IHP.ServerSideComponent.Types
import IHP.HSX.QQ (hsx)

import qualified Data.Typeable as Typeable

component :: forall component action props. (Component component action props, Typeable component) => Html
component = componentWithProps @component Nothing

componentWithProps :: forall component action props. (Component component action props, Typeable component) => Maybe props -> Html
componentWithProps props = componentFromState $ initialState @component props

componentFromState :: forall component action props. (Component  component action props, Typeable component) => component -> Html
componentFromState state = [hsx|<div class="ihp-ssc" data-path={path}>{render state}</div>|]
    where
        path = "/SSC/" <> typeName
        typeName = (undefined :: component)
                |> Typeable.typeOf
                |> show