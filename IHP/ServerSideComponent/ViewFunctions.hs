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
import Data.Aeson ( ToJSON )
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

component :: forall component action props. (Component component action props, Typeable component) => Html
component = componentFromState Nothing $ initialState @component Nothing

componentWithProps :: forall component action props. (Component component action props, Typeable component) => props -> Html
componentWithProps props = componentFromState (Just props) $ initialState @component (Just props)

componentFromState :: forall component action props. (Component component action props, Typeable component) =>  Maybe props -> component ->  Html
componentFromState props state  = [hsx|<div class="ihp-ssc" data-path={path} data-props={jsonProps}>{render state}</div>|]
    where
        jsonProps = case props of
            Nothing -> ""
            Just p -> toStrict . toLazyText . (encodeToTextBuilder @props) $ p 
        path = "/SSC/" <> typeName
        typeName = (undefined :: component)
                |> Typeable.typeOf
                |> show