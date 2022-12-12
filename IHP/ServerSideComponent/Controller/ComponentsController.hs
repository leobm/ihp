{-# LANGUAGE  UndecidableInstances #-}
module IHP.ServerSideComponent.Controller.ComponentsController where

import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC
import IHP.ControllerSupport 
import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Network.HTTP2.Server (requestPath)
import IHP.Controller.RequestContext
import IHP.Controller.Context
import Data.Aeson ( FromJSON )
import Data.Aeson.Text (encodeToTextBuilder)

instance (Component component controller props, FromJSON controller, FromJSON props) => WSApp (ComponentsController component) where
    initialState = ComponentsController

    run = do
        let props = param @ByteString "props"
        let state :: component = SSC.initialState $ decodeStrict @props props
        instanceRef <- newIORef (ComponentInstance { state })
        let ?instanceRef = instanceRef

        nextState <- componentDidMount state
        SSC.setState nextState

        forever do
            actionPayload :: LByteString <- receiveData

            let theAction = Aeson.eitherDecode @controller actionPayload

            case theAction of
                Right theAction -> do
                    currentState <- SSC.getState

                    nextState <- SSC.action currentState theAction
                    SSC.setState nextState
                Left error -> putStrLn (cs error)