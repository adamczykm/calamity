{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Calamity.HTTP.Internal.Config (
  HttpConfigEff (..),
  getHttpConfig,
  interpretHttpConfigDefault,
  interpretHttpConfigFromClient,
  baseReqConfig,
) where

import Network.HTTP.Client (Manager)
import Network.HTTP.Req (HttpConfig, defaultHttpConfig, httpConfigAltManager, httpConfigCheckResponse)
import Optics
import Polysemy
import Polysemy.Reader qualified as P

{- | Effect for accessing HTTP configuration

This effect provides access to the 'HttpConfig' used by the @req@ library for
making HTTP requests. It allows the bot to use a custom 'Manager' (e.g., for
proxy support or custom TLS settings) without threading it through every
function that makes HTTP requests.

The effect is typically interpreted in one of two ways:

* 'interpretHttpConfigDefault' - Uses default HTTP configuration with no custom manager
* 'interpretHttpConfigFromClient' - Uses the bot's 'Client.httpManager' for proxy/TLS support
-}
data HttpConfigEff m a where
  GetHttpConfig :: HttpConfigEff m HttpConfig

makeSem ''HttpConfigEff
{- ^ Generates: @getHttpConfig :: (Member HttpConfigEff r) => Sem r HttpConfig@

'getHttpConfig' retrieves the current HTTP configuration. This is the primary
way to access the HTTP config within the effect system. Most users won't call
this directly; instead, 'invoke' and related functions use it automatically.
-}

-- | Base config used by Calamity (disables response checking)
baseReqConfig :: HttpConfig
baseReqConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

{- | Default HTTP config interpreter

Interprets 'HttpConfigEff' by providing the base HTTP configuration with no
custom manager. This is suitable for standalone HTTP requests outside the bot
runtime, or when you don't need proxy/custom TLS support.

Example usage:

@
interpretHttpConfigDefault $ do
  result <- invoke (GetGatewayBot)
  ...
@
-}
interpretHttpConfigDefault :: Sem (HttpConfigEff ': r) a -> Sem r a
interpretHttpConfigDefault =
  interpret $ \GetHttpConfig -> pure baseReqConfig

{- | Bot HTTP config interpreter

Interprets 'HttpConfigEff' by using the bot client's HTTP manager. This enables
proxy support (via HTTP_PROXY/HTTPS_PROXY environment variables) and custom TLS
settings.

This interpreter is automatically used in 'runBotIO' and related bot runtime
functions, so most users don't need to call it directly.

Note: The @client@ type parameter is polymorphic to avoid circular imports with
'Calamity.Client.Types.Client'. It works with any type that has an @httpManager@
field accessible via optics.
-}
interpretHttpConfigFromClient ::
  forall client r a.
  (Member (P.Reader client) r, LabelOptic "httpManager" A_Lens client client Manager Manager) =>
  Sem (HttpConfigEff ': r) a ->
  Sem r a
interpretHttpConfigFromClient =
  interpret $ \GetHttpConfig -> do
    mgr <- P.asks @client (^. #httpManager)
    pure $ baseReqConfig {httpConfigAltManager = Just mgr}
