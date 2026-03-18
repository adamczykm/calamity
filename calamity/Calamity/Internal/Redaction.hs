module Calamity.Internal.Redaction (
  redactToken,
  redactTokensInText,
  redactDiscordRouteText,
  redactDiscordJSON
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LB
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

redactToken :: Text -> Text
redactToken _ = "<token_redacted>"

redactTokensInText :: (Foldable f) => f Text -> Text -> Text
redactTokensInText tokens text = foldl' redactOne text tokens
  where
    redactOne acc token
      | T.null token = acc
      | otherwise = T.replace token (redactToken token) acc

redactDiscordRouteText :: Text -> Text
redactDiscordRouteText = T.intercalate "/" . go . T.splitOn "/"
  where
    go ("interactions" : id' : token : "callback" : rest) =
      "interactions" : id' : redactToken token : "callback" : go rest
    go ("webhooks" : id' : token : rest) =
      "webhooks" : id' : redactToken token : go rest
    go (segment : rest) = segment : go rest
    go [] = []

redactDiscordJSON :: LB.ByteString -> LB.ByteString
redactDiscordJSON payload = case Aeson.decode payload of
  Just value -> Aeson.encode (redactJSONValue value)
  Nothing -> Aeson.encode (Aeson.String "<payload_redacted>")

redactJSONValue :: Aeson.Value -> Aeson.Value
redactJSONValue = \case
  Aeson.Object object ->
    Aeson.Object $ KeyMap.mapWithKey redactObjectEntry object
  Aeson.Array values ->
    Aeson.Array $ V.map redactJSONValue values
  value -> value
  where
    redactObjectEntry key value
      | Key.toText key `elem` redactedJSONFields = Aeson.String (redactToken "")
      | otherwise = redactJSONValue value

redactedJSONFields :: [Text]
redactedJSONFields =
  [ "token"
  , "access_token"
  ]
