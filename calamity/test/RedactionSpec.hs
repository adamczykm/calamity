{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Calamity.Gateway.Intents (defaultIntents)
import Calamity.Internal.Redaction (redactDiscordJSON, redactDiscordRouteText, redactToken, redactTokensInText)
import Calamity.Types.Model.Channel (Channel)
import qualified Calamity.Types.Model.Channel.Webhook as Webhook
import Calamity.Types.Model.Guild (Guild)
import Calamity.Types.Model.Interaction (InteractionToken (..))
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake (Snowflake (..))
import Calamity.Types.Token (Token (..))
import qualified Calamity.Gateway.Types as Gateway
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)
import qualified TextShow

main :: IO ()
main = do
  assertEqual "redactToken" "<token_redacted>" (redactToken "secret")
  assertEqual
    "redactTokensInText"
    "before <token_redacted> after"
    (redactTokensInText ["secret"] "before secret after")
  assertEqual
    "redactDiscordRouteText interaction"
    "https://discord.com/api/v10/interactions/123/<token_redacted>/callback"
    (redactDiscordRouteText "https://discord.com/api/v10/interactions/123/secret/callback")
  assertEqual
    "redactDiscordRouteText webhook"
    "https://discord.com/api/v10/webhooks/456/<token_redacted>/messages/@original"
    (redactDiscordRouteText "https://discord.com/api/v10/webhooks/456/secret/messages/@original")

  let redactedJSON =
        LBS8.unpack . redactDiscordJSON $
          "{\"token\":\"secret\",\"access_token\":\"other\",\"nested\":{\"token\":\"inner\"},\"ok\":1}"
  assertContains "redactDiscordJSON token marker" "<token_redacted>" redactedJSON
  assertNotContains "redactDiscordJSON token" "secret" redactedJSON
  assertNotContains "redactDiscordJSON access_token" "other" redactedJSON
  assertNotContains "redactDiscordJSON nested token" "inner" redactedJSON
  assertContains "redactDiscordJSON preserves other fields" "\"ok\":1" redactedJSON

  assertRedactedShow "Token show" "secret" (show (BotToken "secret"))
  assertRedactedShow "Token showt" "secret" (T.unpack (TextShow.showt (BotToken "secret")))
  assertRedactedShow "InteractionToken show" "secret" (show (InteractionToken "secret"))
  assertRedactedShow "InteractionToken showt" "secret" (T.unpack (TextShow.showt (InteractionToken "secret")))

  let identify =
        Gateway.IdentifyData
          "secret"
          (Gateway.IdentifyProps "browser" "device")
          False
          Nothing
          Nothing
          Nothing
          defaultIntents
  assertRedactedShow "IdentifyData show" "secret" (show identify)

  let resume = Gateway.ResumeData "secret" "session" 7
  assertRedactedShow "ResumeData show" "secret" (show resume)

  let webhook =
        Webhook.Webhook
          (Snowflake 1)
          1
          (Nothing :: Maybe (Snowflake Guild))
          (Nothing :: Maybe (Snowflake Channel))
          (Nothing :: Maybe (Snowflake User))
          "name"
          "avatar"
          (Just "secret")
  assertRedactedShow "Webhook show" "secret" (show webhook)
  assertRedactedShow "Webhook showt" "secret" (T.unpack (TextShow.showt webhook))

  let encodedIdentify = LBS8.unpack $ Aeson.encode identify
  assertContains "IdentifyData JSON keeps raw token for protocol use" "secret" encodedIdentify

assertRedactedShow :: String -> String -> String -> IO ()
assertRedactedShow label secret rendered = do
  assertNotContains label secret rendered
  assertContains (label <> " marker") "<token_redacted>" rendered

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = pure ()
  | otherwise = dieTest $ label <> ": expected " <> show expected <> ", got " <> show actual

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack
  | needle `isInfixOf` haystack = pure ()
  | otherwise = dieTest $ label <> ": missing substring " <> show needle <> " in " <> show haystack

assertNotContains :: String -> String -> String -> IO ()
assertNotContains label needle haystack
  | needle `isInfixOf` haystack = dieTest $ label <> ": found forbidden substring " <> show needle <> " in " <> show haystack
  | otherwise = pure ()

isInfixOf :: String -> String -> Bool
isInfixOf needle = T.isInfixOf (T.pack needle) . T.pack

dieTest :: String -> IO ()
dieTest message = do
  putStrLn message
  exitFailure
