{-# LANGUAGE TemplateHaskell #-}

-- | Channel webhooks
module Calamity.Types.Model.Channel.Webhook (Webhook (..)) where

import Calamity.Internal.Redaction (redactToken)
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow qualified

data Webhook = Webhook
  { id :: Snowflake Webhook
  , type_ :: Integer
  , guildID :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , user :: Maybe (Snowflake User)
  , name :: Text
  , avatar :: Text
  , token :: Maybe Text
  }
  deriving (Eq)
  deriving (HasID Webhook) via HasIDField "id" Webhook

instance Show Webhook where
  showsPrec _ Webhook {id, type_, guildID, channelID, user, name, avatar, token} =
    showString "Webhook {id = "
      . shows id
      . showString ", type_ = "
      . shows type_
      . showString ", guildID = "
      . shows guildID
      . showString ", channelID = "
      . shows channelID
      . showString ", user = "
      . shows user
      . showString ", name = "
      . shows name
      . showString ", avatar = "
      . shows avatar
      . showString ", token = "
      . shows (redactToken <$> token)
      . showChar '}'

deriving via TextShow.FromStringShow Webhook instance TextShow.TextShow Webhook

instance Aeson.FromJSON Webhook where
  parseJSON = Aeson.withObject "Webhook" $ \v -> do
    user <- v .:? "user"
    userID <- traverse (.: "id") user

    Webhook
      <$> v .: "id"
      <*> v .: "type"
      <*> v .:? "guild_id"
      <*> v .:? "channel_id"
      <*> pure userID
      <*> v .: "name"
      <*> v .: "avatar"
      <*> v .:? "token"

$(makeFieldLabelsNoPrefix ''Webhook)
