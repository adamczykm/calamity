{-# LANGUAGE TemplateHaskell #-}

-- | Discord tokens
module Calamity.Types.Token (
  Token (..),
  formatToken,
  rawToken
) where

import Calamity.Internal.Redaction (redactToken)
import Data.Text (Text)
import Optics.TH
import TextShow qualified

data Token
  = BotToken Text
  | UserToken Text

formatToken :: Token -> Text
formatToken (BotToken t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken t) = t
rawToken (UserToken t) = t

instance Show Token where
  showsPrec d (BotToken t) =
    showParen (d > 10) $
      showString "BotToken " . shows (redactToken t)
  showsPrec d (UserToken t) =
    showParen (d > 10) $
      showString "UserToken " . shows (redactToken t)

deriving via TextShow.FromStringShow Token instance TextShow.TextShow Token

$(makeFieldLabelsNoPrefix ''Token)
