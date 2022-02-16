{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Http.Encoders () where

import Servant (MimeRender (..), PlainText)
import Http.Types
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson

instance MimeRender PlainText AuthResponse where
  mimeRender _ = pack . show

instance ToJSON AuthResponse where
  toJSON Allow = object [("response", "allow")]
  toJSON Deny  = object [("response", "deny")]
