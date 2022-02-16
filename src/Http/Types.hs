module Http.Types (
  AuthResponse (..)
) where

data AuthResponse = Allow | Deny
  deriving (Show)
