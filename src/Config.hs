{-# LANGUAGE DeriveGeneric #-}
module Config where
import Control.Applicative
import GHC.Generics(Generic)
import Data.Aeson( FromJSON(parseJSON)
                 , genericParseJSON
                 , defaultOptions
                 )
import Data.Aeson.Types(fieldLabelModifier, camelTo2)

data Config = Config
              { nixProjectTool :: Tool
              } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '-'
    }


-- | Like Either, but parses JSON without using tags.
data OneOf l r = IsL l
               | IsR r
               deriving (Show)
instance (FromJSON a, FromJSON b) => FromJSON (OneOf a b) where
  parseJSON v = fmap IsL (parseJSON v) <|> fmap IsR (parseJSON v)


data Tool = Tool { attribute :: Maybe String
                 , location :: Maybe (OneOf Git FilePath)
                 , expression :: Maybe String
                 } deriving (Generic, Show)
instance FromJSON Tool


data Git = Git { git :: String
               , rev :: String
               , sha256 :: String
               } deriving (Generic, Show)
instance FromJSON Git
