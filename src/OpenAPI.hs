{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenAPI where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import Data.Generics.Labels
import qualified Data.Map.Strict as M
import Data.Text
import GHC.Generics

data OpenAPI = OpenAPI
  { openapi :: Text,
    info :: Info,
    -- servers
    paths :: Paths,
    components :: Maybe Components
    -- security
    -- tags
    -- externalDocs
  }
  deriving (Generic, FromJSON, Show)

data Info = Info
  { title :: Text,
    description :: Maybe Text,
    -- contact :: Contact,
    -- license :: License,
    version :: Text
  }
  deriving (Generic, FromJSON, Show)

data PathItem = PathItem
  { get :: Maybe Operation,
    put :: Maybe Operation,
    post :: Maybe Operation,
    delete :: Maybe Operation
  }
  deriving (Generic, FromJSON, Show)

type Paths = M.Map Text PathItem

data Response = Response
  { description :: Text,
    -- headers
    content :: Maybe (M.Map Text MediaType)
    -- links
  }
  deriving (Generic, FromJSON, Show)

type Responses = M.Map Text Response

data ResponseOrReference = ResponseReference Reference | ResponseData Response
  deriving (Generic, Show)

instance FromJSON ResponseOrReference where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding = UntaggedValue
        }

data Operation = Operation
  { tags :: Maybe [Text],
    summary :: Maybe Text,
    description :: Maybe Text,
    -- externalDocs :: Maybe ExternalDocs,
    operationId :: Maybe Text,
    parameters :: Maybe [ParameterOrReference],
    requestBody :: Maybe RequestBodyOrReference,
    responses :: Responses,
    -- callbacks
    deprecated :: Maybe Bool
    -- security
    -- servers
  }
  deriving (Generic, FromJSON, Show)

data RequestBody = RequestBody
  { description :: Maybe Text,
    required :: Maybe Bool,
    content :: M.Map Text MediaType
  }
  deriving (Generic, FromJSON, Show)

data RequestBodyOrReference = RequestBodyReference Reference | RequestBodyData RequestBody
  deriving (Generic, Show)

instance FromJSON RequestBodyOrReference where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding = UntaggedValue
        }

data Schema = Schema
  { -- "type" field
    itemType :: Text,
    -- allOf
    -- oneOf
    -- anyOf
    -- not
    items :: Maybe SchemaOrReference,
    properties :: Maybe (M.Map Text SchemaOrReference),
    -- additionalProperties
    -- description
    format :: Maybe Text,
    -- "default" field
    defaultValue :: Maybe Text,
    enum :: Maybe [Text],
    nullable :: Maybe Bool,
    required :: [Text]
    -- discriminator
    -- readOnly
    -- writeOnly
    -- xml
    -- externalDocs
    -- example :: Maybe Example
    -- deprecated
  }
  deriving (Generic, Show, Eq)

instance FromJSON Schema where
  parseJSON = withObject "schema" $ \o -> do
    itemType <- o .: "type"
    items <- o .:? "items"
    when (itemType == "array" && items == Nothing) $
      prependFailure "parsing Schema failed - " $
        fail "type 'array' requires 'items' to be specified."
    properties <- o .:? "properties"
    format <- o .:? "format"
    defaultValue <- o .:? "default"
    enum <- o .:? "enum"
    nullable <- o .:? "nullable"
    required <- o .:? "required" .!= []
    return Schema {..}

data Reference = Reference
  { ref :: Text
  }
  deriving (Generic, Show, Eq)

data SchemaOrReference = SchemaReference Reference | SchemaData Schema
  deriving (Generic, Show, Eq)

instance FromJSON SchemaOrReference where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding = UntaggedValue
        }

instance FromJSON Reference where
  parseJSON (Object o) = do
    ref <- o .:? "$ref"
    case ref of
      Nothing -> fail $ show o
      Just x -> return Reference {ref = x}
  parseJSON x = fail $ show x

data Parameter = Parameter
  { name :: Text,
    -- "in" field
    location :: Text,
    description :: Maybe Text,
    required :: Maybe Bool,
    deprecated :: Maybe Bool,
    style :: Maybe Text,
    explode :: Maybe Bool,
    allowEmptyValue :: Maybe Bool,
    schema :: Maybe SchemaOrReference
  }
  deriving (Generic, Show, Eq)

instance FromJSON Parameter where
  parseJSON = withObject "parameter" $ \o -> do
    name <- o .: "name"
    location <- o .: "in"
    when (location /= "query" && location /= "header" && location /= "path" && location /= "cookie") $
      prependFailure "parsing Parameter failed - " $
        fail (unpack $ "'location' value '" <> location <> "' is invalid. allowed values: 'query'/'header'/'path'/'cookie'")
    description <- o .:? "description"
    required <- o .:? "required"
    when (location == "path" && (required == Nothing || required == Just False)) $
      prependFailure "parsing Parameter failed - " $
        fail "'required' must be 'true' when 'location=path'"
    deprecated <- o .:? "deprecated"
    style <- o .:? "style"
    explode <- o .:? "explode"
    allowEmptyValue <- o .:? "allowEmptyValue"
    schema <- o .:? "schema"
    return Parameter {..}

data ParameterOrReference = ParameterReference Reference | ParameterData Parameter
  deriving (Generic, Show, Eq)

instance FromJSON ParameterOrReference where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding = UntaggedValue
        }

data MediaType = MediaType
  { schema :: SchemaOrReference
  }
  deriving (Generic, FromJSON, Show)

type Schemas = M.Map Text SchemaOrReference

data Components = Components
  { schemas :: Maybe Schemas,
    responses :: Maybe (M.Map Text ResponseOrReference),
    parameters :: Maybe (M.Map Text ParameterOrReference),
    -- examples
    requestBodies :: Maybe (M.Map Text RequestBodyOrReference)
    -- headers
    -- securitySchemes
    -- links
    -- callbacks
  }
  deriving (Generic, FromJSON, Show)

renameField :: Text -> Text -> (Text -> Text)
renameField dst src x = case x of
  dst -> src

decodeOpenApi :: BS.ByteString -> Either String OpenAPI
decodeOpenApi = eitherDecode
