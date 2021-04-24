{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text, splitOn)
import Language.TypeScript.Syntax
import OpenAPI

mapMaybeMap :: Maybe (M.Map k a) -> M.Map k a
mapMaybeMap (Just m) = m
mapMaybeMap Nothing = M.empty

mapMaybeArray :: Maybe [a] -> [a]
mapMaybeArray (Just a) = a
mapMaybeArray Nothing = []

class GenerateAST a b where
  genAST :: a -> b

instance GenerateAST OpenAPI Module where
  genAST openApi = Module components'
    where
      components' = mapMaybeArray (genAST <$> openApi ^. #components)

instance GenerateAST Components [Global] where
  genAST components = schemas'
    where
      schemas' = mapMaybeArray (genAST <$> components ^. #schemas)

instance GenerateAST Schemas [Global] where
  genAST schemas =
    M.elems $ M.mapWithKey mapper schemas
    where
      mapper :: Text -> SchemaOrReference -> Global
      mapper name schema = case genAST schema of
        Object o ->
          GlobalInterface
            InterfaceDeclaration
              { name = name,
                properties = o,
                extends = Nothing
              }
        _ -> error "Schema is not an object"

instance GenerateAST SchemaOrReference Type where
  genAST (SchemaData schema) = case schema ^. #itemType of
    "string" -> String
    "object" ->
      Object $
        mapMaybeMap $ mapKeys . mapValues <$> properties
      where
        properties = schema ^. #properties
        required = schema ^. #required
        mapKeys = M.mapKeys $ \k ->
          if k `elem` required
            then StringKey k
            else Optional $ StringKey k
        mapValues = M.map genAST
    "integer" -> Number
    "boolean" -> Boolean
    "array" -> case schema ^. #items of
      Just schemaOrRef -> Generic (TypeRef "Array") [genAST schemaOrRef]
      Nothing -> error "ItemType 'array' without 'items'"
    x -> TypeRef x
  genAST (SchemaReference (Reference ref)) = TypeRef $ cleanRef ref
    where
      cleanRef :: Text -> Text
      cleanRef = last . splitOn "/"
