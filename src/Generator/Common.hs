{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Common where

import Control.Lens hiding (List)
import Control.Monad (guard, when)
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Map.Strict (Map, empty, map, mapKeys)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text, concat, dropAround, head, null, singleton, splitOn, tail, toTitle, unpack)
import Generator (GenerateAST, genAST)
import Language.TypeScript.Syntax
import OpenAPI
import Prelude hiding (concat, head, map, null, tail)

mapMaybeMap :: Maybe (Map k a) -> Map k a
mapMaybeMap (Just m) = m
mapMaybeMap Nothing = empty

mapMaybeArray :: Maybe [a] -> [a]
mapMaybeArray (Just a) = a
mapMaybeArray Nothing = []

schemaToType :: SchemaOrReference -> Type
schemaToType = genAST

instance GenerateAST SchemaOrReference Type where
  genAST (SchemaData schema) = case schema ^. #itemType of
    -- TODO: handle nullable values
    "string" -> String
    "object" ->
      Object $
        mapMaybeMap $ mapKeys' . mapValues' <$> properties
      where
        properties = schema ^. #properties
        required = schema ^. #required
        mapKeys' = mapKeys $ \k ->
          if k `elem` required
            then StringKey k
            else Optional $ StringKey k
        mapValues' = map genAST
    "integer" -> Number
    "boolean" -> Boolean
    "array" -> case schema ^. #items of
      Just schemaOrRef -> List $ genAST schemaOrRef
      Nothing -> error "ItemType 'array' without 'items'"
    x -> TypeRef x
  genAST (SchemaReference (Reference ref)) = TypeRef $ cleanRef ref
    where
      cleanRef :: Text -> Text
      cleanRef = last . splitOn "/"

getOperationName :: Text -> Text -> Operation -> Text
getOperationName path verb op = fromMaybe name $ op ^. #operationId
  where
    name = fromMaybe "unknownEndpoint" $ do
      let chunks = omitFirstEmpty $ splitOn "/" path
      guard (not $ L.null chunks)

      let chunks' = L.map formatChunk chunks
      guard (allJust chunks')

      let concatChunks = (verb <>) . concat . catMaybes
      pure $ concatChunks chunks'

    omitFirstEmpty :: [Text] -> [Text]
    omitFirstEmpty [] = []
    omitFirstEmpty (x : xs) = case x of
      "" -> xs
      _ -> x : xs

    allJust :: [Maybe a] -> Bool
    allJust = all ((== True) . isJust)

    formatChunk :: Text -> Maybe Text
    formatChunk s = do
      guard (not $ null s)

      let preprocess = capitalize . removeDashes
      let processBracket = ("By" <>) . preprocess . dropAround isBracket

      case head s of
        '{' -> pure $ processBracket s
        _ -> pure $ preprocess s

    isBracket :: Char -> Bool
    isBracket '{' = True
    isBracket '}' = True
    isBracket _ = False

    capitalize :: Text -> Text
    capitalize str = case null str of
      True -> ""
      False -> toUpper (head str) <> tail str
        where
          toUpper = singleton . Char.toUpper

    removeDashes :: Text -> Text
    removeDashes s = concat $ L.map capitalize chunks
      where
        chunks = splitOn "-" s
