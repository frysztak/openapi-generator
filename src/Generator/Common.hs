{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Common where

import Control.Lens ((^.))
import Control.Monad (guard, join, when)
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Map.Strict as M (Map, empty, map, mapKeys, mapMaybe)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text, concat, dropAround, head, null, singleton, splitOn, tail, toTitle, unpack)
import Generator (GenerateAST, genAST)
import Language.TypeScript.Syntax
import OpenAPI
import Prelude hiding (concat, head, null, tail)

mapMaybeMap :: Maybe (Map k a) -> Map k a
mapMaybeMap (Just m) = m
mapMaybeMap Nothing = empty

mapMaybeArray :: Maybe [a] -> [a]
mapMaybeArray (Just a) = a
mapMaybeArray Nothing = []

schemaToType :: SchemaOrReference -> Maybe Type
schemaToType = genAST

instance GenerateAST SchemaOrReference (Maybe Type) where
  genAST (SchemaData schema) = fmap makeNullable' type'
    where
      makeNullable' = case schema ^. #nullable of
        Just True -> makeNullable
        _ -> id

      type' = case schema ^. #itemType of
        "string" -> case schema ^. #enum of
          Nothing -> Just String
          Just enumValues ->
            Just $
              foldl1
                (Language.TypeScript.Syntax.Operation Union)
                (Prelude.map StringLiteral enumValues)
        "object" ->
          Just $ Object objProps
          where
            properties = schema ^. #properties
            required = schema ^. #required
            mapKeys' = mapKeys $ \k ->
              if k `elem` required
                then StringKey k
                else Optional $ StringKey k
            mapValues' = M.mapMaybe genAST
            objProps = mapMaybeMap $ mapKeys' . mapValues' <$> properties
        "integer" -> Just Number
        "boolean" -> Just Boolean
        "array" -> case schema ^. #items of
          Just schemaOrRef -> (Just . List) =<< genAST schemaOrRef
          Nothing -> error "ItemType 'array' without 'items'"
        x -> Just $ TypeRef x
  genAST (SchemaReference (Reference ref)) = Just $ TypeRef $ cleanRef ref

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

fixSchemaName :: Text -> Text
fixSchemaName "Error" = "ErrorResponse"
fixSchemaName x = x

makeIndexModule :: Text -> Module
makeIndexModule name = Module "index.ts" [ExportAll name]

makeNullable :: Type -> Type
makeNullable t = Language.TypeScript.Syntax.Operation Union t Null
