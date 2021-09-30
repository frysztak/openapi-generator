{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Interface where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text, splitOn)
import Generator (GenerateAST, Generator, genAST)
import Generator.Common
import Language.TypeScript.Syntax
import OpenAPI

interfaceGenerator :: Generator
interfaceGenerator = genAST

instance GenerateAST OpenAPI [Module] where
  genAST openApi =
    [ Module
        { fileName = "models.ts",
          body = components'
        },
      makeIndexModule "./models"
    ]
    where
      components' = mapMaybeArray (genAST <$> openApi ^. #components)

instance GenerateAST Components [Global] where
  genAST components = schemas'
    where
      schemas' = mapMaybeArray (genAST <$> components ^. #schemas)
      parameters' = mapMaybeArray (genAST <$> components ^. #parameters) :: [Global]

instance GenerateAST Schemas [Global] where
  genAST schemas =
    (flatten . M.elems . mapValues' . mapKeys') schemas
    where
      mapKeys' = M.mapKeys fixSchemaName
      mapValues' = M.mapWithKey schemaOrRefToGlobal

instance GenerateAST Parameters [Global] where
  genAST parameters =
    (flatten . M.elems . mapValues' . mapKeys') parameters
    where
      mapKeys' = M.mapKeys fixSchemaName
      mapValues' = M.mapWithKey parameterOrRefToGlobal

schemaToGlobal :: Text -> Schema -> [Global]
schemaToGlobal parentName schema = case schema ^. #itemType of
  "string" -> case schema ^. #enum of
    Nothing -> []
    Just enumValues ->
      [ Export $
          GlobalVar
            VariableDeclaration
              { variableType = Const,
                identifier = VariableName enumValuesName,
                typeReference = Nothing,
                initialValue = Just $ makeADTEnumValues enumValues
              },
        Export $
          GlobalTypeAlias
            enumName
            ( IndexedAccess
                (Typeof (TypeRef enumValuesName))
                Number
            )
      ]
    where
      enumName = getEnumName parentName
      enumValuesName = getEnumValuesName enumName
  "object" ->
    nestedGlobals
      ++ [ (Export . GlobalInterface)
             InterfaceDeclaration
               { name = fixSchemaName parentName,
                 properties = objProps,
                 extends = Nothing
               }
         ]
    where
      properties = schema ^. #properties
      required = schema ^. #required
      mapKeys' = M.mapKeys $ \k ->
        if k `elem` required
          then StringKey k
          else Optional $ StringKey k
      mapValues' = M.mapMaybeWithKey (\k v -> schemaToTypeRef (parentName <> capitalize k) v)
      objProps = mapMaybeMap $ mapKeys' . mapValues' <$> properties

      nestedGlobals =
        (flatten . M.elems . mapMaybeMap) $
          M.mapWithKey (\k v -> schemaOrRefToGlobal (parentName <> capitalize k) v)
            <$> properties
  "array" -> case schema ^. #items of
    Just s -> schemaOrRefToGlobal (parentName <> "ListItem") s
    Nothing -> error "ItemType 'array' without 'items'"
  _ -> []

schemaOrRefToGlobal :: Text -> SchemaOrReference -> [Global]
schemaOrRefToGlobal name (SchemaData s) = schemaToGlobal name s
schemaOrRefToGlobal _ _ = []

parameterOrRefToGlobal :: Text -> ParameterOrReference -> [Global]
parameterOrRefToGlobal name (ParameterData p) = case p ^. #schema of
  Just s -> schemaOrRefToGlobal name s
  _ -> []
parameterOrRefToGlobal _ _ = []

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]

getEnumValuesName :: Text -> Text
getEnumValuesName name = name <> "Values"

makeADTEnum :: [Text] -> Type
makeADTEnum enumValues =
  foldl1
    (\acc enum -> Language.TypeScript.Syntax.Operation Union enum acc)
    (Prelude.map StringLiteral enumValues)

makeADTEnumValues :: [Text] -> Expression
makeADTEnumValues enumValues =
  EAs
    (EArrayLiteral (Prelude.map EString enumValues))
    (TypeRef "const")