{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Enum where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text, splitOn)
import Generator (GenerateAST, Generator, genAST)
import Generator.Common
import Language.TypeScript.Syntax
import OpenAPI

enumGenerator :: Generator
enumGenerator = genAST

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]

instance GenerateAST OpenAPI [Module] where
  genAST openApi =
    [ Module
        { fileName = "models.ts",
          body = components'
        }
    ]
    where
      components' = mapMaybeArray (genAST <$> openApi ^. #components)

instance GenerateAST Components [Global] where
  genAST components = schemas'
    where
      schemas' = mapMaybeArray (genAST <$> components ^. #schemas)

instance GenerateAST Schemas [Global] where
  genAST schemas =
    flatten $ M.elems $ M.mapWithKey mapper schemas
    where
      mapper :: Text -> SchemaOrReference -> [Global]
      mapper name (SchemaData schema) = case (schema ^. #itemType, schema ^. #enum) of
        ("string", Just enumValues) ->
          [ Export $
              GlobalVar
                VariableDeclaration
                  { variableType = Const,
                    identifier = VariableName valuesName,
                    typeReference = Nothing,
                    initialValue = Just $ makeADTEnumValues enumValues
                  },
            Export $
              GlobalTypeAlias
                name
                ( IndexedAccess
                    (Typeof (TypeRef valuesName))
                    Number
                )
          ]
        _ -> []
        where
          valuesName = name <> "Values"
      mapper _ _ = []

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