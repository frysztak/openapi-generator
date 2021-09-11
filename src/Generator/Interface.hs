{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Interface where

import Control.Lens
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

instance GenerateAST Schemas [Global] where
  genAST schemas =
    M.elems $ M.mapWithKey mapper schemas
    where
      mapper :: Text -> SchemaOrReference -> Global
      mapper name schema = case genAST schema of
        Object o ->
          (Export . GlobalInterface)
            InterfaceDeclaration
              { name = fixSchemaName name,
                properties = o,
                extends = Nothing
              }
        _ -> error "Schema is not an object"
