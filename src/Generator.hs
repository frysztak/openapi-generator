{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import Language.TypeScript
import OpenAPI (OpenAPI, SchemaOrReference)

class GenerateAST a b where
  genAST :: a -> b

type Generator = OpenAPI -> Module

generate :: OpenAPI -> [Generator] -> Module
generate openApi gs = mconcat $ gs <*> [openApi]

