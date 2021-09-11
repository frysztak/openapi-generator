{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import Control.Lens
import Data.List (findIndex)
import Language.TypeScript (Module (Module, body, fileName))
import OpenAPI (OpenAPI, SchemaOrReference)

class GenerateAST a b where
  genAST :: a -> b

type Generator = OpenAPI -> [Module]

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]

generate :: OpenAPI -> [Generator] -> [Module]
generate openApi gs = mergeModules $ flatten (gs <*> [openApi])

mergeModules :: [Module] -> [Module]
mergeModules = foldl merge []
  where
    merge :: [Module] -> Module -> [Module]
    merge acc tsModule = case findIndex (\m -> fileName m == fileName tsModule) acc of
      Nothing -> acc ++ [tsModule]
      Just idx ->
        acc & element idx
          .~ Module
            { fileName = fileName tsModule,
              body = body (acc !! idx) ++ body tsModule
            }