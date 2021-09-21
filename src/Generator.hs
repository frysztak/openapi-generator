{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import Control.Lens
import Control.Monad.Reader
import Data.List (findIndex)
import Language.TypeScript (Module (Module, body, fileName))
import OpenAPI (OpenAPI, SchemaOrReference)

class GenerateAST a b where
  genAST :: a -> b

data Env p = Env
  { openApi :: OpenAPI,
    param :: p
  }
  deriving (Show)

class GenerateAST' a b where
  genAST' :: Reader (Env a) b

askParam :: Reader (Env a) a
askParam = do
  env <- ask
  pure $ param env

askOpenApi :: Reader (Env a) OpenAPI
askOpenApi = do
  env <- ask
  pure $ openApi env

askEnv :: Reader (Env a) (OpenAPI, a)
askEnv = do
  openApi <- askOpenApi
  param <- askParam
  pure (openApi, param)

makeEnv :: OpenAPI -> a -> Env a
makeEnv openApi a =
  Env
    { openApi = openApi,
      param = a
    }

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