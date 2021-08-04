{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generator.Fetch where

import Control.Lens hiding (Const)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text hiding (concat, map)
import Generator (GenerateAST, Generator, genAST)
import Generator.Common (getOperationName, mapMaybeArray, schemaToType)
import Language.TypeScript
import OpenAPI

fetchGenerator :: Generator
fetchGenerator = genAST

instance GenerateAST OpenAPI Module where
  genAST openApi = Module $ configInterface : functions'
    where
      configInterface =
        Export . GlobalInterface $
          InterfaceDeclaration
            { name = "ClientConfig",
              extends = Nothing,
              properties =
                M.fromList
                  [ (StringKey "baseUrl", String)
                  ]
            }
      functions = genAST $ openApi ^. #paths
      functions' = [Export . GlobalVar] <*> functions

instance GenerateAST Paths [VariableDeclaration] where
  genAST paths = concat . M.elems $ M.mapWithKey (curry genAST) paths

instance GenerateAST (Text, PathItem) [VariableDeclaration] where
  genAST (endpoint, item) = catMaybes [get', post', put', delete']
    where
      get' = gen "get" <$> item ^. #get
      post' = gen "post" <$> item ^. #post
      put' = gen "put" <$> item ^. #put
      delete' = gen "delete" <$> item ^. #delete

      gen :: Text -> Operation -> VariableDeclaration
      gen verb op = genAST (endpoint, verb, op)

instance GenerateAST (Text, Text, Operation) VariableDeclaration where
  genAST (endpoint, verb, op) = VariableDeclaration {..}
    where
      variableType = Const
      identifier = getOperationName endpoint verb op
      typeReference = Nothing
      fetchConfigLambda = \e ->
        ELambda $
          Lambda
            { async = Just False,
              args =
                [ FunctionArg
                    { name = "config",
                      optional = Nothing,
                      typeReference = Just $ TypeRef "ClientConfig",
                      defaultValue = Nothing
                    }
                ],
              body = LambdaBodyExpr e,
              returnType = Nothing
            }
      fetchLambda =
        ELambda $
          Lambda
            { async = Just True,
              args = getParameters op,
              body = LambdaBodyExpr ENull,
              returnType = do
                type' <- getResponseType $ op ^. #responses
                pure $ Generic (TypeRef "Promise") [type']
            }
      initialValue = Just $ fetchConfigLambda fetchLambda

getParameters :: Operation -> [FunctionArg]
getParameters op = map getParameter parameters'
  where
    parameters' = mapMaybeArray $ op ^. #parameters

getParameter :: ParameterOrReference -> FunctionArg
getParameter (ParameterReference (Reference ref)) =
  FunctionArg
    { name = ref,
      optional = Just False,
      typeReference = Just String,
      defaultValue = Nothing
    }
getParameter (ParameterData p) = FunctionArg {..}
  where
    name = p ^. #name
    optional = fmap not $ p ^. #required
    typeReference = fmap schemaToType $ p ^. #schema
    defaultValue = Nothing

getResponseType :: Responses -> Maybe Type
getResponseType rs = do
  code200 <- rs M.!? "200"
  content <- code200 ^. #content
  json <- content M.!? "application/json"
  pure $ schemaToType $ json ^. #schema
