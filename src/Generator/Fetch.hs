{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generator.Fetch where

import Control.Lens hiding (Const)
import Data.Default
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
  genAST openApi =
    Module
      { fileName = "fetch.ts",
        body = importModels : configInterface : functions'
      }
    where
      importModels = GlobalImport $ NamespaceImport "M" "./models"

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
      identifier = VariableName $ getOperationName endpoint verb op
      typeReference = Nothing
      fetchConfigLambda = \e ->
        ELambda $
          Lambda
            { async = Just False,
              args =
                [ makeFunctionArg
                    { name = "config",
                      typeReference = Just $ TypeRef "ClientConfig"
                    }
                ],
              body = LambdaBodyExpr e,
              returnType = Nothing
            }

      returnType' = getResponseType $ op ^. #responses
      returnType'' = fmap (\t -> Generic (TypeRef "Promise") [t]) returnType'

      fetchLambda =
        ELambda $
          Lambda
            { async = Nothing,
              args = getParameters op,
              returnType = returnType'',
              body = getFetchBody endpoint verb op returnType'
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
  pure $ QualifiedName "M" $ schemaToType $ json ^. #schema

getFetchBody :: Text -> Text -> Operation -> Maybe Type -> LambdaBody
getFetchBody path verb op returnType = LambdaBodyStatements stmts
  where
    controller =
      makeVariableDeclaration
        { identifier = VariableName "controller",
          typeReference = Nothing,
          initialValue = Just $ ENew (NewExpression "AbortController" [] [])
        }

    signal =
      makeVariableDeclaration
        { identifier =
            VariableBinding
              [ VariableBindingElement
                  { identifier = "signal",
                    bindingPattern = Nothing,
                    initialValue = Nothing
                  }
              ],
          typeReference = Nothing,
          initialValue = Just $ EVarRef "controller"
        }

    baseUrl =
      makeVariableDeclaration
        { identifier =
            VariableBinding
              [ VariableBindingElement
                  { identifier = "baseUrl",
                    bindingPattern = Nothing,
                    initialValue = Nothing
                  }
              ],
          typeReference = Nothing,
          initialValue = Just $ EVarRef "config"
        }

    url =
      makeVariableDeclaration
        { identifier = VariableName "url",
          typeReference = Nothing,
          initialValue =
            Just
              ( EBinaryOp
                  Add
                  (EVarRef "baseUrl")
                  (EString path)
              )
        }

    promise =
      makeVariableDeclaration
        { identifier = VariableName "promise",
          typeReference = Nothing,
          initialValue =
            Just $
              makePromise
                returnType
                [ makeTryCatch
                    [ fetch,
                      json,
                      checkResponse,
                      resolve
                    ]
                    [ reject
                    ]
                ]
        }
      where
        fetch =
          StatementVar $
            makeVariableDeclaration
              { identifier = VariableName "response",
                initialValue =
                  Just $
                    EAwait $
                      EFunctionCall
                        (EVarRef "fetch")
                        [ EVarRef "url",
                          EObjectLiteral
                            [ ShorthandPropertyAssignment "signal",
                              PropertyAssignment (PropertyExplicitName "method") (EString $ toUpper verb)
                            ]
                        ]
              }

        json =
          StatementVar $
            makeVariableDeclaration
              { identifier = VariableName "json",
                initialValue = Just $ EAwait $ EFunctionCall (EPropertyAccess (EVarRef "response") (EVarRef "json")) []
              }

        checkResponse =
          StatementIf $
            IfStatement
              { condition = EUnaryOp Not (EPropertyAccess (EVarRef "response") (EVarRef "ok")),
                thenBlock =
                  [ StatementExpr $
                      EFunctionCall
                        (EVarRef "reject")
                        [ EObjectLiteral
                            [ ShorthandPropertyAssignment "response",
                              PropertyAssignment (PropertyExplicitName "body") (EVarRef "json")
                            ]
                        ]
                  ],
                elseIf = Nothing,
                elseBlock = Nothing
              }

        resolve = StatementExpr $ EFunctionCall (EVarRef "resolve") [EVarRef "json"]
        reject = StatementExpr $ EFunctionCall (EVarRef "reject") [EVarRef "err"]

    cancel =
      StatementExpr $
        EBinaryOp
          Assignment
          (EPropertyAccess (makeAsAny $ EVarRef "promise") (EVarRef "cancel"))
          ( ELambda $
              Lambda
                { async = Nothing,
                  args = [],
                  returnType = Nothing,
                  body =
                    LambdaBodyExpr $
                      EFunctionCall
                        (EPropertyAccess (EVarRef "controller") (EVarRef "abort"))
                        []
                }
          )
    return = Return $ EVarRef "promise"

    vars = [StatementVar] <*> [controller, signal, baseUrl, url, promise]
    stmts = vars <> [cancel, return]

--- helpers
makePromise :: Maybe Type -> [Statement] -> Expression
makePromise typeArg body =
  ENew $
    NewExpression
      { constructor = "Promise",
        typeArguments = catMaybes [typeArg],
        arguments =
          [ ELambda $
              Lambda
                { async = Just True,
                  args =
                    [ makeFunctionArg {name = "resolve"},
                      makeFunctionArg {name = "reject"}
                    ],
                  returnType = Nothing,
                  body = LambdaBodyStatements body
                }
          ]
      }

makeTryCatch :: Block -> Block -> Statement
makeTryCatch try catch =
  StatementTry $
    TryStatement
      { tryBlock = try,
        catchClause =
          CatchClause
            { variableName = "err",
              variableType = Nothing,
              catchBlock = catch
            },
        finallyBlock = Nothing
      }

makeAsAny :: Expression -> Expression
makeAsAny e = EAs e Any

instance Default FunctionArg where
  def =
    FunctionArg
      { name = "",
        optional = Nothing,
        typeReference = Nothing,
        defaultValue = Nothing
      }

makeFunctionArg :: FunctionArg
makeFunctionArg = def

instance Default VariableDeclaration where
  def =
    VariableDeclaration
      { variableType = Const,
        identifier = VariableName "",
        typeReference = Nothing,
        initialValue = Nothing
      }

makeVariableDeclaration :: VariableDeclaration
makeVariableDeclaration = def
