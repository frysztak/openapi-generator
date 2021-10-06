{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generator.Fetch where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Default
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text, toUpper)
import Generator
import Generator.Common (capitalize, cleanRef, getMediaTypeName, getOperationName, getResponseName, lowercase, makeIndexModule, mapMaybeArray, schemaToType, schemaToTypeRef)
import Language.TypeScript
import OpenAPI

fetchGenerator :: Generator
fetchGenerator = genAST

instance GenerateAST OpenAPI [Module] where
  genAST openApi =
    [ Module
        { fileName = "fetch.ts",
          body = importModels : importCommon : configInterface : functions'
        },
      Module
        { fileName = "common.ts",
          body = common
        },
      makeIndexModule "./fetch"
    ]
    where
      importModels = GlobalImport $ NamespaceImport "M" "./models"
      importCommon = GlobalImport $ NamedImport ["buildUrl", "buildHeaders"] "../common"
      configInterface = Export $ GlobalInterface $ makeFetchConfigInterface openApi

      functions =
        runReader
          genAST'
          (makeEnv openApi (openApi ^. #paths))
      functions' = [Export . GlobalVar] <*> functions

      common =
        makeTypeAliases
          ++ [ (Export . GlobalFunc) makeBuildUrlFunc,
               (Export . GlobalFunc) $ runReader makeBuildHeadersFunc $ makeEnv openApi (),
               GlobalFunc makePopulateEndpointPathParamsFunc
             ]

instance GenerateAST' Paths [VariableDeclaration] where
  genAST' = do
    (openApi, paths) <- askEnv
    pure $
      concat . M.elems $
        M.mapWithKey (\k v -> runReader genAST' $ makeEnv openApi (k, v)) paths

instance GenerateAST' (Text, PathItem) [VariableDeclaration] where
  genAST' = do
    (openApi, (endpoint, item)) <- askEnv

    let gen' = gen openApi endpoint
    let get' = gen' "get" <$> item ^. #get
    let post' = gen' "post" <$> item ^. #post
    let put' = gen' "put" <$> item ^. #put
    let delete' = gen' "delete" <$> item ^. #delete

    pure $ catMaybes [get', post', put', delete']
    where
      gen :: OpenAPI -> Text -> Text -> Operation -> VariableDeclaration
      gen openApi endpoint verb op = runReader genAST' $ makeEnv openApi (endpoint, verb, op)

instance GenerateAST' (Text, Text, Operation) VariableDeclaration where
  genAST' = do
    (openApi, (endpoint, verb, op)) <- askEnv

    let fetchConfigLambda = \e ->
          ELambda $
            Lambda
              { async = Just False,
                args =
                  [ makeFunctionArg
                      { name = "config",
                        typeReference = Just $ TypeRef "FetchConfig"
                      }
                  ],
                body = LambdaBodyExpr e,
                returnType = Nothing
              }

    let returnType' = getResponseType endpoint verb op
    let returnType'' = fmap (\t -> Generic (TypeRef "Promise") [t]) returnType'

    let fetchLambda =
          ELambda $
            Lambda
              { async = Nothing,
                args = getParameters endpoint verb op,
                returnType = returnType'',
                body = runReader getFetchBody $ makeEnv openApi (endpoint, verb, op, returnType')
              }

    pure $
      VariableDeclaration
        { variableType = Const,
          identifier = VariableName $ getOperationName endpoint verb op,
          typeReference = Nothing,
          initialValue = Just $ fetchConfigLambda fetchLambda
        }

getParameters :: Text -> Text -> Operation -> [FunctionArg]
getParameters endpoint verb op = sortFunctionArgs $ map getParam parameters' ++ requestBodyParam
  where
    opName = getOperationName endpoint verb op
    getParam = getParameter opName
    parameters' = mapMaybeArray $ op ^. #parameters
    requestBodyParam = getRequestBodyParameter opName op

sortFunctionArgs :: [FunctionArg] -> [FunctionArg]
sortFunctionArgs =
  sortBy
    ( \a b -> case (a ^. #optional, b ^. #optional) of
        (Just True, Just True) -> Prelude.EQ
        (Just False, Just True) -> Prelude.LT
        (Just True, _) -> Prelude.GT
        (_, _) -> Prelude.EQ
    )

getParametersAtLocation :: Operation -> Text -> [ParameterOrReference]
getParametersAtLocation op loc = parameters'
  where
    parameters = mapMaybeArray $ op ^. #parameters
    parameters' =
      filter
        ( \case
            ParameterData Parameter {location} -> location == loc
            _ -> False -- TODO?
        )
        parameters

mapParametersToObject :: [ParameterOrReference] -> Maybe Expression
mapParametersToObject p = case null objectLiterals of
  True -> Nothing
  _ -> Just $ EObjectLiteral objectLiterals
  where
    objectLiterals =
      foldl
        ( \acc param ->
            case param of
              ParameterData Parameter {name} ->
                acc ++ [ShorthandPropertyAssignment name]
              _ -> acc
        )
        []
        p

getParametersObject :: Text -> Operation -> Maybe Expression
getParametersObject loc op = mapParametersToObject $ getParametersAtLocation op loc

getPathQueryParameters :: Operation -> [Expression]
getPathQueryParameters op = case (path, query) of
  (Just path', Just query') -> [path', query']
  (Nothing, Just query') -> [emptyObj, query']
  (Just path', Nothing) -> [path']
  (_, _) -> []
  where
    query = getParametersObject "query" op
    path = getParametersObject "path" op
    emptyObj = EObjectLiteral []

getParameter :: Text -> ParameterOrReference -> FunctionArg
getParameter _ (ParameterReference (Reference ref)) =
  FunctionArg
    { name = lowercase typeName,
      optional = Just False,
      typeReference = Just $ rewriteImportedTypes (TypeRef typeName),
      defaultValue = Nothing
    }
  where
    typeName = cleanRef ref
getParameter opName (ParameterData p) = FunctionArg {..}
  where
    name = p ^. #name
    optional = fmap not $ p ^. #required
    schemaName = capitalize opName <> capitalize name <> "Param"
    typeReference = fmap rewriteImportedTypes (schemaToTypeRef schemaName =<< (p ^. #schema))
    defaultValue = Nothing

getRequestBodyParameter :: Text -> Operation -> [FunctionArg]
getRequestBodyParameter parentName op = catMaybes [requestBodyToFuncArg parentName =<< parameter']
  where
    parameter' = op ^. #requestBody

requestBodyToFuncArg :: Text -> RequestBodyOrReference -> Maybe FunctionArg
requestBodyToFuncArg _ (RequestBodyReference (Reference ref)) =
  Just
    ( makeFunctionArg
        { name = "requestBody",
          typeReference = (Just . rewriteImportedTypes) $ TypeRef (cleanRef ref)
        } ::
        FunctionArg
    )
requestBodyToFuncArg parentName (RequestBodyData p) = case (jsonArg, octetArg) of
  (_, Just _) -> octetArg
  (Just _, _) -> jsonArg
  (Nothing, Nothing) ->
    error $ "Request body contains '" ++ show (M.keys (p ^. #content)) ++ ", which is not supported.  Only JSON and Octet Stream are supported."
  where
    json = (p ^. #content) M.!? "application/json"
    schemaName = capitalize parentName <> "RequestBody"
    jsonArg =
      fmap
        ( \MediaType {schema} ->
            makeFunctionArg
              { name = "requestBody",
                typeReference = (Just . rewriteImportedTypes) =<< schemaToTypeRef schemaName schema
              } ::
              FunctionArg
        )
        json

    octet = (p ^. #content) M.!? "application/octet-stream"
    octetArg =
      fmap
        ( \MediaType {schema} ->
            makeFunctionArg
              { name = "requestBody",
                typeReference = Just (TypeRef "Blob")
              } ::
              FunctionArg
        )
        octet

getResponseType :: Text -> Text -> Operation -> Maybe Type
getResponseType endpoint verb op = do
  let rs = op ^. #responses
  code200 <- rs M.!? "200"
  type' <- case code200 of
    (ResponseReference (Reference ref)) -> Just $ TypeRef (cleanRef ref)
    (ResponseData r) -> do
      content <- r ^. #content
      json <- content M.!? "application/json"
      let name =
            capitalize (getOperationName endpoint verb op)
              <> getResponseName "200"
              <> getMediaTypeName "application/json"
              <> "Response"
      schemaToTypeRef name (json ^. #schema)

  pure $ rewriteImportedTypes type'

getFetchBody :: Reader (Env (Text, Text, Operation, Maybe Type)) LambdaBody
getFetchBody = do
  (openApi, (path, verb, op, returnType)) <- askEnv
  let useBearer = usesBearerToken openApi
  let bearerToken =
        mapTrue
          ( VariableBindingElement
              { identifier = "bearerToken",
                bindingPattern = Nothing,
                initialValue = Nothing
              }
          )
          useBearer
  let buildHeadersBearerArg = mapTrue (EVarRef "bearerToken") useBearer

  let baseUrl =
        makeVariableDeclaration
          { identifier =
              VariableBinding
                ( VariableBindingElement
                    { identifier = "baseUrl",
                      bindingPattern = Nothing,
                      initialValue = Nothing
                    } :
                  bearerToken
                ),
            typeReference = Nothing,
            initialValue = Just $ EVarRef "config"
          }
  let url =
        makeVariableDeclaration
          { identifier = VariableName "url",
            typeReference = Nothing,
            initialValue =
              Just
                ( EFunctionCall
                    (EVarRef "buildUrl")
                    ( [ EVarRef "baseUrl",
                        EString path
                      ]
                        ++ getPathQueryParameters op
                    )
                )
          }

  let signalArg = ShorthandPropertyAssignment "signal"

  let methodArg =
        PropertyAssignment
          (PropertyExplicitName "method")
          (EString $ toUpper verb)

  let requestBody = op ^. #requestBody
  let hasBlobArg = case requestBody of
        Just reqBody -> case reqBody of
          RequestBodyData r -> M.member "application/octet-stream" (r ^. #content)
          _ -> False
        _ -> False
  let blobContentType =
        mapTrue
          ( EObjectLiteral
              [ PropertyAssignment
                  (PropertyExplicitName "Content-Type")
                  (EString "application/octet-stream")
              ]
          )
          hasBlobArg
  let headersArg =
        PropertyAssignment
          (PropertyExplicitName "headers")
          ( EFunctionCall
              (EVarRef "buildHeaders")
              (buildHeadersBearerArg ++ blobContentType ++ catMaybes [getParametersObject "header" op])
          )
  let dataArg = case op ^. #requestBody of
        Just reqBody ->
          [ PropertyAssignment
              (PropertyExplicitName "body")
              bodyValue
          ]
          where
            bodyValue = case hasBlobArg of
              False ->
                EFunctionCall
                  ( EPropertyAccess (EVarRef "JSON") (EVarRef "stringify")
                  )
                  [EVarRef "requestBody"]
              True -> EVarRef "requestBody"
        Nothing -> []

  let fetch =
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
                          ( [ signalArg,
                              methodArg,
                              headersArg
                            ]
                              ++ dataArg
                          )
                      ]
            }
  let promise =
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
  let vars = [StatementVar] <*> [controller, signal, baseUrl, url, promise]
  pure $ LambdaBodyStatements (vars <> [cancel, return])
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

makePopulateEndpointPathParamsFunc :: FunctionDef
makePopulateEndpointPathParamsFunc =
  FunctionDef
    { name = "populateEndpointPathParams",
      async = Nothing,
      args =
        [ makeFunctionArg
            { name = "endpoint",
              typeReference = Just String
            },
          makeFunctionArg
            { name = "pathParams",
              optional = Just True,
              typeReference = Just $ TypeRef "PathParams"
            }
        ],
      returnType = Just String,
      body =
        [ Return $
            EFunctionCall
              (EPropertyAccess (makeEntriesCall "pathParams") (EVarRef "reduce"))
              [ ELambda $
                  makeLambda
                    { args =
                        [ makeFunctionArg {name = "populatedEndpoint"},
                          makeFunctionArg {name = "pathParam"}
                        ],
                      body =
                        LambdaBodyStatements
                          [ StatementVar $
                              makeVariableDeclaration
                                { identifier =
                                    VariableArrayBinding
                                      [ VariableArrayBindingElement {identifier = "name", initialValue = Nothing},
                                        VariableArrayBindingElement {identifier = "value", initialValue = Nothing}
                                      ],
                                  initialValue = Just $ EVarRef "pathParam"
                                },
                            Return $
                              EFunctionCall
                                (EPropertyAccess (EVarRef "populatedEndpoint") (EVarRef "replace"))
                                [wrapInCurlyBrackets "name", EFunctionCall (EVarRef "String") [EVarRef "value"]]
                          ]
                    },
                EVarRef "endpoint"
              ]
        ]
    }

makeTypeAliases :: [Global]
makeTypeAliases =
  [ GlobalTypeAlias
      "PathParams"
      (Generic (TypeRef "Record") [String, makeUnion [String, Number, Boolean]]),
    GlobalTypeAlias
      "QueryParams"
      (Generic (TypeRef "Record") [String, makeUnion [String, Number, Boolean, List String, List Number, List Boolean]])
  ]
  where
    makeUnion :: [Type] -> Type
    makeUnion =
      foldl1
        (Language.TypeScript.Operation Union)

makeBuildUrlFunc :: FunctionDef
makeBuildUrlFunc =
  FunctionDef
    { name = "buildUrl",
      async = Nothing,
      args =
        [ makeFunctionArg
            { name = "baseUrl",
              typeReference = Just String
            },
          makeFunctionArg
            { name = "endpoint",
              typeReference = Just String
            },
          makeFunctionArg
            { name = "pathParams",
              optional = Just True,
              typeReference = Just $ TypeRef "PathParams"
            },
          makeFunctionArg
            { name = "queryParams",
              optional = Just True,
              typeReference = Just $ TypeRef "QueryParams"
            }
        ],
      returnType = Just String,
      body =
        [ StatementVar $
            makeVariableDeclaration
              { identifier = VariableName "url",
                initialValue =
                  Just $
                    ENew
                      ( NewExpression
                          { constructor = "URL",
                            typeArguments = [],
                            arguments =
                              [ EBinaryOp
                                  Add
                                  (EVarRef "baseUrl")
                                  ( EFunctionCall
                                      (EVarRef "populateEndpointPathParams")
                                      [ EVarRef "endpoint",
                                        EVarRef "pathParams"
                                      ]
                                  )
                              ]
                          }
                      )
              },
          StatementVar $
            makeVariableDeclaration
              { identifier = VariableName "stringifiedQueryParams",
                typeReference = Just $ Generic (TypeRef "Record") [String, String],
                initialValue =
                  Just $
                    EFunctionCall
                      (EPropertyAccess (makeEntriesCall "queryParams") (EVarRef "reduce"))
                      [ ELambda $
                          makeLambda
                            { args =
                                [ makeFunctionArg {name = "acc"},
                                  makeFunctionArg {name = "queryParam"}
                                ],
                              body =
                                LambdaBodyStatements
                                  [ StatementVar $
                                      makeVariableDeclaration
                                        { identifier =
                                            VariableArrayBinding
                                              [ VariableArrayBindingElement {identifier = "name", initialValue = Nothing},
                                                VariableArrayBindingElement {identifier = "value", initialValue = Nothing}
                                              ],
                                          initialValue = Just $ EVarRef "queryParam"
                                        },
                                    Return $
                                      EObjectLiteral
                                        [ SpreadAssignment "acc",
                                          PropertyAssignment
                                            (PropertyComputedName "name")
                                            (EFunctionCall (EVarRef "String") [EVarRef "value"])
                                        ]
                                  ]
                            },
                        EObjectLiteral []
                      ]
              },
          StatementExpr $
            EBinaryOp
              Assignment
              (EPropertyAccess (EVarRef "url") (EVarRef "search"))
              ( EFunctionCall
                  ( EPropertyAccess
                      ( ENew $
                          NewExpression
                            { constructor = "URLSearchParams",
                              typeArguments = [],
                              arguments = [EVarRef "stringifiedQueryParams"]
                            }
                      )
                      (EVarRef "toString")
                  )
                  []
              ),
          Return $
            EFunctionCall
              (EPropertyAccess (EVarRef "url") (EVarRef "toString"))
              []
        ]
    }

makeBuildHeadersFunc :: Reader (Env ()) FunctionDef
makeBuildHeadersFunc = do
  openApi <- askOpenApi
  let useBearer = usesBearerToken openApi
  let bearerFunctionArg =
        mapTrue
          ( makeFunctionArg
              { name = "bearerToken",
                optional = Just True,
                typeReference = Just String
              } ::
              FunctionArg
          )
          useBearer
  let initialBearerHeader =
        mapTrue
          ( PropertyAssignment
              (PropertyExplicitName "Authorization")
              (EBinaryOp Add (EString "Bearer ") (EVarRef "bearerToken"))
          )
          useBearer
  pure $
    FunctionDef
      { name = "buildHeaders",
        async = Nothing,
        args =
          bearerFunctionArg
            ++ [ makeFunctionArg
                   { name = "headerParams",
                     optional = Just True,
                     typeReference = Just $ Generic (TypeRef "Record") [String, String]
                   } ::
                   FunctionArg
               ],
        returnType = Just $ Generic (TypeRef "Record") [String, String],
        body =
          [ Return $
              EFunctionCall
                (EPropertyAccess (makeEntriesCall "headerParams") (EVarRef "reduce"))
                [ ELambda $
                    makeLambda
                      { args =
                          [ makeFunctionArg {name = "headers"},
                            makeFunctionArg {name = "headerParam"}
                          ],
                        body =
                          LambdaBodyStatements
                            [ StatementVar $
                                makeVariableDeclaration
                                  { identifier =
                                      VariableArrayBinding
                                        [ VariableArrayBindingElement {identifier = "name", initialValue = Nothing},
                                          VariableArrayBindingElement {identifier = "value", initialValue = Nothing}
                                        ],
                                    initialValue = Just $ EVarRef "headerParam"
                                  },
                              Return $
                                EObjectLiteral
                                  [ SpreadAssignment "headers",
                                    PropertyAssignment
                                      (PropertyComputedName "name")
                                      (EVarRef "value")
                                  ]
                            ]
                      },
                  EObjectLiteral
                    ( PropertyAssignment
                        (PropertyExplicitName "Content-Type")
                        (EString "application/json; charset=UTF-8") :
                      initialBearerHeader
                    )
                ]
          ]
      }

makeFetchConfigInterface :: OpenAPI -> InterfaceDeclaration
makeFetchConfigInterface openApi =
  InterfaceDeclaration
    { name = "FetchConfig",
      extends = Nothing,
      properties =
        M.fromList
          ( (StringKey "baseUrl", String) : bearerToken
          )
    }
  where
    bearerToken =
      mapTrue
        (StringKey "bearerToken", Language.TypeScript.Operation Union String Undefined)
        (usesBearerToken openApi)

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

instance Default Lambda where
  def =
    Lambda
      { async = Nothing,
        args = [],
        returnType = Nothing,
        body = LambdaBodyStatements []
      }

makeLambda :: Lambda
makeLambda = def

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

makeEntriesCall :: Text -> Expression
makeEntriesCall name =
  EFunctionCall
    ( EPropertyAccess (EVarRef "Object") (EVarRef "entries")
    )
    [EBinaryOp LogicOR (EVarRef name) (EObjectLiteral [])]

wrapInCurlyBrackets :: Text -> Expression
wrapInCurlyBrackets name =
  EBinaryOp
    Add
    (EString "{")
    ( EBinaryOp Add (EVarRef name) (EString "}")
    )

usesBearerToken :: OpenAPI -> Bool
usesBearerToken openApi = do
  let components = openApi ^. #components
  let securitySchemes = (^. #securitySchemes) =<< components
  isJust $ M.lookup "Bearer" =<< securitySchemes

mapTrue :: a -> Bool -> [a]
mapTrue f True = [f]
mapTrue f _ = []

rewriteImportedTypes :: Type -> Type
rewriteImportedTypes (TypeRef t) = QualifiedName "M" $ TypeRef t
rewriteImportedTypes (List t) = List $ rewriteImportedTypes t
rewriteImportedTypes t = t
