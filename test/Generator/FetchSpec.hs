{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generator.FetchSpec where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Text
import Generator.Common
import Generator.Fetch
import Language.TypeScript
import OpenAPI
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "getResponseType" $ do
    let decodeResponses = decode :: BS.ByteString -> Maybe Responses

    it "can process refs" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "$ref": "#/components/schemas/ApiResponse"
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just (QualifiedName "M" $ TypeRef "ApiResponse")

    it "can process empty inline schema" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "additionalProperties": {
            "type": "integer",
            "format": "int32"
          }
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just (Object M.empty)

    it "can process string schema" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "type": "string"
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just String

    it "can process string array schema" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just (List String)

    it "can process ref array schema" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "type": "array",
          "items": {
            "$ref": "#/components/schemas/Pet"
          }
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just (List (QualifiedName "M" (TypeRef "Pet")))

    it "can process ref 2D array schema" $ do
      let responses =
            decodeResponses
              [r|
{
  "200": {
    "description": "successful operation",
    "content": {
      "application/json": {
        "schema": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {
              "$ref": "#/components/schemas/Pet"
            }
          }
        }
      }
    }
  }
}
|]
      responses `shouldNotBe` Nothing
      responses `shouldNotBe` Just M.empty

      let responseType = getResponseType =<< responses
      responseType `shouldBe` Just (List $ List (QualifiedName "M" (TypeRef "Pet")))

  describe "sortFunctionArgs" $ do
    it "can sort args" $ do
      let makeArg = \name optional ->
            FunctionArg
              { name = name,
                optional = optional,
                typeReference = Nothing,
                defaultValue = Nothing
              }
      let args =
            [ makeArg "arg1" (Just True),
              makeArg "arg2" Nothing,
              makeArg "arg3" (Just True),
              makeArg "arg4" (Just False)
            ]
      let result = sortFunctionArgs args
      result
        `shouldBe` [ makeArg "arg2" Nothing,
                     makeArg "arg4" (Just False),
                     makeArg "arg1" (Just True),
                     makeArg "arg3" (Just True)
                   ]

  describe "requestBodyToFuncArg" $ do
    let decodeReqBody = decode :: BS.ByteString -> Maybe RequestBodyOrReference

    it "works with content JSON ref" $ do
      let reqBody =
            decodeReqBody
              [r|
{
  "content": {
    "application/json": {
      "schema": {
        "$ref": "#/components/schemas/Order"
      }
    }
  }
}
|]
      reqBody `shouldNotBe` Nothing

      let funcArg = requestBodyToFuncArg =<< reqBody
      funcArg
        `shouldBe` Just
          ( FunctionArg
              { name = "requestBody",
                optional = Nothing,
                typeReference = Just (QualifiedName "M" (TypeRef "Order")),
                defaultValue = Nothing
              }
          )

    it "works with octet stream" $ do
      let reqBody =
            decodeReqBody
              [r|
{
  "content": {
    "application/octet-stream": {
      "schema": {
        "type": "string",
        "format": "binary"
      }
    }
  }
}
|]
      reqBody `shouldNotBe` Nothing

      let funcArg = requestBodyToFuncArg =<< reqBody
      funcArg
        `shouldBe` Just
          ( FunctionArg
              { name = "requestBody",
                optional = Nothing,
                typeReference = Just (TypeRef "Blob"),
                defaultValue = Nothing
              }
          )

    it "works with ref" $ do
      let reqBody =
            decodeReqBody
              [r|
{
        "$ref": "#/components/requestBodies/OrderRequestBody"
}
|]
      reqBody `shouldNotBe` Nothing

      let funcArg = requestBodyToFuncArg =<< reqBody
      funcArg
        `shouldBe` Just
          ( FunctionArg
              { name = "requestBody",
                optional = Nothing,
                typeReference = Just (QualifiedName "M" (TypeRef "OrderRequestBody")),
                defaultValue = Nothing
              }
          )