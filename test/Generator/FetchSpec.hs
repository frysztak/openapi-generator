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