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
    let decodeOperation = decode :: BS.ByteString -> Maybe Operation

    it "can process refs" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
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
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just (QualifiedName "M" $ TypeRef "ApiResponse")

    --    it "can process empty inline schema" $ do
    --      let responses =
    --            decodeResponses
    --              [r|
    --{
    --  "200": {
    --    "description": "successful operation",
    --    "content": {
    --      "application/json": {
    --        "schema": {
    --          "type": "object",
    --          "additionalProperties": {
    --            "type": "integer",
    --            "format": "int32"
    --          }
    --        }
    --      }
    --    }
    --  }
    --}--|]
    --      responses `shouldNotBe` Nothing
    --      responses `shouldNotBe` Just M.empty
    --
    --      let responseType = getResponseType =<< responses
    --      responseType `shouldBe` Just (Object M.empty)
    --
    it "can process string schema" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
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
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just String

    it "can process string array schema" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
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
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just (List String)

    it "can process ref array schema" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
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
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just (List (QualifiedName "M" (TypeRef "Pet")))

    it "can process ref 2D array schema" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
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
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just (List $ List (QualifiedName "M" (TypeRef "Pet")))

    it "can process inline object" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [],
  "responses": {
    "200": {
      "description": "successful operation",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "required": ["name", "id", "isFluffy", "tags"],
            "properties": {
              "name": {
                "type": "string",
                "example": "doggie"
              },
              "id": {
                "type": "integer",
                "format": "int64",
                "example": 10
              },
              "isFluffy": {
                "type": "boolean"
              },
              "tags": {
                "type": "array",
                "items": {
                  "$ref": "#/components/schemas/Tag"
                }
              }
            }
          }
        }
      }
    }
  }
}
|]
      operation `shouldNotBe` Nothing

      let responseType = getResponseType "/pet/{petId}" "get" =<< operation
      responseType `shouldBe` Just (QualifiedName "M" (TypeRef "GetPetByIdSuccessResponse"))

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

  describe "getParameters" $ do
    let decodeOperation = decode :: BS.ByteString -> Maybe Operation

    it "can process ref" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [
    {
      "$ref": "#/components/parameters/PetParam"
    }
  ],
  "responses": {}
}
|]
      operation `shouldNotBe` Nothing

      let params = fmap (getParameters "/pet/{petId}" "get") operation
      params
        `shouldBe` Just
          [ FunctionArg
              { name = "petParam",
                optional = Just False,
                typeReference = Just (QualifiedName "M" $ TypeRef "PetParam"),
                defaultValue = Nothing
              }
          ]

    it "can process string/number/boolean/array ref" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [
    {
      "name": "petId",
      "in": "path",
      "required": true,
      "schema": {
        "type": "string"
      }
    },
    {
      "name": "petAge",
      "in": "query",
      "required": true,
      "schema": {
        "type": "number"
      }
    },
    {
      "name": "petFluffy",
      "in": "query",
      "required": true,
      "schema": {
        "type": "boolean"
      }
    },
    {
      "name": "petTags",
      "in": "query",
      "required": true,
      "schema": {
        "type": "array",
        "items": {
          "$ref": "#/components/schemas/Tag"
        }
      }
    }
  ],
  "responses": {}
}
|]
      operation `shouldNotBe` Nothing

      let params = fmap (getParameters "/pet/{petId}" "get") operation
      params
        `shouldBe` Just
          [ FunctionArg {name = "petId", optional = Just False, typeReference = Just String, defaultValue = Nothing},
            FunctionArg {name = "petAge", optional = Just False, typeReference = Just Number, defaultValue = Nothing},
            FunctionArg {name = "petFluffy", optional = Just False, typeReference = Just Boolean, defaultValue = Nothing},
            FunctionArg {name = "petTags", optional = Just False, typeReference = Just (List (QualifiedName "M" $ TypeRef "Tag")), defaultValue = Nothing}
          ]

    it "can process inline object" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [
    {
      "name": "pet",
      "in": "query",
      "required": true,
      "schema": {
        "type": "object",
        "required": ["name", "id", "isFluffy", "tags"],
        "properties": {
          "name": {
            "type": "string",
            "example": "doggie"
          },
          "id": {
            "type": "integer",
            "format": "int64",
            "example": 10
          },
          "isFluffy": {
            "type": "boolean"
          },
          "tags": {
            "type": "array",
            "items": {
              "$ref": "#/components/schemas/Tag"
            }
          }
        }
      }
    }
  ],
  "responses": {}
}
|]
      operation `shouldNotBe` Nothing

      let params = fmap (getParameters "/pet/{petId}" "get") operation
      params
        `shouldBe` Just
          [ FunctionArg
              { name = "pet",
                optional = Just False,
                typeReference = Just (QualifiedName "M" $ TypeRef "GetPetByIdPetParam"),
                defaultValue = Nothing
              }
          ]

    it "can process inline enum" $ do
      let operation =
            decodeOperation
              [r|
{
  "operationId": "getPetById",
  "parameters": [
    {
      "name": "petStatus",
      "in": "query",
      "required": true,
      "schema": {
        "type": "string",
        "enum": ["available", "pending", "sold"]
      }
    }
  ],
  "responses": {}
}
|]
      operation `shouldNotBe` Nothing

      let params = fmap (getParameters "/pet/{petId}" "get") operation
      params
        `shouldBe` Just
          [ FunctionArg
              { name = "petStatus",
                optional = Just False,
                typeReference = Just (QualifiedName "M" $ TypeRef "GetPetByIdPetStatusParamEnum"),
                defaultValue = Nothing
              }
          ]
