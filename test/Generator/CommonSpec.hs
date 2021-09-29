{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generator.CommonSpec where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Text
import Generator.Common
import Language.TypeScript.Syntax
import OpenAPI
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "getOperationName" $ do
    let commonOperation =
          OpenAPI.Operation
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (M.fromList [])
            Nothing

    it "can read 'operationId' if provided" $ do
      let operation = commonOperation {operationId = Just "getPets"}
      let name = getOperationName "/pet/{petId}" "get" operation
      name `shouldBe` "getPets"

    it "returns 'unknownEndpoint' if path is not provided" $ do
      let operation = commonOperation {operationId = Nothing}
      let name = getOperationName "" "get" operation
      name `shouldBe` "unknownEndpoint"

    describe "generates name" $ do
      it "for constant path" $ do
        let operation = commonOperation {operationId = Nothing}
        let name = getOperationName "/store/inventory" "get" operation
        name `shouldBe` "getStoreInventory"

      it "for path with dashes" $ do
        let operation = commonOperation {operationId = Nothing}
        let name = getOperationName "/employee/employee-production-profile" "get" operation
        name `shouldBe` "getEmployeeEmployeeProductionProfile"

      it "for positional argument" $ do
        let operation = commonOperation {operationId = Nothing}
        let name = getOperationName "/pet/{petId}" "get" operation
        name `shouldBe` "getPetByPetId"

      it "for nested positional argument" $ do
        let operation = commonOperation {operationId = Nothing}
        let name = getOperationName "/store/order/{orderId}" "delete" operation
        name `shouldBe` "deleteStoreOrderByOrderId"

  describe "schemaToTypeRef" $ do
    let decodeSchemaOrRef = decode :: BS.ByteString -> Maybe SchemaOrReference

    it "works for string" $ do
      let schema =
            decodeSchemaOrRef
              [r|
{
  "type": "string",
  "example": "doggie"
}
|]
      schema `shouldNotBe` Nothing

      let type' = schemaToTypeRef "Pet" =<< schema
      type' `shouldNotBe` Nothing
      type' `shouldBe` Just String

    it "works for embedded enum" $ do
      let schema =
            decodeSchemaOrRef
              [r|
{
  "type": "string",
  "description": "pet status in the store",
  "enum": [
    "available",
    "pending",
    "sold"
  ]
}
|]
      schema `shouldNotBe` Nothing

      let type' = schemaToTypeRef "PetStatus" =<< schema
      type' `shouldNotBe` Nothing
      type' `shouldBe` Just (TypeRef "PetStatusEnum")

    it "works for object embedded as list item" $ do
      let schema =
            decodeSchemaOrRef
              [r|
{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64",
        "example": 10
      },
      "name": {
        "type": "string",
        "example": "doggie"
      }
    }
  }
}
|]
      schema `shouldNotBe` Nothing

      let type' = schemaToTypeRef "PetToys" =<< schema
      type' `shouldNotBe` Nothing
      type' `shouldBe` Just (List (TypeRef "PetToysListItem"))

    it "works for enum embedded as list item" $ do
      let schema =
            decodeSchemaOrRef
              [r|
{
  "type": "array",
  "items": {
    "type": "string",
    "description": "pet status in the store",
    "enum": [
      "available",
      "pending",
      "sold"
    ]
  }
}
|]
      schema `shouldNotBe` Nothing

      let type' = schemaToTypeRef "PetToys" =<< schema
      type' `shouldNotBe` Nothing
      type' `shouldBe` Just (List (TypeRef "PetToysListItemEnum"))

    it "works for object" $ do
      let schema =
            decodeSchemaOrRef
              [r|
{
  "required": ["name", "photoUrls"],
  "type": "object",
  "properties": {
    "id": {
      "type": "integer",
      "format": "int64",
      "example": 10
    },
    "name": {
      "type": "string",
      "example": "doggie"
    },
    "isFluffy": {
      "type": "boolean"
    },
    "category": {
      "$ref": "#/components/schemas/Category"
    },
    "photoUrls": {
      "type": "array",
      "items": {
        "type": "string"
      }
    },
    "tags": {
      "type": "array",
      "items": {
        "$ref": "#/components/schemas/Tag"
      }
    },
    "status": {
      "type": "string",
      "description": "pet status in the store",
      "enum": ["available", "pending", "sold"]
    }
  }
}
|]
      schema `shouldNotBe` Nothing

      let type' = schemaToTypeRef "Pet" =<< schema
      type' `shouldNotBe` Nothing
      type'
        `shouldBe` Just
          ( Object
              ( M.fromList
                  [ (Optional (StringKey "id"), Number),
                    (StringKey "name", String),
                    (Optional (StringKey "isFluffy"), Boolean),
                    (Optional (StringKey "category"), TypeRef "Category"),
                    (StringKey "photoUrls", List String),
                    (Optional (StringKey "tags"), List (TypeRef "Tag")),
                    (Optional (StringKey "status"), TypeRef "PetStatusEnum")
                  ]
              )
          )
