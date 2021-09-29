{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generator.InterfaceSpec where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Text
import Generator (genAST)
import Generator.Common
import Generator.Interface
import Language.TypeScript
import OpenAPI
import Test.Hspec
import Text.RawString.QQ (r)

genGlobals :: Schemas -> [Global]
genGlobals = genAST

spec :: Spec
spec = do
  describe "genAST" $ do
    let decodeSchemas = decode :: BS.ByteString -> Maybe Schemas

    it "can process embedded enums" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
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
      },
      "toys": {
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
              "example": "ball"
            }
          }
        }
      }
    }
  }
}
|]
      schemas `shouldNotBe` Nothing

      case schemas of
        Just s' -> do
          let globals = genGlobals =<< [s']
          globals
            `shouldBe` [ Export
                           ( GlobalVar
                               ( VariableDeclaration
                                   { variableType = Const,
                                     identifier = VariableName "PetStatusEnumValues",
                                     typeReference = Nothing,
                                     initialValue = Just (EAs (EArrayLiteral [EString "available", EString "pending", EString "sold"]) (TypeRef "const"))
                                   }
                               )
                           ),
                         Export
                           ( GlobalTypeAlias
                               "PetStatusEnum"
                               ( IndexedAccess (Typeof (TypeRef "PetStatusEnumValues")) Number
                               )
                           ),
                         Export
                           ( GlobalInterface
                               ( InterfaceDeclaration
                                   { name = "PetToysListItem",
                                     extends = Nothing,
                                     properties =
                                       M.fromList
                                         [ (Optional (StringKey "id"), Number),
                                           (Optional (StringKey "name"), String)
                                         ]
                                   }
                               )
                           ),
                         Export
                           ( GlobalInterface
                               ( InterfaceDeclaration
                                   { name = "Pet",
                                     extends = Nothing,
                                     properties =
                                       M.fromList
                                         [ (Optional (StringKey "id"), Number),
                                           (StringKey "name", String),
                                           (Optional (StringKey "category"), TypeRef "Category"),
                                           (StringKey "photoUrls", List String),
                                           (Optional (StringKey "tags"), List (TypeRef "Tag")),
                                           (Optional (StringKey "status"), TypeRef "PetStatusEnum"),
                                           (Optional (StringKey "toys"), List (TypeRef "PetToysListItem"))
                                         ]
                                   }
                               )
                           )
                       ]
        _ -> fail "ee"
