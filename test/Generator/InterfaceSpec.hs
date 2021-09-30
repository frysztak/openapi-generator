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

genSchemas :: Schemas -> [Global]
genSchemas = genAST

genParameters :: Parameters -> [Global]
genParameters = genAST

makeInterface :: Text -> Object -> Global
makeInterface name props =
  Export
    ( GlobalInterface
        ( InterfaceDeclaration
            { name = name,
              extends = Nothing,
              properties = props
            }
        )
    )

spec :: Spec
spec = do
  describe "Schemas generator" $ do
    let decodeSchemas = decode :: BS.ByteString -> Maybe Schemas

    it "can process type string/number/boolean" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
    "type": "object",
    "required": ["name", "id", "isFluffy"],
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
      }
    }
  }
}
|]
      schemas `shouldNotBe` Nothing
      let globals = maybe [] genSchemas schemas

      globals
        `shouldBe` [ makeInterface
                       "Pet"
                       ( M.fromList
                           [ (StringKey "name", String),
                             (StringKey "id", Number),
                             (StringKey "isFluffy", Boolean)
                           ]
                       )
                   ]

    it "can process inline enums inside object" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
    "type": "object",
    "properties": {
      "status": {
        "type": "string",
        "description": "pet status in the store",
        "enum": ["available", "pending", "sold"]
      }
    }
  }
}
|]
      schemas `shouldNotBe` Nothing
      let globals = maybe [] genSchemas schemas

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
                     makeInterface
                       "Pet"
                       ( M.fromList
                           [ (Optional (StringKey "status"), TypeRef "PetStatusEnum")
                           ]
                       )
                   ]

    it "can process inline object inside array" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
    "type": "object",
    "properties": {
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
      let globals = maybe [] genSchemas schemas

      globals
        `shouldBe` [ makeInterface
                       "PetToysListItem"
                       ( M.fromList
                           [ (Optional (StringKey "id"), Number),
                             (Optional (StringKey "name"), String)
                           ]
                       ),
                     makeInterface
                       "Pet"
                       ( M.fromList
                           [ (Optional (StringKey "toys"), List (TypeRef "PetToysListItem"))
                           ]
                       )
                   ]

    it "can process ref inside object" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "$ref": "#/components/schemas/PetName"
      }
    }
  }
}
|]
      schemas `shouldNotBe` Nothing
      let globals = maybe [] genSchemas schemas

      globals
        `shouldBe` [ makeInterface
                       "Pet"
                       ( M.fromList
                           [ (StringKey "name", TypeRef "PetName")
                           ]
                       )
                   ]

    it "can process ref inside array" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "Pet": {
    "type": "object",
    "required": ["tags"],
    "properties": {
      "tags": {
        "type": "array",
        "items": {
          "$ref": "#/components/schemas/Tag"
        }
      }
    }
  }
}
|]
      schemas `shouldNotBe` Nothing
      let globals = maybe [] genSchemas schemas

      globals
        `shouldBe` [ makeInterface
                       "Pet"
                       ( M.fromList
                           [ (StringKey "tags", List $ TypeRef "Tag")
                           ]
                       )
                   ]

  describe "Parameters generator" $ do
    let decodeParameters = decode :: BS.ByteString -> Maybe Parameters

    it "can process type string/number/boolean/array" $ do
      let parameters =
            decodeParameters
              [r|
{
  "PetParam": {
    "in": "query",
    "name": "pet",
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
|]
      parameters `shouldNotBe` Nothing

      let globals = maybe [] genParameters parameters

      globals
        `shouldBe` [ makeInterface
                       "PetParam"
                       ( M.fromList
                           [ (StringKey "name", String),
                             (StringKey "id", Number),
                             (StringKey "isFluffy", Boolean),
                             (StringKey "tags", List $ TypeRef "Tag")
                           ]
                       )
                   ]

    it "can process inline object inside array" $ do
      let parameters =
            decodeParameters
              [r|
{
  "PetParam": {
    "in": "query",
    "name": "pet",
    "schema": {
      "type": "object",
      "required": ["name", "toys"],
      "properties": {
        "name": {
          "type": "string",
          "example": "doggie"
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
}
|]
      parameters `shouldNotBe` Nothing

      let globals = maybe [] genParameters parameters

      globals
        `shouldBe` [ makeInterface
                       "PetParamToysListItem"
                       ( M.fromList
                           [ (Optional (StringKey "id"), Number),
                             (Optional (StringKey "name"), String)
                           ]
                       ),
                     makeInterface
                       "PetParam"
                       ( M.fromList
                           [ (StringKey "name", String),
                             (StringKey "toys", List (TypeRef "PetParamToysListItem"))
                           ]
                       )
                   ]
