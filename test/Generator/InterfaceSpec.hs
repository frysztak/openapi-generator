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

genPaths :: Paths -> [Global]
genPaths = genAST

genResponses :: Responses -> [Global]
genResponses = genAST

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

    it "doesn't append 'Enum' if schema name already ends with it" $ do
      let schemas =
            decodeSchemas
              [r|
{
  "PetStatusEnum": {
    "enum": ["available", "pending", "sold"],
    "type": "string"
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
                                 initialValue =
                                   Just
                                     ( EAs
                                         ( EArrayLiteral
                                             [ EString "available",
                                               EString "pending",
                                               EString "sold"
                                             ]
                                         )
                                         (TypeRef "const")
                                     )
                               }
                           )
                       ),
                     Export
                       ( GlobalTypeAlias
                           "PetStatusEnum"
                           (IndexedAccess (Typeof (TypeRef "PetStatusEnumValues")) Number)
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

  describe "Paths generator" $ do
    describe "Parameters inside Operation" $ do
      let decodePaths = decode :: BS.ByteString -> Maybe Paths

      it "generates nothing for string/number/boolean/array ref" $ do
        let paths =
              decodePaths
                [r|
{
  "/pet/{petId}": {
    "get": {
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
  }
}
|]
        paths `shouldNotBe` Nothing

        let globals = maybe [] genPaths paths
        globals `shouldBe` []

      it "can process inline object inside array" $ do
        let paths =
              decodePaths
                [r|
{
  "/pet/{petId}": {
    "get": {
      "operationId": "getPetById",
      "parameters": [
        {
          "name": "petId",
          "in": "path",
          "required": true,
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
      ],
      "responses": {}
    }
  }
}
|]
        paths `shouldNotBe` Nothing

        let globals = maybe [] genPaths paths

        globals
          `shouldBe` [ makeInterface
                         "GetPetByIdPetIdParamToysListItem"
                         ( M.fromList
                             [ (Optional (StringKey "id"), Number),
                               (Optional (StringKey "name"), String)
                             ]
                         ),
                       makeInterface
                         "GetPetByIdPetIdParam"
                         ( M.fromList
                             [ (StringKey "name", String),
                               (StringKey "toys", List (TypeRef "GetPetByIdPetIdParamToysListItem"))
                             ]
                         )
                     ]

  describe "Responses generator" $ do
    describe "Global Responses" $ do
      let decodeResponses = decode :: BS.ByteString -> Maybe Responses

      it "generates nothing for refs" $ do
        let responses =
              decodeResponses
                [r|
{
  "Pet": {
    "description": "a pet to be returned",
    "content": {
      "application/json": {
        "schema": {
          "$ref": "#/components/schemas/Pet"
        }
      }
    }
  }
}
|]
        responses `shouldNotBe` Nothing

        let globals = maybe [] genResponses responses
        globals `shouldBe` []

      it "can process embedded object" $ do
        let responses =
              decodeResponses
                [r|
{
  "Pet": {
    "description": "a pet to be returned",
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
|]
        responses `shouldNotBe` Nothing

        let globals = maybe [] genResponses responses
        globals
          `shouldBe` [ makeInterface
                         "PetResponse"
                         ( M.fromList
                             [ (StringKey "name", String),
                               (StringKey "id", Number),
                               (StringKey "isFluffy", Boolean),
                               (StringKey "tags", List (TypeRef "Tag"))
                             ]
                         )
                     ]

      it "can process inline object inside array" $ do
        let responses =
              decodeResponses
                [r|
{
  "Pet": {
    "description": "a pet to be returned",
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "required": ["name", "id", "isFluffy", "tags", "toys"],
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
  }
}
|]
        responses `shouldNotBe` Nothing

        let globals = maybe [] genResponses responses
        globals
          `shouldBe` [ makeInterface
                         "PetResponseToysListItem"
                         ( M.fromList
                             [ (Optional (StringKey "id"), Number),
                               (Optional (StringKey "name"), String)
                             ]
                         ),
                       makeInterface
                         "PetResponse"
                         ( M.fromList
                             [ (StringKey "name", String),
                               (StringKey "id", Number),
                               (StringKey "isFluffy", Boolean),
                               (StringKey "tags", List (TypeRef "Tag")),
                               (StringKey "toys", List (TypeRef "PetResponseToysListItem"))
                             ]
                         )
                     ]

    describe "Responses inside Operation" $ do
      let decodePaths = decode :: BS.ByteString -> Maybe Paths
      let decodeResponses = decodePaths

      it "generates nothing for refs" $ do
        let paths =
              decodePaths
                [r|
{
  "/pet/{petId}": {
    "get": {
      "operationId": "getPetById",
      "parameters": [],
      "responses": {
        "200": {
          "description": "successful operation",
          "content": {
            "application/json": {
              "schema": {
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
        paths `shouldNotBe` Nothing

        let globals = maybe [] genPaths paths
        globals `shouldBe` []

      it "can process embedded object" $ do
        let paths =
              decodePaths
                [r|
{
  "/pet/{petId}": {
    "get": {
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
  }
}
|]
        paths `shouldNotBe` Nothing

        let globals = maybe [] genPaths paths
        globals
          `shouldBe` [ makeInterface
                         "GetPetByIdSuccessResponse"
                         ( M.fromList
                             [ (StringKey "name", String),
                               (StringKey "id", Number),
                               (StringKey "isFluffy", Boolean),
                               (StringKey "tags", List (TypeRef "Tag"))
                             ]
                         )
                     ]

      it "can process inline object inside array" $ do
        let paths =
              decodePaths
                [r|
{
  "/pet/{petId}": {
    "get": {
      "operationId": "getPetById",
      "parameters": [],
      "responses": {
        "200": {
          "description": "successful operation",
          "content": {
            "application/json": {
              "schema": {
                "type": "object",
                "required": ["name", "id", "isFluffy", "tags", "toys"],
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
        }
      }
    }
  }
}
|]
        paths `shouldNotBe` Nothing

        let globals = maybe [] genPaths paths
        globals
          `shouldBe` [ makeInterface
                         "GetPetByIdSuccessResponseToysListItem"
                         ( M.fromList
                             [ (Optional (StringKey "id"), Number),
                               (Optional (StringKey "name"), String)
                             ]
                         ),
                       makeInterface
                         "GetPetByIdSuccessResponse"
                         ( M.fromList
                             [ (StringKey "name", String),
                               (StringKey "id", Number),
                               (StringKey "isFluffy", Boolean),
                               (StringKey "tags", List (TypeRef "Tag")),
                               (StringKey "toys", List (TypeRef "GetPetByIdSuccessResponseToysListItem"))
                             ]
                         )
                     ]
