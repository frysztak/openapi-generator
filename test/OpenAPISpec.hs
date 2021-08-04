{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenAPISpec where

import Data.Aeson
import Data.ByteString.Lazy
import OpenAPI
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "schema parser" $ do
    let schemaDef =
          Schema
            { itemType = "",
              items = Nothing,
              format = Nothing,
              defaultValue = Nothing,
              enum = Nothing,
              properties = Nothing,
              required = [],
              nullable = Nothing
            }

    it "can parse basic schema" $ do
      let expected = SchemaData $ schemaDef {itemType = "string", format = Just "binary"}
      eitherDecode [r|{"type": "string", "format": "binary"}|] `shouldBe` (Right expected :: Either String SchemaOrReference)

    it "can parse reference" $ do
      let expected = SchemaReference $ Reference "#/components/schemas/User"
      eitherDecode [r|{"$ref": "#/components/schemas/User"}|] `shouldBe` (Right expected :: Either String SchemaOrReference)

    it "can parse schema with internal reference" $ do
      let expected = SchemaData $ schemaDef {itemType = "array", items = Just $ SchemaReference $ Reference "#/components/schemas/User"}
      eitherDecode [r|{"type": "array", "items": {"$ref": "#/components/schemas/User"}}|] `shouldBe` (Right expected :: Either String SchemaOrReference)

    it "can parse enum" $ do
      let expected = SchemaData $ schemaDef {itemType = "string", defaultValue = Just "available", enum = Just ["available", "pending", "sold"]}
      eitherDecode [r|{"type": "string", "default": "available", "enum": ["available", "pending", "sold"]}|] `shouldBe` (Right expected :: Either String SchemaOrReference)

    it "fails for type 'array' without items" $ do
      let expected = "Error in $: parsing Schema failed - type 'array' requires 'items' to be specified."
      eitherDecode [r|{"type": "array"}|] `shouldBe` (Left expected :: Either String SchemaOrReference)
