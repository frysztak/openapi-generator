{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.CommonSpec where

import qualified Data.Map.Strict as M
import Data.Text
import Generator.Common
import OpenAPI
import Test.Hspec

spec :: Spec
spec = do
  describe "getOperationName" $ do
    let commonOperation =
          Operation
            { tags = Nothing,
              summary = Nothing,
              description = Nothing,
              operationId = Nothing,
              parameters = Nothing,
              requestBody = Nothing,
              responses = M.fromList [],
              deprecated = Nothing
            }

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