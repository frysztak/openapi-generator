{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Text
import Generator (generate)
import Generator.Fetch
import Language.TypeScript.Printer (pprint)
import OpenAPI

main :: IO ()
main = do
  contents <- BL.readFile "openapi.json"
  case decodeOpenApi contents of
    Left err -> putStrLn $ "Failed to parse JSON: '" ++ err ++ "'"
    Right openApi ->
      do
        -- let ast = generate openApi [interfaceGenerator, fetchGenerator]
        let ast = generate openApi [fetchGenerator]
        putStrLn $ unpack $ pprint ast

        return ()

  return ()
