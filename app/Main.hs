{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Codegen
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import Language.TypeScript.Printer (pprint)
import Language.TypeScript.Syntax (Module)
import OpenAPI
import Text.Pretty.Simple (pShow)

showPaths :: Either String OpenAPI -> IO ()
showPaths (Left x) = return ()
showPaths (Right x) = putStrLn $ show $ M.keys (paths x)

-- showPath :: OpenAPI -> Text -> IO ()
-- showPath api pathName = case M.lookup (unpack pathName) $ paths api of
--   Nothing -> putStrLn $ unpack $ "Path '" <> pathName <> "' not found"
--   Just x -> LTextIO.putStrLn $ pShow x

getSchema :: Text -> OpenAPI -> Maybe SchemaOrReference
getSchema n openApi = openApi ^. #components . _Just . #schemas . _Just ^. at n

main :: IO ()
-- main = test

main = do
  contents <- BL.readFile "openapi.json"
  case decodeOpenApi contents of
    Left err -> putStrLn $ "Failed to parse JSON: '" ++ err ++ "'"
    Right openApi ->
      do
        let ast = genAST openApi :: Module
        putStrLn $ unpack $ pprint ast

        return ()

  -- case decodeOpenApi contents of
  --   Left err -> putStrLn $ "Failed to parse JSON: '" ++ err ++ "'"
  --   Right openApi -> showPath openApi "/user/{username}"

  -- LTextIO.putStrLn $ pShow openApi
  -- showPaths openApi
  return ()
