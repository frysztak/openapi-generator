{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Text (unpack)
import Data.Text.IO (writeFile)
import Data.Version (showVersion)
import Generator (generate)
import Generator.Enum
import Generator.Fetch
import Generator.Interface
import GitHash
import Language.TypeScript.Printer (pprint)
import Language.TypeScript.Syntax (fileName)
import OpenAPI (decodeOpenApi)
import Options.Applicative
import Paths_openapi_generator (version)
import System.Directory
import System.Exit (die)
import System.FilePath (takeBaseName, (</>))
import Prelude hiding (writeFile)

data CLIArgs = CLIArgs
  { input :: [String],
    outputDir :: String
  }

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiString :: Mod OptionFields [String] -> Parser [String]
multiString desc = concat <$> some single
  where
    single = option (str >>= parseStringList) desc

cliParser :: Parser CLIArgs
cliParser =
  CLIArgs
    <$> multiString
      ( short 'i'
          <> long "input"
          <> metavar "JSON"
          <> help "OpenAPI schema in .json"
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "outdir"
          <> value "api"
          <> showDefault
          <> help "Output directory"
      )

main :: IO ()
main = do
  options <- execParser opts
  runGenerator options
  where
    opts =
      info
        (cliParser <**> helper)
        (fullDesc <> header ("OpenAPI client generator v" ++ showVersion version ++ " (" ++ giHash gi ++ ")"))
    gi = $$tGitInfoCwd

runGenerator :: CLIArgs -> IO ()
runGenerator CLIArgs {input, outputDir} = do
  createDirectoryIfMissing True outputDir

  forM_ input $ \inputFileName -> do
    let baseName = takeBaseName inputFileName
    let targetDir = outputDir </> baseName
    createDirectoryIfMissing True targetDir
    putStrLn $ "Running generator for " <> inputFileName <> "..."

    contents <- BL.readFile inputFileName
    case decodeOpenApi contents of
      Left err -> die $ "Failed to parse JSON: '" <> err <> "'"
      Right openApi ->
        do
          let modules = generate openApi [interfaceGenerator, fetchGenerator, enumGenerator]
          forM_ modules $ \tsModule -> do
            let moduleName = fileName tsModule
            let dir = if moduleName == "common.ts" then outputDir else targetDir
            let modulePath = dir </> unpack (fileName tsModule)
            writeFile modulePath $ pprint tsModule
            putStrLn $ "Generated " <> modulePath
