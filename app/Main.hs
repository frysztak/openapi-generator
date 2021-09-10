{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import OpenAPI (decodeOpenApi)
import Options.Applicative
import System.Exit

data CLIArgs = CLIArgs
  { input :: [String],
    outputDir :: String
  }

parseStringList :: Monad m => String -> m [String]
parseStringList = return . Prelude.words

multiString desc = Prelude.concat <$> some single
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
main = runGenerator =<< execParser opts
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> header "OpenAPI client generator"
        )

runGenerator :: CLIArgs -> IO ()
runGenerator CLIArgs {input, outputDir} = do
  contents <- BL.readFile "openapi.json"

  case decodeOpenApi contents of
    Left err -> die $ "Failed to parse JSON: '" ++ err ++ "'"
    Right openApi ->
      do
        -- let ast = generate openApi [interfaceGenerator, fetchGenerator]
        let modules = generate openApi [fetchGenerator]
        putStrLn $ unpack $ pprint (modules !! 0)

        return ()

  return ()
