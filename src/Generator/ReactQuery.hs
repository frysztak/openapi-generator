{-# LANGUAGE OverloadedStrings #-}

module Generator.ReactQuery where

import Generator (Generator)
import Language.TypeScript

reactQueryGenerator :: Generator
reactQueryGenerator openApi = Module []