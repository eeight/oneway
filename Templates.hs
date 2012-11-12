{-# LANGUAGE TemplateHaskell #-}
module Templates( simpleCxxTemplate
                , onewayCxxTemplate
                , cxxTemplate
                ) where

import qualified Data.ByteString.Char8 as B

import Language.Haskell.TH.Syntax

import Parser

simpleCxxTemplate,  onewayCxxTemplate, cxxTemplate :: Template
[simpleCxxTemplate, onewayCxxTemplate, cxxTemplate] = let
    template str = case parse (B.pack str) of
        Left err -> error $ "Cannot parse internal template: " ++ err
        Right template -> template
    strings = [ $(runIO (readFile "simple_cxx.template") >>= liftString)
              , $(runIO (readFile "oneway_cxx.template") >>= liftString)
              , $(runIO (readFile "cxx.template") >>= liftString)
              ]
    in map template strings
