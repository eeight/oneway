module Templates( simpleCxxTemplate
                , onewayCxxTemplate
                , cxxTemplate
                ) where

import qualified Data.ByteString.Char8 as B

import System.IO.Unsafe

import Parser

unsafeFileContents :: String -> B.ByteString
unsafeFileContents = unsafePerformIO . B.readFile

simpleCxxTemplateString = unsafeFileContents "simple_cxx.template"
onewayCxxTemplateString = unsafeFileContents "oneway_cxx.template"
cxxTemplateString = unsafeFileContents "cxx.template"


simpleCxxTemplate,  onewayCxxTemplate, cxxTemplate :: Template

[simpleCxxTemplate, onewayCxxTemplate, cxxTemplate] = let
    template str = case parse str of
        Left err -> error $ "Cannot parse internal template: " ++ err
        Right template -> template
    strings = [ simpleCxxTemplateString
              , onewayCxxTemplateString
              , cxxTemplateString
              ]
    in map (closeTemplate . template) strings
