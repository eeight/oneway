module CxxGenerator (generateCxx) where

import qualified OnewayCxxGenerator
import qualified SimpleCxxGenerator

import qualified Data.ByteString.Char8 as B

import Automata

generateCxx :: Automata -> B.ByteString -> IO ()
generateCxx automata name = do
    putStrLn "#include \"abstract_template.h\"\n"
    putStrLn "// Generated code. Do not edit.\n"
    putStrLn ""
    putStrLn "// ONEWAY"
    OnewayCxxGenerator.generateCxx automata name
    putStrLn ""
    putStrLn "// SIMPLE"
    SimpleCxxGenerator.generateCxx automata name
