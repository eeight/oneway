module CxxGenerator (generateCxx) where

import qualified OnewayCxxGenerator
import qualified SimpleCxxGenerator

import qualified Data.ByteString.Char8 as B

import Automata
import Generator

generateCxx :: (TextGeneratorMonad m) => Automata -> B.ByteString -> m ()
generateCxx automata name = do
    put $ B.pack "#include \"abstract_template.h\"\n\n"
    put $ B.pack "// Generated code. Do not edit.\n\n"
    put $ B.pack "\n"
    put $ B.pack "// ONEWAY\n"
    OnewayCxxGenerator.generateCxx automata name
    put $ B.pack "\n"
    put $ B.pack "// SIMPLE\n"
    SimpleCxxGenerator.generateCxx automata name
