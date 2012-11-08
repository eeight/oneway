module CxxGenerator (generateCxx) where

import qualified OnewayCxxGenerator
import qualified SimpleCxxGenerator

import qualified Data.ByteString.Char8 as B

import Automata
import Generator
import Templates

generateCxx :: Automata -> B.ByteString -> B.ByteString
generateCxx automata name = render cxxTemplate $ do
    bind "simple" $ SimpleCxxGenerator.generateCxx automata name
    bind "oneway" $ OnewayCxxGenerator.generateCxx automata  name
