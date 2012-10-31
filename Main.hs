{-# LANGUAGE LambdaCase #-}
import Parser(parseTemplate)
import Automata(buildAutomata, viableStates, incomingStates)
import CxxGenerator(generateCxx)

import qualified Data.ByteString.Char8 as B

import Control.Monad
import Data.Attoparsec(parseOnly)
import Data.Vector(toList)
import System.Environment(getArgs, getProgName)
import System.Exit
import System.IO
import Text.Printf(printf)

usage = do
    printf "Usage: %s <toplevel class name> [filename] [--debug]\n" =<< getProgName
    putStrLn "\t--debug Show generated automata instead of generating template"

parse k contents = case parseOnly parseTemplate contents of
    Left err -> do
        hPutStrLn stderr $ "Error in template syntax: " ++ err
        exitFailure
    Right template -> k $ template

generate classname template =
            generateCxx (buildAutomata template) (B.pack classname)

debug template = do
            putStrLn "template:"
            print template
            putStrLn "automata:"
            let auto = buildAutomata template
            print auto
            putStrLn "incoming states:"
            print $ incomingStates auto
            putStrLn "viable states:"
            print $ viableStates auto

main = getArgs >>= \case
    [filename, "--debug"] -> parse debug =<< B.readFile filename
    ["--debug", filename] -> parse debug =<< B.readFile filename
    ["--debug"] -> parse debug =<< B.getContents
    [classname, filename] ->
        parse (generate classname) =<< B.readFile filename
    [classname] -> parse (generate classname) =<< B.getContents
    _ -> usage
