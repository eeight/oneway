module Main where

import Parser(parse)
import Automata(buildAutomata, viableStates, incomingStates)
import CxxGenerator(generateCxx)

import qualified Data.ByteString.Char8 as B

import Control.Monad
import System.Environment(getArgs, getProgName)
import System.Exit
import System.IO
import Text.Printf(printf)

usage = do
    printf "Usage: %s <toplevel class name> [filename] [--debug]\n" =<< getProgName
    putStrLn "\t--debug Show generated automata instead of generating template"
    exitFailure

doParse k contents = case parse contents of
    Left err -> do
        hPutStrLn stderr $ "Error in template syntax: " ++ err
        exitFailure
    Right template -> k $ template

generate classname template = B.putStr $
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

main2 = doParse (generate "template") =<< B.readFile "t.ow"

main = getArgs >>= \x -> case x of
    [filename, "--debug"] -> doParse debug =<< B.readFile filename
    ["--debug", filename] -> doParse debug =<< B.readFile filename
    ["--debug"] -> doParse debug =<< B.getContents
    [classname, filename] ->
        doParse (generate classname) =<< B.readFile filename
    [classname] -> doParse (generate classname) =<< B.getContents
    _ -> usage
