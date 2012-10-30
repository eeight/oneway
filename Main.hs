import Parser
import Automata
import Generator

import Control.Monad
import Data.Vector(toList)
import Data.Attoparsec
import qualified Data.ByteString.Char8 as B

main = do
    template <- liftM (parseOnly parseTemplate) B.getContents
    case template of
        Left err -> putStrLn $ "Error in template syntax: " ++ err
        Right template -> do
            putStrLn "template:"
            print template
            putStrLn "automata:"
            let auto = buildAutomata template
            print auto
            putStrLn "incoming states:"
            print $ incomingStates auto
            putStrLn "viable states:"
            print $ viableStates auto
            generate auto (B.pack "toplevel_template") 0
