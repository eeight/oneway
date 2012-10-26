import Parser
import Automata
import Generator

import Control.Monad
import Data.Vector(toList)
import Data.Attoparsec
import qualified Data.ByteString.Char8 as B

main = do
    Right template <- liftM (parseOnly parseTemplate) B.getContents
    print template
    let auto = buildAutomata template
    putStrLn ""
    forM_ (toList auto) print
    putStrLn ""
    generate auto 0
