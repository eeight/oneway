module Generator where

import Automata

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Search as B
import qualified Data.Vector as V

import Control.Monad
import Data.Vector((!))
import Text.Printf

escape str = let
    escapes = [
            ("\\", "\\\\"),
            ("\"", "\\\""),
            ("\n", "\\n"),
            ("\r", "\\r"),
            ("\r", "\\r")]
    replaceOne (a, b) = B.concat . L.toChunks . B.replace (B.pack a) (B.pack b)
    replaced = foldr replaceOne str escapes
    in B.unpack replaced

generateIncoming :: Automata -> Int -> IO ()
generateIncoming automata state =
    forM_ (inEdges $ automata ! state) $ \(from, str) -> do
        printf "\tif (state == %d) write(\"%s\")\n" from (escape str)

generate :: Automata -> Int -> IO ()
generate automata begin = do
    let (realBegin, carry) = fastforward automata begin
    printf "begin:\n"
    printf "\twrite(\"%s\")\n" (escape carry)
    printf "\tstate = %d\n" realBegin
    printf "\n"

    let go i = do
        let (State out _) = automata ! i
        forM_ out $ \(next, edge) -> case edge of
            SetVariable name -> do
                printf "set %s:\n" (B.unpack name)
                generateIncoming automata i
                printf "\twrite(\"%s\")\n" (escape name)
                let (next', carry) = fastforward automata next
                printf "\twrite(\"%s\")\n" (escape carry)
                printf "\tstate = %d\n" next'
                go next

            BeginBlock name -> do
                printf "==== BEGIN ====\n"
                generate automata next
                printf "==== END ====\n"
                printf "add %s:\n" (B.unpack name)
                generateIncoming automata i
                printf "\treturn %s\n" (B.unpack name)


            Append _ -> go next
            _ -> return ()
        when (null out) $ do
            printf "end:\n"
            generateIncoming automata i
    go realBegin
