module Generator where

import Automata

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Search as B
import qualified Data.Vector as V
import qualified Data.IntSet as S

import Control.Monad
import Data.Function(on)
import Data.List(sort, sortBy, groupBy)
import Data.Vector((!))
import Text.Printf

escape str = let
    escapes = [
            ("\r", "\\r"),
            ("\n", "\\n"),
            ("\"", "\\\""),
            ("\\", "\\\\")]
    replaceOne (a, b) = B.concat . L.toChunks . B.replace (B.pack a) (B.pack b)
    replaced = foldr replaceOne str escapes
    in B.unpack replaced

viableStates :: Automata -> S.IntSet
viableStates automata =
    S.fromList $ map (fst . fastforward automata) [0..V.length automata - 1]

generateIncoming :: Automata -> Int -> IO ()
generateIncoming automata state = let
    states = viableStates automata
    fromStates = filter
            (\(from, str) -> from `S.member` states && not (B.null str))
            (inEdges $ automata ! state)
    fromStates' = groupBy ((==) `on` snd) $
            sortBy (compare `on` snd) fromStates
    fromGroups = map (\((i, str):ss) -> (sort $ i:map fst ss, str)) fromStates'
    in forM_ fromGroups $ \(fs, str) -> 
            printf "\tif (state in %s) write(\"%s\")\n" (show fs) (escape str)

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
                printf "\twrite($%s)\n" (B.unpack name)
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
