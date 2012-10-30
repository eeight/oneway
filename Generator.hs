module Generator where

import Automata

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Search as B
import qualified Data.IntSet as S
import qualified Data.Map as M

import Control.Monad
import Data.Char(toLower, toUpper)
import Data.Function(on)
import Data.List(sort, sortBy, groupBy, tails)
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

makeVariableName = go . B.unpack where
    go [] = []
    go ('_':x:xs) = toUpper x:go xs
    go (x:xs) = toLower x:go xs

makeClassName name = case makeVariableName name of
    (x:xs) -> toUpper x:xs
    [] -> []

generate :: Automata -> B.ByteString -> Int -> IO ()
generate automata name indent = let
    states = viableStates automata
    inStates = incomingStates automata 

    generateTemplate automata name indent = let
        doPut level str = putStr (concat $ replicate level "  ") >> putStr str
        put = doPut indent
        put' = doPut (indent + 1)
        put'' = doPut (indent + 2)
        put''' = doPut (indent + 3)

        doMaybePutText level str
            | B.null str = return ()
            | otherwise = let
                str' = escape str
                in doPut level $ printf "put(\"%s\", %d);\n" str' (length str')
        maybePutText = doMaybePutText indent
        maybePutText' = doMaybePutText (indent + 1)
        maybePutText'' = doMaybePutText (indent + 2)
        maybePutText''' = doMaybePutText (indent + 3)

        generateIncoming state = let
            fromStates = (state, B.empty):maybe [] id (M.lookup state inStates)
            fromStates' = filter ((`S.member` states) . fst) fromStates
            fromStates'' = groupBy ((==) `on` snd) $
                    sortBy (compare `on` snd) fromStates'
            fromGroups = map (\((i, str):ss) -> (sort $ i:map fst ss, str))
                    fromStates''
            in do
                put'' $ printf "// incoming in %d\n" state
                put'' "switch (state_) {\n"
                put'' "default:\n"
                put''' "  throw WrongState();\n"
                forM_ fromGroups $ \(fs, str) -> do
                        put' " "
                        forM_ fs $ printf " case %d:"
                        printf "\n"
                        maybePutText''' str
                        put''' "break;\n"
                put'' "}\n"

        generatePiece (SetVariable from to name:ts) = do
            let varName = makeVariableName name
            let varName' = makeClassName name
            put' $ printf "template <class... T>\n"
            put' $ printf "void set%s(T... t) {\n" varName'
            generateIncoming from
            put'' "put(t...);\n"
            case fastforward ts of
                Just (next', carry) -> do
                    maybePutText'' carry
                    put'' $ printf "state_ = %d;\n" next'
                Nothing -> put'' $ printf "state_ = %d;\n" to
            put' "}\n"

        generatePiece (Block state name body:_) = do
            generateTemplate body name (indent + 1)
            let className = makeClassName name
            put' $ printf "%s *add%s() {\n" className className
            generateIncoming state
            put'' $ printf "return reinterpret_cast<%s*>(this);\n" className
            put' "}\n"

        generatePiece _ = return ()
        in do
            let Just (realBegin, carry) = fastforward automata
            let className = makeClassName name
            put $ printf "class %s : private AbstractTemplate {\n" className
            put "public:\n"
            put' $ printf "%s() {\n" className
            maybePutText'' carry
            put'' $ printf "state_ = %d;\n" realBegin
            put' $ "}\n"

            forM_ (tails automata) generatePiece

            put' $ printf "~%s() {\n" className
            generateIncoming (finalState automata)
            put' "}\n"

            put "};\n"

    in do
        generateTemplate automata name indent
