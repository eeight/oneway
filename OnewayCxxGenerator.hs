module OnewayCxxGenerator(generateCxx) where

import Automata
import CxxFormatter
import Generator
import StringMerger
import Templates

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow(second)
import Control.Monad
import Data.Function(on)
import Data.List(sort, sortBy, groupBy, tails)

generateCxx :: Automata -> B.ByteString -> B.ByteString
generateCxx automata name = let
    states = viableStates automata
    inStates = incomingStates automata

    generateTemplate automata name = let
        maybePutText str indent
            | B.null str = return ()
            | otherwise = subtemplate "put" $ do
                indented indent
                bind "str" (escape str)
                bind "length" (B.length str)

        generateIncoming state indent = let
            fromStates =
                    groupBy ((==) `on` snd) $
                    sortBy (compare `on` snd) $
                    filter ((`IS.member` states) . fst) $
                    (state, B.empty):maybe [] id (M.lookup state inStates)
            fromGroups = map (\((i, str):ss) -> (sort $ i:map fst ss, str))
                    fromStates
            merger = stringMerger $ map snd fromGroups
            constants = stringConstants merger
            in subtemplate "incoming" $ do
                indented indent
                bind "state" state
                forM_ constants $ \(name, value) ->
                    subtemplate "constant" $ do
                        bind "name" name
                        bind "str" (escape value)
                forM_ fromGroups $ \(fs, str) -> subtemplate "group" $ do
                    forM_ fs $ subtemplate "state" . bind "state"
                    when (not $ B.null str) $ subtemplate "put" $ do
                        let (name, offset) = oneString merger str
                        bind "name" name
                        bind "offset" offset
                        bind "length" (B.length str)

        generateVariable :: (B.ByteString, [(Int, Int, B.ByteString)])
                         -> TextGenerator ()
        generateVariable (name, sorts) = do
            subtemplate "variable" $ do
                bindNames name
                bind "max_count" $ length sorts
                forM_ (zip [0::Int ..] sorts) $ \(index, (from, to, carry)) ->
                    subtemplate "iteration" $ do
                        bind "index" index
                        generateIncoming from 2
                        maybePutText carry 2
                        bind "next_state" to

        generateBlock :: (B.ByteString, Int, Automata)
                      -> TextGenerator ()
        generateBlock (name, state, body) = do
            subtemplate "subtemplate" $ do
                indented 1
                generateTemplate body name
            subtemplate "add_block" $ do
                bindNames name
                generateIncoming state 0

        variables = let
            go map [] = map
            go map ((SetVariable from to name:ts):tss) = let
                var = case fastforward ts of
                    Just (next, carry) -> (from, next, carry)
                    Nothing -> (from, to, B.empty)
                map' = M.insertWith (++) name [var] map
                in go map' tss
            go map (_:tss) = go map tss

            list = M.toList $ go M.empty (tails automata)
            in map (second reverse) list

        blocks = go S.empty [] automata where
            go _ acc []  = acc
            go seen acc (Block state name body:ts)
                | name `S.member` seen = error $
                    "Duplicate block `" ++ (B.unpack name) ++ "'"
                | otherwise = go
                    (S.insert name seen)
                    ((name, state, body):acc)
                    ts
            go seen acc (_:ts) = go seen acc ts

        in do
            let Just (realBegin, carry) = fastforward automata
            bindNames name
            bind "realBegin" realBegin
            maybePutText carry 0

            generateIncoming (finalState automata) 0

            forM_ variables generateVariable
            forM_ blocks generateBlock

    context = subtemplate "template" $
        generateTemplate automata name
    in render onewayCxxTemplate context
