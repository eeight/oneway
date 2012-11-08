module OnewayCxxGenerator(generateCxx) where

import Automata
import CxxFormatter
import Generator
import StringMerger
import Templates

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import qualified Data.Map as M

import Control.Monad
import Data.Function(on)
import Data.List(sort, sortBy, groupBy, tails)

generateCxx :: Automata -> B.ByteString -> B.ByteString
generateCxx automata name = let
    states = viableStates automata
    inStates = incomingStates automata

    generateTemplate automata name = let
        maybePutText str
            | B.null str = return ()
            | otherwise = subtemplate "put" $ do
                bind "str" (escape str)
                bind "length" (B.length str)

        generateIncoming state = let
            fromStates =
                    groupBy ((==) `on` snd) $
                    sortBy (compare `on` snd) $
                    filter ((`S.member` states) . fst) $
                    (state, B.empty):maybe [] id (M.lookup state inStates)
            fromGroups = map (\((i, str):ss) -> (sort $ i:map fst ss, str))
                    fromStates
            merger = stringMerger $ map snd fromGroups
            constants = stringConstants merger
            in subtemplate "incoming" $ do
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

        generatePiece (SetVariable from to name:ts) =
            subtemplate "set_variable" $ do
                bindNames name
                generateIncoming from
                case fastforward ts of
                    Just (next', carry) -> do
                        maybePutText carry
                        bind "next_state" next'
                    Nothing -> bind "next_state" to

        generatePiece (Block state name body:_) = do
            subtemplate "subtemplate" $ do
                indented 1
                generateTemplate body name
            subtemplate "add_block" $ do
                bindNames name
                generateIncoming state

        generatePiece _ = return ()
        in do
            let Just (realBegin, carry) = fastforward automata
            bindNames name
            bind "realBegin" realBegin
            maybePutText carry

            generateIncoming (finalState automata)

            forM_ (tails automata) generatePiece

    context = subtemplate "template" $
        generateTemplate automata name
    in render onewayCxxTemplate context
