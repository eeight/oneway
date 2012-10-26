{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Automata( Edge(..)
               , State(..)
               , Automata
               , buildAutomata
               , fastforward
               , carry
               ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Control.Arrow(first, second)
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector((!))
import Data.List(find)

import Parser


data Edge = Append B.ByteString
          | SetVariable B.ByteString
          | BeginBlock B.ByteString
          | EndBlock
    deriving (Show)

data State = State
    { outEdges :: [(Int, Edge)]
    , inEdges :: [(Int, B.ByteString)]
    } deriving (Show)

type STAutomata s = VM.STVector s State
type Automata = V.Vector State

growAutomata automata count = do
    v <- readSTRef automata
    writeSTRef automata =<< VM.grow v count

addState automata = do
    v <- readSTRef automata >>= flip VM.grow 1
    let result = VM.length v - 1
    VM.write v result (State [] [])
    writeSTRef automata v
    return result

addOutState target ann State{..} =
    State ((target, ann):outEdges) inEdges

addInState source str State{..} =
    State outEdges ((source, str):inEdges)

modify v index f = VM.write v index . f =<< VM.read v index

modifyRef ref index f = do
    vector <- readSTRef ref
    modify vector index f

statesNumber [] = 1
statesNumber (t:ts) = statesNumber ts + case t of
    TemplateBlock _ body -> statesNumber body
    _ -> 1

buildSTAutomata :: Template -> ST s (STAutomata s)
buildSTAutomata template = do
    VM.replicate (statesNumber template) (State [] []) >>= \automata -> let
        mod = modify automata

        go _ _ [] = return ()
        go lastState statesNumber (t:ts) = let
            appendState edge = do
                mod lastState (addOutState statesNumber edge)
                go statesNumber (statesNumber + 1) ts
            in case t of
                TemplateString str -> appendState $ Append str
                TemplateVariable name -> appendState $ SetVariable name
                TemplateBlock name body -> do
                    subAutomata <- buildSTAutomata body
                    let subLength = VM.length subAutomata
                    forM_ [0..subLength - 1] $ \i -> do
                        State{..} <- VM.read subAutomata i
                        let outEdges' = map (first (+ statesNumber)) outEdges
                        VM.write automata (i + statesNumber)
                                (State outEdges' [])
                    mod lastState (addOutState statesNumber (BeginBlock name))
                    mod (statesNumber + subLength - 1)
                            (addOutState lastState EndBlock)
                    go lastState (statesNumber + subLength) ts
        in go 0 1 template >> return automata

fillInEdges automata = do
    let hasIn state from = do
        State{..} <- VM.read automata state
        return $ find ((== from) . fst) inEdges /= Nothing

    let appendIn state [] = return False
        appendIn state ((from, str):fs) = do
            hasIn state from >>= \x -> if x
                then appendIn state fs
                else do
                    modify automata state (addInState from str)
                    appendIn state fs
                    return True
    let
        ap (Append str) = Just str
        ap EndBlock = Just B.empty
        ap _ = Nothing

    let propagate i = do
        State{..} <- VM.read automata i
        forM_ outEdges $ \(next, edge) -> case ap edge of
            Just str -> do
                let nextInEdges =
                        (i, str):map (second $ flip B.append str) inEdges
                appendIn next nextInEdges >>= \x -> if x
                    then propagate next
                    else return()
            _ -> return ()
    (begin, _) <- liftM (flip fastforward 0) $ V.unsafeFreeze automata
    forM_ [begin..VM.length automata - 1] propagate

buildAutomata :: Template -> Automata
buildAutomata t = runST $ do
    a <- buildSTAutomata t
    fillInEdges a
    V.freeze a

fastforward' :: Automata -> Int -> Maybe Int -> (Int, B.ByteString)
fastforward' automata state target = second (B.concat . reverse) $ go [] state where
    go acc state = case automata V.! state of
        (State [(next, Append str)] _) ->
            if Just next == target then (state, acc) else go (str:acc) next
        _ -> (state, acc)

fastforward :: Automata -> Int -> (Int, B.ByteString)
fastforward automata state = fastforward' automata state Nothing

carry :: Automata -> Int -> Int -> Maybe B.ByteString
carry automata from to = let
    (target, str) = fastforward' automata from (Just to)
    in if target == to then Just str else Nothing
