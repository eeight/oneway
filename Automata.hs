{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Automata( Edge(..)
               , State(..)
               , Automata
               , buildAutomata
               ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Control.Monad
import Control.Monad.ST
import Data.STRef

import Parser

data Edge = Append B.ByteString
          | SetVariable B.ByteString
          | BeginBlock B.ByteString
          | EndBlock
    deriving (Show)

data State = State
    { outEdges :: [(Int, Edge)]
    , inEdges :: [(Int, Edge)]
    } deriving (Show)

type STAutomata s = VM.STVector s State
type Automata = V.Vector State

growAutomata automata count = do
    v <- readSTRef automata
    writeSTRef automata =<< VM.grow v count

addState automata = do
    v <- readSTRef automata >>= flip VM.grow 1
    let result = pred $ VM.length v
    VM.write v result (State [] [])
    writeSTRef automata v
    return result

addOutState target ann State{..} =
    State ((target, ann):outEdges) inEdges

addInState source ann State{..} =
    State outEdges ((source, ann):inEdges)

modify v index f = VM.write v index . f =<< VM.read v index

modifyRef ref index f = do
    vector <- readSTRef ref
    modify vector index f

buildSTAutomata :: Template -> ST s (STAutomata s)
buildSTAutomata template = do
    automata <- newSTRef =<< VM.new 0
    lastState <- newSTRef =<< addState automata
    let appendState edge = do
        state <- addState automata
        prevState <- readSTRef lastState
        modifyRef automata prevState (addOutState state edge)
        writeSTRef lastState state

    forM_ template $ \case
        TemplateString str -> appendState $ Append str
        TemplateVariable name -> appendState $ SetVariable name
        TemplateBlock name body -> do
            subAutomata <- buildSTAutomata body
            let subLength = VM.length subAutomata
            growAutomata automata subLength
            state <- readSTRef lastState
            v <- readSTRef automata
            let base = VM.length v - subLength
            forM_ [0..pred subLength] $ \i -> do
                State{..} <- VM.read subAutomata i
                let outEdges' = map (\(i, e) -> (i + base, e)) outEdges
                VM.write v (i + base) (State outEdges' [])
            modify v state (addOutState base (BeginBlock name))
            modify v (pred $ VM.length v) (addOutState state EndBlock)

    readSTRef automata

fillInEdges automata = forM_ [0..pred (VM.length automata)] $ \i -> do
    (State outEdges _) <- VM.read automata i
    forM_ outEdges $ \(target, edge) -> modify automata target (addInState i edge)

buildAutomata :: Template -> Automata
buildAutomata t = runST $ do
    a <- buildSTAutomata t
    fillInEdges a
    V.freeze a
