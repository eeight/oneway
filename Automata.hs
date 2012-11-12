{-# LANGUAGE TupleSections #-}
module Automata( Edge(..)
               , Automata
               , buildAutomata
               , incomingStates
               , viableStates
               , finalState
               , fastforward
               ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.IntSet as S

import Control.Arrow((***), first, second)
import Control.Monad(liftM, forM_)
import Control.Monad.State(evalState, execState, modify, get, State)
import Data.List(tails)

import Parser

type Automata = [Edge]

data Edge = Append Int Int B.ByteString
          | SetVariable Int Int B.ByteString [Escape]
          | Block Int B.ByteString Automata

brief :: B.ByteString -> B.ByteString
brief str
    | B.length str > 10 = (B.take 7 str) `B.append` (B.pack "...")
    | otherwise = str

instance Show Edge where
    show (Append from to str) =
        "Append " ++ show from ++ " " ++ show to ++ " " ++ show (brief str)
    show (SetVariable from to name escapes) =
        "SetVariable " ++ show from ++
            " " ++ show to ++
            " " ++ B.unpack name ++
            " " ++ show escapes
    show (Block state name body) =
        "Block " ++ show state ++ " " ++ show name ++ " " ++ show body

buildAutomata :: Template -> Automata
buildAutomata template = evalState (go template) (0, 0) where
    go [] = return []
    go (t:ts) = do
        (n, lastN) <- get
        let n' = n + 1
        modify $ succ *** (const n')
        let ret piece = liftM (piece:) $ go ts
        case t of
            TemplateString str -> ret $ Append lastN n' str
            TemplateVariable name escapes -> ret $ SetVariable lastN n' name escapes
            TemplateBlock name block -> do
                subtemplate <- go block
                modify $ second (const lastN)
                ret $ Block lastN name subtemplate

finalState :: Automata -> Int
finalState automata = case (last automata) of
    Append _ to _ -> to
    SetVariable _ to _ _ -> to
    Block state _ _ -> state

incomingStates :: Automata -> M.Map Int [(Int, B.ByteString)]
incomingStates automata = execState (forM_ automata go) M.empty where
    copyList src dst f = modify $ \map ->
            M.insertWith (++) dst (f $ maybe [] id $ M.lookup src map) map

    go (Append from to str) =
        copyList from to $ ((from, str):) . map (second $ flip B.append str)

    go (SetVariable _ to _ _) = return ()

    go (Block state _ subblock) = do
        forM_ subblock go
        let lastState = finalState subblock
        copyList lastState state ((lastState, B.empty):)

fastforward :: Automata -> Maybe (Int, B.ByteString)
fastforward (Append _ to str:_) = Just (to, str)
fastforward (SetVariable state _ _ _:_) = Just (state, B.empty)
fastforward (Block state _ _:_) = Just (state, B.empty)
fastforward _ = Nothing

viableStates :: Automata -> S.IntSet
viableStates automata = let
    append = modify . S.insert

    appendFirst tail = maybe (return ()) (append . fst) (fastforward tail)

    go [] = return ()
    go (t:ts) = case t of
        Append{} -> return ()
        SetVariable _ to _ _ -> if null ts
            then append to
            else appendFirst ts
        Block _ _ body -> appendFirst body >> forM_ (tails body) go

    in execState (appendFirst automata >> forM_ (tails automata) go) S.empty
