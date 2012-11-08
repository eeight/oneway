module SimpleCxxGenerator(generateCxx) where

import Automata
import CxxFormatter
import Generator
import Templates

import qualified Control.Monad.State as State
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Trans(lift)

generateCxx :: Automata -> B.ByteString -> B.ByteString
generateCxx automata name = let
    generateTemplate automata name parentName = let
        generateSubtemplate (Block _ name' body) =
            subtemplate "subtemplate" $ indented 1 >>
                generateTemplate body name' (Just name)
        generateSubtemplate _ = return ()

        addField (Block _ name _) = lift $ do
            let tpl = subtemplate "block" (bindNames name)
            subtemplate "field" tpl
            subtemplate "unique_field" tpl

        addField (SetVariable _ _ name) = do
            let tpl = subtemplate "var" (bindNames name)
            lift $ subtemplate "field" tpl
            seen <- State.gets $ S.member name
            when (not seen) $ do
                State.modify (S.insert name)
                lift $ subtemplate "unique_field" tpl

        addField (Append _ _ str) = lift $ do
            let tpl = subtemplate "str" (bind "str" (escape str))
            subtemplate "field" tpl
            subtemplate "unique_field" tpl

        in do
            bindNames name

            forM_ automata generateSubtemplate 

            case parentName of
                Nothing -> subtemplate "no_parent" (bindNames name)
                Just name' -> subtemplate "has_parent" $ do 
                    bindNames name
                    bindNames' name' "ParentName" "parentName"
                    
            State.evalStateT (forM_ automata addField) S.empty

    context = subtemplate "template" $
        generateTemplate automata name Nothing
    in render simpleCxxTemplate context
