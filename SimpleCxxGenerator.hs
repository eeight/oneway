{-# LANGUAGE LambdaCase #-}
module SimpleCxxGenerator( generateCxx
                         ) where
import Automata
import CxxFormatter

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Text.Printf

generateCxx :: Automata -> B.ByteString -> IO ()
generateCxx automata name = let
    generateTemplate automata name indent = let
        doPut level str = putStr (concat $ replicate level "  ") >> putStr str
        put = doPut indent
        put' = doPut (indent + 1)
        put'' = doPut (indent + 2)
        put''' = doPut (indent + 3)

        generateSubtemplate (Block _ name body) =
                generateTemplate body name (indent + 1)
        generateSubtemplate _ = return ()

        generateMethod (Block _ name body) = lift $ do
            let className = makeClassName name
            let variableName = makeVariableName name
            put' $ printf "%s* add%s() {\n" className className
            put'' $ printf "%s_.emplace_back();\n" variableName
            put'' $ printf "return &%s_.back();\n" variableName
            put' "}\n"
        generateMethod (SetVariable _ _ name) = do
            seen <- gets $ S.member name
            when (not seen) $ do
                modify (S.insert name)
                lift $ do
                    let className = makeClassName name
                    let variableName = makeVariableName name
                    put' $ printf "void set%s(const char* value) {\n"
                            className
                    put'' $ printf "%s_ = value;\n" variableName
                    put' "}\n"
        generateMethod _ = return ()

        generateField (Block _ name _) = do
            let className = makeClassName name
            let variableName = makeVariableName name
            put' $ printf "std::vector<%s> %s_;\n" className variableName
        generateField (SetVariable _ _ name) = do
            let variableName = makeVariableName name
            put' $ printf "std::string %s_;\n" variableName
        generateField _ = return ()

        in do
            let className = makeClassName name
            put $ printf "class %s {\n" className
            put "public:\n"

            forM_ automata generateSubtemplate
            runStateT (forM_ automata generateMethod) S.empty

            put' "std::string generate(std::string* string) const {\n"
            forM_ automata $ \case
                Append _ _ str -> do
                    let l = B.length  str
                    put'' $ printf "*string += \"%s\";\n" (escape str)
                SetVariable _ _ name -> do
                    let variableName = makeVariableName name
                    put'' $ printf "*string += %s_;\n" variableName
                Block _ name _ -> do
                    let variableName = makeVariableName name
                    put'' $ printf "for (const auto& subblock: %s_) {\n"
                            variableName
                    put''' "subblock.generate(string);\n"
                    put'' "}\n"
            put' "}\n"

            putStrLn ""
            put "private:\n"
            forM_ automata generateField

            put "};\n"

    in generateTemplate automata name 0
