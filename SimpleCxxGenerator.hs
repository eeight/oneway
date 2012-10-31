{-# LANGUAGE LambdaCase #-}
module SimpleCxxGenerator(generateCxx) where

import Automata
import CxxFormatter

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Text.Printf

generateCxx :: Automata -> B.ByteString -> IO ()
generateCxx automata name = let
    generateTemplate automata name indent parentName = let
        doPut level str =
                putStr (concat $ replicate level "  ") >> putStr str
        put = doPut indent
        put' = doPut (indent + 1)
        put'' = doPut (indent + 2)
        put''' = doPut (indent + 3)

        generateSubtemplate (Block _ name' body) = generateTemplate
                body
                name'
                (indent + 1)
                (Just name)
        generateSubtemplate _ = return ()

        generateMethod (Block _ name body) = lift $ do
            let className = makeClassName name
            let variableName = makeVariableName name
            put' $ printf "%s* add%s() {\n" className className
            put'' $ printf "%s_.emplace_back();\n" variableName
            put'' $ printf "return &%s_.back();\n" variableName
            put' "}\n"
            put' $ printf "%s::Oneway* addOneway%s() {\n" className className
            put'' $ printf "%s_.emplace_back();\n" variableName
            put'' $ printf "return %s_.back().oneway();\n" variableName
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
            let onewayClassName = makeOnewayClassName name
            put $ printf "class %s {\n" className
            put "public:\n"

            case parentName of
                Nothing -> put' $
                    printf "typedef %s Oneway;\n" onewayClassName
                Just name' -> put' $
                    printf "typedef %s::Oneway::%s Oneway;\n"
                            (makeClassName name') (makeOnewayClassName name)

            forM_ automata generateSubtemplate
            runStateT (forM_ automata generateMethod) S.empty

            put' "void generate(std::string* string) const {\n"
            put'' "if (ONEWAY_OVERRIDE_) {\n";
            put''' "ONEWAY_OVERRIDE_->finalize();\n"
            put''' "string->append(\n"
            put''' "    ONEWAY_OVERRIDE_->data(),\n"
            put''' "    ONEWAY_OVERRIDE_->data() + ONEWAY_OVERRIDE_->size());\n"
            put''' "return;\n"
            put'' "}\n";
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

            put' "Oneway* oneway() const {\n"
            put'' "ONEWAY_OVERRIDE_ = Oneway();\n"
            put'' "return &ONEWAY_OVERRIDE_.get();\n"
            put' "}\n";

            putStrLn ""
            put "private:\n"
            forM_ automata generateField
            put' "mutable boost::optional<Oneway> ONEWAY_OVERRIDE_;\n"

            put "};\n"

    in do
        putStrLn "#include <boost/optional.hpp>\n"
        putStrLn ""
        generateTemplate automata name 0 Nothing
