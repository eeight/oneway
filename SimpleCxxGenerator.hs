{-# LANGUAGE LambdaCase #-}
module SimpleCxxGenerator(generateCxx) where

import Automata
import CxxFormatter
import Generator

import qualified Control.Monad.State as State
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Trans(lift)
import Text.Printf

generateCxx :: (TextGeneratorMonad m) => Automata -> B.ByteString -> m ()
generateCxx automata name = let
    generateTemplate automata name parentName = let
        generateSubtemplate (Block _ name' body) =
            indented 1 $ generateTemplate body name' (Just name)
        generateSubtemplate _ = return ()

        generateMethod (Block _ name body) = lift $ gen 1
                (unlines [
                    "$Name* add$name() {",
                    "  ${name}_.emplace_back();",
                    "  return &${name}_.back();",
                    "}",
                    "",
                    "$Name::Oneway* addOneway$Name() {",
                    "    ${name}_.emplace_back();",
                    "    return ${name}_.back().oneway();",
                    "}"
                ]) (namesContext name)

        generateMethod (SetVariable _ _ name) = do
            seen <- State.gets $ S.member name
            when (not seen) $ do
                State.modify (S.insert name)
                lift $ gen 1
                    (unlines [
                        "void set$Name(const char* value) {",
                        "  ${name}_ = value;",
                        "}"
                    ]) (namesContext name)
        generateMethod _ = return ()

        generateField (Block _ name _) =
            gen 1 "std::vector<$Name> ${name}_;\n" (namesContext name)
        generateField (SetVariable _ _ name) = 
            gen 1  "std::string ${name}_;\n" (namesContext name)
        generateField _ = return ()

        in do
            let ctx = namesContext name
            gen 0 "class $Name {\npublic:" ctx

            indented 1 $ case parentName of
                Nothing -> gen 0 "typedef ${Name}Oneway Oneway;\n" ctx
                Just name' -> gen 0
                    "typedef $PName::Oneway::${Name}Oneway Oneway;\n"
                    (("PName", makeClassName name'):ctx)

            forM_ automata generateSubtemplate
            State.runStateT (forM_ automata generateMethod) S.empty

            indented 1 $ put (B.pack $ unlines [
                "void generate(std::string* string) const {",
                "  if (ONEWAY_OVERRIDE_) {",
                "    ONEWAY_OVERRIDE_->finalize();",
                "    string->append(",
                "        ONEWAY_OVERRIDE_->data(),",
                "        ONEWAY_OVERRIDE_->data() + " ++
                    "ONEWAY_OVERRIDE_->size());",
                "  }",
                "return;",
                "}" ])
            forM_ automata $ \case
                Append _ _ str -> put $ B.pack $
                        printf "*string += \"%s\";\n" (escape str)
                SetVariable _ _ name -> 
                    gen 2 "*string += ${name}_;\n" (namesContext name)
                Block _ name _ -> gen 0
                    (unlines [
                        "for (const $Name& subblock: ${name}_) {",
                        "  subblock.generate(string);",
                        "}"
                    ]) (namesContext name)

            put (B.pack $ unlines [
                "  Oneway* oneway() const {",
                "    ONEWAY_OVERRIDE_ = Oneway();",
                "    return &ONEWAY_OVERRIDE_.get();",
                "  }",
                "",
                "private:",
                "   mutable boost::optional<Oneway> ONEWAY_OVERRIDE_;"
                ])

            forM_ automata generateField

            put $ B.pack "};\n"

    in do
        put $ B.pack "#include <boost/optional.hpp>\n"
        put $ B.pack "\n"
        generateTemplate automata name Nothing
