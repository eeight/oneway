module Generator( TextGeneratorMonad(..)
                , TextGenerator
                , runGenerator
                , indent
                , gen
                ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Template as Template

import Control.Arrow((***))
import Control.Monad.Writer

indent :: Int -> B.ByteString -> B.ByteString
indent i text = let
    lines = B.lines text
    indenter = B.concat  $ replicate i (B.pack "  ")
    indentedLines = map (B.append indenter) lines
    in B.concat indentedLines

class (Monad m) => TextGeneratorMonad m where
    put :: B.ByteString -> m ()
    indented :: Int -> m () -> m ()

gen :: (TextGeneratorMonad m) => Int -> String -> [(String, String)] -> m ()
gen i template context = let
    template' = T.pack template
    context' = map (T.pack *** T.pack) context
    accessor x = case lookup x context' of
        Just var -> var
        Nothing -> error $ "Unbound variable " ++ T.unpack x ++ " in template"
    text = T.encodeUtf8 $ Template.substitute template' accessor
    text' = B.concat $ L.toChunks text
    in put (indent i text')

newtype TextGenerator a = TextGenerator
    { runTextGenerator :: Int -> ([B.ByteString], a) }

instance Monad TextGenerator where
    return a = TextGenerator $ const ([], a)
    (TextGenerator gen) >>= f = TextGenerator $ \indent -> let
        (result, v) = gen indent
        (result', v') = runTextGenerator (f v) indent
        in (result ++ result', v')

instance TextGeneratorMonad TextGenerator where
    put string = TextGenerator $ \i -> ([indent i string], ())
    indented i gen = put (runGenerator gen i)

runGenerator :: TextGenerator () -> Int -> B.ByteString
runGenerator gen i = B.concat $ fst $ runTextGenerator gen i
