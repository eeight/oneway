module CxxFormatter (escape
                    , makeVariableName
                    , makeClassName
                    , bindNames'
                    , bindNames
                    , escaperName
                    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Search as B
import qualified Data.IntSet as S
import qualified Data.Map as M

import Generator
import Parser

import Data.Char(toLower, toUpper, isLower)

escape :: B.ByteString -> B.ByteString
escape str = let
    escapes = [
            ("\r", "\\r"),
            ("\n", "\\n"),
            ("\"", "\\\""),
            ("\\", "\\\\")]
    replaceOne (a, b) = B.concat . L.toChunks . B.replace (B.pack a) (B.pack b)
    in foldr replaceOne str escapes

makeVariableName :: B.ByteString -> String
makeVariableName = go . B.unpack where
    go [] = []
    go s@(x:xs) | not ('_' `elem` s) && any isLower s = toLower x:xs
    go ('_':x:xs) = toUpper x:go xs
    go (x:xs) = toLower x:go xs

makeClassName :: B.ByteString -> String
makeClassName name = case makeVariableName name of
    (x:xs) -> toUpper x:xs
    [] -> []


bindNames' :: B.ByteString -> String -> String -> TextGenerator ()
bindNames' name cname vname = do
    bind cname $ B.pack $ makeClassName name
    bind vname $ B.pack $ makeVariableName name

bindNames :: B.ByteString -> TextGenerator ()
bindNames name = bindNames' name "Name" "name"

escaperName :: Escape -> B.ByteString
escaperName XmlEscape = B.pack "xmlEscape"
escaperName JsonEscape = B.pack "jsonEscape"
