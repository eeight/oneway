module CxxFormatter (escape
                    , makeVariableName
                    , makeClassName
                    , namesContext
                    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Search as B
import qualified Data.IntSet as S
import qualified Data.Map as M

import Data.Char(toLower, toUpper)

escape str = let
    escapes = [
            ("\r", "\\r"),
            ("\n", "\\n"),
            ("\"", "\\\""),
            ("\\", "\\\\")]
    replaceOne (a, b) = B.concat . L.toChunks . B.replace (B.pack a) (B.pack b)
    replaced = foldr replaceOne str escapes
    in B.unpack replaced

makeVariableName = go . B.unpack where
    go [] = []
    go ('_':x:xs) = toUpper x:go xs
    go (x:xs) = toLower x:go xs

makeClassName name = case makeVariableName name of
    (x:xs) -> toUpper x:xs
    [] -> []

namesContext :: B.ByteString -> [(String, String)]
namesContext name = [
        ("name", makeVariableName name), ("Name", makeClassName name)]
