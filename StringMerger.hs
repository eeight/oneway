module StringMerger( StringMerger
                   , stringMerger
                   , stringConstants
                   , oneString
                   ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Data.Function(on)
import Data.List(find, sort)
import Numeric(showHex)

data StringMerger = StringMerger
    { smStrings :: [B.ByteString]
    }


stringMerger :: [B.ByteString] -> StringMerger
stringMerger =
    StringMerger . go [] . reverse . reverseAll . sort . reverseAll
  where
    reverseAll = map B.reverse

    go acc [] = acc
    go acc (s:ss) = if B.null s || any (B.isSuffixOf s) acc
        then go acc ss
        else go (s:acc) ss

stringName str = "STRING_CONST_" ++ concatMap hex hash where
    hash = B.unpack $ SHA256.hash str
    hex = ($ "") . showHex . fromEnum

stringConstants :: StringMerger -> [(String, B.ByteString)]
stringConstants = map makeConst . smStrings where
    makeConst str = (stringName str, str)

oneString :: StringMerger -> B.ByteString -> (String, Int)
oneString (StringMerger strings) str =
        case find (B.isSuffixOf str) strings of
            Just string -> (stringName string, B.length string - B.length str)
            Nothing -> (stringName str, 0)
