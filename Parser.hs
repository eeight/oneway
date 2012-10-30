module Parser( Template
             , TemplatePiece(..)
             , parseTemplate
             ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator(many1)

type Template = [TemplatePiece]

data TemplatePiece = TemplateString B.ByteString
                   | TemplateVariable B.ByteString
                   | TemplateBlock B.ByteString Template
    deriving (Show)

parseTemplate :: A.Parser Template
parseTemplate = template <* A.endOfInput where
    template = many1 piece

    piece = block <|> variable <|> string

    control = str "{{" *> A.takeWhile1 (/= '}') <* str "}}" where
        str = A.string . B.pack

    block = do
        header <- control
        when (B.head header /= '#') $
                fail "Block begin tag must begin with `#'"
        let name = B.tail header
        body <- template
        footer <- control
        when (B.head footer /= '/') $
                fail "Block end tag must begin with `/'"
        let name' = B.tail footer
        when (name /= name) $
                fail $ "Begin and end tags mismatch:  " ++
                    B.unpack name ++ " and " ++ B.unpack name'
        return $ TemplateBlock name body

    variable = do
        name <- control
        when (B.head name == '#' || B.head name == '/') $
            fail "Variable name cannot start with '#' or '/'"
        return $ TemplateVariable name

    string = TemplateString <$> A.takeWhile1 (/= '{')
