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

    block = do
        A.string $ B.pack "{{#"
        name <- A.takeWhile1 (/= '}')
        A.string $ B.pack "}}"
        body <- template
        A.string $ B.pack "{{/"
        A.string name
        A.string $ B.pack "}}"
        return $ TemplateBlock name body

    variable = do
        A.string $ B.pack "{{"
        name <- A.takeWhile1 (/= '}')
        when (B.head name == '#' || B.head name == '/') $
            fail "Variable name cannot start with '#' or '/'"
        A.string $ B.pack "}}"
        return $ TemplateVariable name

    string = TemplateString <$> A.takeWhile1 (/= '{')
