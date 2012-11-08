module Parser( Template
             , TemplatePiece(..)
             , parse
             , closeTemplate
             ) where

import qualified Data.ByteString.Char8 as B

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.ByteString as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Prim as P

import Control.Applicative((*>), (<*), (<$>))
import Control.Monad
import Data.Function(fix)
import Text.Parsec.Prim((<|>))

type Template = [TemplatePiece]

data TemplatePiece = TemplateString B.ByteString
                   | TemplateVariable B.ByteString
                   | TemplateBlock B.ByteString Template
    deriving (Show)

p_template :: P.GenParser Char () Template
p_template = p_oneTemplate <* P.eof where
    p_oneTemplate = P.many p_piece

    p_piece = P.try p_block <|> P.try p_variable <|> P.try p_string

    p_control = B.pack <$> (
        P.string "{{" *> P.many1 (P.noneOf "}") <* P.string "}}")

    p_block = do
        header <- p_control
        when (B.head header /= '#') $
                fail "Block begin tag must begin with `#'"
        let name = B.tail header
        body <- p_oneTemplate
        footer <- p_control
        when (B.head footer /= '/') $
                fail $ "Block end tag must begin with `/': " ++
                    (B.unpack footer)
        let name' = B.tail footer
        when (name /= name') $
                fail $ "Begin and end tags mismatch: " ++
                    B.unpack name ++ " and " ++ B.unpack name'
        return $ TemplateBlock name body

    p_variable = do
        name <- p_control
        when (B.head name == '#' || B.head name == '/') $
            fail $ "Variable name cannot start with '#' or '/': " ++
                B.unpack name
        return $ TemplateVariable name

    p_string = let
        p_noBrace = P.many1 (P.noneOf "{")
        p_oneBrace = P.try $ do
            P.char '{'
            ('{':) . (:[]) <$> P.noneOf "{"
        pieces = P.many1 (p_noBrace <|> p_oneBrace)
        in TemplateString . B.pack . concat <$> pieces

parse :: B.ByteString -> Either String Template
parse str = case P.parse p_template "" str of
    Left err -> Left $ show err
    Right ans -> Right ans

findSubtemplate [] template = Just $ template
findSubtemplate (a:as) template =
    go template >>= findSubtemplate as
  where
    go [] = Nothing
    go ((TemplateBlock name body):_) | name == a = Just body
    go (_:bs) = go bs

closeTemplate = fix . flip close where
    close = map . go
    go result var@(TemplateVariable name)
        | B.head name == '$' && ':'  `B.elem` name = let
            (replacedName', path') = B.span (/= ':') name
            replacedName = B.tail replacedName'
            path = B.tail path'
            in case findSubtemplate (B.splitWith (== '/') path) result of
                Just subtemplate ->
                    TemplateBlock replacedName subtemplate
                Nothing -> error $ "Subtemplate " ++ (B.unpack path) ++ " is not found"
        | otherwise = var
    go result (TemplateBlock name body) =
            TemplateBlock name (close result body)
    go _ a = a
