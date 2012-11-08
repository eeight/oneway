{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator( TextGenerator
                , StringArg
                , bind
                , subtemplate
                , indented
                , render
                ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Control.Monad.State
import Data.Char(isSpace)

import Parser

data TemplateContext = TemplateContext
    { tgIndent :: Int
    , tgVariables :: M.Map B.ByteString B.ByteString
    , tgSubtemplates :: M.Map B.ByteString [TemplateContext]
    } deriving (Show)

emptyTemplateContext :: TemplateContext
emptyTemplateContext = TemplateContext 0 M.empty M.empty

modifyIndent f ctx = ctx{tgIndent=f $ tgIndent ctx}
modifyVars f ctx = ctx{tgVariables=f $ tgVariables ctx}
modifySubs f ctx = ctx{tgSubtemplates=f $ tgSubtemplates ctx}

mapAppend k v = M.alter alter k where
    alter Nothing = Just $ [v]
    alter (Just vs) = Just $ v:vs

type TextGenerator = State TemplateContext

class StringArg a where
    toString :: a -> B.ByteString

instance StringArg B.ByteString where
    toString = id

instance StringArg String where
    toString = B.pack

instance (Show s) => StringArg s where
    toString = B.pack . show

bind :: (StringArg s) => String -> s -> TextGenerator ()
bind name value = modify $ modifyVars $
        M.insert (B.pack name) (toString value)

subtemplate :: String -> TextGenerator () -> TextGenerator ()
subtemplate name tpl = let
    subCtx = execState tpl emptyTemplateContext
    in modify $ modifySubs $ mapAppend (B.pack name) subCtx

indented :: Int -> TextGenerator ()
indented i = modify $ modifyIndent (+ i)

indent level str 
    | '\n' `B.elem` str = let
        lines = B.lines str
        indenter = B.concat $ replicate level (B.pack "    ")
        lines' = map (B.append indenter) lines
        in B.unlines lines'
    | otherwise = str

removeBlankLines :: B.ByteString -> B.ByteString
removeBlankLines = B.unlines . filter (not .  B.all isSpace) . B.lines

render :: Template -> TextGenerator () -> B.ByteString
render template generator = let
    renderOne :: [B.ByteString]
              -> TemplatePiece
              -> TemplateContext
              -> [B.ByteString]
    renderOne _ (TemplateString str) _ = [str]
    renderOne stack (TemplateVariable name) ctx =
        case M.lookup name (tgVariables ctx) of
            Just str -> [str]
            Nothing -> let
                path = B.unpack $ B.intercalate (B.pack "/") $ reverse stack
                name' = B.unpack name
                in error $ "Undefined variable `" ++ name' ++
                    "' in template `" ++ path ++ "'"
    renderOne stack (TemplateBlock name body) ctx = let
        subdicts = maybe [] reverse $ M.lookup name (tgSubtemplates ctx)
        doRender subCtx = indent (tgIndent subCtx) $
            renderTemplate (name:stack) body subCtx
        in map doRender subdicts

    renderTemplate stack body ctx = B.concat $
            concatMap (flip (renderOne stack) ctx) body

    context = execState generator emptyTemplateContext
    in removeBlankLines $ renderTemplate [] template context
