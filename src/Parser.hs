{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE TypeApplications                #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Parser where

import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1)
import Types
import Data.Foldable (asum)
import Data.Char (isAlphaNum, isAlpha)
import qualified Data.Text as T
import Control.Monad (void, (<=<))
import Pretty (toBurst, toHaskell)
import Eval (eval)
import Transform
import Language.Haskell.Interpreter hiding (eval)
import GenType

type Parser = Parsec Void Text


sp :: Parser ()
sp = space space1 (skipLineComment "--") (skipBlockComment "{-" "-}")


l :: Text -> Parser ()
l = void . symbol sp

okFirst :: Char -> Bool
okFirst '_' = True
okFirst x = isAlpha x

okAfter :: Char -> Bool
okAfter '_' = True
okAfter x = isAlphaNum x

var :: Parser Text
var = do
  r <- asum
    [ bracketed $ var
    , l "Util.Id.Id" *> between (l "\"") "\"" rawvar
    , lexeme sp rawvar
    ]
  case elem r ["match", "with", "fun"] of
    False -> pure r
    True -> fail "matched a var against a keyword"

rawvar :: Parser Text
rawvar = fmap T.pack $ (:) <$> satisfy okFirst <*> many (satisfy okAfter)

bracketed :: Parser a -> Parser a
bracketed = between (l "(")  (l ")")

exprTerm :: Parser Expr
exprTerm = asum
  [ bracketed expr
  , do
      l "fun"
      ps <- bracketed $ sepBy param $ l ","
      l "->"
      e <- expr
      pure $ Lambda ps e
  , do
      l "match"
      e <- expr
      l "with"
      a <- some alt
      pure $ Match e a
  , Var <$> var
  ]

param :: Parser (Text, Type)
param = do
  v <- var
  l ":"
  t <- parseType
  pure (v, t)

alt :: Parser (Pat, Expr)
alt = do
  l "|"
  con <- var

  l "_"
  l "->"
  e <- expr
  pure $ (RecordCtor con, e)

expr :: Parser Expr
expr = makeExprParser exprTerm
  [ [ InfixL $ Apply <$ l ""  ]
  , [ Postfix $ Proj1 <$ (try $ l "." >> l "0")
    , Postfix $ Proj2 <$ (try $ l "." >> l "1")
    ]
  , [ InfixL $ Tuple <$ l ","]
  ]

typeTerm :: Parser Type
typeTerm = asum
  [ between (l "(")  (l ")") parseType
  , TyVar <$> var
  ]

parseType :: Parser Type
parseType = makeExprParser typeTerm
  [ [ InfixR $ TyTuple <$ symbol sp "*"   ]
  , [ InfixR $ TyArr <$ l "->"  ]
  ]

decl :: Parser Decl
decl = do
  l "fix"
  (v, t) <- bracketed param
  l "="
  e <- expr
  pure $ Fix v t e



main :: IO ()
main = do
  let res = fmap fixTerm $ parse decl "interactive" test
  putStrLn $ either errorBundlePretty (show . toHaskell) res
  putStrLn $ show $ toBurst $ toBurstDecl @(Bool)
  putStrLn $ show $ toBurst $ toBurstDecl @(Maybe Bool)
  either (putStrLn . errorBundlePretty) (putStrLn . either showGHC (\f -> show $ f ([False, True, True, False], 1)) <=< eval @(([Bool], Int) -> [Bool])) res

showGHC :: InterpreterError -> String
showGHC (UnknownError s) = s
showGHC (WontCompile ges) = unlines $ fmap showx ges
showGHC (NotAllowed s) = s
showGHC (GhcException s) = s

showx :: GhcError -> String
showx = errMsg

test :: Text
test = T.unlines
  [ "fix (f : bool -> boo) ="
  , "  fun (x : bool) ->"
  , "    match (x with"
  , "      | False _ -> True"
  , "      | True _ -> False"
  ]
  -- [ "fix ((Util.Id.Id \"f\"):list * nat -> list) = "
  -- , "fun (x:list * nat) -> "
  -- , "match x . 0 with"
  -- , "  | Nil _ -> x . 0"
  -- , "  | Cons _ -> (match x . 1 with"
  -- , "                 | O _ -> x . 0"
  -- , "                 | S _ -> f (Un_Cons (x . 0) . 1, Un_S (x . 1)))"
  -- ]

