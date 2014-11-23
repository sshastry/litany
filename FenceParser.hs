module FenceParser where

import Types
import Control.Applicative
import Data.Char (toLower)
import Text.ParserCombinators.Parsec hiding ((<|>),many,optional,State)

downcase :: String -> String
downcase s = map toLower s

-- skip whitespaces on the right
lexeme p = p <* spaces

{- http://blog.barrucadu.co.uk/2013/05/27/a-gentle-introduction-to-parsec/
p <|> q executes p and, if p fails without consuming any input, executes q.
This is sometimes a bit restrictive, and so the parser try p executes p, and if
p fails it acts like it hasn’t consumed any input. (The downside of this
approach is that we no longer have short circuit evaluation; thus instead of
stopping at failure, we keep going. This can have performance implications.) -}
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

pure' :: (Show a) => a -> Parser a
pure' x = (string $ downcase $ show x) *> (pure x)

delimP :: Parser Delimiter
delimP = lexeme $ foldl1 (<||>) (map pure' delimiters)

langP :: Parser Language
langP = lexeme $ (foldl1 (<||>) (map pure' languages))
            <||> (string "" *> pure None)

fnameP :: Parser FilePath
fnameP = lexeme $ many (noneOf [' ',']'])
-- this hardcodes ' ' or ']' as the termination of a filename

fenceP :: Parser (Delimiter, Language, FilePath)
fenceP = spaces
            *> (string "[[") *> spaces
            *> ((,,) <$> delimP <*> langP <*> fnameP)
            <* (string "]]") <* spaces
