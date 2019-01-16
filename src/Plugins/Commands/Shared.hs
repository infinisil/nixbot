module Plugins.Commands.Shared where

import           Data.Char
import           Data.Functor         (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme n = try $ n <* (void space1 <|> eof)

word :: String -> Parser ()
word n = lexeme (void $ string n)

parseWord :: Parser String
parseWord = some (satisfy (not . isSpace)) <* (void space1 <|> eof)
