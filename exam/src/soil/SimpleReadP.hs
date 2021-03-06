-- | Modified version of SimpleReadP from the lectures.
--   Makes ReadP similar to SimpleParse 

module SimpleReadP where
import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)

type Parser a = ReadP a

(<|>) = (+++)
parse = readP_to_S
-- allow whitespace before eof
parseEof p = parse $ do { r <- p; whitespace; eof; return r}


-- Lexical combinators: ----------------------------------------------

alphaNum       :: ReadP Char
alphaNum       = satisfy isAlphaNum

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` " \t\n" 

whitespace :: Parser String
whitespace = many $ satisfy isWhitespace

token :: Parser a -> Parser a
token p = whitespace >> p

symbol = token . string

schar = token . char
