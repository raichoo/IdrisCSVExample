module Parser

import public Control.Monad.Identity

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

unquotedString : Parser String
unquotedString = map pack $ some (satisfy (/= ','))

quotedString : Parser String
quotedString = map pack $ some (satisfy (/= '"'))

dquotedString : Parser String
dquotedString = do
  q <- dquote (dquote quotedString)
  pure $ "\"" ++ q ++ "\""

quotedChar : Parser String
quotedChar = do
  c <- satisfy (/= '"')
  pure (cast c)

quotedField' : Parser String
quotedField' = do
  s  <- quotedChar
  s' <- quotedField'
  pure $ s ++ s'

unquotedField : Parser String
unquotedField = unquotedString

quotedField : Parser String
quotedField = dquote quotedString

field : Parser String
field = quotedField <|> unquotedField

fields : Parser (List String)
fields = field  `sepBy1` char ','

Format : Type
Format = List String

parse : String -> Either String Format
parse = parse fields

parse' : String -> Format
parse' s = case parse fields s of
                Right csv => csv
                Left  _   => []
