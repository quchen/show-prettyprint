{-# LANGUAGE LambdaCase #-}

-- | __This module may change arbitrarily between versions.__ It is exposed only
-- for documentary purposes.
module Text.Show.Prettyprint.Internal where



import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen as Ppr hiding ((<>))
import Text.Trifecta                as Tri



-- $setup
-- >>> :{
-- let testParse p s = case parseString p mempty s of
--         Success x -> print x
--         Failure ErrInfo{ _errDoc = e } -> putStrLn ("ERROR " ++ show (plain e))
-- :}



-- | Prettyparser for a 'show'-generated string
shownP :: Parser Doc
shownP = valueP <* eof

-- | Prettyparser for a constructor, which is roughly a word applied to
-- arguments.
--
-- >>> testParse valueP "Just ('c', Left ())"
-- Just ('c',Left ())
valueP :: Parser Doc
valueP = do
    thing <- choice [identifierP, numberP, stringP, charP]
    args <- many argP
    pure (if null args
        then thing
        else thing <+> align (sep args) )

-- | An identifier is a liberal version of a "variable or constructor", which
-- roughly means that it's a printable word without parentheses.
--
-- >>> testParse identifierP "_foo'bar"
-- _foo'bar
identifierP :: Parser Doc
identifierP = token (p <?> "identifier")
  where
    p = fmap Ppr.text (some (alphaNum <|> oneOf "'_"))

-- | Number in integer or scientific notation.
--
-- >>> testParse numberP "123456"
-- 123456
--
-- >>> testParse numberP "-123.4e56"
-- -1.234e58
numberP :: Parser Doc
numberP = p <?> "number"
  where
    p = integerOrDouble >>= \case
        Left i -> pure (Ppr.integer i)
        Right d -> pure (Ppr.double d)

-- | A quoted string literal
--
-- >>> testParse stringP "\"Hello world!\""
-- "Hello world!"
stringP :: Parser Doc
stringP = token (p <?> "string literal")
  where
    p = fmap (dquotes . Ppr.string) stringLiteral

-- | A quoted char literal
--
-- >>> testParse charP "'c'"
-- 'c'
charP :: Parser Doc
charP = token (p <?> "char literal")
  where
    p = fmap (squotes . Ppr.char) Tri.charLiteral

-- | Anything that could be considered an argument to something else.
--
-- >>> testParse argP "()"
-- ()
--
-- >>> testParse argP "['h', 'e', 'l', 'l', 'o']"
-- ['h','e','l','l','o']
argP :: Parser Doc
argP = (token . choice) [unitP, tupleP, listP, recordP, valueP]

-- | '()' prettyparser
--
-- >>> testParse unitP "()"
-- ()
unitP :: Parser Doc
unitP = p <?> "unit"
  where
    p = fmap Ppr.string (Tri.string "()")

-- | Prettyparser for tuples from size 1. Since 1-tuples are just parenthesized
-- expressions to first order approximation, this parser handles those as well.
--
-- >>> testParse tupleP "((), True, 'c')"
-- ((),True,'c')
tupleP :: Parser Doc
tupleP = p <?> "tuple"
  where
    p = fmap (encloseSep lparen rparen Ppr.comma) (Tri.parens (do
        x <- argP
        xs <- many (Tri.comma *> argP)
        pure (x:xs) ))

-- | List prettyparser
--
-- >>> testParse listP "[\"Hello\", \"world\"]"
-- ["Hello","world"]
listP :: Parser Doc
listP = p <?> "list"
  where
    p = fmap (encloseSep lbracket rbracket Ppr.comma)
             (Tri.brackets (sepBy argP Tri.comma))

-- | Record syntax prettyparser
--
-- >>> testParse recordP "{ r1 = (), r2 = Just True }"
-- {r1 = (),r2 = Just True}
recordP :: Parser Doc
recordP = p <?> "record"
  where
    p = fmap (encloseSep lbrace rbrace Ppr.comma) (Tri.braces (sepBy recordEntryP Tri.comma))
    recordEntryP = do
        lhs <- token identifierP
        _ <- token (Tri.char '=')
        rhs <- argP
        pure (lhs <+> Ppr.string "=" <+> rhs)
