{-# LANGUAGE LambdaCase #-}

-- | __This module may change arbitrarily between versions.__ It is exposed only
-- for documentary purposes.
module Text.Show.Prettyprint.Internal (
    parseShowString,
    shownP,
    valueP,
    identifierP,
    numberP,
    stringLitP,
    charLitP,
    argP,
    unitP,
    tupleP,
    listP,
    recordP,
) where



import Control.Applicative
import Data.Text.Prettyprint.Doc as Ppr
import Text.Trifecta             as Tri



-- $setup
--
-- >>> import Text.PrettyPrint.ANSI.Leijen (plain)
-- >>> :{
-- let testParse p s = case parseString p mempty s of
--         Success x -> print x
--         Failure ErrInfo{ _errDoc = e } -> putStrLn ("ERROR " ++ show (plain e))
-- :}


parseShowString :: String -> Result (Doc ann)
parseShowString = parseString shownP mempty

-- | Prettyparser for a 'show'-generated string
shownP :: Parser (Doc ann)
shownP = valueP <* eof

-- | Prettyparser for a constructor, which is roughly a word applied to
-- arguments.
--
-- >>> testParse valueP "Just ('c', Left ())"
-- Just ('c',Left ())
valueP :: Parser (Doc ann)
valueP = do
    thing <- choice [identifierP, numberP, stringLitP, charLitP]
    args <- many argP
    pure (if null args
        then thing
        else thing <+> align (sep args) )

-- | An identifier is a liberal version of a "variable or constructor", which
-- roughly means that it's a printable word without parentheses.
--
-- >>> testParse identifierP "_foo'bar"
-- _foo'bar
identifierP :: Parser (Doc ann)
identifierP = token (p <?> "identifier")
  where
    p = fmap Ppr.pretty (some (alphaNum <|> oneOf "'_"))

-- | Number in integer or scientific notation.
--
-- >>> testParse numberP "123456"
-- 123456
--
-- >>> testParse numberP "-123.4e56"
-- -1.234e58
numberP :: Parser (Doc ann)
numberP = p <?> "number"
  where
    p = integerOrDouble >>= \case
        Left i -> pure (pretty i)
        Right d -> pure (pretty d)

-- |
-- >>> testParse stringLitP "\"Hello world!\""
-- "Hello world!"
stringLitP :: Parser (Doc ann)
stringLitP = token (p <?> "string literal")
  where
    p = fmap (dquotes . pretty) (stringLiteral :: Parser String)

-- |
-- >>> testParse charLitP "'c'"
-- 'c'
charLitP :: Parser (Doc ann)
charLitP = token (p <?> "char literal")
  where
    p = fmap (squotes . pretty) Tri.charLiteral

-- | Anything that could be considered an argument to something else.
--
-- >>> testParse argP "()"
-- ()
--
-- >>> testParse argP "['h', 'e', 'l', 'l', 'o']"
-- ['h','e','l','l','o']
argP :: Parser (Doc ann)
argP = (token . choice) [unitP, tupleP, listP, recordP, valueP]

-- |
-- >>> testParse unitP "()"
-- ()
unitP :: Parser (Doc ann)
unitP = p <?> "unit"
  where
    p = fmap pretty (Tri.string "()")

-- | Prettyparser for tuples from size 1. Since 1-tuples are just parenthesized
-- expressions to first order approximation, this parser handles those as well.
--
-- >>> testParse tupleP "((), True, 'c')"
-- ((),True,'c')
tupleP :: Parser (Doc ann)
tupleP = p <?> "tuple"
  where
    p = fmap (encloseSep lparen rparen Ppr.comma) (Tri.parens (do
        x <- argP
        xs <- many (Tri.comma *> argP)
        pure (x:xs) ))

-- | List prettyparser. Lists can be heterogeneous, which is realistic if we
-- consider ill-defined Show instances.
--
-- >>> testParse listP "[\"Hello\", World]"
-- ["Hello",World]
listP :: Parser (Doc ann)
listP = p <?> "list"
  where
    p = fmap (encloseSep lbracket rbracket Ppr.comma)
             (Tri.brackets (sepBy argP Tri.comma))

-- |
-- >>> testParse recordP "{ r1 = (), r2 = Just True }"
-- {r1 = (),r2 = Just True}
recordP :: Parser (Doc ann)
recordP = p <?> "record"
  where
    p = fmap (encloseSep lbrace rbrace Ppr.comma) (Tri.braces (sepBy recordEntryP Tri.comma))
    recordEntryP = do
        lhs <- token identifierP
        _ <- token (Tri.char '=')
        rhs <- argP
        pure (lhs <+> pretty "=" <+> rhs)
