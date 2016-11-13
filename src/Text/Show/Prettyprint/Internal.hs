{-# LANGUAGE LambdaCase #-}

-- | __This module may change arbitrarily between versions.__ It is exposed only
-- for documentary purposes.
module Text.Show.Prettyprint.Internal where



import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen as Ppr hiding ((<>))
import Text.Trifecta                as Tri



-- | Prettyparser for a 'show'-generated string
shownP :: Parser Doc
shownP = valueP <* eof

-- | Prettyparser for a constructor, which is roughly a word applied to
-- arguments.
valueP :: Parser Doc
valueP = do
    thing <- choice [identifierP, numberP, stringP, charP]
    args <- many argP
    pure (if null args
        then thing
        else thing <+> align (sep args) )

-- | An identifier is a liberal version of a "variable or constructor", which
-- roughly means that it's a printable word without parentheses.
identifierP :: Parser Doc
identifierP = token (p <?> "identifier")
  where
    p = fmap Ppr.text (some (alphaNum <|> oneOf "'_"))

-- | Number in integer or scientific notation.
numberP :: Parser Doc
numberP = p <?> "number"
  where
    p = integerOrDouble >>= \case
        Left i -> pure (Ppr.integer i)
        Right d -> pure (Ppr.double d)

-- | A quoted string literal
stringP :: Parser Doc
stringP = token (p <?> "string literal")
  where
    p = fmap (dquotes . Ppr.string) stringLiteral

-- | A quoted char literal
charP :: Parser Doc
charP = token (p <?> "char literal")
  where
    p = fmap (squotes . Ppr.char) Tri.charLiteral

-- | Anything that could be considered an argument to something else.
argP :: Parser Doc
argP = (token . choice) [unitP, tupleP, listP, recordP, valueP]

-- | '()' prettyparser
unitP :: Parser Doc
unitP = p <?> "unit"
  where
    p = fmap Ppr.string (Tri.string "()")

-- | Prettyparser for tuples from size 1. Since 1-tuples are just parenthesized
-- expressions to first order approximation, this parser handles those as well.
tupleP :: Parser Doc
tupleP = p <?> "tuple"
  where
    p = fmap (encloseSep lparen rparen Ppr.comma) (Tri.parens (do
        x <- argP
        xs <- many (Tri.comma *> argP)
        pure (x:xs) ))

-- | List prettyparser
listP :: Parser Doc
listP = p <?> "list"
  where
    p = fmap (encloseSep lbracket rbracket Ppr.comma)
             (Tri.brackets (sepBy argP Tri.comma))

-- | Record syntax prettyparser
recordP :: Parser Doc
recordP = p <?> "record"
  where
    p = fmap (encloseSep lbrace rbrace Ppr.comma) (Tri.braces (sepBy recordEntryP Tri.comma))
    recordEntryP = do
        lhs <- token identifierP
        _ <- token (Tri.char '=')
        rhs <- argP
        pure (lhs <+> Ppr.string "=" <+> rhs)
