{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}



-- | Format a 'show'-generated string to make it nicer to read.
--
-- For example, consider this nested data structure:
--
-- >>> :{
-- let nestedExample = fromList
--         [ ("hello", Left  (Pair True ()))
--         , ("world", Right (Record { r1 = ('c', -1.2e34), r2 = 123 }))
--         , ("!"    , Left  (Pair False ())) ]
-- :}
--
-- Applying 'show' to it results in the fairly dense representation
--
-- >>> print nestedExample
-- fromList [("!",Left (Pair False ())),("hello",Left (Pair True ())),("world",Right (Record {r1 = ('c',-1.2e34), r2 = 123}))]
--
-- With the functions defined in this module, we can make this output a bit more
-- readable,
--
-- >>> prettyPrintErr nestedExample
-- fromList [("!"
--           ,Left (Pair False ()))
--          ,("hello",Left (Pair True ()))
--          ,("world"
--           ,Right (Record {r1 = ('c'
--                                ,-1.2e34)
--                          ,r2 = 123}))]
module Text.Show.Prettyprint (
    prettifyShow,
    prettyShow,
    prettyPrint,

    -- * Diagnostic functions
    prettifyShowErr,
    prettyShowErr,
    prettyPrintErr,
) where



import Control.Applicative
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen as Ppr hiding ((<>))
import Text.Trifecta                as Tri



-- $setup
-- >>> data Record a b = Record { r1 :: a, r2 :: b } deriving Show
-- >>> data Pair a b = Pair a b deriving Show
-- >>> import Data.Map (fromList)



-- | Prettyprint a string produced by 'show'. On parse error, silently fall back
-- to a non-prettyprinted version.
prettifyShow :: String -> String
prettifyShow s = case parseString shownP mempty s of
    Success x -> show x
    Failure _ -> s

-- | 'prettifyShow' with the 'show' baked in.
prettyShow :: Show a => a -> String
prettyShow = prettifyShow . show

-- | 'prettifyShow' with the 'show' and the 'putStrLn' baked in.
prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . prettyShow

-- | Attempt to prettify a string produced by 'show'. Report error information
-- on failure.
--
-- >>> putStrLn (prettifyShowErr "Imbalanced (Parenthesis)) here")
-- ERROR (interactive):1:25: error: expected: char literal,
--     end of input, identifier, list,
--     number, record, string literal,
--     tuple, unit
-- Imbalanced (Parenthesis)) here<EOF>
--                         ^
prettifyShowErr :: String -> String
prettifyShowErr s = case parseString shownP mempty s of
    Success x -> show x
    Failure ErrInfo{ _errDoc = e } -> "ERROR " <> show (plain e)

-- | 'prettifyShowErr' with the 'show' baked in.
prettyShowErr :: Show a => a -> String
prettyShowErr = prettifyShowErr . show

-- | 'prettifyShowErr' with the 'show' and the 'putStrLn' baked in.
prettyPrintErr :: Show a => a -> IO ()
prettyPrintErr = putStrLn . prettyShowErr


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
