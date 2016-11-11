{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Format a 'show'-generated string to make it nicer to read.
--
-- >>> :{
-- (putStrLn . prettyShow . Data.Map.fromList)
--     [("hello", Just True), ("world", Nothing), ("!", Just False)]
-- :}
-- fromList [("!",Just False)
--          ,("hello",Just True)
--          ,("world",Nothing)]
--
-- See the readme for some more examples.
module Text.Show.Prettyprint (
    prettifyShow,
    prettyShow,

    -- * Diagnostic functions
    prettifyShowErr,
    prettyShowErr,
) where



import Control.Applicative
import Data.Monoid
import Text.PrettyPrint.Leijen as Ppr hiding ((<>))
import Text.Trifecta           as Tri



-- | Prettyprint a string produced by 'show'. On parse error, silently fall back
-- to a non-prettyprinted version.
prettifyShow :: String -> String
prettifyShow s = case parseString conP mempty s of
    Success x -> show x
    Failure _ -> s

-- | 'prettifyShow' with the 'show' baked in.
prettyShow :: Show a => a -> String
prettyShow = prettifyShow . show

-- | Attempt to prettify a string produced by 'show'. Report error information
-- on failure.
prettifyShowErr :: String -> String
prettifyShowErr s = case parseString conP mempty s of
    Success x -> show x
    Failure ErrInfo{ _errDoc = e } -> "ERROR " <> show e

-- | 'prettifyShowErr' with the 'show' baked in.
prettyShowErr :: Show a => a -> String
prettyShowErr = prettifyShowErr . show



conP :: Parser Doc
conP = do
    thing <- (token . choice)
        [ word
        , number
        , fmap (dquotes . Ppr.string) stringLiteral ]
    args <- many argP
    pure (if null args
        then thing
        else thing <+> align (sep args) )

word :: Parser Doc
word = variable <|> constructor

number :: Parser Doc
number = p <?> "number"
  where
    p = integerOrDouble >>= \case
        Left i -> pure (Ppr.integer i)
        Right d -> pure (Ppr.double d)

identifierStartingWith :: CharParsing f => f Char -> f Doc
identifierStartingWith x = liftA2 (\c cs -> Ppr.string (c:cs)) (x <|> Tri.char '_') (many (alphaNum <|> oneOf "'_"))

variable :: Parser Doc
variable = identifierStartingWith lower <?> "variable"

constructor :: Parser Doc
constructor = identifierStartingWith upper <?> "constructor"

argP :: Parser Doc
argP = (token . choice) [unitP, tupleP, listP, recordP, conP]

unitP :: Parser Doc
unitP = p <?> "()"
  where
    p = fmap Ppr.string (Tri.string "()")

tupleP :: Parser Doc
tupleP = p <?> "tuple"
  where
    p = fmap (encloseSep lparen rparen Ppr.comma) (Tri.parens (do
        x <- argP
        xs <- many (Tri.comma *> argP)
        pure (x:xs) ))

listP :: Parser Doc
listP = p <?> "list"
  where
    p = fmap (encloseSep lbracket rbracket Ppr.comma)
             (Tri.brackets (sepBy argP Tri.comma))

recordP :: Parser Doc
recordP = p <?> "{...}"
  where
    p = fmap (encloseSep lbrace rbrace Ppr.comma) (Tri.braces (sepBy recordEntryP Tri.comma))
    recordEntryP = do
        lhs <- token word
        _ <- token (Tri.char '=')
        rhs <- argP
        pure (lhs <+> Ppr.string "=" <+> rhs)
