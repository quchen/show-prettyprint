{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Show.Prettyprint where



import Control.Applicative
import Data.Monoid
import Text.PrettyPrint.Leijen as Ppr hiding ((<>))
import Text.Trifecta           as Tri



prettifyShow :: String -> String
prettifyShow s = case parseString conP mempty s of
    Success x -> show x
    Failure ErrInfo{ _errDoc = e } -> "ERROR " <> show e

prettyShow :: Show a => a -> String
prettyShow = prettifyShow . show


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

test2 :: String
test2 = prettifyShow "Hello Foo (\"(Bar\", Haha) (Baz (A { foo = C, bar = D, qux = (E,\"He)llo World!\",G,H,[A,B,c,d,e,Fghi]) } ) (B,C) [Baz A1 B2, (Baz A3 (B4)), (Baz A5 (B6)), (Baz (A7) B8)]) (Foo) (Bar) (Baz (A) (B))"

test :: String
test = prettifyShow "Set  (fromList [(Name \"A string with (parenthesis\",Ann  (Entry (Quality 1 1) (Ann  False) (Ann  (Map [Ann  (Bound (Ann  (Id \"lorem\"))),Ann  (Variable (Ann  (Id \"ipsum\")))])))),(Name \"string\",Ann  (Entry (Quality 1 1) (Ann  True) (Ann  (Internal (Ann  (Reduce (Ann  (Id \"dolor\")) (Ann  (Id \"sit\")))))))),(Name \"Another } here\",Ann  (Entry (Quality 1 1) (Ann  (Or [Ann  (Not (Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal (Ann  One))]))))),Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal (Ann  Three))]))),Ann  (Is (Ann  Flagged) (Ann  Type) (Ann  (Multi [Ann  (Literal (Ann  Two))])))])) (Ann  (Internal (Ann  (Concat (Ann  (Id \"amet\"))))))))])"
