-- | These functions are identical to the ones in the main module, but instead
-- of falling back to the un-prettyprinted input, they will report an error on
-- failure.
--
-- >>> putStrLn (prettifyShowErr "Imbalanced (Parenthesis)) here")
-- ERROR (interactive):1:25: error: expected: char literal, end of input, identifier,
--     list, number, record, string literal, tuple, unit
-- 1 | Imbalanced (Parenthesis)) here<EOF> 
--   |                         ^           
module Text.Show.Prettyprint.Diagnostic (
    prettifyShowErr,
    prettyShowErr,
    prettyPrintErr,
) where



import           Data.Text.Prettyprint.Doc    as Doc
import           Text.Trifecta                as Tri

import Text.Show.Prettyprint.Internal


-- | Attempt to prettify a string produced by 'show'. Report error information
-- on failure.
prettifyShowErr :: String -> String
prettifyShowErr s = case parseString shownP mempty s of
    Success x -> show x
    Failure ErrInfo{ _errDoc = e } -> "ERROR " <> show (Doc.unAnnotate e)

-- | 'prettifyShowErr' with the 'show' baked in.
prettyShowErr :: Show a => a -> String
prettyShowErr = prettifyShowErr . show

-- | 'prettifyShowErr' with the 'show' and the 'putStrLn' baked in.
prettyPrintErr :: Show a => a -> IO ()
prettyPrintErr = putStrLn . prettyShowErr
