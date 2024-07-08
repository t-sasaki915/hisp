module LispReader.LispReader (read, makeStringInputStream) where

import Control.Monad.Trans.State.Strict (State, get, put)
import Prelude hiding (read)

-- draft
type InputStream = (String, Int)

makeStringInputStream :: String -> InputStream
makeStringInputStream str = (str, 0)

data SyntaxType = Constituent | TerminatingMacroChar | NonTerminatingMacroChar
                | SingleEscape | Invalid | MultipleEscape | Whitespace

readChar :: State InputStream Char
readChar = do
    (str, i) <- get
    _        <- put (str, i + 1)
    return (str !! i)

read :: State InputStream String
read = do
    char <- readChar
    return [char]
