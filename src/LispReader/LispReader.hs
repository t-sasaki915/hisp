module LispReader.LispReader (read, makeStringInputStream) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity)
import Prelude hiding (read)
import Text.Regex.Posix ((=~))

-- draft
type InputStream = (String, Int)

makeStringInputStream :: String -> InputStream
makeStringInputStream str = (str, 0)

data SyntaxType = Constituent | TerminatingMacroChar | NonTerminatingMacroChar
                | SingleEscape | Invalid | MultipleEscape | Whitespace
                deriving (Eq, Show)

categoriseType :: Char -> SyntaxType
categoriseType char | [char] =~ "[a-z]" = Constituent
categoriseType char | [char] =~ "[A-Z]" = Constituent
categoriseType char | [char] =~ "[0-9]" = Constituent
categoriseType '\b' = Constituent -- Backspace
categoriseType '\t' = Whitespace  -- Tab
categoriseType '\n' = Whitespace  -- Newline and Linefeed
categoriseType '\f' = Whitespace  -- Page
categoriseType '\r' = Whitespace  -- Return
categoriseType ' '  = Whitespace
categoriseType '!'  = Constituent
categoriseType '"'  = TerminatingMacroChar
categoriseType '#'  = NonTerminatingMacroChar
categoriseType '$'  = Constituent
categoriseType '%'  = Constituent
categoriseType '&'  = Constituent
categoriseType '\'' = TerminatingMacroChar
categoriseType '('  = TerminatingMacroChar
categoriseType ')'  = TerminatingMacroChar
categoriseType '*'  = Constituent
categoriseType '+'  = Constituent
categoriseType ','  = TerminatingMacroChar
categoriseType '-'  = Constituent
categoriseType '.'  = Constituent
categoriseType '/'  = Constituent
categoriseType ':'  = Constituent
categoriseType ';'  = TerminatingMacroChar
categoriseType '<'  = Constituent
categoriseType '='  = Constituent
categoriseType '>'  = Constituent
categoriseType '?'  = Constituent
categoriseType '@'  = Constituent
categoriseType '['  = Constituent
categoriseType '\\' = SingleEscape
categoriseType ']'  = Constituent
categoriseType '^'  = Constituent
categoriseType '_'  = Constituent
categoriseType '`'  = TerminatingMacroChar
categoriseType '{'  = Constituent
categoriseType '|'  = MultipleEscape
categoriseType '}'  = Constituent
categoriseType '~'  = Constituent
categoriseType _    = Invalid

readChar :: ExceptT String (StateT InputStream Identity) Char
readChar = do
    (str, i) <- lift get
    _        <- lift $ put (str, i + 1)
    return (str !! i)

read :: ExceptT String (StateT InputStream Identity) String
read = do
    char <- readChar
    return (char : show (categoriseType char))
