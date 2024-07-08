module LispReader.LispReader (read, makeStringInputStream) where

import Util (guaranteeM)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Functor ((<&>))
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

unreadChar :: ExceptT String (StateT InputStream Identity) ()
unreadChar = do
    (str, i) <- lift get
    lift $ put (str, i - 1)

isEOF :: ExceptT String (StateT InputStream Identity) Bool
isEOF = do
    (str, i) <- lift get
    return (i >= length str)

read :: ExceptT String (StateT InputStream Identity) String
read = do
    _ <- guaranteeM (isEOF <&> not) "READ-ERROR end of file"
    x <- readChar
    case categoriseType x of
        Invalid                 -> throwE "READ-ERROR invalid"
        Whitespace              -> read
        TerminatingMacroChar    -> return "function call: read macro"
        NonTerminatingMacroChar -> return "function call: read macro"
        SingleEscape            -> do
            _ <- guaranteeM (isEOF <&> not) "READ-ERROR end of file"
            y <- readChar
            return ("value: escaped " ++ [y])
        MultipleEscape          -> do
            token <- escapedTokenRead ""
            return ("value: escaped " ++ token)
        Constituent             -> do
            token <- tokenRead [x]
            return ("value: token " ++ token)
    where
        escapedTokenRead :: String -> ExceptT String (StateT InputStream Identity) String
        escapedTokenRead token = do
            _ <- guaranteeM (isEOF <&> not) "READ-ERROR end of file"
            y <- readChar
            case categoriseType y of
                Constituent             -> escapedTokenRead (token ++ [y])
                NonTerminatingMacroChar -> escapedTokenRead (token ++ [y])
                TerminatingMacroChar    -> escapedTokenRead (token ++ [y])
                Whitespace              -> escapedTokenRead (token ++ [y])
                SingleEscape            -> do
                    _ <- guaranteeM (isEOF <&> not) "READ-ERROR end of file"
                    z <- readChar
                    escapedTokenRead (token ++ [z])
                MultipleEscape          -> tokenRead token
                Invalid                 -> throwE "READ-ERROR invalid"


        tokenRead :: String -> ExceptT String (StateT InputStream Identity) String
        tokenRead token = do
            eof <- isEOF
            if eof then
                return token

            else do
                y <- readChar
                case categoriseType y of
                    Constituent             -> tokenRead (token ++ [y])
                    NonTerminatingMacroChar -> tokenRead (token ++ [y])
                    SingleEscape            -> do
                        _ <- guaranteeM (isEOF <&> not) "READ-ERROR end of file"
                        z <- readChar
                        tokenRead (token ++ [z])
                    MultipleEscape          -> escapedTokenRead token
                    Invalid                 -> throwE "READ-ERROR invalid"
                    TerminatingMacroChar    -> do
                        _ <- unreadChar
                        return token
                    Whitespace              -> do
                        _ <- unreadChar -- the reference says that unread it if appropriate
                        return token
