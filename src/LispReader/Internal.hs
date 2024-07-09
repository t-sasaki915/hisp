{-# LANGUAGE LambdaCase #-}

module LispReader.Internal (internalRead) where

import TypeSystem.LispData (LispData(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString, head, pack)
import Data.ByteString.Internal (w2c, c2w)
import Data.Functor ((<&>))
import System.IO.Streams (InputStream, readExactly, atEOF, unRead)
import Text.Regex.Posix ((=~))

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

readChar :: InputStream ByteString -> IO Char
readChar s = readExactly 1 s <&> (w2c . Data.ByteString.head)

unreadChar :: Char -> InputStream ByteString -> IO ()
unreadChar c = unRead (pack [c2w c])

internalRead :: InputStream ByteString -> Bool -> LispData -> Bool ->
                ExceptT LispData IO LispData
internalRead inputStream eofErrorP eofValue recursiveP = do
    lift (atEOF inputStream) >>= \case
        True | eofErrorP -> throwE END_OF_FILE
        True             -> return eofValue
        False -> do
            x <- lift $ readChar inputStream
            case categoriseType x of
                Invalid ->
                    throwE (READER_ERROR "Invalid Character")

                Whitespace ->
                    internalRead inputStream eofErrorP eofValue recursiveP

                TerminatingMacroChar ->
                    assignMacroAnalyser x

                NonTerminatingMacroChar ->
                    assignMacroAnalyser x

                SingleEscape ->
                    lift (atEOF inputStream) >>= \case
                        True  -> multipleEscapeAnalyse ""
                        False -> do
                            y <- lift $ readChar inputStream
                            tokenAnalyse [y]

                MultipleEscape ->
                    multipleEscapeAnalyse ""

                Constituent ->
                    tokenAnalyse [x]
    where
        assignMacroAnalyser :: Char -> ExceptT LispData IO LispData
        assignMacroAnalyser = \case
            '('  -> return (SIMPLE_STRING "LIST")
            ')'  -> throwE (READER_ERROR "UNEXPECTED ')'")
            '\'' -> return (SIMPLE_STRING "QUOTE")
            ';'  -> return (SIMPLE_STRING "COMMENT")
            '"'  -> return (SIMPLE_STRING "STRING")
            '`'  -> return (SIMPLE_STRING "STRUCTURE")
            ','  -> throwE (READER_ERROR "UNEXPECTED ','")
            '#'  -> return (SIMPLE_STRING "DISPATCHING")
            _    -> throwE (READER_ERROR "ILLEGAL BEHAVIOUR")

        multipleEscapeAnalyse :: String -> ExceptT LispData IO LispData
        multipleEscapeAnalyse token =
            lift (atEOF inputStream) >>= \case
                True  -> throwE END_OF_FILE
                False -> do
                    y <- lift $ readChar inputStream
                    case categoriseType y of
                        Constituent             -> multipleEscapeAnalyse (token ++ [y])
                        TerminatingMacroChar    -> multipleEscapeAnalyse (token ++ [y])
                        NonTerminatingMacroChar -> multipleEscapeAnalyse (token ++ [y])
                        Whitespace              -> multipleEscapeAnalyse (token ++ [y])
                        SingleEscape            -> do
                            lift (atEOF inputStream) >>= \case
                                True  -> multipleEscapeAnalyse token
                                False -> do
                                    z <- lift $ readChar inputStream
                                    multipleEscapeAnalyse (token ++ [z])
                        MultipleEscape          -> tokenAnalyse token
                        Invalid                 -> throwE (READER_ERROR "Invalid Character")
    
        tokenAnalyse :: String -> ExceptT LispData IO LispData
        tokenAnalyse token =
            lift (atEOF inputStream) >>= \case
                True  -> return (SIMPLE_STRING ("VALUE: " ++ token))
                False -> do
                    y <- lift $ readChar inputStream
                    case categoriseType y of
                        Constituent             -> tokenAnalyse (token ++ [y])
                        NonTerminatingMacroChar -> tokenAnalyse (token ++ [y])
                        SingleEscape            -> do
                            lift (atEOF inputStream) >>= \case
                                True -> throwE END_OF_FILE
                                False -> do
                                    z <- lift $ readChar inputStream
                                    tokenAnalyse (token ++ [z])
                        MultipleEscape          -> multipleEscapeAnalyse token
                        Invalid                 -> throwE (READER_ERROR "Invalid Character")
                        TerminatingMacroChar    -> do
                            _ <- lift $ unreadChar y inputStream
                            return (SIMPLE_STRING ("VALUE: " ++ token))
                        Whitespace              -> do
                            return (SIMPLE_STRING ("VALUE: " ++ token))
