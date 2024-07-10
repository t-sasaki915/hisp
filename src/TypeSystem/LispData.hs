module TypeSystem.LispData (LispData(..), internalTypeOf) where

import TypeSystem.LispType (LispType(..))

-- draft

data LispData = NIL
              | T
              | SIMPLE_STRING String
              | INTEGER Integer
              | END_OF_FILE
              | READER_ERROR String
              | LIST [LispData]
              | SYMBOL String
              | CONS LispData LispData
              deriving Eq

instance Show LispData where
    show NIL                        = "NIL"
    show T                          = "T"
    show (SIMPLE_STRING s)          = "\"" ++ s ++ "\""
    show (INTEGER z)                = show z
    show END_OF_FILE                = "End of File!!"
    show (READER_ERROR e)           = "Reader Error!! " ++ e
    show (LIST [SYMBOL "QUOTE", x]) = '\'' : show x
    show (LIST xs)                  = "(" ++ unwords (map show xs) ++ ")"
    show (SYMBOL s)                 = s
    show (CONS a b)                 = "(" ++ show a ++ " . " ++ show b ++ ")"

internalTypeOf :: LispData -> LispType
internalTypeOf = \case
    NIL               -> BOOLEAN'
    T                 -> BOOLEAN'
    (SIMPLE_STRING _) -> SIMPLE_STRING'
    (INTEGER _)       -> INTEGER'
    END_OF_FILE       -> END_OF_FILE'
    (READER_ERROR _)  -> READER_ERROR'
    (LIST _)          -> LIST'
    (SYMBOL _)        -> SYMBOL'
    (CONS _ _)        -> CONS'
