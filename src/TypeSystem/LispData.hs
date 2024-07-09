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
              deriving (Eq, Show)

internalTypeOf :: LispData -> LispType
internalTypeOf = \case
    NIL               -> BOOLEAN'
    T                 -> BOOLEAN'
    (SIMPLE_STRING _) -> SIMPLE_STRING'
    (INTEGER _)       -> INTEGER'
    END_OF_FILE       -> END_OF_FILE'
    (READER_ERROR _)  -> READER_ERROR'
    (LIST _)          -> LIST'
