{-# LANGUAGE LambdaCase #-}

module TypeSystem.LispType (LispType(..), precedences, internalTypep) where

data LispType = NIL
              | BOOLEAN
              | FUNCTION
              | COMPILED_FUNCTION
              | GENERIC_FUNCTION
              | STANDARD_GENERIC_FUNCTION
              | CLASS
              | BUILT_IN_CLASS
              | STRUCTURE_CLASS
              | STANDARD_CLASS
              | METHOD
              | STANDARD_METHOD
              | STRUCTURE_OBJECT
              | STANDARD_OBJECT
              | METHOD_COMBINATION
              | T
              | SYMBOL
              | KEYWORD
              | PACKAGE
              | NUMBER
              | COMPLEX
              | REAL
              | FLOAT
              | SHORT_FLOAT
              | SINGLE_FLOAT
              | DOUBLE_FLOAT
              | LONG_FLOAT
              | RATIONAL
              | RATIO
              | INTEGER
              | SIGNED_BYTE
              | UNSIGNED_BYTE
              | BIT
              | FIXNUM
              | BIGNUM
              | CHARACTER
              | BASE_CHAR
              | STANDARD_CHAR
              | EXTENDED_CHAR
              | LIST
              | NULL
              | CONS
              | ATOM
              | ARRAY
              | SIMPLE_ARRAY
              | VECTOR
              | SIMPLE_VECTOR
              | BIT_VECTOR
              | SIMPLE_BIT_VECTOR
              | STRING
              | BASE_STRING
              | SIMPLE_STRING
              | SIMPLE_BASE_STRING
              | SEQUENCE
              | HASH_TABLE
              | PATHNAME
              | LOGICAL_PATHNAME
              | STREAM
              | BROADCAST_STREAM
              | CONCATENATED_STREAM
              | ECHO_STREAM
              | FILE_STREAM
              | STRING_STREAM
              | SYNONYM_STREAM
              | TWO_WAY_STREAM
              deriving Eq

precedences :: LispType -> [LispType]
precedences = \case
    NIL                       -> []
    BOOLEAN                   -> [BOOLEAN, SYMBOL, T]
    FUNCTION                  -> [FUNCTION, T]
    COMPILED_FUNCTION         -> [COMPILED_FUNCTION, FUNCTION, T]
    GENERIC_FUNCTION          -> [GENERIC_FUNCTION, FUNCTION, T]
    STANDARD_GENERIC_FUNCTION -> [STANDARD_GENERIC_FUNCTION, GENERIC_FUNCTION, FUNCTION, T]
    CLASS                     -> [CLASS, STANDARD_OBJECT, T]
    BUILT_IN_CLASS            -> [BUILT_IN_CLASS, CLASS, STANDARD_OBJECT, T]
    STRUCTURE_CLASS           -> [STRUCTURE_CLASS, CLASS, STANDARD_OBJECT, T]
    STANDARD_CLASS            -> [STANDARD_CLASS, CLASS, STANDARD_OBJECT, T]
    METHOD                    -> [METHOD, T]
    STANDARD_METHOD           -> [STANDARD_METHOD, METHOD, STANDARD_OBJECT, T]
    STRUCTURE_OBJECT          -> [STRUCTURE_OBJECT, T]
    STANDARD_OBJECT           -> [STANDARD_OBJECT, T]
    METHOD_COMBINATION        -> [METHOD_COMBINATION, T]
    T                         -> [T]
    SYMBOL                    -> [SYMBOL, T]
    KEYWORD                   -> [KEYWORD, SYMBOL, T]
    PACKAGE                   -> [PACKAGE, T]
    NUMBER                    -> [NUMBER, T]
    COMPLEX                   -> [COMPLEX, NUMBER, T]
    REAL                      -> [REAL, NUMBER, T]
    FLOAT                     -> [FLOAT, REAL, NUMBER, T]
    SHORT_FLOAT               -> [SHORT_FLOAT, FLOAT, REAL, NUMBER, T]
    SINGLE_FLOAT              -> [SINGLE_FLOAT, FLOAT, REAL, NUMBER, T]
    DOUBLE_FLOAT              -> [DOUBLE_FLOAT, FLOAT, REAL, NUMBER, T]
    LONG_FLOAT                -> [LONG_FLOAT, FLOAT, REAL, NUMBER, T]
    RATIONAL                  -> [RATIONAL, REAL, NUMBER, T]
    RATIO                     -> [RATIO, RATIONAL, REAL, NUMBER, T]
    INTEGER                   -> [INTEGER, RATIONAL, REAL, NUMBER, T]
    SIGNED_BYTE               -> [SIGNED_BYTE, INTEGER, RATIONAL, REAL, NUMBER, T]
    UNSIGNED_BYTE             -> [UNSIGNED_BYTE, SIGNED_BYTE, INTEGER, RATIONAL, REAL, NUMBER, T]
    BIT                       -> [BIT, UNSIGNED_BYTE, SIGNED_BYTE, INTEGER, RATIONAL, REAL, NUMBER, T]
    FIXNUM                    -> [FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, T]
    BIGNUM                    -> [BIGNUM, INTEGER, RATIONAL, REAL, NUMBER, T]
    CHARACTER                 -> [CHARACTER, T]
    BASE_CHAR                 -> [BASE_CHAR, CHARACTER, T]
    STANDARD_CHAR             -> [STANDARD_CHAR, BASE_CHAR, CHARACTER, T]
    EXTENDED_CHAR             -> [EXTENDED_CHAR, CHARACTER, T]
    LIST                      -> [LIST, SEQUENCE, T]
    NULL                      -> [NULL, SYMBOL, LIST, SEQUENCE, T]
    CONS                      -> [CONS, LIST, SEQUENCE, T]
    ATOM                      -> [ATOM, T]
    ARRAY                     -> [ARRAY, T]
    SIMPLE_ARRAY              -> [SIMPLE_ARRAY, ARRAY, T]
    VECTOR                    -> [VECTOR, ARRAY, SEQUENCE, T]
    SIMPLE_VECTOR             -> [SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE, T]
    BIT_VECTOR                -> [BIT_VECTOR, VECTOR, ARRAY, SEQUENCE, T]
    SIMPLE_BIT_VECTOR         -> [SIMPLE_BIT_VECTOR, BIT_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE, T]
    STRING                    -> [STRING, VECTOR, ARRAY, SEQUENCE, T]
    BASE_STRING               -> [BASE_STRING, STRING, VECTOR, ARRAY, SEQUENCE, T]
    SIMPLE_STRING             -> [SIMPLE_STRING, STRING, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE, T]
    SIMPLE_BASE_STRING        -> [SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING, STRING, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE, T]
    SEQUENCE                  -> [SEQUENCE, T]
    HASH_TABLE                -> [HASH_TABLE, T]
    PATHNAME                  -> [PATHNAME, T]
    LOGICAL_PATHNAME          -> [LOGICAL_PATHNAME, PATHNAME, T]
    STREAM                    -> [STREAM, T]
    BROADCAST_STREAM          -> [BROADCAST_STREAM, STREAM, T]
    CONCATENATED_STREAM       -> [CONCATENATED_STREAM, STREAM, T]
    ECHO_STREAM               -> [ECHO_STREAM, STREAM, T]
    FILE_STREAM               -> [FILE_STREAM, STREAM, T]
    STRING_STREAM             -> [STRING_STREAM, STREAM, T]
    SYNONYM_STREAM            -> [SYNONYM_STREAM, STREAM, T]
    TWO_WAY_STREAM            -> [TWO_WAY_STREAM, STREAM, T]

internalTypep :: LispType -> LispType -> Bool
internalTypep NIL _ = False
internalTypep _ NIL = False
internalTypep _ T   = True
internalTypep t1 t2 = t2 `elem` precedences t1
