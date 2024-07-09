{-# LANGUAGE LambdaCase #-}

module TypeSystem.LispType (LispType(..), precedences, internalTypep) where

data LispType = NIL'
              | BOOLEAN'
              | FUNCTION'
              | COMPILED_FUNCTION'
              | GENERIC_FUNCTION'
              | STANDARD_GENERIC_FUNCTION'
              | CLASS'
              | BUILT_IN_CLASS'
              | STRUCTURE_CLASS'
              | STANDARD_CLASS'
              | METHOD'
              | STANDARD_METHOD'
              | STRUCTURE_OBJECT'
              | STANDARD_OBJECT'
              | METHOD_COMBINATION'
              | T'
              | TYPE_ERROR'
              | SIMPLE_TYPE_ERROR'
              | CONTROL_ERROR'
              | PROGRAM_ERROR'
              | UNDEFINED_FUNCTION'
              | UNBOUND_SLOT'
              | CONDITION'
              | WARNING'
              | STYLE_WARNING'
              | SERIOUS_CONDITION'
              | ERROR'
              | CELL_ERROR'
              | PARSE_ERROR'
              | STORAGE_CONDITION'
              | SIMPLE_ERROR'
              | SIMPLE_CONDITION'
              | SIMPLE_WARNING'
              | RESTART'
              | SYMBOL'
              | KEYWORD'
              | UNBOUND_VARIABLE'
              | PACKAGE'
              | PACKAGE_ERROR'
              | NUMBER'
              | COMPLEX'
              | REAL'
              | FLOAT'
              | SHORT_FLOAT'
              | SINGLE_FLOAT'
              | DOUBLE_FLOAT'
              | LONG_FLOAT'
              | RATIONAL'
              | RATIO'
              | INTEGER'
              | SIGNED_BYTE'
              | UNSIGNED_BYTE'
              | BIT'
              | FIXNUM'
              | BIGNUM'
              | RANDOM_STATE'
              | ARITHMETIC_ERROR'
              | DIVISION_BY_ZERO'
              | FLOATING_POINT_INVALID_OPERATION'
              | FLOATING_POINT_INEXACT'
              | FLOATING_POINT_OVERFLOW'
              | FLOATING_POINT_UNDERFLOW'
              | CHARACTER'
              | BASE_CHAR'
              | STANDARD_CHAR'
              | EXTENDED_CHAR'
              | LIST'
              | NULL'
              | CONS'
              | ATOM'
              | ARRAY'
              | SIMPLE_ARRAY'
              | VECTOR'
              | SIMPLE_VECTOR'
              | BIT_VECTOR'
              | SIMPLE_BIT_VECTOR'
              | STRING'
              | BASE_STRING'
              | SIMPLE_STRING'
              | SIMPLE_BASE_STRING'
              | SEQUENCE'
              | HASH_TABLE'
              | PATHNAME'
              | LOGICAL_PATHNAME'
              | FILE_ERROR'
              | STREAM'
              | BROADCAST_STREAM'
              | CONCATENATED_STREAM'
              | ECHO_STREAM'
              | FILE_STREAM'
              | STRING_STREAM'
              | SYNONYM_STREAM'
              | TWO_WAY_STREAM'
              | STREAM_ERROR'
              | END_OF_FILE'
              | PRINT_NOT_READABLE'
              | READTABLE'
              | READER_ERROR'
              deriving Eq

precedences :: LispType -> [LispType]
precedences = \case
    NIL'                              -> []
    BOOLEAN'                          -> [BOOLEAN', SYMBOL', T']
    FUNCTION'                         -> [FUNCTION', T']
    COMPILED_FUNCTION'                -> [COMPILED_FUNCTION', FUNCTION', T']
    GENERIC_FUNCTION'                 -> [GENERIC_FUNCTION', FUNCTION', T']
    STANDARD_GENERIC_FUNCTION'        -> [STANDARD_GENERIC_FUNCTION', GENERIC_FUNCTION', FUNCTION', T']
    CLASS'                            -> [CLASS', STANDARD_OBJECT', T']
    BUILT_IN_CLASS'                   -> [BUILT_IN_CLASS', CLASS', STANDARD_OBJECT', T']
    STRUCTURE_CLASS'                  -> [STRUCTURE_CLASS', CLASS', STANDARD_OBJECT', T']
    STANDARD_CLASS'                   -> [STANDARD_CLASS', CLASS', STANDARD_OBJECT', T']
    METHOD'                           -> [METHOD', T']
    STANDARD_METHOD'                  -> [STANDARD_METHOD', METHOD', STANDARD_OBJECT', T']
    STRUCTURE_OBJECT'                 -> [STRUCTURE_OBJECT', T']
    STANDARD_OBJECT'                  -> [STANDARD_OBJECT', T']
    METHOD_COMBINATION'               -> [METHOD_COMBINATION', T']
    T'                                -> [T']
    TYPE_ERROR'                       -> [TYPE_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    SIMPLE_TYPE_ERROR'                -> [SIMPLE_TYPE_ERROR', SIMPLE_CONDITION', TYPE_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    CONTROL_ERROR'                    -> [CONTROL_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    PROGRAM_ERROR'                    -> [PROGRAM_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    UNDEFINED_FUNCTION'               -> [UNDEFINED_FUNCTION', CELL_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    UNBOUND_SLOT'                     -> [UNBOUND_SLOT', CELL_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    CONDITION'                        -> [CONDITION', T']
    WARNING'                          -> [WARNING', CONDITION', T']
    STYLE_WARNING'                    -> [STYLE_WARNING', WARNING', CONDITION', T']
    SERIOUS_CONDITION'                -> [SERIOUS_CONDITION', CONDITION', T']
    ERROR'                            -> [ERROR', SERIOUS_CONDITION', CONDITION', T']
    CELL_ERROR'                       -> [CELL_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    PARSE_ERROR'                      -> [PARSE_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    STORAGE_CONDITION'                -> [STORAGE_CONDITION', SERIOUS_CONDITION', CONDITION', T']
    SIMPLE_ERROR'                     -> [SIMPLE_ERROR', SIMPLE_CONDITION', ERROR', SERIOUS_CONDITION', CONDITION', T']
    SIMPLE_CONDITION'                 -> [SIMPLE_CONDITION', CONDITION', T']
    SIMPLE_WARNING'                   -> [SIMPLE_WARNING', SIMPLE_CONDITION', WARNING', CONDITION', T']
    RESTART'                          -> [RESTART', T']
    SYMBOL'                           -> [SYMBOL', T']
    KEYWORD'                          -> [KEYWORD', SYMBOL', T']
    UNBOUND_VARIABLE'                 -> [UNBOUND_VARIABLE', CELL_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    PACKAGE'                          -> [PACKAGE', T']
    PACKAGE_ERROR'                    -> [PACKAGE_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    NUMBER'                           -> [NUMBER', T']
    COMPLEX'                          -> [COMPLEX', NUMBER', T']
    REAL'                             -> [REAL', NUMBER', T']
    FLOAT'                            -> [FLOAT', REAL', NUMBER', T']
    SHORT_FLOAT'                      -> [SHORT_FLOAT', FLOAT', REAL', NUMBER', T']
    SINGLE_FLOAT'                     -> [SINGLE_FLOAT', FLOAT', REAL', NUMBER', T']
    DOUBLE_FLOAT'                     -> [DOUBLE_FLOAT', FLOAT', REAL', NUMBER', T']
    LONG_FLOAT'                       -> [LONG_FLOAT', FLOAT', REAL', NUMBER', T']
    RATIONAL'                         -> [RATIONAL', REAL', NUMBER', T']
    RATIO'                            -> [RATIO', RATIONAL', REAL', NUMBER', T']
    INTEGER'                          -> [INTEGER', RATIONAL', REAL', NUMBER', T']
    SIGNED_BYTE'                      -> [SIGNED_BYTE', INTEGER', RATIONAL', REAL', NUMBER', T']
    UNSIGNED_BYTE'                    -> [UNSIGNED_BYTE', SIGNED_BYTE', INTEGER', RATIONAL', REAL', NUMBER', T']
    BIT'                              -> [BIT', UNSIGNED_BYTE', SIGNED_BYTE', INTEGER', RATIONAL', REAL', NUMBER', T']
    FIXNUM'                           -> [FIXNUM', INTEGER', RATIONAL', REAL', NUMBER', T']
    BIGNUM'                           -> [BIGNUM', INTEGER', RATIONAL', REAL', NUMBER', T']
    RANDOM_STATE'                     -> [RANDOM_STATE', T']
    ARITHMETIC_ERROR'                 -> [ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    DIVISION_BY_ZERO'                 -> [DIVISION_BY_ZERO', ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    FLOATING_POINT_INVALID_OPERATION' -> [FLOATING_POINT_INVALID_OPERATION', ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    FLOATING_POINT_INEXACT'           -> [FLOATING_POINT_INEXACT', ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    FLOATING_POINT_OVERFLOW'          -> [FLOATING_POINT_OVERFLOW', ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    FLOATING_POINT_UNDERFLOW'         -> [FLOATING_POINT_UNDERFLOW', ARITHMETIC_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    CHARACTER'                        -> [CHARACTER', T']
    BASE_CHAR'                        -> [BASE_CHAR', CHARACTER', T']
    STANDARD_CHAR'                    -> [STANDARD_CHAR', BASE_CHAR', CHARACTER', T']
    EXTENDED_CHAR'                    -> [EXTENDED_CHAR', CHARACTER', T']
    LIST'                             -> [LIST', SEQUENCE', T']
    NULL'                             -> [NULL', SYMBOL', LIST', SEQUENCE', T']
    CONS'                             -> [CONS', LIST', SEQUENCE', T']
    ATOM'                             -> [ATOM', T']
    ARRAY'                            -> [ARRAY', T']
    SIMPLE_ARRAY'                     -> [SIMPLE_ARRAY', ARRAY', T']
    VECTOR'                           -> [VECTOR', ARRAY', SEQUENCE', T']
    SIMPLE_VECTOR'                    -> [SIMPLE_VECTOR', VECTOR', SIMPLE_ARRAY', ARRAY', SEQUENCE', T']
    BIT_VECTOR'                       -> [BIT_VECTOR', VECTOR', ARRAY', SEQUENCE', T']
    SIMPLE_BIT_VECTOR'                -> [SIMPLE_BIT_VECTOR', BIT_VECTOR', VECTOR', SIMPLE_ARRAY', ARRAY', SEQUENCE', T']
    STRING'                           -> [STRING', VECTOR', ARRAY', SEQUENCE', T']
    BASE_STRING'                      -> [BASE_STRING', STRING', VECTOR', ARRAY', SEQUENCE', T']
    SIMPLE_STRING'                    -> [SIMPLE_STRING', STRING', VECTOR', SIMPLE_ARRAY', ARRAY', SEQUENCE', T']
    SIMPLE_BASE_STRING'               -> [SIMPLE_BASE_STRING', BASE_STRING', SIMPLE_STRING', STRING', VECTOR', SIMPLE_ARRAY', ARRAY', SEQUENCE', T']
    SEQUENCE'                         -> [SEQUENCE', T']
    HASH_TABLE'                       -> [HASH_TABLE', T']
    PATHNAME'                         -> [PATHNAME', T']
    LOGICAL_PATHNAME'                 -> [LOGICAL_PATHNAME', PATHNAME', T']
    FILE_ERROR'                       -> [FILE_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    STREAM'                           -> [STREAM', T']
    BROADCAST_STREAM'                 -> [BROADCAST_STREAM', STREAM', T']
    CONCATENATED_STREAM'              -> [CONCATENATED_STREAM', STREAM', T']
    ECHO_STREAM'                      -> [ECHO_STREAM', STREAM', T']
    FILE_STREAM'                      -> [FILE_STREAM', STREAM', T']
    STRING_STREAM'                    -> [STRING_STREAM', STREAM', T']
    SYNONYM_STREAM'                   -> [SYNONYM_STREAM', STREAM', T']
    TWO_WAY_STREAM'                   -> [TWO_WAY_STREAM', STREAM', T']
    STREAM_ERROR'                     -> [STREAM_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    END_OF_FILE'                      -> [END_OF_FILE', STREAM_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']
    PRINT_NOT_READABLE'               -> [PRINT_NOT_READABLE', ERROR', SERIOUS_CONDITION', CONDITION', T']
    READTABLE'                        -> [READTABLE', T']
    READER_ERROR'                     -> [READER_ERROR', PARSE_ERROR', STREAM_ERROR', ERROR', SERIOUS_CONDITION', CONDITION', T']

internalTypep :: LispType -> LispType -> Bool
internalTypep NIL' _ = False
internalTypep _ NIL' = False
internalTypep _ T'   = True
internalTypep t1 t2  = t2 `elem` precedences t1
