{-# LANGUAGE LambdaCase #-}

module TypeSystem.LispData (LispData(..), internalTypeOf) where

import TypeSystem.LispType (LispType(..))

data LispData = NIL
              | T
              | SIMPLE_STRING String
              | INTEGER Integer

internalTypeOf :: LispData -> LispType
internalTypeOf = \case
    NIL               -> BOOLEAN'
    T                 -> BOOLEAN'
    (SIMPLE_STRING _) -> SIMPLE_STRING'
    (INTEGER _)       -> INTEGER'
