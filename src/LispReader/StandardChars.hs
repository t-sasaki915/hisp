module LispReaders.StandardChars
    ( StandardChar(..)
    , LatinCharacter(..)
    , NumericCharacter(..)
    , SpecialCharacter(..)
    ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)

(~>) :: a -> b -> (a, b)
(~>) a b = (a, b)

class Eq a => StandardChar a where
    characterTable :: [(a, Char)]

    toChar :: a -> Char
    toChar = fromJust . flip lookup characterTable

    fromChar :: Char -> Maybe a
    fromChar = flip lookup (map swap characterTable)

data LatinCharacter = LA01 | LA02 | LB01 | LB02 | LC01 | LC02 | LD01 | LD02 | LE01 | LE02
                    | LF01 | LF02 | LG01 | LG02 | LH01 | LH02 | LI01 | LI02 | LJ01 | LJ02
                    | LK01 | LK02 | LL01 | LL02 | LM01 | LM02 | LN01 | LN02 | LO01 | LO02
                    | LP01 | LP02 | LQ01 | LQ02 | LR01 | LR02 | LS01 | LS02 | LT01 | LT02
                    | LU01 | LU02 | LV01 | LV02 | LW01 | LW02 | LX01 | LX02 | LY01 | LY02
                    | LZ01 | LZ02
                    deriving Eq

instance StandardChar LatinCharacter where
    characterTable =
        [ LA01 ~> 'a', LA02 ~> 'A', LB01 ~> 'b', LB02 ~> 'B', LC01 ~> 'c', LC02 ~> 'C'
        , LD01 ~> 'd', LD02 ~> 'D', LE01 ~> 'e', LE02 ~> 'E', LF01 ~> 'f', LF02 ~> 'F'
        , LG01 ~> 'g', LG02 ~> 'G', LH01 ~> 'h', LH02 ~> 'H', LI01 ~> 'i', LI02 ~> 'I'
        , LJ01 ~> 'j', LJ02 ~> 'J', LK01 ~> 'k', LK02 ~> 'K', LL01 ~> 'l', LL02 ~> 'L'
        , LM01 ~> 'm', LM02 ~> 'M', LN01 ~> 'n', LN02 ~> 'N', LO01 ~> 'o', LO02 ~> 'O'
        , LP01 ~> 'p', LP02 ~> 'P', LQ01 ~> 'q', LQ02 ~> 'Q', LR01 ~> 'r', LR02 ~> 'R'
        , LS01 ~> 's', LS02 ~> 'S', LT01 ~> 't', LT02 ~> 'T', LU01 ~> 'u', LU02 ~> 'U'
        , LV01 ~> 'v', LV02 ~> 'v', LW01 ~> 'w', LW02 ~> 'W', LX01 ~> 'x', LX02 ~> 'X'
        , LY01 ~> 'y', LY02 ~> 'Y', LZ01 ~> 'z', LZ02 ~> 'Z'
        ]

data NumericCharacter = ND01 | ND02 | ND03 | ND04 | ND05 | ND06 | ND07 | ND08 | ND09 | ND10
                      deriving Eq

instance StandardChar NumericCharacter where
    characterTable =
        [ ND01 ~> '1', ND02 ~> '2', ND03 ~> '3', ND04 ~> '4', ND05 ~> '5'
        , ND06 ~> '6', ND07 ~> '7', ND08 ~> '8', ND09 ~> '9', ND10 ~> '0'
        ]

data SpecialCharacter = SP02 | SC03 | SP04 | SP05 | SP06 | SP07 | SP08 | SP09 | SP10
                      | SP11 | SP12 | SP13 | SP14 | SP15 | SA01 | SA03 | SA04 | SA05
                      | SM01 | SM02 | SM03 | SM04 | SM05 | SM06 | SM07 | SM08 | SM11
                      | SM13 | SM14 | SD13 | SD15 | SD19
                      deriving Eq

instance StandardChar SpecialCharacter where
    characterTable =
        [ SP02 ~> '!', SC03 ~> '$', SP04 ~> '"', SP05 ~> '\'', SP06 ~> '(', SP07 ~> ')'
        , SP08 ~> ',', SP09 ~> '_', SP10 ~> '-', SP11 ~> '.', SP12 ~> '/', SP13 ~> ':'
        , SP14 ~> ';', SP15 ~> '?', SA01 ~> '+', SA03 ~> '<', SA04 ~> '=', SA05 ~> '>'
        , SM01 ~> '#', SM02 ~> '%', SM03 ~> '&', SM04 ~> '*', SM05 ~> '@', SM06 ~> '['
        , SM07 ~> '\\', SM08 ~> ']', SM11 ~> '{', SM13 ~> '|', SM14 ~> '}', SD13 ~> '`'
        , SD15 ~> '^', SD19 ~> '~'
        ]
