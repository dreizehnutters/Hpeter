{-
    Hilfsmittel.
-}
module Util (

    Pos (Pos, lineNo, colNo),
    Loc (Loc, pos, val)

) where

    {-
        Eine Quelltextposition.
    -}
    data Pos = Pos { lineNo, colNo :: Int } deriving (Eq, Show)

    {-
        Lokalisierte Werte. Ein Wert eines Typs |Loc val| ist ein Wert des Typs |val| zusammen mit
        einer Quelltextposition.
    -}
    data Loc val = Loc { pos :: Pos
                       , val :: val
                       } deriving (Eq, Show)
