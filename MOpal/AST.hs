{-
    Datentypen für abstrakte Syntaxbäume.
-}
module MOpal.AST (

    {- Datenkonstruktoren -}
    Prog  (..),
    Def   (..),
    Left' (..),
    Type  (..),
    Expr  (..),


    -- und Datentypen für alle weiteren syntaktischen Konstrukte

) where

  import Util

  data Prog = Prog [Loc Def]
      deriving (Eq, Show)

  data Def = Def Left' (Expr)
      deriving (Eq, Show)

  data Left' = LeftMain (Loc Type)
             | LeftIdent (Loc String) [(Loc String, Type)] (Loc Type)
             deriving (Eq, Show)

  data Type = NAT | BOOL | UNDEFINED
      deriving (Eq, Show)

  data Expr = Num (Loc Int)
            | TRUE
            | FALSE
            | Apply (Loc String) (Maybe [Loc Expr])
            | Condition (Loc Expr) (Loc Expr) (Maybe (Loc Expr))
            | Bottom
            deriving (Eq, Show)
