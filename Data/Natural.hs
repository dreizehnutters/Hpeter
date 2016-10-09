-- |Support for natural numbers.
-- see <http://hackage.haskell.org/packages/archive/natural-numbers/0.1.1.1/doc/html/src/Data-Natural.html>
module Data.Natural (

    Natural

) where

    import Data.Monoid

    {-|
        The type of natural numbers.
    -}
    newtype Natural = Natural Integer deriving (Eq, Ord)

    {-
        We do not just provide a minimal complete definition. The reason is that all default
        implementations use toEnum and fromEnum internally. Unfortunately, these use Int instead of
        Integer, which can lead to overflows. Furthermore, we can avoid the negativity check in some
        methods.
    -}
    instance Enum Natural where

        succ (Natural integer) = Natural (succ integer)

        pred = fromInteger . pred . toInteger

        toEnum = fromInteger . toEnum

        fromEnum = fromEnum . toInteger

        enumFrom (Natural start) = map Natural (enumFrom start)

        enumFromThen (Natural start) (Natural next) = map Natural $
                                                      if start > next
                                                          then enumFromThenTo start next 0
                                                          else enumFromThen start next

        enumFromTo (Natural start) (Natural end) = map Natural (enumFromTo start end)

        enumFromThenTo (Natural start) (Natural next) (Natural end) = map Natural $
                                                                      enumFromThenTo start next end

    instance Show Natural where

        showsPrec prec (Natural integer) = showsPrec prec integer

    instance Read Natural where

        readsPrec prec str = map (first fromInteger) (readsPrec prec str) where

            -- This is Control.Arrow.first, specialized to (->).
            first :: (val -> val') -> (val,other) -> (val',other)
            first fun (val,other) = (fun val,other)

    instance Num Natural where

        Natural integer1 + Natural integer2 = Natural (integer1 + integer2)

        Natural integer1 - Natural integer2 = fromInteger (integer1 - integer2)

        Natural integer1 * Natural integer2 = Natural (integer1 * integer2)

        abs = id

        signum (Natural integer) = Natural (signum integer)

        fromInteger integer | integer >= 0 = Natural integer
                            | otherwise    = error "Data.Natural: natural cannot be negative"

    instance Real Natural where

        toRational = toRational . toInteger

    instance Integral Natural where

        quotRem (Natural integer1) (Natural integer2) = let

                                                            (quot,rem) = quotRem integer1 integer2

                                                        in (Natural quot,Natural rem)

        {-
            Although an implementation of divMod is generally not needed for a minimal complete
            definition, we have to include one, since the default implementation relies on negative
            numbers being available.
        -}
        divMod = quotRem

        toInteger (Natural integer) = integer
