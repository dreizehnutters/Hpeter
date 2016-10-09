{-
    Definiert die Struktur von μOM-Programmen.
-}
module MOM.Code (

    Code,
    toString,
    fromString,
    Op (LDPAR, LDNAT, LDNIL, ADD, SUB, MUL, DIV, CONS, HEAD, TAIL, SWAP, ABORT, CALL, ICALL, RET, BRANCH),
    Cond (Always, Empty, NonEmpty, Zero, NonZero, Equal, LessThan, Error)

) where

    import Data.Maybe
    import Data.Natural

    -- Code
    {-
        Ein Programm, dargestellt als eine Liste von Befehlen. Der Index eines Befehls in der Liste
        entspricht seiner Adresse.
    -}
    type Code = [Op]

    {-
        Konvertiert ein Programm in eine Assemblerdarstellung.
    -}
    toString :: Code -> String
    toString = unlines . map opToString

    {-
        Generiert ein Programm von einer Assemblerdarstellung.
    -}
    fromString :: String -> Code
    fromString = map opFromString . lines

    {-
        Ein einzelner Befehl.
    -}
    data Op = LDPAR Natural
            | LDNAT Natural
            | LDNIL
            | ADD
            | SUB
            | MUL
            | DIV
            | CONS
            | HEAD
            | TAIL
            | SWAP
            | ABORT
            | CALL Natural Natural
            | ICALL Natural
            | RET
            | BRANCH Cond Integer

    opFromString :: String -> Op
    opFromString = opFromStrings . words

    opFromStrings :: [String] -> Op
    opFromStrings ["LDPAR",idxStr]             = case maybeRead idxStr of
                                                     Just idx -> LDPAR idx
                                                     Nothing  -> error "illegal index in LDPAR"
    opFromStrings ["LDNAT",naturalStr]         = case maybeRead naturalStr of
                                                     Just natural -> LDNAT natural
                                                     Nothing      -> error "illegal natural in LDNAT"
    opFromStrings ["LDNIL"]                    = LDNIL
    opFromStrings ["ADD"]                      = ADD
    opFromStrings ["SUB"]                      = SUB
    opFromStrings ["MUL"]                      = MUL
    opFromStrings ["DIV"]                      = DIV
    opFromStrings ["CONS"]                     = CONS
    opFromStrings ["HEAD"]                     = HEAD
    opFromStrings ["TAIL"]                     = TAIL
    opFromStrings ["SWAP"]                     = SWAP
    opFromStrings ["ABORT"]                    = ABORT
    opFromStrings ["CALL",paramCntStr,dstStr]  = case (maybeRead paramCntStr,maybeRead dstStr) of
                                                     (,) (Just paramCnt)
                                                         (Just dst)      -> CALL paramCnt dst
                                                     (,) (Just paramCnt)
                                                         Nothing         -> error "illegal CALL \
                                                                                  \destination"
                                                     _                   -> error "illegal \
                                                                                  \parameter \
                                                                                  \count in CALL"
    opFromStrings ["ICALL",paramCntStr]        = case maybeRead paramCntStr of
                                                     Just paramCnt       -> ICALL paramCnt
                                                     Nothing             -> error "illegal \
                                                                                  \parameter \
                                                                                  \count in ICALL"
    opFromStrings ["RET"]                      = RET
    opFromStrings ["BRANCH",condStr,offsetStr] = case maybeRead offsetStr of
                                                     Just offset -> BRANCH (condFromString condStr)
                                                                           offset
                                                     Nothing     -> error "illegal offset in BRANCH"
    opFromStrings _                            = error "unknown opcode"

    opToString :: Op -> String
    opToString (LDPAR idx)          = "LDPAR " ++ show idx
    opToString (LDNAT natural)      = "LDNAT " ++ show natural
    opToString LDNIL                = "LDNIL"
    opToString ADD                  = "ADD"
    opToString SUB                  = "SUB"
    opToString MUL                  = "MUL"
    opToString DIV                  = "DIV"
    opToString CONS                 = "CONS"
    opToString HEAD                 = "HEAD"
    opToString TAIL                 = "TAIL"
    opToString SWAP                 = "SWAP"
    opToString ABORT                = "ABORT"
    opToString (CALL paramCnt dst)  = "CALL " ++ show paramCnt ++ " " ++ show dst
    opToString (ICALL paramCnt)     = "ICALL " ++ show paramCnt
    opToString RET                  = "RET"
    opToString (BRANCH cond offset) = "BRANCH " ++ condToString cond ++ " " ++ show offset

    maybeRead :: (Read val) => String -> Maybe val
    maybeRead str = case [val | (val,"") <- reads str] of
                        [val] -> Just val
                        _     -> Nothing

    {-
        Eine Sprungbedingung.
    -}
    data Cond = Always | Empty | NonEmpty | Zero | NonZero | Equal | LessThan | Error deriving (Eq, Ord, Enum, Bounded)

    condMnemonics :: [(Cond,String)]
    condMnemonics = [
                       (Always,  "always"),
                       (Empty,   "e"),
                       (NonEmpty,"ne"),
                       (Zero,    "z"),
                       (NonZero, "nz"),
                       (Equal,   "eq"),
                       (LessThan,"lt"),
                       (Error,   "error")
                   ]

    condFromString :: String -> Cond
    condFromString str = maybe (error ("unknown condition " ++ str))
                               id
                               (lookup str (map (uncurry (flip (,))) condMnemonics))

    condToString :: Cond -> String
    condToString = fromJust . flip lookup condMnemonics
