{-
    Parsen von μOpal-Programmen.
-}
module MOpal.Parser (

    progParser,
    defParser ,
    leftParser,
    typeParser,
    exprParser

)  where

    import Data.Char
    import NYI
    import Util
    import Parser
    import MOpal.AST

    {-
        Liest ein μOpal-Programm. Zunächst werden alle Whitespace-Zeichen, die vor dem ersten Token
        stehen, entfernt. Die Whitespace-Zeichen, die sich hinter Token befinden, werden durch
        Anwendung der Funktion |token| entfernt.
    -}
    progParser :: Parser Prog
    progParser = (whitespace <&> listDefParser) =>> (\(_, defs) -> Prog defs )

    listDefParser:: Parser [Loc Def]
    listDefParser = ifNext (== '\EOT') (success [])
                                       ((token (loc defParser) <&> token listDefParser) =>> uncurry (:) )
    
    defParser :: Parser (Def)
    defParser = ((token (fixed "DEF")) <&> (token leftParser) <&> (token (fixed "==")) <&> (token exprParser)) =>> (\ ((), (left, ((), expr))) -> Def left expr)

    leftParser :: Parser (Left')
    leftParser = ifNext (=='M') (((token (fixed "MAIN")) <&> (token (fixed ":")) <&> (token (loc typeParser)) ) =>> (\ (_,(_, t)) -> LeftMain t)) 
                                ((token alphaString <&> token  (fixed "(") <&> token identListParser  <&> token (fixed ":") <&> (token (loc typeParser))) =>> (\ (ident, ((), (identList, (_,t)))) -> LeftIdent ident identList t))
    
    identListParser:: Parser [(Loc String, Type)]
    identListParser = ifNext (==')') ((token (single) <&> token (success[])) =>> snd)
                                     (((token identParser) <&> (token identListParser') ) =>> uncurry (:))
    
    identListParser'::Parser [(Loc String, Type)]
    identListParser' = ifNext (==')') ((token (single) <&> token (success[])) =>> snd)
                                      ((token  (fixed ",") <&> token identParser <&> token identListParser') =>> (uncurry (:)).snd)
                                        
    identParser::Parser (Loc String, Type)
    identParser = token ((alphaString) <&> token (fixed ":") <&> token typeParser ) =>> (\(ident,(_, t)) -> (ident, t) )
    
    applyParser:: Parser Expr                                                    
    applyParser = (token alphaString <&> (ifNext (=='(') (((token single) <&> (token exprListParser)) =>> snd)
                                                         (success [Loc (Pos 0 0) Bottom]) )) =>> f where
                                                            f (ident, exprList) | exprList == [Loc (Pos 0 0) Bottom] = Apply ident Nothing 
                                                                                | otherwise          = Apply ident (Just exprList)
   
    exprListParser ::Parser [Loc Expr]
    exprListParser = ifNext (==')')  ((token (single) <&> token (success[])) =>> snd)
                                    (((token (loc exprParser)) <&> (token exprListParser' )) =>> uncurry (:) ) 

                                    
    exprListParser':: Parser [Loc Expr]
    exprListParser' = ifNext (==')')     ((token (single) <&> token (success[])) =>> snd)
                                        ((token (fixed ",") <&> (token (loc exprParser)) <&> (token exprListParser')) =>> (uncurry (:)).snd)
                                        
    
    condParser:: Parser Expr
    condParser = (token (fixed "IF") <&> token (loc exprParser)  <&> token (fixed "THEN") <&> token (loc exprParser) 
                    <&> ( ifNext (=='E')  (token (fixed "ELSE") <&> token (loc exprParser) =>> snd)
                                           (success (Loc (Pos 0 0) Bottom)) ) <&> token (fixed "FI") ) =>> f where
                                                    f ((), (a, ((), (b, (c ,_ ))))) | val c == Bottom = Condition a b Nothing 
                                                                                    | otherwise = Condition a b (Just c)
                                                                              
   
    typeParser :: Parser (Type)
    typeParser = ifNext (== 'B') (token (fixed "BOOL") =>> (\ _ -> BOOL)) 
                                 (token (fixed "NAT") =>> (\ _ -> NAT))
                                 --(failure " 3 ")
                                 
    exprParser :: Parser Expr
    exprParser = ifNext (=='T') (token (fixed "TRUE") =>> (\ _ -> TRUE))
                                $ ifNext (=='F')  (token (fixed "FALSE") =>> (\ _ -> FALSE))
                                                $ ifNext isDigit (token (loc nat) =>> (\x -> Num x))
                                                                $ ifNext  (=='I') condParser
                                                                                  applyParser 
    {-
    Liest eine einzelne Ziffer.
    -}
    digit :: Parser Int
    digit = ifNext isDigit (single =>> \char -> ord char - ord '0')
                           (failure "Digit expected")

    {-
        Liest eine maximale nicht-leere Folge von Ziffern.
    -}
    digits :: Parser [Int]
    digits =  digit                              <&>
              ifNext isDigit digits (success []) =>>
              \(dig,digs) -> dig : digs

    {-
        Liest eine natürliche Zahl.
    -}
    nat :: Parser Int
    nat = digits =>> foldl ((+).(* 10)) 0

    alphaString :: Parser (Loc String)
    alphaString = loc alphaString'



    alphaString' :: Parser String
    alphaString' = alphaChar <&> ifNext isAlpha alphaString' (success "") =>> uncurry (:)

    alphaChar :: Parser Char
    alphaChar = ifNext isAlpha (single)
                (failure "alpha char expected")




    {-
        Entfernt eine maximale Folge von Whitespace-Zeichen.
    -}
    whitespace :: Parser ()
    whitespace = ifNext isSpace (single <&> whitespace =>> const ())
                                (success ())

    {-
        Ein Parser der Form |token parser| arbeitet zunächst wie |parser| und liest im Anschluss
        daran eine maximale Folge von Whitespace-Zeichen. Die Ausgabe des gesamten Parsers ist die
        Ausgabe von |parser|. Scheitert |parser|, so scheitert sofort der gesamte Parser. |token|
        sollte Parser für einzelne Token angewendet werden, damit der auf die Token folgende
        Whitespace entfernt wird. Der Whitespace vor dem ersten Token wird durch |program| entfernt.
    -}
    token :: Parser out -> Parser out
    token parser = parser <&> whitespace =>> fst


    main = putStr ""