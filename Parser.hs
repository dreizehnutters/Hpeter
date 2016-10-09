{-
    Grundlegende Unterstützung für LL(1)-Parsing (unabhängig von einer konkreten Sprache).
-}
module Parser (

    Parser,
    failure,
    success,
    ifNext,
    (<&>), --kombinieren
    (=>>), --ast
    loc,
    single,
    fixed,
    run

) where

    import NYI
    import Util

    {-
        Ein Wert eines Typs |Parser out| ist ein Parser, der eine Ausgabe vom Typ |out| produziert.
        Intern ist ein Parser eine Funktion, die eine Eingabe entweder in eine Fehlermeldung oder in
        ein Paar aus einem Ausgabewert und der verbleibenden Eingabe konvertiert. Der Parser muss
        nicht selbst Zeilen- und Spaltennummern der Eingabezeichen berechnen, da die Eingabe bereits
        die entsprechenden Angaben enthält. Die Funktion |run| liefert dem Parser diese Information.
        |run| sorgt außerdem dafür, dass ein Parser immer eine unendliche Eingabe erhält. Nach der
        eigentlichen Eingabe folgen unendlich viele EOT-Steuerzeichen.
    -}
    newtype Parser out = Parser ([Loc Char] -> Either (Loc String) (out,[Loc Char]))

    {-
        Konvertiert eine Fehlermeldung in einen Parser, der keine Zeichen liest und grundsätzlich
        mit dieser Fehlermeldung scheitert.
    -}
    failure :: String -> Parser out
    failure msg = Parser (\ ((Loc pos _) : _) -> Left (Loc pos msg))
    --(Loc poc c : locChars) = Parser (Left (Loc pos msg))
  
    {-
        Konvertiert einen Wert in einen Parser, der keine Zeichen liest, sondern lediglich den
        angegebenen Wert ausgibt.
    -}
    success :: out -> Parser out
    success out = Parser $ \ stream -> Right (out, stream)

    {-
        Ein Parser der Form |ifNext cond thenParser elseParser| wendet zunächst |cond| auf das
        nächste Eingabezeichen an, ohne es aus der Eingabe zu entfernen. Ist das Ergebnis dieser
        Anwendung |True|, arbeitet der Parser wie |thenParser| weiter, ansonsten wie |elseParser|.
    -}
    ifNext :: (Char -> Bool) -> Parser out -> Parser out -> Parser out
    ifNext cond (Parser thenParserFun) (Parser elseParserFun) = Parser resultatParserFun where
        resultatParserFun input@(Loc pos char : chars) | cond char = thenParserFun input
                                                       | otherwise = elseParserFun input

    {-
        Ein Parser der Form |parser1 <&> parser2| arbeitet zunächst wie |parser1| und anschließend
        wie |parser2|. Die Ausgabe ist das Paar aus den Ausgabewerten der Teilparser. Das Scheitern
        eines der Teilparser führt zum sofortigen Scheitern des gesamten Parsers mit derselben
        Fehlermeldung.
    -}
    infixr 4 <&>
    (<&>) :: Parser out1 -> Parser out2 -> Parser (out1,out2)
    (Parser trans1) <&> (Parser trans2) = Parser $ trans' where
      trans' stream = case trans1 stream of
                         Left locMsg            -> Left locMsg
                         Right (out1, stream')  -> case trans2 stream' of
                                                     Left locMsg           -> Left locMsg
                                                     Right (out2,stream'') -> Right $ ((out1, out2), stream'')

    {-
        Ein Parser der Form |parser =>> fun| arbeitet zunächst wie |parser|. Wird |parser|
        erfolgreich beendet, wird |fun| auf dessen Ausgabe angewendet und das Ergebnis als Ausgabe
        des gesamten Parsers geliefert. Scheitert |parser|, so scheitert der gesamte Parser mit
        derselben Fehlermeldung.
    -}
    infixl 3 =>>
    (=>>) :: Parser out -> (out -> out') -> Parser out'
    Parser trans =>> fun = Parser $ \stream -> case trans stream of
                                                   Left locMsg         -> Left locMsg
                                                   Right (out,stream') -> Right (fun out,stream')

    {-
        Ein Parser der Form |loc parser| arbeitet im Wesentlichen wie |parser|. Der einzige
        Unterschied ist, dass er neben der Ausgabe von |parser| noch die Quelltextposition ausgibt,
        die aktuell war, bevor |parser| arbeitete.
    --kontextcheck
    -}
    loc :: Parser out -> Parser (Loc out)
    loc (Parser fun) = Parser locFun where
        locFun stream@(x:xs) = case fun stream of  
                                Left locMsg          -> Left locMsg
                                Right (out, stream') -> Right ((Loc (pos x) out), stream')
    {-
        Liest das nächste Zeichen und gibt dieses aus, falls es sich dabei nicht um EOT handelt.
        Andernfalls scheitert dieser Parser.
    -}
    single :: Parser Char
    single = ifNext (== '\EOT') (failure "Unexpected end of file") uncheckedSingle
 
    uncheckedSingle :: Parser Char
    uncheckedSingle = Parser $ \(Loc _ char : toks) -> Right (char, toks)

    {-
        Liest die gegebene Zeichenfolge von der Eingabe, falls sie dort unmittelbar steht.
        Andernfalls scheitert der Parser. Die gegebene Zeichenfolge darf auch EOT-Zeichen enthalten.
    -}
    
    fixed :: String -> Parser ()
    fixed []     = success ()
    fixed (x:xs) = ifNext (==x) (uncheckedSingle <&> fixed xs =>> snd)
                                (failure $ "unexpected Char, was expecting '" ++ x:"'!")

    toLocStream :: String -> [Loc Char]
    toLocStream = concat                                                            .
                  zipWith (\lineNo -> zipWith Loc (map (Pos lineNo) [1 ..])) [1 ..] .
                  map (++ "\n")                                                     .
                  lines

    {-
        Ermittelt für einen gegebenen Parser und eine Eingabe das Resultat des entsprechenden
        Parsingvorgangs. Dieses ist entweder eine Fehlermeldung oder eine Parserausgabe. |run| sorgt
        dafür, dass der Parser EOT-Zeichen erhält, sobald die reguläre Eingabe aufgebraucht ist.
        Nach Abschluss des Parsens muss das nächste Zeichen der Eingabe ein EOT-Zeichen sein. Ist
        dies nicht der Fall, wird ein Fehler signalisiert.
    -}
    run :: Parser out -> String -> Either (Loc String) out
    run parser input = let

                           Parser trans = parser <&> fixed "\EOT" =>> fst

                       in case trans (toLocStream (input ++ repeat '\EOT')) of
                              Left locMsg   -> Left locMsg
                              Right (out,_) -> Right out
 
    main = putStrLn ""