{-
    Ein Compiler für μOpal-Programme. Da das Modul Main (statt MOpal.Compiler) heißt und es eine
    Variable main enthält, kann daraus eine ausführbare Datei mittels

        ghc --make MOpal/Compiler.hs

    erzeugt werden. Diese erhält das zu compilierende Programm über die Standardeingabe.
-}
module Main (

    main

) where

    import NYI
    import Util
    import Parser
    import MOpal.AST
    import MOpal.Parser
    import MOpal.CtxCheck
    import MOM.Code

    -- Hauptprogramm
    {-
        Das Hauptprogramm des Compilers.
    -}
    main :: IO ()
    main = do
               input <- getContents
               case run program input of
                   Left  (Loc pos msg) -> putStr $ "parsing failed:\n\n" ++
                                                   msg                   ++
                                                   " at line "           ++
                                                   show (lineNo pos)     ++
                                                   ", column "           ++
                                                   show (colNo pos)
		   Right (Loc _ prog)  -> case checkCtxConds prog of
                                              Nothing  -> putStr $ toString (compile prog)
                                              Just msg -> putStr $ "context check failed:\n\n" ++ msg

    compile :: Program -> Code
    compile = nyi
