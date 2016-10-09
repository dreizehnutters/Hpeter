module MOpal.Interpreter (

    runInt

) where

    import Prelude hiding (fromInteger, toInteger)

    import NYI
    import MOpal.CtxCheck
    import MOpal.AST
    import Util
    import MOpal.Parser
    import Parser
    import Data.Either

    {-
        Interpretiert das Programm. Das Resultat dieser Funktion stellt das Resultat das Programms
        dar, welches je nach Typ von MAIN entweder eine natürliche Zahl oder ein Wahrheitswert ist.
        Falls die Bedeutung des Programms _|_ (undefiniert) ist, bricht |run| mit einem
        Laufzeitfehler ab oder terminiert nicht.
    -}
    
    runInt :: String -> Either Int Bool
    runInt xs = case run progParser xs of 
                    Left _ -> error ""
                    Right prog -> run' prog
    
    run' :: Prog -> Either Int Bool 
    run' (Prog defs) = applyMain defs defs
    
    applyMain :: [Loc Def] -> [Loc Def] ->  Either Int Bool
    applyMain [] _ = error "No Main"
    applyMain (Loc _ (Def  (LeftMain _) expr):xs) defs = interpretExpr expr [] defs
    applyMain (Loc _ (Def (LeftIdent _ _ _) _):xs) defs = applyMain xs defs
    
    interpretExpr :: Expr -> [(String, Either Int Bool)] -> [Loc Def] -> Either Int Bool
    interpretExpr (Num i) _  _ = Left (val i)
    interpretExpr TRUE _   _  = Right True
    interpretExpr FALSE _   _ = Right False
    interpretExpr (Apply  var Nothing)  params  _ = case lookup (val var) params of 
                                                        Just val -> val
                                                        Nothing -> error ""
    interpretExpr (Apply f  (Just expressions) ) params defs  = applyFun (val f) (map (\expr -> interpretExpr (val expr) params defs) expressions) defs defs
    interpretExpr (Condition i t Nothing) params defs = case interpretExpr (val i) params defs of
                                                            Right True -> interpretExpr (val t) params defs
                                                            Right False -> error "No else"
    interpretExpr (Condition  i t (Just e)) params defs =  case interpretExpr (val i) params defs of
                                                            Right True -> interpretExpr (val t) params defs
                                                            Right False -> interpretExpr (val e) params defs
     
    applyFun :: String -> [Either Int Bool] -> [Loc Def] -> [Loc Def] -> Either Int Bool
    applyFun "add" values _ _    = Left $ foldl1 (+) (lefts values)
    applyFun "sub" values _ _    = Left $ foldl1 (-) (lefts values)
    applyFun "mul" values _ _    = Left $ foldl1 (*) (lefts values)
    applyFun "div" values _ _    = Left $ foldl1 (div) (lefts values)
    applyFun "and" values _ _    = Right $ and (rights values)
    applyFun "or" values _ _     = Right $ or (rights values)
    applyFun "not" [Right b] _ _ = Right (not b)
    applyFun "eq" [Left l, Left r] _ _ = Right (l == r)
    applyFun "lt" [Left l, Left r] _ _ = Right (l < r)
    
    
    applyFun _ _ []  _ = error "Function doesn't exist"
    applyFun f values (Loc _ (Def (LeftIdent ident params _) expr):xs) defs | (val ident) == f = interpretExpr expr (zipWith (\x y -> (val (fst y) , x)) values params) defs
                                                                            | otherwise = applyFun f values xs defs
    applyFun f values (Loc _ (Def (LeftMain  _) _ ): xs) defs = applyFun f values xs defs 
