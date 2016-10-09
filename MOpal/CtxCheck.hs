module MOpal.CtxCheck (

    checkCtxConds,
    runCheck

) where

    import NYI
    import Util
    import MOpal.Parser
    import MOpal.AST
    import Parser
    import Data.Char
    
    {-
        Führt eine Kontextprüfung durch. Wenn keine Fehler fest gestellt wurden, ist das Resultat
        |Nothing|. Wurde ein Fehler gefunden, ist das Ergebnis |Just msg|, wobei |msg| die
        entsprechende Fehlermeldung ist.
    -}

    checkCtxConds :: Prog -> Maybe String
    checkCtxConds xs = nyi


    --vordefinierte Funktionen und ihre Typen
    predefined = [("add", [NAT,NAT,NAT]  ), ("sub", [NAT,NAT,NAT]) , 
                  ("mul", [NAT,NAT,NAT]) , ("div",[NAT,NAT,NAT]), 
                  ("and", [BOOL,BOOL,BOOL]), ("or", [BOOL,BOOL,BOOL]), 
                  ("not",[BOOL,BOOL,BOOL]), ("eq",[NAT,NAT,BOOL]) , 
                  ("lt", [NAT,NAT,BOOL])]
    

    --Überprüfen aller Bedingungen
    runCheck :: String -> Maybe [String]
    runCheck xs = case run progParser xs of
                     Left locMsg -> Just [val locMsg]
                     Right (Prog defs) ->  message defs 
    
    
    message:: [Loc Def] -> Maybe [String]
    message defs = case (singleMainChecker defs ++ multipleDeclarationChecker defs ++ multipleParameterChecker defs ++
                                                        parametersDeclaredChecker defs ++ typeChecker defs defs ++ thenElseChecker defs defs ++ applyChecker defs defs) of
                                                        [] -> Nothing
                                                        e -> Just e
    
    
    --Fehlermeldung mit Positionsangabe
    errorMessage :: Pos -> String-> String
    errorMessage (Pos lineNo colNo) xs = "Error " ++ (show lineNo) ++ ":" ++ (show colNo) ++ " : " ++ xs
    
    --Überprüfung auf einzelne Main
    singleMainChecker :: [Loc Def] -> [String]
    singleMainChecker defs = singleMainChecker' defs 0 []
    
    singleMainChecker' :: [Loc Def] -> Int -> [String] -> [String]
    singleMainChecker' [] 1 _ = []
    singleMainChecker' [] 0 xs = ["No Main"]
    singleMainChecker'  [] _ xs = (tail xs)
    singleMainChecker' (Loc pos (Def ((LeftMain _) ) _):xs) i ys     = singleMainChecker' xs (i+1) (ys ++ [(errorMessage pos "Main has already been declared" )])  
    singleMainChecker'  (x:xs)                               i ys           = singleMainChecker' xs i ys
    
    --Überprüfen, ob Funktionen mehrfach deklariert wurden
    multipleDeclarationChecker :: [Loc Def] -> [String]
    multipleDeclarationChecker defs = multipleDeclarationChecker' defs (map fst predefined) [] 
    
    multipleDeclarationChecker' :: [Loc Def] -> [String] -> [String] -> [String]
    multipleDeclarationChecker' [] _ [] = []
    multipleDeclarationChecker' [] _ xs = xs
    multipleDeclarationChecker' (Loc _ (Def ((LeftIdent (Loc pos ident) _ _ ) ) _):xs) declared ys | elem ident declared = multipleDeclarationChecker' xs declared (ys
                                                                                                                            ++ [(errorMessage pos (ident ++ " has already been declared")  )])
                                                                                                   | otherwise           = multipleDeclarationChecker' xs (ident:declared) ys
    multipleDeclarationChecker' (x:xs)                                                 declared ys = multipleDeclarationChecker' xs declared ys
    
    
    --überprüfen, ob Parameter mehrmals deklariert wurden
    multipleParameterChecker :: [Loc Def] -> [String]
    multipleParameterChecker defs = multipleParameterChecker' defs []
    
    multipleParameterChecker' :: [Loc Def] -> [String] -> [String] 
    multipleParameterChecker' [] [] = []
    multipleParameterChecker' [] xs = xs
    multipleParameterChecker'  (Loc _ (Def ((LeftIdent _ params _ ) ) _):xs) ys = case multipleParameterCheckerSingle params of 
                                                                                    Nothing       -> multipleParameterChecker' xs ys
                                                                                    Just messages -> multipleParameterChecker' xs (ys ++ messages)
    multipleParameterChecker' (x:xs)                                        ys = multipleParameterChecker' xs ys
    
    multipleParameterCheckerSingle :: [(Loc String, Type)] -> Maybe [String]
    multipleParameterCheckerSingle params = multipleParameterCheckerSingle' params [] []
    
    multipleParameterCheckerSingle' :: [(Loc String, Type)] -> [String] -> [String] -> Maybe [String]
    multipleParameterCheckerSingle' [] _ [] = Nothing
    multipleParameterCheckerSingle' [] _ ys = Just ys
    multipleParameterCheckerSingle' ( (Loc pos param, _) :xs) declared ys | elem param declared = multipleParameterCheckerSingle' xs declared (ys
                                                                                                                            ++ [(errorMessage pos ("Parameter " ++ param 
                                                                                                                                                    ++ " has already been declared")  )])
                                                                          | otherwise = multipleParameterCheckerSingle' xs (param:declared) ys
    multipleParameterCheckerSingle' (x:xs) declared ys = multipleParameterCheckerSingle' xs declared ys
    
    --überprüft, ob verwendete Parameter deklariert wurden
    parametersDeclaredChecker :: [Loc Def] -> [String]
    parametersDeclaredChecker defs = parametersDeclaredChecker' defs (map fst ((declaredFunctions defs) ++ predefined )) []
        
    parametersDeclaredChecker' :: [Loc Def] -> [String] -> [String] ->[String]
    parametersDeclaredChecker' [] _ [] = []
    parametersDeclaredChecker' [] _ ys = ys
    parametersDeclaredChecker' (Loc _ def@(Def _ expr):xs) declared ys = parametersDeclaredChecker' xs declared (ys ++ (parametersDeclaredCheckerSingle (declaredParameters def) declared expr))
    
    parametersDeclaredCheckerSingle :: [String] -> [String] -> Expr -> [String]
    parametersDeclaredCheckerSingle _ _ (Num _) = []
    parametersDeclaredCheckerSingle _ _ TRUE = []
    parametersDeclaredCheckerSingle _ _ FALSE = []
    parametersDeclaredCheckerSingle declaredParams funcs (Apply (Loc pos param) Nothing) | elem param declaredParams = []
                                                                                 | otherwise         = [errorMessage pos ("Parameter " ++ param ++ " was not declared in this scope")]
    parametersDeclaredCheckerSingle declaredParams  funcs (Apply (Loc pos func) (Just params )) | elem func funcs = concat (map (\expr -> parametersDeclaredCheckerSingle declaredParams funcs (val expr)) params)
                                                                                                | otherwise       = [errorMessage pos ("Function " ++ func ++ " was not declared") ]
    parametersDeclaredCheckerSingle declaredParams funcs (Condition x y (Just z)) = concat (map (\expr -> parametersDeclaredCheckerSingle declaredParams funcs (val expr)) [x,y,z])
    parametersDeclaredCheckerSingle declaredParams funcs (Condition x y Nothing) = concat (map (\expr -> parametersDeclaredCheckerSingle declaredParams funcs (val expr)) [x,y])
                                                                           
    
    --erzeugt Liste der deklarierten Parameter einer Definition
    declaredParameters :: Def -> [String]
    declaredParameters (Def ((LeftMain _) ) _) = []
    declaredParameters (Def ((LeftIdent _ params _ ) ) _) = declaredParameters' params
    
    declaredParameters' :: [(Loc String, Type)] -> [String]
    declaredParameters' [] = []
    declaredParameters' ( (param, _) :xs) = (val param): (declaredParameters' xs) 

    --erzeugt Liste der Typen von Parametern
    declaredParameterTypes :: Def -> [(String, Type)]
    declaredParameterTpes (Def ((LeftMain _) ) _) = []
    declaredParameterTypes (Def ((LeftIdent _ params _ ) ) _) = declaredParameterTypes' params
    
    declaredParameterTypes' :: [(Loc String, Type)] -> [(String, Type)]
    declaredParameterTypes' [] = []
    declaredParameterTypes' ( (param , t) :xs) = ((val param), t ) : (declaredParameterTypes' xs)
    
    --erzeugt Liste der deklarier Funktionen und ihrer Typen
    declaredFunctions :: [Loc Def] ->[(String, [Type]) ]
    declaredFunctions [] = []
    declaredFunctions (Loc _ (Def ((LeftIdent (Loc pos ident) params t ) ) _):xs) = (ident,((map snd params) ++ [(val t)])):declaredFunctions xs 
    declaredFunctions (x:xs) = declaredFunctions xs
    
    --überprüft Typkorrektheit
    typeChecker :: [Loc Def] -> [Loc Def] -> [String]
    typeChecker [] _ = []
    typeChecker (def:xs) defs = case (typeChecker' (val def) defs) of 
                                    [] -> typeChecker xs defs
                                    ys -> ys:typeChecker xs defs
    typeChecker' :: Def -> [Loc Def] -> String
    typeChecker' (Def (LeftMain t) expr) defs | t1 == (val t) || t1 == UNDEFINED = []
                                              |  otherwise      = errorMessage (pos t) ("Couldn't match expected type " 
                                                                                                ++ (show (val t)) ++ " with actual type " ++ (show t1))
                                                            where 
                                                               t1 = typeChecker'' defs [] expr
    typeChecker' (Def (LeftIdent _ params t) expr) defs |t1 == (val t) || t1 == UNDEFINED  = []
                                                        |otherwise      = errorMessage (pos t) ("Couldn't match expected type " 
                                                                                                ++ (show (val t)) ++ " with actual type " ++ (show t1))
                                                               where 
    
                                                           t1 = typeChecker'' defs params expr
    -- überprüft einzelnen Ausdruck auf seinen Typen
    typeChecker'' :: [Loc Def] -> [(Loc String, Type)] -> Expr -> Type
    typeChecker'' _ _ TRUE = BOOL
    typeChecker'' _ _ FALSE = BOOL
    typeChecker'' _ _ (Num _) = NAT
    typeChecker'' _ params (Apply var Nothing) = case lookup (val var) (map (\(lstr, t) -> ((val lstr),t)  ) params) of
                                                    Nothing -> UNDEFINED
                                                    Just t1 -> t1
    
    typeChecker'' defs params (Apply var (Just _)) = lookupType (map val defs) (val var)
    typeChecker'' defs params (Condition _ lexpr _) = typeChecker'' defs params (val lexpr)
    
    --ermittelt Typ der gesuchten Funktion
    lookupType :: [Def] -> String -> Type
    lookupType [] _ = UNDEFINED
    lookupType ((Def (LeftMain _) _) :defs) f = case lookup f predefined of
                                                      Nothing -> lookupType defs f
                                                      Just t -> last t
    lookupType ((Def (LeftIdent lf _ t) _) :defs) f | (val lf) == f =  case lookup f predefined of 
                                                                           Nothing -> (val t)
                                                                           Just t -> last t
                                                    | otherwise = case lookup f predefined of 
                                                                           Nothing -> lookupType defs f
                                                                           Just t -> last t
    --überprüft auf Korrektheit der if then else Ausdrücke
    thenElseChecker:: [Loc Def] -> [Loc Def] -> [String]
    thenElseChecker [] _ = []
    thenElseChecker ((Loc p (Def (LeftMain _) expr) ):xs)  defs           = (thenElseChecker' expr [] defs) ++ (thenElseChecker xs defs)
    thenElseChecker ((Loc p (Def (LeftIdent _ params _) expr) ):xs)  defs = (thenElseChecker' expr params defs) ++ (thenElseChecker xs defs)
    
    thenElseChecker' :: Expr -> [(Loc String, Type)] -> [Loc Def] -> [String]
    thenElseChecker' (Condition i t Nothing) params defs | typeChecker'' defs params (val i) /= BOOL = 
                                                         (errorMessage (pos i) "BOOL expected"):(thenElseChecker' (val t) params defs) ++ 
                                                            (thenElseChecker' (val i) params defs )
                                                    |otherwise = thenElseChecker' (val t) params defs ++ (thenElseChecker' (val i) params defs )
    thenElseChecker' (Condition i t  (Just e)) params defs | typeChecker'' defs params (val i) /= BOOL && 
                                                         (typeChecker'' defs params (val t)) /= (typeChecker'' defs params (val e))
                                                         = (errorMessage (pos i) "BOOL expected"):((errorMessage (pos t) "THEN and ELSE have to be of same type" )
                                                             :( (thenElseChecker' (val t) params defs ) ++ (thenElseChecker' (val e) params defs))) ++ 
                                                            (thenElseChecker' (val i) params defs )
                                                      | typeChecker'' defs params (val i) /= BOOL = 
                                                         (errorMessage (pos i) "BOOL expected"):((thenElseChecker' (val t) params defs) 
                                                         ++ (thenElseChecker' (val e) params defs)) ++ 
                                                            (thenElseChecker' (val i) params defs ) 
                                                      | (typeChecker'' defs params (val t)) /= (typeChecker'' defs params (val e))
                                                         = ((errorMessage (pos t) "THEN and ELSE have to be of same type" )
                                                             :( (thenElseChecker' (val t) params defs ) ++ (thenElseChecker' (val e) params defs))) ++ 
                                                            (thenElseChecker' (val i) params defs )
                                                      | otherwise = (thenElseChecker' (val t) params defs ) ++ (thenElseChecker' (val e) params defs) ++ 
                                                            (thenElseChecker' (val i) params defs )
    thenElseChecker' _ _ _ = []
    
    
    --überprüft, ob Funktionen mit richtigen Parametern aufgerufen werden
    applyChecker:: [Loc Def] -> [Loc Def] -> [String]
    applyChecker [] _ = []
    applyChecker ((Loc _ (Def (LeftIdent _ params _ ) expr)):xs) defs = (applyChecker' expr defs params) ++ applyChecker xs defs
    applyChecker ((Loc _ (Def (LeftMain _ ) expr)):xs) defs = (applyChecker' expr defs [] ) ++ applyChecker xs defs
    
    applyChecker':: Expr -> [Loc Def] -> [(Loc String, Type)] -> [String]
    applyChecker' (Num _) _ _ = []
    applyChecker' TRUE _ _ = []
    applyChecker' FALSE _ _ = []
    applyChecker' (Apply _ Nothing) _ _ = []
    applyChecker' (Apply f (Just pars)) defs params = case lookup (val f)  (predefined ++ declaredFunctions defs)  of
                                                            Nothing -> []
                                                            Just par -> if (map (\p -> typeChecker'' defs params (val p) ) pars) == (init par) 
                                                                            then concat (map  (\p ->  applyChecker' (val p) defs params) pars)  else
                                                                            [errorMessage (pos f) "Function " ++ (val f) ++ show (init par) ++ 
                                                                                        "is not applicable for the arguments " ++ show (map (\p -> typeChecker'' defs params (val p)) pars ) ]
    applyChecker' (Condition i t Nothing) defs params = (applyChecker' (val i) defs params) ++ (applyChecker' (val t) defs params)
    applyChecker' (Condition i t (Just e) ) defs params = (applyChecker' (val i) defs params) ++ (applyChecker' (val t) defs params) ++ (applyChecker' (val e) defs params)
    applyChecker' Bottom _ _ = []
    
    