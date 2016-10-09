module ArithmeticParser where

import Parser
import NYI
import Util
import qualified Data.Map as Map
import Data.Char
import Data.Maybe (fromJust)
--import Map.lookup

{-
E -> E + T | T
T -> T * F | F
F -> (E) | id


--
S -> D ; E
D -> id = Num D'
D'-> eps | , D

E -> TE'
E'-> eps | + TE'
T -> FT'
T'-> eps | * FT'
F -> (E) | id
--

"a=5, b=4, c=3; a + b * c"
"a=5, b=4, c=3; (a + (b + c) + d)"
"a=5, b=4, c=3; ((a + b) + c) + d"





-}
--data Defs = Defs [Def]
--data Def  = ... 


data AExpr = Var (Loc String)
           | Sum (Loc AExpr) (Loc AExpr)
           | Prod (Loc AExpr) (Loc AExpr)
           deriving (Show)


arithmeticParser :: Parser(Map.Map String Int, Loc AExpr)
arithmeticParser = whiteSpace <&> dParser <&> token (fixed ";") <&> eParser =>> 
                        \ (_, (assigment, (_, aExpr))) -> (assigment, aExpr)


-- Lesen von einer Ziffer
digit :: Parser Int
digit = ifNext isDigit (single =>> \char -> ord char - ord '0')
                       (failure "Digit expected")


digits :: Parser [Int]
digits = digit <&> ifNext isDigit digits (success []) =>> uncurry (:)  {-\(dig, digs) -> dig : digs -}

-- Liest eine nat Zahl
nat :: Parser Int 
nat = digits =>> foldl ((+) . (*10)) 0

-- XYSZ () unit
whiteSpace :: Parser ()
whiteSpace = ifNext isSpace (single <&> whiteSpace =>> const ())
                            (success ())



token :: Parser out -> Parser out
token parser = parser <&> whiteSpace =>> fst 


alphaChar :: Parser Char
alphaChar = ifNext isAlpha (single)
                           (failure "alpha char expected")


alphaString :: Parser (Loc String)
alphaString = loc alphaString'



alphaString' :: Parser String
alphaString' = alphaChar <&> ifNext isAlpha alphaString' (success "") =>> uncurry (:)


dParser :: Parser (Map.Map String Int)
dParser = def <&> dParser' =>>  Map.fromList . uncurry (:)

--def == D in der grammantik
--d ist lifting map dings

dParser' :: Parser [(String, Int)]
dParser' = ifNext (== ',') (token single <&> def <&> dParser' =>> uncurry (:) . snd)
                           (success [])


def :: Parser (String, Int)
def = token alphaString' <&> token (fixed "=") <&> token nat =>>
        \ (name, (_, num)) -> (name, num)

fParser :: Parser (Loc AExpr)
-- fst . snd ('(', (e,")"))
{--<&> ist rechtsasso --}
fParser = ifNext (== '(') (token single <&> eParser <&> fixed ")" =>> fst . snd) --'(' E ')'
                          (loc (token alphaString =>> Var)) --id

{-- 
(AEXPR, [AEXPR])
(a, [b,c]) --> a*b*c

--}

tParser :: Parser (Loc AExpr)
tParser = fParser <&> tParser' =>> uncurry (foldl (\x y -> Loc (pos x) (Prod x y))) 


tParser' :: Parser [Loc AExpr]
tParser' = ifNext (== '*') (token single <&> (fParser <&> tParser') =>> uncurry (:) . snd) --('*', (f, [f]))
                           (success []) --eps



eParser :: Parser (Loc AExpr)
eParser = tParser <&> eParser' =>> uncurry (foldl (\x y -> Loc (pos x) (Sum x y)))



eParser' :: Parser [Loc AExpr]
eParser' = ifNext (== '+') (token single <&> (tParser <&> eParser') =>> uncurry (:) . snd) --('*', (f, [f]))
                           (success []) --eps


 
a = "a + b * c"
b = "(a + (b + c) + d)"
c = "((a + b) + c) + d"
x = "a=5, b=4, c=3; a * b + c"
y = "a=5, b=4, c=3; a * (b + c)"
z = "a=5, b=4, c=3; ((a + b) *  c)"



evaluate :: String -> Int
evaluate input = let
   -- Right (assigment, (Loc _ aExpr)) = run arithmeticParser input
                 in case run arithmeticParser input of
                    Right (assigment, (Loc _ aExpr)) -> evaluate' aExpr assigment
                    Left locMsg                      -> error "ERROR"
                     --evaluate' aExpr assigment


evaluate' :: AExpr -> Map.Map String Int -> Int
evaluate' (Var (Loc _ x)) a = fromJust $ (Map.lookup x a)
evaluate' (Sum (Loc _ e1) (Loc _ e2))    a = evaluate' e1 a + evaluate' e2 a
evaluate' (Prod (Loc _ e1) (Loc _ e2))    a = evaluate' e1 a * evaluate' e2 a




main = putStrLn "build complete"