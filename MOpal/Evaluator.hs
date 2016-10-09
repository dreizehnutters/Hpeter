module MOpal.Evaluator(

    eval

) where

    import MOpal.Interpreter
    import MOpal.Parser
    import Parser
    import Data.Char
    import Util

--TODO
eval :: String -> String
eval xs = case checkCtxConds ast of 
            nyi -> show (runInt ast) 
            otherwise -> show error 
            where ast = (run progParser xs)

