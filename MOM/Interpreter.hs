{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-
    Ein Interpreter für μOM-Programme. Da das Modul Main (statt MOM.Interpreter) heißt und es eine
    Variable main enthält, kann daraus eine ausführbare Datei mittels

        ghc --make MOM/Interpreter.hs

    erzeugt werden. Diese erhält das zu interpretierende Programm über die Standardeingabe.
-}
module Main (

    main,

) where

    import           Data.Natural

    import           Control.Monad.State.Strict hiding (State)
    import qualified Control.Monad.State.Strict
    import           Control.Monad.Error

    import           MOM.Code

    -- Hauptprogramm
    {-
        Das Hauptprogramm des Interpreters.
    -}
    main :: IO ()
    main = do
               input <- getContents
               let

                   (maybeAbortMsg,state) = run (fromString input)

                   msg                   = case maybeAbortMsg of
                                               Nothing       -> "finished successfully"
                                               Just abortMsg -> "aborted: " ++ abortMsg

                   pcOutput              = "program counter: " ++ show (pc state)

                   errorStateOutput      = "error flag is "     ++
                                           (if errorState state
                                                then ""
                                                else "not ")    ++
                                           "set"
                   stackOutput           = "stack: " :
                                           map (("    " ++) . either show show) (stack state)

                   frameOutput           = "frames: " :
                                           map (("    " ++) . frameToString) (frames state)

                   outputLines           = msg              :  "" :
                                           pcOutput         :  "" :
                                           errorStateOutput :  "" :
                                           stackOutput      ++ "" :
                                           frameOutput

               putStr (unlines outputLines)

    -- State

    type StackElem = Either Natural [Natural]

    data State = State { pc :: Int, errorState :: Bool, stack :: [StackElem], frames :: [Frame] }

    data Frame = Frame { retAddr :: Int, params :: [StackElem] }

    frameToString :: Frame -> String
    frameToString (Frame retAddr params) = "return address: " ++ show retAddr ++
                                           "    parameters: " ++ show params

    -- Executions
    type Exec = Control.Monad.State.Strict.State State

    type OpExec = ErrorT String Exec

    class (Functor execution, Monad execution) => Execution execution where

        getState :: execution State

        setState :: State -> execution ()

    instance Execution Exec where

        getState = get

        setState = put

    instance Execution OpExec where

        getState = lift getState

        setState = lift . setState

    getPC :: (Execution execution) => execution Int
    getPC = fmap pc getState

    setPC :: (Execution execution) => Int -> execution ()
    setPC pc = do
                   state <- getState
                   setState (state { pc = pc })

    getErrorState :: (Execution execution) => execution Bool
    getErrorState = fmap errorState getState

    setErrorState :: (Execution execution) => Bool -> execution ()
    setErrorState errorState = do
                                   state <- getState
                                   setState (state { errorState = errorState })

    getStack :: (Execution execution) => execution [StackElem]
    getStack = fmap stack getState

    setStack :: (Execution execution) => [StackElem] -> execution ()
    setStack stack = do
                         state <- getState
                         setState (state { stack = stack })

    getFrames :: (Execution execution) => execution [Frame]
    getFrames = fmap frames getState

    setFrames :: (Execution execution) => [Frame] -> execution ()
    setFrames frames = do
                           state <- getState
                           setState (state { frames = frames })

    -- Complete runs
    run :: Code -> (Maybe String,State)
    run code = runState (execCode code) initState

    initState :: State
    initState = State { pc = 0, errorState = False, stack = [], frames = [] }

    execCode :: Code -> Exec (Maybe String)
    execCode code = do
                        pc <- getPC
                        if 0 <= pc && pc < length code
                            then do
                                     preStepState <- getState
                                     let

                                         op = code !! pc

                                     setPC (succ pc)
                                     stepResult <- runErrorT (step op)
                                     case stepResult of
                                         Left abortMsg -> do
                                                              setState preStepState
                                                              return (Just abortMsg)
                                         Right True    -> do
                                                              setState preStepState
                                                              return Nothing
                                         Right False   -> execCode code
                            else return (Just "PC out of bounds")

    -- Steps
    continue :: OpExec Bool
    continue = return False

    quit :: OpExec Bool
    quit = return True

    abort :: String -> OpExec out
    abort = fail

    pop :: OpExec StackElem
    pop = do
              stack <- getStack
              case stack of
                  []           -> abort "stack exhausted"
                  elem : elems -> do
                                      setStack elems
                                      return elem

    popNatural :: OpExec Natural
    popNatural = do
                     elem <- pop
                     case elem of
                         Left natural -> return natural
                         _            -> abort "wrong operand type on top of stack"

    popNaturalList :: OpExec [Natural]
    popNaturalList = do
                         elem <- pop
                         case elem of
                             Right naturals -> return naturals
                             _              -> abort "wrong operand type on top of stack"

    push :: StackElem -> OpExec ()
    push elem = do
                    stack <- getStack
                    setStack (elem : stack)

    pushNatural :: Natural -> OpExec ()
    pushNatural = push . Left

    pushNaturalList :: [Natural] -> OpExec ()
    pushNaturalList = push . Right

    checkCond :: Cond -> OpExec Bool
    checkCond Always   = return True
    checkCond Empty    = do
                             naturals <- popNaturalList
                             return (null naturals)
    checkCond NonEmpty = do
                             naturals <- popNaturalList
                             return (not . null $ naturals)
    checkCond Zero     = do
                             natural <- popNatural
                             return (natural == 0)
    checkCond NonZero  = do
                             natural <- popNatural
                             return (natural /= 0)
    checkCond Equal    = do
                             natural2 <- popNatural
                             natural1 <- popNatural
                             return (natural1 == natural2)
    checkCond LessThan = do
                             natural2 <- popNatural
                             natural1 <- popNatural
                             return (natural1 < natural2)
    checkCond Error    = getErrorState

    step :: Op -> OpExec Bool
    step (LDPAR idx)            = do
                                      frames <- getFrames
                                      case frames of
                                          []                 -> abort "not in a function"
                                          Frame _ params : _ -> if idx < toEnum (length params)
                                                                    then do
                                                                             push (params !! fromEnum idx)
                                                                             continue
                                                                    else abort "index out of bounds"
    step (LDNAT natural)        = do
                                      pushNatural natural
                                      continue
    step LDNIL                  = do
                                      pushNaturalList []
                                      continue
    step ADD                    = do
                                      natural2 <- popNatural
                                      natural1 <- popNatural
                                      pushNatural (natural1 + natural2)
                                      continue
    step SUB                    = do
                                      setErrorState False
                                      natural2 <- popNatural
                                      natural1 <- popNatural
                                      if natural1 >= natural2
                                          then pushNatural (natural1 - natural2)
                                          else setErrorState True
                                      continue
    step MUL                    = do
                                      natural2 <- popNatural
                                      natural1 <- popNatural
                                      pushNatural (natural1 * natural2)
                                      continue
    step DIV                    = do
                                      setErrorState False
                                      natural2 <- popNatural
                                      natural1 <- popNatural
                                      if natural2 == 0
                                          then setErrorState True
                                          else pushNatural (natural1 `div` natural2)
                                      continue
    step CONS                   = do
                                      naturals <- popNaturalList
                                      natural  <- popNatural
                                      pushNaturalList (natural : naturals)
                                      continue
    step HEAD                   = do
                                      setErrorState False
                                      list <- popNaturalList
                                      if null list
                                          then setErrorState True
                                          else pushNatural (head list)
                                      continue
    step TAIL                   = do
                                      setErrorState False
                                      list <- popNaturalList
                                      if null list
                                          then setErrorState True
                                          else pushNaturalList (tail list)
                                      continue
    step SWAP                   = do
                                      elem2 <- pop
                                      elem1 <- pop
                                      push elem2
                                      push elem1
                                      continue
    step ABORT                  = abort "aborted by request"
    step (CALL paramCnt dst)    = do
                                      pc <- getPC
                                      revParams <- replicateM (fromEnum paramCnt) pop
                                          -- scheitert wegen toInt bei sehr großer Parameteranzahl
                                      frames <- getFrames
                                      setFrames (Frame pc (reverse revParams) : frames)
                                      setPC (fromEnum dst)
                                          -- scheitert wegen toInt bei sehr großer Zieladresse
                                      continue
    step (ICALL paramCnt)       = do
                                      dst <- popNatural
                                      step (CALL paramCnt dst)
    step RET                    = do
                                      frames <- getFrames
                                      case frames of
                                          []                  -> quit
                                          Frame pc _ : frames -> do
                                                                     setPC pc
                                                                     setFrames frames
                                                                     continue
    step (BRANCH cond offset)   = do
                                      fulfilled <- checkCond cond
                                      when fulfilled (do
                                                          pc <- getPC
                                                          setPC (pc + fromInteger offset))
                                          -- scheitert bei sehr großer Zieladresse
                                      continue
