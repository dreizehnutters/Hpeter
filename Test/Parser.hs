{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module Parser (

    prop_ParserRunIsCorrect

) where

    import Test.QuickCheck hiding (Success, Failure)

    import Util
    import Parser

    -- * Parsers with textual representation
    data ShowableParser out = ShowableParser (Parser out) String

    generalFailureParser :: ShowableParser out
    generalFailureParser = ShowableParser (failure "") "failure \"\""

    failureParser :: ShowableParser ()
    failureParser = generalFailureParser

    successParser :: ShowableParser ()
    successParser = ShowableParser (success ()) "success ()"

    ifNextParser :: Bool -> Char -> ShowableParser out -> ShowableParser out
    ifNextParser equals char (ShowableParser parser text) = ShowableParser parser' text' where

        ShowableParser otherParser otherText = generalFailureParser
        {-
            Using generalFailureParser is problematic since the parser implementation could act
            specially when encountering an immediately failing parser.
        -}

        parser' = if equals then ifNext (== char) parser otherParser
                            else ifNext (/= char) otherParser parser

        text'   = "ifNext ("                           ++
                  (if equals then "==" else "/=")      ++
                  " "                                  ++
                  show char                            ++
                  ") ("                                ++
                  (if equals then text else otherText) ++
                  ") ("                                ++
                  (if equals then otherText else text) ++
                  ")"

    seqParser :: ShowableParser out1 -> ShowableParser out2 -> ShowableParser (out1,out2)
    seqParser (ShowableParser parser1 text1)
              (ShowableParser parser2 text2) = ShowableParser parser' text' where

        parser' = parser1 <&> parser2

        text'   = "(" ++ text1 ++ ") <&> (" ++ text2 ++ ")"

    mapParser :: ShowableParser out -> ShowableParser out
    mapParser (ShowableParser parser text) = ShowableParser parser' text' where

        parser' = parser =>> id

        text'   = "(" ++ text ++ ") =>> id"

    locParser :: ShowableParser out -> ShowableParser (Loc out)
    locParser (ShowableParser parser text) = ShowableParser parser' text' where

        parser' = loc parser

        text'   = "loc (" ++ text ++ ")"

    singleParser :: ShowableParser Char
    singleParser = ShowableParser single "single"

    fixedParser :: String -> ShowableParser ()
    fixedParser str = ShowableParser (fixed str) ("fixed " ++ show str)

    -- * Parser runs
    data ParserRun result = forall out. (Eq out, Show out) =>
                            ParserRun (ShowableParser out) String (result out)

    -- * Utilities
    first :: String -> Char -> Char
    first input follow = head (input ++ [follow])

    -- * Parser generators
    data AnyParser = forall out. (Eq out, Show out) => AnyParser (ShowableParser out)

    type ParserGen = Gen AnyParser

    parserGen :: ParserGen
    parserGen = oneof [
                          failureParserGen,
                          successParserGen,
                          ifNextParserGen,
                          seqParserGen,
                          mapParserGen,
                          locParserGen,
                          singleParserGen,
                          fixedParserGen
                      ]

    failureParserGen :: ParserGen
    failureParserGen = return $ AnyParser failureParser

    successParserGen :: ParserGen
    successParserGen = return $ AnyParser successParser

    ifNextParserGen :: ParserGen
    ifNextParserGen = do
                          equals <- arbitrary
                          char <- arbitrary
                          AnyParser parser <- parserGen
                          return $ AnyParser (ifNextParser equals char parser)

    seqParserGen :: ParserGen
    seqParserGen = do
                       AnyParser parser1 <- parserGen
                       AnyParser parser2 <- parserGen
                       return $ AnyParser (seqParser parser1 parser2)

    mapParserGen :: ParserGen
    mapParserGen = do
                       AnyParser parser <- parserGen
                       return $ AnyParser (mapParser parser)

    locParserGen :: ParserGen
    locParserGen = do
                       AnyParser parser <- parserGen
                       return $ AnyParser (locParser parser)

    singleParserGen :: ParserGen
    singleParserGen = return $ AnyParser singleParser

    fixedParserGen :: ParserGen
    fixedParserGen = do
                         str <- arbitrary
                         return $ AnyParser (fixedParser str)

    -- * Generators of lookahead-based parser runs
    type StdRunGen result = Char -> Gen (ParserRun result)

    -- ** Utilities
    oneStdRunGenOf :: [StdRunGen result] -> StdRunGen result
    oneStdRunGenOf stdRunGens follow = oneof (map ($ follow) stdRunGens)

    ifNextGen :: StdRunGen result -> StdRunGen result
    ifNextGen gen follow = do
                               equals <- arbitrary
                               ParserRun parser input result <- gen follow
                               let

                                   lookahead = first input follow

                               return $ ParserRun (ifNextParser equals lookahead parser)
                                                  input
                                                  result

    -- ** Successful runs
    newtype Success out = Success out

    type SuccessfulRunGen = StdRunGen Success

    successfulGen :: SuccessfulRunGen
    successfulGen = oneStdRunGenOf [
                                       successfulSuccessGen,
                                       successfulIfNextGen,
                                       successfulSeqGen,
                                       successfulMapGen,
                                       successfulSingleGen,
                                       successfulFixedGen
                                   ]

    successfulSuccessGen :: SuccessfulRunGen
    successfulSuccessGen _ = return $ ParserRun successParser "" (Success ())

    successfulIfNextGen :: SuccessfulRunGen
    successfulIfNextGen = ifNextGen successfulGen

    successfulSeqGen :: SuccessfulRunGen
    successfulSeqGen follow = do
                                  ParserRun parser2 input2 (Success out2) <- successfulGen follow
                                  let

                                      first2 = first input2 follow

                                  ParserRun parser1 input1 (Success out1) <- successfulGen first2
                                  return $ ParserRun (parser1 `seqParser` parser2)
                                                     (input1 ++ input2)
                                                     (Success (out1,out2))

    successfulMapGen :: SuccessfulRunGen
    successfulMapGen follow = do
                                  ParserRun parser input result <- successfulGen follow
                                  return $ ParserRun (mapParser parser) input result

    successfulSingleGen :: SuccessfulRunGen
    successfulSingleGen _ = do
                                char <- arbitrary `suchThat` (/= '\EOT')
                                return $ ParserRun singleParser [char] (Success char)

    successfulFixedGen :: SuccessfulRunGen
    successfulFixedGen _ = do
                               str <- arbitrary
                               return $ ParserRun (fixedParser str) str (Success ())

    -- ** Failing runs
    data Failure out = Failure

    type FailingRunGen = StdRunGen Failure

    failingGen :: FailingRunGen
    failingGen follow = if follow == '\EOT'
                            then oneof [withoutSingleGen '\EOT',singleGen]
                            else withoutSingleGen follow where

        withoutSingleGen = oneStdRunGenOf [
                                              failingFailureGen,
                                              failingIfNextGen,
                                              failingSeqGen,
                                              failingMapGen,
                                              failingFixedGen
                                          ]

        singleGen        = return $ ParserRun singleParser "" Failure

    failingFailureGen :: FailingRunGen
    failingFailureGen _ = return $ ParserRun failureParser "" Failure

    failingIfNextGen :: FailingRunGen
    failingIfNextGen = ifNextGen failingGen

    failingSeqGen :: FailingRunGen
    failingSeqGen follow = do
                               firstFails <- arbitrary
                               if firstFails
                                   then do
                                            ParserRun parser1 input1 _ <- failingGen follow
                                            AnyParser parser2 <- parserGen
                                            return $ ParserRun (parser1 `seqParser` parser2)
                                                               input1
                                                               Failure
                                   else do
                                            ParserRun parser2 input2 _ <- failingGen follow
                                            let

                                                first2 = first input2 follow

                                            ParserRun parser1 input1 _ <- successfulGen first2
                                            return $ ParserRun (parser1 `seqParser` parser2)
                                                               (input1 ++ input2)
                                                               Failure

    failingMapGen :: FailingRunGen
    failingMapGen follow = do
                               ParserRun parser input _ <- failingGen follow
                               return $ ParserRun (mapParser parser) input Failure

    failingFixedGen :: FailingRunGen
    failingFixedGen follow = do
                                 okStr <- arbitrary
                                 postStr <- arbitrary `suchThat` \str -> not (null str)     &&
                                                                         head str /= follow
                                 return $ ParserRun (fixedParser (okStr ++ postStr)) okStr Failure

    -- * General parser runs
    type GeneralRun = ParserRun (Either Pos)

    instance Arbitrary GeneralRun where

        arbitrary = generalGen

        shrink    = const []

    instance Show GeneralRun where

        show (ParserRun (ShowableParser _ parserText) input result) = text' where

            text'      = "run (" ++ parserText ++ ") " ++ show input ++ " == " ++ resultText

            resultText = case result of
                             Left pos -> "Left (Loc (" ++ show pos ++ ") _)"
                             rightOut -> show rightOut

    -- * General generators
    type GeneralGen = Gen GeneralRun

    generalGen :: GeneralGen
    generalGen = oneof [successfulGeneralGen,failingGeneralGen,locGeneralGen]
    {-NOTE:
        For successful parser runs, we don’t check whether then actual parser reads the complete
        input. Maybe it isn’t necessary to check this.

        In addition, we rely on the fact that our failing runs aren’t intended to fail because
        there is input left, but because an expected character was not found in the input stream.
        If they should fail because there is input left, we would have to make sure that we don’t
        run them with an additional input that starts with EOT, since then the actual parser would
        think that it reached the end of input.
    -}

    successfulGeneralGen :: GeneralGen
    successfulGeneralGen = do
                               ParserRun parser input (Success out) <- successfulGen '\EOT'
                               return $ ParserRun parser input (Right out)

    failingGeneralGen :: GeneralGen
    failingGeneralGen = do
                            post <- arbitrary `suchThat` (not . null)
                            ParserRun parser input _ <- failingGen (head post)
                            return $ ParserRun parser (input ++ post) (Left (posAfter input))

    locGeneralGen :: GeneralGen
    locGeneralGen = do
                        pre <- arbitrary
                        char <- arbitrary `suchThat` (/= '\EOT')
                        post <- arbitrary
                        return $ ParserRun (seqParser (fixedParser pre)
                                                      (seqParser (locParser singleParser)
                                                                 (fixedParser post)))
                                           (pre ++ [char] ++ post)
                                           (Right ((),(Loc (posAfter pre) char,())))

    posAfter :: String -> Pos
    posAfter = foldl nextPos (Pos 1 1) where

        nextPos (Pos lineNo _)     '\n' = Pos (succ lineNo) 1
        nextPos (Pos lineNo colNo) _    = Pos lineNo (succ colNo)

    -- * Correctness property
    prop_ParserRunIsCorrect :: GeneralRun -> Bool
    prop_ParserRunIsCorrect (ParserRun (ShowableParser parser _) input result) = isCorrect where

        isCorrect = case run parser input of
                        Left  (Loc pos _) -> result == Left pos
                        Right out         -> result == Right out
