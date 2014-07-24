-- | Parser for the EPD notation
--
-- -- https://chessprogramming.wikispaces.com/Extended+Position+Description
module Test.Puzzle.EPD
       ( EPD(..)
       , Operation(..)
       , epdParser
       ) where

import Control.Monad (liftM, void)
import Data.Char (ord)

import Text.ParserCombinators.Parsec

import Chess.Board
import Chess.Move


------------------------------------------------------------------------------
-- EPD operations with the position
data EPD = EPD Board [ Operation ]

------------------------------------------------------------------------------
-- An EPD operation
data Operation = AnalysisCountNodes           
               | AnalysisCountSeconds
               | AvoidMove [ Move ]
               | BestMove [ Move ]
               | Comment Int String
               | Evaluation Int
               | MateFullMoveCount Int
               | AcceptDraw
               | ClaimDraw
               | OfferDraw
               | RejectDraw
               | ECO String
               | FullMoveNumber Int
               | HalfMoveClock Int
               | Id String
               | NewInChess String
               | Nop
               | PredictedMove Move
               | PredictedVariation [ Move ]
               | RepetitionCount Int
               | Resign
               | SuppliedMove Move
               | TelecomGameSelector String
               | TelecomReceiverId String
               | TelecomSenderId String
               | VariationName Int String deriving Show


------------------------------------------------------------------------------
-- Parses mulitple EPDs separated by newlines
epdParser :: Parser [ EPD ]
epdParser = do
  epds <- lineParser `endBy` (char ';' >> newline)
  eof
  return epds


------------------------------------------------------------------------------
-- Parses a single EPD
lineParser :: Parser EPD
lineParser = do
  b <- boardParser
  void $ char ' '
  ops <- operationParser b `sepBy` try (char ';' >> char ' ')
  return $ EPD b ops


------------------------------------------------------------------------------
operationParser :: Board -> Parser Operation
operationParser b =
  simpleOperationParser "acn" AnalysisCountNodes
  <|> simpleOperationParser "acs" AnalysisCountSeconds
  <|> simpleOperationParser "draw_accept" AcceptDraw
  <|> simpleOperationParser "draw_claim" ClaimDraw
  <|> simpleOperationParser "draw_offer" OfferDraw
  <|> simpleOperationParser "draw_reject" RejectDraw
  <|> simpleOperationParser "noop" Nop
  <|> simpleOperationParser "resign" Resign
  <|> intOperationParser "ce" Evaluation
  <|> intOperationParser "dm" MateFullMoveCount
  <|> intOperationParser "fmvn" FullMoveNumber
  <|> intOperationParser "hmvc" HalfMoveClock
  <|> intOperationParser "rc" RepetitionCount
  <|> stringOperationParser "eco" ECO
  <|> stringOperationParser "id" Id
  <|> stringOperationParser "nic" NewInChess
  <|> stringOperationParser "tcgs" TelecomGameSelector
  <|> stringOperationParser "tcri" TelecomReceiverId
  <|> stringOperationParser "tcsi" TelecomSenderId
  <|> countedOperationParser 'c' Comment
  <|> countedOperationParser 'v' VariationName
  <|> movesOperationParser "am" AvoidMove b
  <|> movesOperationParser "bm" BestMove b
  <|> moveOperationParser "pm" PredictedMove b
  <|> moveOperationParser "sm" SuppliedMove b
  -- TODO pv PredictedVariation


------------------------------------------------------------------------------
simpleOperationParser :: String -> Operation -> Parser Operation
simpleOperationParser opcode constr = try (string opcode) >> return constr


------------------------------------------------------------------------------
intOperationParser :: String -> (Int -> Operation) -> Parser Operation
intOperationParser opcode constr =
  argumentOperationParser opcode constr intParser


------------------------------------------------------------------------------
stringOperationParser :: String -> (String -> Operation) -> Parser Operation
stringOperationParser opcode constr =
  argumentOperationParser opcode constr stringParser


------------------------------------------------------------------------------
argumentOperationParser
  :: String
  -> (a -> Operation)
  -> Parser a
  -> Parser Operation
argumentOperationParser opcode constr p = 
  liftM constr (try (string opcode >> char ' ') >> p)


------------------------------------------------------------------------------
intParser :: Parser Int
intParser = liftM read $ many1 digit


------------------------------------------------------------------------------
stringParser :: Parser String
stringParser = between (char '"') (char '"') (many $ noneOf "\"")


------------------------------------------------------------------------------
countedOperationParser
  :: Char                         -- ^ opcode
  -> (Int -> String -> Operation) -- ^ constructor
  -> Parser Operation
countedOperationParser opcode constr = do
  n <- try $ char opcode >> digit
  void $ char ' '
  s <- stringParser
  return $ constr (ord n - ord '0') s


------------------------------------------------------------------------------
movesOperationParser
  :: String
  -> ([Move] -> Operation)
  -> Board
  -> Parser Operation
movesOperationParser opcode constr b = do
  void $ try $ string opcode >> char ' '
  ms <- moveSansParser b `sepBy` char ' '
  return $ constr ms


------------------------------------------------------------------------------
moveOperationParser
  :: String
  -> (Move -> Operation)
  -> Board
  -> Parser Operation
moveOperationParser opcode constr b = do
  void $ try $ string opcode >> char ' '
  m <- moveSansParser b
  return $ constr m
