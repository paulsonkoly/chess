module Chess.UCI
       ( uci
       ) where

import           Control.Lens hiding (from, to)
import           Control.Applicative (liftA)
import           Data.List
import           Data.IORef
import           System.Exit
import           Data.Maybe
import           System.IO

import           Text.ParserCombinators.Parsec

import           Chess.Move
import           Chess.Board
import           Chess.Search


data SearchOption = MovetimeMsc Int | Infinity deriving (Show)
                    
data Command = CmdUci 
             | CmdIsReady 
             | CmdUciNewGame 
             | CmdPosition Board
             | CmdGo SearchOption
             | CmdStop
             | CmdQuit
            deriving (Show)

data Response = RspId String String
              | RspUciOk
              | RspReadyOk
              | RspBestMove Move
              | RspInfo String
              | RspOption String 
            

---------------- show ------------------
instance Show Response where
  show RspUciOk           = "uciok"
  show RspReadyOk         = "readyok"
  show (RspInfo info)     = "info " ++ info
  show (RspId name value) = "id " ++ name ++ " " ++ value
  show (RspBestMove move) = "bestmove " ++ renderShortMove move
  show (RspOption text)   = "option " ++ text


------------------ parsers --------------
uciUciParser :: Parser Command
uciUciParser = string "uci" >> return CmdUci

uciIsReadyParser :: Parser Command
uciIsReadyParser = string "isready" >> return CmdIsReady

uciNewGameParser :: Parser Command
uciNewGameParser = string "ucinewgame" >> return CmdUciNewGame

uciStopParser :: Parser Command
uciStopParser = string "stop" >> return CmdStop

uciQuitParser :: Parser Command
uciQuitParser = string "quit" >> return CmdQuit

uciIntParser :: Parser Int
uciIntParser = liftA read $ many1 digit

uciGoParser :: Parser Command
uciGoParser = do
                string "go" >> spaces
                mbTimeout <- optionMaybe (string "movetime" >> spaces >> uciIntParser)
                return $ case mbTimeout of
                            Nothing -> CmdGo Infinity
                            Just timeout -> CmdGo $ MovetimeMsc timeout
                    
    
uciPositionParser :: CharParser () Command
uciPositionParser = do
  _ <- string "position" >> (many1 $ char ' ')
  posType <- string "fen" <|> string "startpos"
  spaces
  pos <- if posType == "fen" then parserBoard else return initialBoard
  spaces
  liftA CmdPosition $ option pos (string "moves" >> parserMoveList pos)
  where
    parserMoveList pos = do
      mm <- optionMaybe (spaces >> parserMove pos)
      case mm of
        Just m  -> parserMoveList $ makeMove m pos
        Nothing -> return pos


uciCmdParser :: Parser Command
uciCmdParser = try uciNewGameParser
               <|> uciUciParser
               <|> uciIsReadyParser
               <|> uciStopParser
               <|> uciQuitParser
               <|> uciGoParser
               <|> uciPositionParser


parseCommand :: String -> Maybe Command
parseCommand line = case parse uciCmdParser "" line of
                Left _ -> Nothing
                Right cmd -> Just cmd


-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: IO ()
uci = do
    hSetBuffering stdout NoBuffering
    lastPosition <- newIORef mkSearchState

    let dialogue = do
                line <- getLine
                case parseCommand line of
                    Nothing -> return ()
                    Just cmd -> do 
                                    responses <- getResponse cmd
                                    let output = intercalate "\n" $ map show responses
                                    putStrLn output
                dialogue
                where
                    getResponse CmdUci = return [RspId "name" "Chess", RspId "author" "Paul Sonkoly", RspUciOk]
                    getResponse CmdIsReady = return [RspReadyOk]
                    getResponse CmdUciNewGame = return []
                    getResponse CmdQuit = exitSuccess
                    getResponse CmdStop = return []
                    getResponse (CmdPosition pos) = do
                      modifyIORef lastPosition (board .~ pos)
                      return []
                    getResponse (CmdGo _) = do
                      p <- readIORef lastPosition
                      (pv, p') <- runSearch (search 4) p
                      writeIORef lastPosition p'
                      let m = fromJust $ first pv
                      return [ RspInfo ("currmove " ++ renderShortMove m), RspBestMove m ]
    dialogue
