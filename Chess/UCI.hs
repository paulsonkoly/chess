module Chess.UCI
       ( uci
       ) where

import Chess.Board hiding (hash)
import Chess.Move
import Chess.Search
import qualified Chess.Search as S (aborted)
import Control.Applicative ((<$>), liftA)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Lens hiding (from, to)
import Control.Monad (forever, join, void, when)
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec


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
  a  <- newTVarIO False
  st <- newIORef $ mkSearchState a
  pondering <- newIORef $ False

  forever $ do
        line <- getLine
        case parseCommand line of
          Nothing -> return ()
          Just cmd -> do 
              abort pondering a
              responses <- getResponse cmd st pondering
              let output = intercalate "\n" $ map show responses
              putStrLn output

   where getResponse CmdUci             _ _ = return [RspId "name" "Chess", RspId "author" "Paul Sonkoly", RspUciOk]
         getResponse CmdIsReady         _ _ = return [RspReadyOk]
         getResponse CmdUciNewGame      _ _ = return []
         getResponse CmdQuit            _ _ = exitSuccess
         getResponse CmdStop            _ _ = return []
         getResponse (CmdPosition pos) st _ = do
              modifyIORef st (board .~ pos)
              return []
         getResponse (CmdGo _)        st pondering = do
              p <- readIORef st
              (r, p') <- runSearch (search 6) ((quiet .~ False) p)
              let m   = fromJust $ join $ first <$> r
                  p'' = (board %~ makeMove m) p'
              writeIORef st p''
              ponder pondering st
              return [ RspInfo ("currmove " ++ renderShortMove m), RspBestMove m ]

         ponder pondering st = void $ do
           writeIORef pondering True
           forkIO $ do
             p <- readIORef st
             (_, p') <- runSearch (search maxBound) $ (quiet .~ True) p
             writeIORef st p'
             atomically $ writeTVar (p'^.S.aborted) False
             writeIORef pondering False

         abort pondering a = do
              p <- readIORef pondering
              when p $ do
                atomically $ writeTVar a True
                atomically $ do
                         av <- readTVar a
                         when av retry


