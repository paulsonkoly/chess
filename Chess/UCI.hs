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
import Control.Monad (forever, join, void, when, liftM)
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec


data SearchOption = MovetimeMsc Int
                  | Infinity
                  | Ponder
                  | WTime Int
                  | BTime Int
                  | WInc Int
                  | BInc Int
                  | MovesToGo Int
                  deriving (Show, Eq)


data Command = CmdUci
             | CmdIsReady 
             | CmdUciNewGame 
             | CmdPosition Board
             | CmdGo [ SearchOption ]
             | CmdStop
             | CmdQuit
             | CmdPonderHit
            deriving (Show)

data Response = RspId String String
              | RspUciOk
              | RspReadyOk
              | RspBestMove Move (Maybe Move)
              | RspNullMove
              | RspInfo String
              | RspOption String 
            

---------------- show ------------------
instance Show Response where
  show RspUciOk              = "uciok"
  show RspReadyOk            = "readyok"
  show (RspInfo info)        = "info " ++ info
  show (RspId name value)    = "id " ++ name ++ " " ++ value
  show (RspBestMove move mp) = "bestmove " ++ renderShortMove move
                               ++ case mp of
                                 Just p  -> " ponder " ++ renderShortMove p
                                 Nothing -> ""
  show RspNullMove           = "bestmove 0000"
  show (RspOption text)      = "option " ++ text


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


uciSearchOptionParser :: Parser [ SearchOption ]
uciSearchOptionParser = foldr1 (<|>)
                        [ try $ argumentOption "movetime" MovetimeMsc
                        , argumentOption "movestogo" MovesToGo
                        , selectableOption "ponder" Ponder
                        , selectableOption "infinite" Infinity
                        , try $ argumentOption "wtime" WTime
                        , try $ argumentOption "btime" BTime
                        , argumentOption "winc" WInc
                        , argumentOption "binc" BInc
                        ] `sepBy` spaces
  where selectableOption name t = string name >> return t
        argumentOption name t = liftM t $ string name >> spaces >> uciIntParser


uciGoParser :: Parser Command
uciGoParser = do
  string "go" >> spaces
  options <- uciSearchOptionParser
  return $ CmdGo options
  
    
uciPositionParser :: CharParser () Command
uciPositionParser = do
  _ <- string "position" >> many1 (char ' ')
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


uciPonderHitParser :: Parser Command
uciPonderHitParser = string "ponderhit" >> return CmdPonderHit


uciCmdParser :: Parser Command
uciCmdParser = try uciNewGameParser
               <|> uciUciParser
               <|> uciIsReadyParser
               <|> uciStopParser
               <|> uciQuitParser
               <|> uciGoParser
               <|> try uciPositionParser
               <|> uciPonderHitParser


-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: IO ()
uci = do
  hSetBuffering stdout NoBuffering
  a  <- newTVarIO False
  st <- newIORef $ mkSearchState a

  forever $ do
        line <- getLine
        case parse uciCmdParser "" line of
          Right cmd -> do
            responses <- getResponse cmd st
            let output = intercalate "\n" $ map show responses
            putStrLn output
          Left err -> putStrLn $ "info parse error : " ++ show err

   where getResponse CmdUci             _ = return [ RspId "name" "Chess"
                                                   , RspId "author" "Paul Sonkoly"
                                                   , RspOption "name Ponder type check default true"
                                                   , RspUciOk
                                                   ]
         getResponse CmdIsReady         _ = return [RspReadyOk]
         getResponse CmdUciNewGame      _ = return []
         getResponse CmdQuit            _ = exitSuccess
         getResponse CmdStop           st = abort st >> return [ RspNullMove ]

         getResponse (CmdPosition pos) st = do
              modifyIORef st (board .~ pos)
              return []

         getResponse (CmdPonderHit)    st = abort st >> goHandler st True

         getResponse (CmdGo opts)      st = if Ponder `elem` opts
                                                 then ponder st
                                                 else goHandler st False

         ponder st = do
           void $ forkIO $ do
             p <- readIORef st
             (_, p') <- runSearch (search maxBound False) p
             writeIORef st p'
             atomically $ writeTVar (p'^.S.aborted) False
           return []
           
         abort st = do
           s <- readIORef st
           atomically $ writeTVar (s^.aborted) True
           atomically $ do
             av <- readTVar (s^.aborted)
             when av retry

         goHandler st pondering = do
              p <- readIORef st
              (r, p') <- runSearch (search 6 pondering) p
              let m   = fromJust $ join $ first <$> r
                  p'' = (board %~ makeMove m) p'
              writeIORef st p''
              return [ RspBestMove m (join $ second <$> r) ]
