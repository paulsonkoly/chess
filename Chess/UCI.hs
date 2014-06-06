-- | Module for UCI protocol.
--
-- This module drives the engine (starts the search) in response to the GUI,
-- and provides the communication between the GUI and the engine using the UCI
-- v2 protocol.
module Chess.UCI
       ( uci
       ) where


------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Maybe
import           System.Exit
import           System.IO

import           Control.Lens ((.~), (^.), (%~))
import           Text.ParserCombinators.Parsec

import           Chess.Board hiding (hash)
import           Chess.Move
import           Chess.Search
import qualified Chess.Search as S (aborted)


------------------------------------------------------------------------------
-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: IO ()
uci = do
  hSetBuffering stdout NoBuffering
  a  <- newTVarIO False
  mx <- newTVarIO 6
  st <- newIORef $ mkSearchState a mx

  forever $ do
        line <- getLine
        case parse uciCmdParser "" line of
          Right cmd -> execute cmd st
          Left err  -> putStrLn $ "info string parse error : " ++ show err


                           -----------------------
                           -- Parser data types --
                           -----------------------

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
                

------------------------------------------------------------------------------
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


                                -------------
                                -- parsers --
                                -------------

------------------------------------------------------------------------------
uciUciParser :: Parser Command
uciUciParser = string "uci" >> return CmdUci


------------------------------------------------------------------------------
uciIsReadyParser :: Parser Command
uciIsReadyParser = string "isready" >> return CmdIsReady


------------------------------------------------------------------------------
uciNewGameParser :: Parser Command
uciNewGameParser = string "ucinewgame" >> return CmdUciNewGame


------------------------------------------------------------------------------
uciStopParser :: Parser Command
uciStopParser = string "stop" >> return CmdStop


------------------------------------------------------------------------------
uciQuitParser :: Parser Command
uciQuitParser = string "quit" >> return CmdQuit


------------------------------------------------------------------------------
uciIntParser :: Parser Int
uciIntParser = do
  sign   <- optionMaybe $ char '-'
  digits <- many1 digit
  return $ read digits * if isJust sign then (-1) else 1


------------------------------------------------------------------------------
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
        argumentOption name t   = liftM t
                                  $ string name >> spaces >> uciIntParser


------------------------------------------------------------------------------
uciGoParser :: Parser Command
uciGoParser = do
  string "go" >> spaces
  options <- uciSearchOptionParser
  return $ CmdGo options

  
------------------------------------------------------------------------------    
uciPositionParser :: Parser Command
uciPositionParser = do
  _ <- string "position" >> many1 (char ' ')
  posType <- string "fen" <|> string "startpos"
  spaces
  pos <- if posType == "fen" then parserBoard else return initialBoard
  spaces
  liftM CmdPosition $ option pos (string "moves" >> parserMoveList pos)
  where
    parserMoveList pos = do
      mm <- optionMaybe (spaces >> parserMove pos)
      case mm of
        Just m  -> parserMoveList $ makeMove m pos
        Nothing -> return pos


------------------------------------------------------------------------------
uciPonderHitParser :: Parser Command
uciPonderHitParser = string "ponderhit" >> return CmdPonderHit


------------------------------------------------------------------------------
uciCmdParser :: Parser Command
uciCmdParser = try uciNewGameParser
               <|> uciUciParser
               <|> uciIsReadyParser
               <|> uciStopParser
               <|> uciQuitParser
               <|> uciGoParser
               <|> try uciPositionParser
               <|> uciPonderHitParser


------------------------------------------------------------------------------
display :: [ Response ] -> IO ()
display rsps = let output = intercalate "\n" $ map show rsps
               in putStrLn output


------------------------------------------------------------------------------
execute :: Command -> IORef SearchState -> IO ()
execute CmdUci             _ =
  display [ RspId "name"   "Chess"
          , RspId "author" "Paul Sonkoly"
          , RspOption "name Ponder type check default true"
          , RspUciOk
          ]
execute CmdIsReady         _ = display [ RspReadyOk ]
execute CmdUciNewGame      _ = return ()
execute CmdQuit            _ = exitSuccess
execute CmdStop           st = do
  s <- readIORef st
  atomically $ writeTVar (s^.aborted) True
  atomically $ do
    av <- readTVar (s^.aborted)
    when av retry
execute (CmdPosition pos) st = modifyIORef st (board .~ pos)
execute (CmdPonderHit)    st = do
  s <- readIORef st
  atomically $ writeTVar (s^.maxDepth) 6
execute (CmdGo opts)      st =
  let mx = if Ponder `elem` opts then maxBound else 6
  in void $ forkIO $ do
    p <- readIORef st
    atomically $ writeTVar (p^.maxDepth) mx
    (r, p') <- runSearch search p
    let mbMove = join $ first <$> r
    rsp <- case mbMove of
      Just m  -> do
        let p'' = (board %~ makeMove m) p'
        writeIORef st p''
        return [ RspBestMove m (join $ second <$> r) ]
      Nothing -> do
        atomically $ writeTVar (p'^.S.aborted) False          
        writeIORef st p'
        return [ RspNullMove ]
    display rsp

