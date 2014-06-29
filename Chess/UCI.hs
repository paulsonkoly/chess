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
import           Chess.TimeControl


------------------------------------------------------------------------------
-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: IO ()
uci = do
  hSetBuffering stdout NoBuffering
  a <- newTVarIO False
  p <- newTVarIO False
  st <- newIORef $ mkSearchState a p

  forever $ do
        line <- getLine
        case parse uciCmdParser "" line of
          Right cmd -> do
            s <- readIORef st
            atomically $ do
              av <- readTVar (s^.aborted)
              when av retry
            execute cmd st
          Left err  -> putStrLn $ "info string parse error : " ++ show err


                           -----------------------
                           -- Parser data types --
                           -----------------------

data Command = CmdUci
             | CmdIsReady 
             | CmdUciNewGame 
             | CmdPosition Board
             | CmdGo Bool TimeControl
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
uciNewGameParser = try (string "ucinewgame") >> return CmdUciNewGame


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
-- The time control parser
uciTimeControlParser :: Parser (Bool, TimeControl -> TimeControl)
uciTimeControlParser = do
  opts <- singleOption `sepBy` spaces
  return $ foldr (\(a, f) (b, g) -> (a || b, f . g)) (False, id) opts
  where singleOption =
          (funcIntParser "movetime" $ \mt -> const $ TimeSpecified mt)
          <|> (funcIntParser "depth" $ \d -> const $ DepthSpecified d)
          <|> (funcParser "infinite" $ const Infinite)
          <|> (funcIntParser "wtime" setWhiteTime)
          <|> (funcIntParser "btime" setBlackTime)
          <|> (funcIntParser "winc" setWhiteInc)
          <|> (funcIntParser "binc" setBlackInc)
          <|> (funcIntParser "movestogo" setMovesToGo)
          <|> (string "ponder" >> return (True, id))

        funcIntParser n f = do
          _ <- try $ string n
          spaces
          i <- uciIntParser
          return (False, f i)

        funcParser n f = string n >> return (False, f)
          

------------------------------------------------------------------------------
uciGoParser :: Parser Command
uciGoParser = do
  string "go" >> spaces
  (ponder, f) <- uciTimeControlParser
  return $ CmdGo ponder $ f Infinite
  
  
------------------------------------------------------------------------------    
uciPositionParser :: Parser Command
uciPositionParser = do
  _ <- try $ string "position" >> many1 (char ' ')
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
uciCmdParser = uciNewGameParser
               <|> uciUciParser
               <|> uciIsReadyParser
               <|> uciStopParser
               <|> uciQuitParser
               <|> uciGoParser
               <|> uciPositionParser
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

execute (CmdPosition pos) st = modifyIORef st (board .~ pos)

execute (CmdPonderHit)    st = do
  s <- readIORef st
  atomically $ writeTVar (s^.pondering) False

execute (CmdGo ponder tc) st = do
  void $ forkIO $ do
    p <- readIORef st
    atomically $ writeTVar (p^.pondering) ponder
    (r, p') <- runSearch (search tc) p
    let mbMove = join $ first <$> r
    rsp <- case mbMove of
      Just m  -> do
        let p'' = (board %~ makeMove m) p'
        writeIORef st p''
        return [ RspBestMove m (join $ second <$> r) ]
      Nothing -> do
        writeIORef st p'
        return [ RspNullMove ]
    display rsp
    atomically $ writeTVar (p'^.S.aborted) False
