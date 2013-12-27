module Chess.UCI where

import           Control.Lens hiding (from, to)
import           Control.Applicative (liftA)
import           Control.Monad.State
import           Data.Bits
import           Data.List
import           Data.IORef
import           System.Exit
import           Data.Char
import           Data.Maybe
import           System.IO

import           Text.ParserCombinators.Parsec

import           Data.Square
import           Chess.Move
import           Chess.Board
import           Chess.Magic
import           Chess.Search
import qualified Chess as C

data SearchOption = MovetimeMsc Int | Infinity deriving (Show)
                    
data Command = CmdUci 
             | CmdIsReady 
             | CmdUciNewGame 
             | CmdPosition Board -- [(Int, Int, Maybe C.PieceType)] 
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
        
        
renderShortMove :: Move -> String
renderShortMove m = maybe nonCastle renderCastle (m^.castle)
  where
    renderCastle Short = "O-O"
    renderCastle Long  = "O-O-O"
    nonCastle = showSquare (m^.from) ++ showSquare (m^.to) ++ (showPromotion $ m^.promotion)
    showSquare sq = (['a' .. 'h'] !! (sq .&. 7)) : (show $ 1 + (sq `shiftR` 3))                                 
    showPromotion (Just C.Queen) = "q"
    showPromotion (Just C.Knight) = "n"
    showPromotion (Just C.Rook) = "r"
    showPromotion (Just C.Bishop) = "b"
    showPromotion _ = ""
    
------------------ parsers --------------
p_cmd_uci = do
                string "uci"
                return CmdUci

p_cmd_isready = do
                string "isready"
                return CmdIsReady           
                
p_cmd_ucinewgame = do
                string "ucinewgame"
                return CmdUciNewGame                
                
p_cmd_stop = do
                string "stop"
                return CmdStop
                
p_cmd_quit = do
                string "quit"
                return CmdQuit

p_int :: Parser Int
p_int = liftA read $ many1 digit
                
p_cmd_go = do
                string "go"
                spaces
                mbTimeout <- optionMaybe (string "movetime" >> spaces >> p_int)
                return $ case mbTimeout of
                            Nothing -> CmdGo Infinity
                            Just timeout -> CmdGo $ MovetimeMsc timeout

p_position :: Parser Board
p_position = liftA (fromJust . fromFEN) $ do
  many $ oneOf "rkbqkpRKBQKP/12345678"
  spaces
  oneOf "wb"
  spaces
  many $ oneOf "kqKQ-"
  spaces
  ((char '-') >> return Nothing) <|> (liftA Just parserSquare)
  spaces
  many1 digit
  spaces
  many1 digit
                    
    
p_cmd_position :: CharParser () Command
p_cmd_position = do
  string "position"
  many1 $ char ' '
  posType <- (string "fen" <|> string "startpos")
  spaces
  pos <- if posType == "fen" then p_position else return initialBoard
  spaces
  liftA CmdPosition $ option pos (string "moves" >> parserMoveList pos)
  where
    parserMoveList pos = do
      mm <- optionMaybe (spaces >> parserMove pos)
      case mm of
        Just m  -> parserMoveList (execState (doMoveM m) pos)
        Nothing -> return pos


p_cmd = (try p_cmd_ucinewgame) <|> p_cmd_uci <|> p_cmd_isready <|> p_cmd_stop <|> p_cmd_quit <|> p_cmd_go <|> p_cmd_position


parseCommand :: String -> Maybe Command
parseCommand line = case parse p_cmd "" line of
                Left _ -> Nothing
                Right cmd -> Just cmd


uci :: IO ()
uci = do
    hSetBuffering stdout NoBuffering
    lastPosition <- newIORef $ initialGameTree

    let dialogue lastPosition = do
                line <- getLine
                case parseCommand line of
                    Nothing -> return ()
                    Just cmd -> do 
                                    responses <- getResponse cmd
                                    let output = intercalate "\n" $ map show $ responses
                                    putStrLn output
                dialogue lastPosition
                where
                    getResponse CmdUci = return [(RspId "name" "Chess"), (RspId "author" "Paul Sonkoly"), RspUciOk]
                    getResponse CmdIsReady = return [RspReadyOk]
                    getResponse CmdUciNewGame = return []
                    getResponse CmdQuit = exitSuccess
                    getResponse CmdStop = return []
                    getResponse (CmdPosition pos) = do
                      modifyIORef lastPosition $ gameTreeFromBoard pos
                      return []
                    getResponse (CmdGo _) = do
                            position <- readIORef lastPosition
                            prettyPrint $ position^.board
                            let (gts, score) = alpha_beta_search position 3
                                move = head $ getPath gts
                            return [ RspInfo ("score " ++ show score)
                                   , RspInfo ("currmove " ++ renderShortMove move)
                                   , RspBestMove move
                                   ]
    dialogue lastPosition
