module Chess.Board.Parser
       ( parserBoard
       ) where

import           Control.Lens
import           Control.Monad
import           Text.ParserCombinators.Parsec

import           Data.Monoid
import           Data.Functor
import           Data.Word
import           Data.Char
import           Data.Bits
import           Data.Maybe

import           Data.Square
import           Data.BitBoard
import           Data.ChessTypes
import           Chess.Board.Board
import qualified Chess as C


parserBoard :: Parser Board
parserBoard = liftM (\b -> (hash .~ calcHash b) b) $ do
  b <- go emptyBoard 56
  spaces
  s <- parserSide
  spaces
  c <- parserCastle
  spaces
  e <- liftM (transEnp <$>) parserEnp
  spaces
  _ <- count 2 $ many digit >> spaces
  return
    $ (enPassant .~ e)
    $ (blackCastleRights .~ snd c)
    $ (whiteCastleRights .~ fst c)
    $ (next .~ s) b
  where
    go b sq = choice [ parserPiece, parserGap, parserSlash, parserSpace ]
      where parserPiece = do
              p <- oneOf "rnbqkpRNBQKP"
              let sbb = fromSquare $ toEnum sq
              go (piecesByColour (charToColour p) <>~ sbb $ (piecesByType (charToPiece p) <>~ sbb) b) $ sq + 1
            parserGap   = liftM digitToInt (oneOf "12345678") >>= \g -> go b $ sq + g
            parserSlash = char '/' >> go b (sq - 16)
            parserSpace = char ' ' >> return b
            charToColour c
              | isLower c = C.Black
              | otherwise = C.White
    parserSide   = (char 'w' >> return C.White) <|> (char 'b' >> return C.Black)
    parserCastle = (char '-' >> return (mempty, mempty)) <|> go' (mempty, mempty)
    parserEnp    = (char '-' >> return Nothing) <|> liftM Just parserSquare
    go' p        = choice [ char 'k' >> go' ((_2 <>~ fromCastle Short) p)
                          , char 'q' >> go' ((_2 <>~ fromCastle Long) p)
                          , char 'K' >> go' ((_1 <>~ fromCastle Short) p)
                          , char 'Q' >> go' ((_1 <>~ fromCastle Long) p)
                          , return p
                          ]
    transEnp sq = offset sq (if fromEnum (rank sq) == 2 then 1 else (-1))
