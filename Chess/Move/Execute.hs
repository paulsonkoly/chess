module Chess.Move.Execute
       ( makeMove
       ) where

import           Control.Lens hiding (to, from)
import           Data.Word
import           Data.Monoid

import qualified Chess as C
import           Data.BitBoard
import           Data.Square
import           Data.ChessTypes
import           Chess.Move.Move
import           Chess.Board hiding (calcHash)
import           Chess.Zobrist

makeMove :: Move -> Board -> Board
makeMove m b = let (f, t) = (fromBB m, toBB m)
                   ft     = f `xor` t
                   mvsTrs = maybe id (\c -> putPiece (rookCaslteBB (b^.next) c) (b^.next) C.Rook) (m^.castle)
                            . maybe id (\e -> putPiece (fromSquare e) (b^.opponent) C.Pawn)  (m^.enPassantTarget)
                            . maybe id (promote t) (m^.promotion)
                            . maybe id (putPiece t $ b^.opponent) (m^.capturedPiece)
                            . putPiece ft (b^.next) (m^.piece)
                   cstlTrs = castleRightsByColour (b^.next) %~ intersect (castleRights m)
                   nxtTrs  = next %~ opponent'
                   enpTrs  = enPassant .~ if isDoubleAdvance m then Just (m^.to) else Nothing
                   hshTrs  = hash .~ calcHash b m
               in hshTrs $ enpTrs $ nxtTrs $ cstlTrs $ mvsTrs b


fromBB :: Move -> BitBoard
fromBB = fromSquare . (^.from)
{-# INLINE fromBB #-}


toBB :: Move -> BitBoard
toBB = fromSquare . (^.to)
{-# INLINE toBB #-}


putPiece :: BitBoard -> C.Color -> C.PieceType -> Board -> Board 
putPiece bb c pt = (piecesByColour c %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE putPiece #-}


zPutPiece :: Square -> C.Color -> C.PieceType -> Word64
zPutPiece x y z = zobrist $ ZobristPiece x y z
{-# INLINE zPutPiece #-}


promote :: BitBoard -> C.PieceType -> Board -> Board
promote bb pt = (pawns %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE promote #-}


rookCasltePos :: C.Color -> Castle -> (Square, Square)
rookCasltePos C.White Long  = (toSquare aFile firstRank, toSquare dFile firstRank)
rookCasltePos C.White Short = (toSquare fFile firstRank, toSquare hFile firstRank)
rookCasltePos C.Black Long  = (toSquare aFile eighthRank, toSquare dFile eighthRank)
rookCasltePos C.Black Short = (toSquare fFile eighthRank, toSquare hFile eighthRank)
{-# INLINE rookCasltePos #-}


rookCaslteBB :: C.Color -> Castle -> BitBoard
rookCaslteBB col ctl = fromList [ fst $ rookCasltePos col ctl, snd $ rookCasltePos col ctl ]
{-# INLINE rookCaslteBB #-}


castleRights :: Move -> CastlingRights
castleRights m
  | m^.piece == C.King                                = mempty
  | (m^.piece == C.Rook) && (file (m^.from) == aFile) = fromCastle Short
  | (m^.piece == C.Rook) && (file (m^.from) == hFile) = fromCastle Long
  | otherwise                                         = fromCastle Short <> fromCastle Long
{-# INLINE castleRights #-}


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && vDist (m^.from) ( m^.to) > 1
{-# INLINE isDoubleAdvance #-}


-- | calculates hash based on the previous board
calcHash :: Board -> Move -> Word64
calcHash b m = b^.hash
               `xor` zPutPiece (m^.from) (m^.colour) (m^.piece)
               `xor` zPutPiece (m^.to) (m^.colour) (m^.piece)
               `xor` capturedZ `xor` castleZ `xor` castleRMZ `xor` enPassantZ `xor` enPassantRMZ `xor` promotionZ `xor` flipSideZ

  where flipSideZ = zobrist (ZobristSide C.White) `xor` zobrist (ZobristSide C.Black)

        capturedZ = maybe 0 (zobrist . ZobristPiece (m^.to) (opponent' $ m^.colour)) (m^.capturedPiece)

        castleZ   = zobrist (ZobristCastlingRights (b^.whiteCastleRights) (b^.blackCastleRights)) -- old castling rights
                    `xor` zobrist (ZobristCastlingRights newWhiteCastle newBlackCastle)

        newWhiteCastle = case m^.colour of
          C.White -> castleRights m `intersect` (b^.whiteCastleRights)
          C.Black -> b^.whiteCastleRights

        newBlackCastle = case m^.colour of
          C.White -> b^.blackCastleRights
          C.Black -> castleRights m `intersect` (b^.blackCastleRights)

        castleRMZ = maybe 0 (\c ->
                              zPutPiece (fst (rookCasltePos (b^.next) c))  (b^.next) C.Rook
                              `xor` zPutPiece (snd (rookCasltePos (b^.next) c)) (b^.next) C.Rook
                            ) (m^.castle)

        enPassantZ   = zobrist (ZobristEnPassant $ b^.enPassant)
                       `xor` zobrist (ZobristEnPassant $ if isDoubleAdvance m then Just (m^.to) else Nothing)

        enPassantRMZ = maybe 0 (\t -> zPutPiece t (opponent' (b^.next)) C.Pawn) (m^.enPassantTarget)

        promotionZ   = maybe 0 (\p -> zPutPiece (m^.to) (m^.colour) C.Pawn `xor` zPutPiece (m^.to) (m^.colour) p) (m^.promotion)
