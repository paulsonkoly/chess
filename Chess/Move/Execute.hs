module Chess.Move.Execute
       ( makeMove
       , makeMoveSimplified
       , calcHash
       ) where

import           Control.Lens hiding (to, from)
import           Data.Word
import           Data.Monoid

import           Data.BitBoard
import           Data.Square
import           Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import           Chess.Move.Move
import           Chess.Board hiding (calcHash)
import qualified Chess.Board as B (opponent)
import           Chess.Zobrist


------------------------------------------------------------------------------
-- | makes a move on the board
makeMove :: Move -> Board -> Board
makeMove m b =
  let mcsTrs  =
        castleRightsByColour (b^.next) %~ intersect (castleRightsMine m)
      opcsTrs =
        castleRightsByColour (b^.B.opponent) %~ intersect (castleRightsOpp m)
      nxtTrs  = next %~ T.opponent
      enpTrs  = enPassant .~ if isDoubleAdvance m
                             then Just (m^.to)
                             else Nothing
      hshTrs  = hash .~ calcHash b m
  in hshTrs $ enpTrs $ nxtTrs $ mcsTrs $ opcsTrs $ makeMoveSimplified m b


------------------------------------------------------------------------------
-- | same as makeMove except faster but only update where the pieces are
makeMoveSimplified :: Move -> Board -> Board
makeMoveSimplified m b =
  let (f, t) = (fromBB m, toBB m)
      ft     = f `xor` t
      modif  = case m^.piece of
        King -> flipPiece (b^.next) King (Left $ m^.to)
        pt   -> flipPiece (b^.next) pt (Right ft)
        
      mvsTrs = optional (m^.castle)
               (flipPiece (b^.next) Rook . Right . rookCastleBB (b^.next))

               . optional (m^.enPassantTarget)
               (flipPiece (b^.B.opponent) Pawn . Right . fromSquare)
               
               . optional (m^.promotion)
               (promote (m^.colour) t)
               
               . optional (m^.capturedPiece)
               (\pt -> flipPiece (b^.B.opponent) pt (Right t))
               
               . modif
  in mvsTrs b


------------------------------------------------------------------------------
fromBB :: Move -> BitBoard
fromBB = fromSquare . (^.from)
{-# INLINE fromBB #-}


------------------------------------------------------------------------------
toBB :: Move -> BitBoard
toBB = fromSquare . (^.to)
{-# INLINE toBB #-}


------------------------------------------------------------------------------
optional :: Maybe a -> (a -> a1 -> a1) -> a1 -> a1
optional = flip (maybe id)
{-# INLINE optional #-}

------------------------------------------------------------------------------
zPutPiece :: Square -> Colour -> PieceType -> Word64
zPutPiece x y z = zobrist $ ZobristPiece x y z
{-# INLINE zPutPiece #-}


------------------------------------------------------------------------------
promote :: Colour -> BitBoard -> PieceType -> Board -> Board
promote c bb pt = flipPiece c pt (Right bb) . flipPiece c Pawn (Right bb)
{-# INLINE promote #-}


------------------------------------------------------------------------------
rookCastlePos :: Colour -> Castle -> (Square, Square)
rookCastlePos White Long  =
  (toSquare aFile firstRank, toSquare dFile firstRank)
rookCastlePos White Short =
  (toSquare fFile firstRank, toSquare hFile firstRank)
rookCastlePos Black Long  =
  (toSquare aFile eighthRank, toSquare dFile eighthRank)
rookCastlePos Black Short =
  (toSquare fFile eighthRank, toSquare hFile eighthRank)
{-# INLINE rookCastlePos #-}


------------------------------------------------------------------------------
rookCastleBB :: Colour -> Castle -> BitBoard
rookCastleBB col ctl =
  fromList [ fst $ rookCastlePos col ctl, snd $ rookCastlePos col ctl ]
{-# INLINE rookCastleBB #-}


------------------------------------------------------------------------------
myFirstRank :: Colour -> Rank
myFirstRank White = firstRank
myFirstRank Black = eighthRank
{-# INLINE myFirstRank #-}


------------------------------------------------------------------------------
oppFirstRank :: Colour -> Rank
oppFirstRank White = eighthRank
oppFirstRank Black = firstRank
{-# INLINE oppFirstRank #-}


------------------------------------------------------------------------------
castleRightsMine :: Move -> CastlingRights
castleRightsMine m
  | m^.piece == King                                                     =
      mempty
  | file (m^.from) == aFile && rank (m^.from) == myFirstRank (m^.colour) =
      fromCastle Short
  | file (m^.from) == hFile && rank (m^.from) == myFirstRank (m^.colour) =
      fromCastle Long
  | otherwise                                                            =
      fromCastle Short <> fromCastle Long
{-# INLINE castleRightsMine #-}


------------------------------------------------------------------------------
-- if I'm capturing to the opponent's rook position then he loses that castle
-- right
castleRightsOpp :: Move -> CastlingRights
castleRightsOpp m
  | file (m^.to) == aFile && rank (m^.to) == oppFirstRank (m^.colour) =
      fromCastle Short
  | file (m^.to) == hFile && rank (m^.to) == oppFirstRank (m^.colour) =
      fromCastle Long
  | otherwise                                                         =
      fromCastle Short <> fromCastle Long
{-# INLINE castleRightsOpp #-}


------------------------------------------------------------------------------
isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == Pawn && vDist (m^.from) ( m^.to) > 1
{-# INLINE isDoubleAdvance #-}


------------------------------------------------------------------------------
-- | calculates hash based on the previous board
calcHash :: Board -> Move -> Word64
calcHash b m =
  b^.hash
  `xor` zPutPiece (m^.from) (m^.colour) (m^.piece)
  `xor` zPutPiece (m^.to) (m^.colour) (m^.piece)
  `xor` capturedZ `xor` castleZ `xor` castleRMZ `xor` enPassantZ
  `xor` enPassantRMZ `xor` promotionZ `xor` flipSideZ

  where
    flipSideZ = zobrist (ZobristSide White) `xor` zobrist (ZobristSide Black)

    capturedZ = maybe 0
                (zobrist . ZobristPiece (m^.to) (T.opponent $ m^.colour))
                (m^.capturedPiece)

    castleZ   = zobrist (ZobristCastlingRights
                         (b^.whiteCastleRights)
                         (b^.blackCastleRights))
                `xor` zobrist (ZobristCastlingRights
                               newWhiteCastle
                               newBlackCastle)

    newWhiteCastle = case m^.colour of
      White -> castleRightsMine m `intersect` (b^.whiteCastleRights)
      Black -> castleRightsOpp  m `intersect` (b^.whiteCastleRights)

    newBlackCastle = case m^.colour of      
      White -> castleRightsOpp  m `intersect` (b^.blackCastleRights)
      Black -> castleRightsMine m `intersect` (b^.blackCastleRights)

    castleRMZ =
      maybe 0 (\c ->
                zPutPiece (fst (rookCastlePos (b^.next) c))  (b^.next) Rook
                `xor`
                zPutPiece (snd (rookCastlePos (b^.next) c)) (b^.next) Rook
              ) (m^.castle)

    enPassantZ   = zobrist (ZobristEnPassant $ b^.enPassant)
                   `xor` zobrist (ZobristEnPassant $ if isDoubleAdvance m
                                                     then Just (m^.to)
                                                     else Nothing)

    enPassantRMZ = maybe 0
                   (\t -> zPutPiece t (T.opponent (b^.next)) Pawn)
                   (m^.enPassantTarget)

    promotionZ   = maybe 0
                   (\p -> zPutPiece (m^.to) (m^.colour) Pawn
                          `xor` zPutPiece (m^.to) (m^.colour) p)
                   (m^.promotion)
