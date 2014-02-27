module Chess.Move.ExecMove
       ( makeMove
       ) where

import           Control.Lens hiding (to, from)
import           Data.List
import           Data.Word
import           Data.Maybe

import qualified Chess as C
import           Data.BitBoard
import           Chess.Move.Move
import           Chess.Board
import           Chess.Zobrist

makeMove :: Move -> Board -> Board
makeMove m b = let (f, t) = (fromBB m, toBB m)
                   ft     = f `xor` t
                   nxt    = b^.next
                   mvsTrs = maybe id (\c -> putPiece (rookCaslteBB (b^.next) c) (b^.next) C.Rook) (m^.castle)
                            . maybe id (\e -> putPiece (bit e) (b^.opponent) C.Pawn)  (m^.enPassantTarget)
                            . maybe id (promote t) (m^.promotion)
                            . maybe id (putPiece t $ b^.opponent) (m^.capturedPiece)
                            . (putPiece ft nxt (m^.piece))
                   cstlTrs = castleRightsByColour (b^.next) %~ intersect (castleRights m)
                   nxtTrs  = next %~ opponent'
                   enpTrs  = enPassant .~ if isDoubleAdvance m then Just (m^.to) else Nothing
                   hshTrs  = hash .~ calcHash b
               in hshTrs $ enpTrs $ nxtTrs $ cstlTrs $ mvsTrs b


fromBB :: Move -> BitBoard
fromBB = bit . (^.from)
{-# INLINE fromBB #-}


toBB :: Move -> BitBoard
toBB = bit . (^.to)
{-# INLINE toBB #-}


putPiece :: BitBoard -> C.Color -> C.PieceType -> Board -> Board 
putPiece bb c pt = (piecesByColour c %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE putPiece #-}


promote :: BitBoard -> C.PieceType -> Board -> Board
promote bb pt = (pawns %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE promote #-}

rookCaslteBB :: C.Color -> Castle -> BitBoard
rookCaslteBB C.White Long  = fromPositionList [ 0,  3  ]
rookCaslteBB C.White Short = fromPositionList [ 5,  7  ]
rookCaslteBB C.Black Long  = fromPositionList [ 56, 59 ]
rookCaslteBB C.Black Short = fromPositionList [ 61, 63 ]
{-# INLINE rookCaslteBB #-}


castleRights :: Move -> [ Castle ]
castleRights m
  | m^.piece == C.King                               = []
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 0) = [ Short ]
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 7) = [ Long  ]
  | otherwise                                        = [ Short, Long ]
{-# INLINE castleRights #-}


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && (9 < abs (m^.from - m^.to))
{-# INLINE isDoubleAdvance #-}


calcHash :: Board -> Word64
calcHash b = foldr1 xor [ zobrist $ ZobristPiece i (fromJust $ pieceColourAt b i) (fromJust $ pieceAt b i) 
                        | i <- [0 .. 63]
                        , pt <- [ pieceAt b i ], isJust pt
                        , pc <- [ pieceColourAt b i ], isJust pc
                        ]
             `xor` zobrist (ZobristSide $ b^.next)
             `xor` zobrist (ZobristCastlingRights (b^.whiteCastleRights) (b^.blackCastleRights))
             `xor` zobrist (ZobristEnPassant $ b^.enPassant)
