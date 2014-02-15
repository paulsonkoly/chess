{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Move
   ( Move
   , SearchResult
   , MoveQueue
   -- * Constructor
   , moves
   , forcingMoves
   -- * Lenses
   , from
   , to
   , piece
   , promotion
   , capturedPiece
   , enPassantTarget
   , castle
   -- * Stateful do move
   , doMoveM
   , undoMoveM
   -- * Parser
   , parserMove
   -- * utils
   , attacking
   , direction
   , checkMate
   , staleMate
   , renderShortMove
   )
   where

import           Control.Lens hiding (from, to, op, at, (|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.PQueue.Max as Q
import           Data.List
import           Data.Functor
import qualified Data.Foldable as F

import           Text.ParserCombinators.Parsec

import qualified Chess as C

import           Chess.Board
import           Chess.Magic
import           Chess.Zobrist
import           Data.BitBoard
import           Data.Square
import           Data.ChessTypes
import           Control.Extras


data Move = Move
            { _from            :: ! Int
            , _to              :: ! Int
            , _piece           :: ! C.PieceType
            , _colour          :: ! C.Color
            , _promotion       :: ! (Maybe C.PieceType)
            , _capturedPiece   :: ! (Maybe C.PieceType)
            , _enPassantTarget :: ! (Maybe Int)
            , _castle          :: ! (Maybe Castle)
            } deriving Show


instance Eq Move  where _ == _      = True
instance Ord Move where compare _ _ = EQ


defaultMove :: Int -> Int -> C.PieceType -> C.Color -> Move
defaultMove f t pt c = Move f t pt c Nothing Nothing Nothing Nothing


$(makeLenses ''Move)


type MoveQueue    = Q.MaxQueue (Int, Move)
type SearchResult = ( [ Move ], Int )


direction :: C.Color -> Int -> Int
direction C.White = id
direction C.Black = (* (-1))
{-# INLINE direction #-}


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && (9 < abs (m^.from - m^.to))
{-# INLINE isDoubleAdvance #-}


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


vacancyCastleBB :: C.Color -> Castle -> BitBoard
vacancyCastleBB C.White Long  = fromPositionList [ 1 .. 3 ]
vacancyCastleBB C.White Short = fromPositionList [ 5, 6 ]
vacancyCastleBB C.Black Long  = fromPositionList [ 57 .. 59 ]
vacancyCastleBB C.Black Short = fromPositionList [ 61, 62 ]
{-# INLINE vacancyCastleBB #-}


checkCastleBB :: C.Color -> Castle -> BitBoard
checkCastleBB C.White Long  = fromPositionList [ 2 .. 4 ]
checkCastleBB C.White Short = fromPositionList [ 4 .. 6 ]
checkCastleBB C.Black Long  = fromPositionList [ 58 .. 60 ]
checkCastleBB C.Black Short = fromPositionList [ 60 .. 62 ]
{-# INLINE checkCastleBB #-}


kingCastleMove :: C.Color -> Castle -> (Int, Int)
kingCastleMove C.White Long  = (4, 2)
kingCastleMove C.White Short = (4, 6)
kingCastleMove C.Black Long  = (60, 58)
kingCastleMove C.Black Short = (60, 62)
{-# INLINE kingCastleMove #-}


castleRights :: Move -> [ Castle ]
castleRights m
  | m^.piece == C.King                               = []
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 0) = [ Short ]
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 7) = [ Long  ]
  | otherwise                                        = [ Short, Long ]
{-# INLINE castleRights #-}


flipMoveM :: (MonadState Board m) => Move -> m ()
flipMoveM m = do
  let (f, t) = (fromBB m, toBB m)
      ft     = f `xor` t
  nxt <- use next
  modify (putPiece ft nxt (m^.piece))
  doOnJust (m^.capturedPiece)   $ \c -> modify $ putPiece t (opponent' nxt) c
  doOnJust (m^.promotion)       $ \p -> modify $ promote t p
  doOnJust (m^.enPassantTarget) $ \e -> modify $ putPiece (bit e) (opponent' nxt) C.Pawn
  doOnJust (m^.castle)          $ \c -> modify $ putPiece (rookCaslteBB nxt c) nxt C.Rook


recalcHash :: (MonadState Board m) => m ()
recalcHash = let val b = foldr1 xor [ zobrist $ ZobristPiece i (fromJust $ pieceColourAt b i) (fromJust $ pieceAt b i) 
                                    | i <- [0 .. 63]
                                    , pt <- [ pieceAt b i ], isJust pt
                                    , pc <- [ pieceColourAt b i ], isJust pc
                                    ]
                         `xor` zobrist (ZobristSide $ b^.next)
                         `xor` zobrist (ZobristCastlingRights (head $ b^.whiteCastleRights) (head $ b^.blackCastleRights))
                         `xor` zobrist (ZobristEnPassant $ head $ b^.enPassant)
             in get >>= \b -> hash .= val b



-- | makes a @Move@ on a @Board@
doMoveM :: (MonadState Board m) => Move -> m ()
doMoveM m = do
  flipMoveM m
  nxt <- use next
  castleRightsByColour nxt %= (\prev -> (head prev `intersect` castleRights m) : prev)
  next                     %= opponent'
  enPassant                %= ((:) $ if isDoubleAdvance m then Just (m^.to) else Nothing)
  recalcHash


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM m = do
  next                     %= opponent'
  flipMoveM m
  enPassant                %= tail
  nxt <- use next
  castleRightsByColour nxt %= tail
  recalcHash


check b c m = let b' = execState (doMoveM m) b
              in  inCheck b' c


inCheck b c = let kP = head $ toList $ piecesOf b c C.King
              in isAttacked b (opponent' c) kP


-- | Sequence of legal moves
moves :: Board -> MoveQueue
moves b = Q.filter (not . check b (b^.next) . snd) $ foldl1 Q.union [f b | f <- [ pawnMoves, regularMoves, castleMoves ]]


-- | Sequence of captures
forcingMoves :: Board -> MoveQueue
forcingMoves b = let cptr m = bit (m^.to) .&. occupancy b /= mempty
                     chk  m = check b (b^.opponent) m || inCheck b (b^.next)
                 in Q.filter (\(_, m) -> cptr m || chk m) $ moves b


checkMate :: Board -> Bool
checkMate b = inCheck b (b^.next) && Q.null (moves b)


staleMate :: Board -> Bool
staleMate b = not (inCheck b (b^.next)) && Q.null (moves b)


pawnMoves :: Board -> MoveQueue
pawnMoves b = pawnEnPassant b `Q.union` captures `Q.union` advances
  where promotable m = if m^.to <= 7 || m^.to >= 56
                       then do
                          promo <- [ C.Queen, C.Rook, C.Knight, C.Bishop ]
                          return $ (promotion .~ Just promo) m
                       else [ m ]
        conv l = moveListToQueue $ concatMap promotable l
        captures = conv $ (\(f, t) -> (capturedPiece .~ pieceAt b t) $ defaultMove f t C.Pawn (b^.next)) <$> pawnCaptures b
        advances = conv $ (\(f, t) -> defaultMove f t C.Pawn (b^.next))                                  <$> pawnAdvances b


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
pawnCaptures :: Board -> [ (Square, Square) ]
pawnCaptures b = do
  let myPawns = myPiecesOf b C.Pawn
      -- files from which we can left/right capture
      cFiles 7 C.White = complement $ fileBB 0
      cFiles 9 C.White = complement $ fileBB 7
      cFiles 7 C.Black = complement $ fileBB 7
      cFiles 9 C.Black = complement $ fileBB 0      
  capture <- [7, 9]  -- left and right capture
  target  <- toList $ opponentsPieces b .&. ((myPawns .&. cFiles capture (b^.next)) `shift` direction (b^.next) capture)
  return (target - direction (b^.next) capture, target)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvances :: Board -> [ (Square, Square) ]
pawnAdvances b = do
  step <- [ 1, 2 ]
  let myPawns'  = myPiecesOf b C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ 31 - direction (b^.next) 23
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, pawn + step * direction (b^.next) 8)
  

pawnEnPassant :: Board -> MoveQueue
pawnEnPassant b = moveListToQueue $ flip (maybe []) (join $ listToMaybe $ b^.enPassant) $ \square ->
  let bb = (bit (square + 1) .|. bit (square - 1)) .&. neighbourFilesBB (square .&. 7) .&. myPiecesOf b C.Pawn
  in do
    pawn <- toList bb
    return
      $ (enPassantTarget .~ Just square)
      $ defaultMove pawn (square + direction (b^.next) 8) C.Pawn $ b^.next


castleMoves :: Board -> MoveQueue
castleMoves b = moveListToQueue $ do
  side <- head $ b^.castleRightsByColour (b^.next)
  let (f, t)    = kingCastleMove (b^.next) side
      checkSqrs = toSeq $ checkCastleBB (b^.next) side
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.opponent)) checkSqrs
  return $ (castle .~ Just side) $ defaultMove f t C.King $ b^.next


-- | non pawn moves nor castles
regularMoves :: Board -> MoveQueue
regularMoves b = moveListToQueue $ do
  pt     <- [ C.Knight, C.Bishop, C.Rook,  C.Queen, C.King ]
  move   <- attacking pt b (b^.next)
  target <- toList $ move^._2
  return $ (capturedPiece .~ pieceAt b target) $ defaultMove (move^._1) target pt $ b^.next


  
  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking :: C.PieceType -> Board -> C.Color -> [ (Int, BitBoard) ]
attacking pt b c =
  let pcs     = toList $ piecesOf b c pt
      notme   = complement $ b^.piecesByColour c
      att pos = (pos, notme .&. m pos)
      m pos   = case pt of
        C.Queen  -> magic C.Queen pos (occupancy b)
        C.Rook   -> magic C.Rook pos (occupancy b)
        C.Bishop -> magic C.Bishop pos (occupancy b)
        C.Knight -> knightAttackBB pos
        C.King   -> kingAttackBB pos
        C.Pawn   -> undefined
  in case pt of
    C.Pawn -> undefined
    _      -> att <$> pcs


-- | the bitboard from where the piece type of the given colour is attacking the specified position
attackedBy :: C.PieceType -> Board -> C.Color -> Int -> BitBoard
attackedBy C.Queen b c pos = magic C.Queen pos (occupancy b) .&. piecesOf b c C.Queen

attackedBy C.Bishop b c pos = magic C.Bishop pos (occupancy b) .&. piecesOf b c C.Bishop

attackedBy C.Rook b c pos = magic C.Rook pos (occupancy b) .&. piecesOf b c C.Rook

attackedBy C.Knight b c pos = knightAttackBB pos .&. piecesOf b c C.Knight

attackedBy C.King b c pos   = kingAttackBB pos  .&. piecesOf b c C.King
-- TODO : en passant
attackedBy C.Pawn b c pos   = let mask  = neighbourFilesBB (pos .&. 7) .&. piecesOf b c C.Pawn
                              in (bit (pos - direction c 7) .|. bit (pos - direction c 9)) .&. mask


-- | are any of the given player's pieces attacking the given square?
-- isAttacked :: Board -> C.Color -> Int -> Bool
isAttacked :: Board -> C.Color -> Int -> Bool
isAttacked b c pos = any (/= mempty)
                     [ attackedBy pt b c pos
                     | pt <- [ C.Queen, C.Bishop, C.Rook, C.Knight, C.King, C.Pawn ]
                     ]


renderShortMove :: Move -> String
renderShortMove m = showSquare (m^.from) ++ showSquare (m^.to) ++ showPromotion (m^.promotion)
  where
    showSquare sq = (['a' .. 'h'] !! (sq .&. 7)) : show (1 + (sq `shiftR` 3))
    showPromotion (Just C.Queen) = "q"
    showPromotion (Just C.Knight) = "n"
    showPromotion (Just C.Rook) = "r"
    showPromotion (Just C.Bishop) = "b"
    showPromotion _ = ""


-- | On a given board parses an UCI protocol style move notation into Move
parserMove :: Board -> Parser Move
parserMove b = do
  f <- parserSquare
  t <- parserSquare
  promotionCh <- optionMaybe $ oneOf "qrbn"
  let promo = charToPt <$> promotionCh
      enp   = if Just C.Pawn == pieceAt b f && isNothing (pieceAt b t) && abs (f - t) .&. 7 /= 0
              then head $ b^.enPassant
              else Nothing
      cstl  = if Just C.King == pieceAt b f && abs (f - t) == 2
              then Just $ if f - t == 2 then Long else Short
              else Nothing
  return
    $ (promotion .~ promo)
    $ (capturedPiece .~ pieceAt b t)
    $ (enPassantTarget .~ enp)
    $ (castle .~ cstl)
    $ defaultMove f t (fromJust $ pieceAt b f) $ b^.next
  where
    charToPt 'q' = C.Queen
    charToPt 'r' = C.Rook
    charToPt 'b' = C.Bishop
    charToPt 'n' = C.Knight


moveListToQueue :: [ Move ] -> MoveQueue
moveListToQueue = Q.fromList . map (\m -> (moveValue m $ m^.colour, m))


-- | heuristic value of a Move
--
-- This heuristic controls the order of the move generator.
moveValue :: Move -> C.Color -> Int
moveValue m c = promo + check + capture + position
  where
    -- or maybe 0 (\p -> pieceValue p - pieceValue m^.piece)
    promo    = maybe 0 pieceValue $ m^.promotion
    check    = 0 -- ???
    capture  = maybe 0 pieceValue $ m^.capturedPiece
    position = positionValue (m^.piece) c (m^.to) - positionValue (m^.piece) c (m^.from)


-- | the number of possible moves from the square
--
-- the penalty for weakening the king position is here for Pawns (for now),
-- by overvalueing the f2, g3, h2 Pawns. Also The central Pawns are overvalued.
--
-- King safety means overvalueing the castle squares
-- 
-- see http://chessprogramming.wikispaces.com/Influence+Quantity+of+Pieces
positionValue :: C.PieceType -> C.Color -> Square -> Int
positionValue C.Pawn C.White sq = V.fromList [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
                                             , 3 , 4 , 4 , 4 , 4 , 6 , 6 , 6
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
                                             ] ! sq

positionValue C.Pawn C.Black sq = positionValue C.Pawn C.White $ (56 - (sq .&. 56)) .|. (sq .&. 7)


positionValue C.Knight _ sq = V.fromList [ 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
                                         , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
                                         , 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
                                         ] ! sq


positionValue C.King C.White sq = V.fromList [ 3 , 5 , 10 , 5 , 5 , 5 , 10 , 3 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 3 , 5 , 5 , 5 , 5 , 5 , 5 , 3
                                             ] ! sq

positionValue C.King C.Black sq = positionValue C.King C.White $ (56 - (sq .&. 56)) .|. (sq .&. 7)


positionValue C.Bishop _ sq = V.fromList [ 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 
                                         , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7 
                                         , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
                                         , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7
                                         , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7
                                         ] ! sq

positionValue C.Rook _ _ = 14


positionValue C.Queen _ sq = V.fromList [ 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
                                        , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
                                        , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
                                        , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
                                        , 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
                                        ] ! sq
                             


