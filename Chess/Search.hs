{-# LANGUAGE TemplateHaskell  #-}

module Chess.Search
       ( negaScout
       , SearchState(..)
       , board
       ) where

import qualified Data.PQueue.Max as Q

import           Control.Monad.State
import           Control.Lens

import           Chess.Board
import qualified Chess.TransPosCache as TPC
import           Chess.TransPosCache (result, typ)
import           Chess.Evaluation
import           Chess.Move

import Debug.Trace

data SearchState = SearchState
                   { _board   :: Board
                   , _tpc     :: TPC.TransPosCache
                   , _tpcHit  :: ! Int
                   , _tpcMiss :: ! Int
                   }

type Search  = State SearchState SearchResult
type Iterate = (TPC.TransPosCacheEntryType, SearchResult)


$(makeLenses ''SearchState)


negaScout
  :: Int -- ^ depth
  -> Search
negaScout d = do
  c <- liftM (\b -> direction (b^.next) 1) $ use board
  mulM c $ negaScout' d d (-inf) inf c


withMove :: Move -> Search -> Search
withMove m ac = do
  board %= execState (doMoveM m)
  r <- ac
  board %= execState (undoMoveM m)
  return r
{-# INLINE withMove #-}


renderVariation :: [ Move ] -> String
renderVariation = unwords . map renderShortMove


tryTransPosCache
  :: Bool                     -- ^ condition
  -> Int -> Int -> Int -> Int -- ^ depth / alpha / beta / colour
  -> (Int -> Int -> Int -> Int -> Search)
  -> Search
tryTransPosCache cond d alpha beta c f =
  if cond
    then do
      b <- use board
      t <- use tpc
      case TPC.transPosCacheLookUp b d t of
        Just (cache', entry) -> do
          tpc .= cache'
          tpcHit += 1
          case entry^.typ of
            TPC.Exact -> return $ entry^.result
            TPC.Lower -> let alpha' = max alpha $ snd $ entry^.result
                         in if alpha' >= beta
                            then return ([], alpha')
                            else f d alpha' beta c
            TPC.Upper -> let beta' = min beta $ snd $ entry^.result
                         in if alpha >= beta'
                            then return ([], alpha)
                            else f d alpha beta' c
        Nothing -> do
          tpcMiss += 1
          f d alpha beta c
    else f d alpha beta c
{-# INLINE tryTransPosCache #-}


updateTransPosCache :: Int -> Iterate -> Search
updateTransPosCache d (t, r) = do
  b <- use board
  tpc %= TPC.transPosCacheInsert b d t r
  return r
{-# INLINE updateTransPosCache #-}


iterateMoves
  :: [ Move ]
  -> Int                                    -- ^ alpha
  -> Int                                    -- ^ beta
  -> Bool                                   -- ^ Report
  -> (Bool -> Move -> Int -> Int -> Search) -- ^ True is fed in in the first iteration (for negascout)
  -> State SearchState Iterate
iterateMoves ml alpha beta rep ac = go True (TPC.Upper, ([], alpha)) ml
  where go _ l [] = return l
        go f p@(_, (_, a)) (m:ms) = do
          n@(pv, score) <-  ac f m a beta
          let cont
                | a     >= beta = return (TPC.Upper, ([], a))
                | score >= beta = return (TPC.Lower, ([], beta))
                | score > a     = info score pv m $ go False (TPC.Exact, n) ms
                | otherwise     = go False p ms
          cont
        info a pv m ac' = if rep
                          then get >>= \st -> trace ("info TPC : " ++ show (hitRatio st) ++ "% "
                                                    ++ show a
                                                    ++ " PV : " ++ renderVariation pv
                                                    ++ " curr : " ++ renderShortMove m) ac'
                          else ac'
            



hitRatio :: SearchState -> Int
hitRatio st = let s = st^.tpcHit + st^.tpcMiss
              in if s == 0 then 0 else (st^.tpcHit * 100) `div` s
        

negaScout'
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Search
negaScout' mx d' alpha' beta' c' = tryTransPosCache (d' /= mx) d' alpha' beta' c' $ \d alpha beta c -> do
  mate <- liftM checkMate $ use board
  if mate
    then liftM (\b -> ([], c * evaluate b)) $ use board
    else if d == 0
          then quiscene alpha beta c
          else do
            ml <- liftM (map snd . Q.toDescList . moves) $ use board
            r <- iterateMoves ml alpha beta (mx == d) $ \f m a b ->
              withMove m $ addM m $ if f
                                    then do
                                      n@(_, score) <- negM $ negaScout' mx (d - 1) (-a - 1) (-a) (-c)
                                      if a < score && score < b
                                        then negM $ negaScout' mx (d - 1) (-b) (-a) (-c)
                                        else return n
                                    else negM $ negaScout' mx (d - 1) (-b) (-a) (-c)
            updateTransPosCache d r
            

quiscene :: Int -> Int -> Int -> Search
quiscene alpha' beta' c' = tryTransPosCache True 0 alpha' beta' c' $ \_ alpha beta c -> do
  standPat <- liftM ((c*) . evaluate) (use board)
  if standPat >= beta
    then updateTransPosCache 0 (TPC.Lower, ([], beta))
    else do
       ml <- liftM (map snd . Q.toDescList . forcingMoves) (use board)
       let mx = max alpha standPat
       r  <- iterateMoves ml mx beta False $ \ _ m a b -> withMove m $ addM m $ negM $ quiscene (-b) (-a) (-c)
       updateTransPosCache 0 r
             

inf :: Int
inf = maxBound


negM   = mulM (-1)
mulM c = liftM (_2 %~ (c*))
addM m = liftM (_1 %~ (m:))
