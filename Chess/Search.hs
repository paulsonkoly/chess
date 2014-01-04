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

type Search = State SearchState SearchResult


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


updateTransPosCache :: Int -> Int -> Int -> SearchResult -> Search
updateTransPosCache d alpha beta r@(pv, score) = let t
                                                       | score <= alpha = TPC.Lower
                                                       | score >= beta  = TPC.Upper
                                                       | otherwise      = TPC.Exact
                                                 in do
                                                   b <- use board
                                                   tpc %= TPC.transPosCacheInsert b d t r
                                                   return r
{-# INLINE updateTransPosCache #-}


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
negaScout' mx d alpha beta c = do
  mate <- liftM checkMate $ use board
  if mate
    then liftM (\b -> ([], c * evaluate b)) $ use board
    else if d == 0
          then quiscene alpha beta c
          else do
            ml <- liftM (map snd . Q.toDescList . moves) $ use board
            go ([], alpha) $ zip ml ([0, 1..] :: [Int])

  where go l [] = return l
        go (pv, alpha') ((m, i) : ms) = info alpha' pv m $ do
          (pv', score) <- withMove m $ addM m $ if i > 0
                                                then do
                                                  (pv'', score') <- negM $ negaScout' mx (d - 1) (-alpha' - 1) (-alpha') (-c)
                                                  if alpha' < score' && score' < beta
                                                    then negM $ negaScout' mx (d - 1) (-beta) (-alpha') (-c)
                                                    else return (pv'', score')
                                                else negM $ negaScout' mx (d - 1) (-beta) (-alpha') (-c)
          let (pv'', alpha'') = greater (pv', score) (pv, alpha')
          if alpha'' >= beta
            then return (pv'', alpha'')
            else go (pv'', alpha'') ms
        info a pv m ac = if d == mx
                         then get >>= \st -> trace ("info " ++ Prelude.replicate d '>'
                                                    ++  "TPC : " ++ show (hitRatio st) ++ "% "
                                                    ++ show a
                                                    ++ " PV : " ++ (renderVariation pv)
                                                    ++ " curr : " ++ renderShortMove m) ac
                         else ac

            

quiscene :: Int -> Int -> Int -> Search
quiscene alpha beta c = do
  standPat <- liftM ((c*) . evaluate) (use board)
  if standPat >= beta
    then return ([], beta)
    else do
       ml <- liftM (map snd . Q.toDescList . forcingMoves) (use board)
       go ([], max alpha standPat) ml

  where go l [] = return l
        go (pv, alpha') (m:ms) = do
          (pv', score) <- withMove m $ addM m $ negM $ quiscene (-beta) (-alpha') (-c)
          if score >= beta
            then return ([], beta)
            else go (greater (pv', score) (pv, alpha')) ms
             

greater :: SearchResult -> SearchResult -> SearchResult
greater (l1, v1) (l2, v2) = if v1 > v2 then (l1, v1) else (l2, v2)

inf :: Int
inf = maxBound

negM   = mulM (-1)
mulM c = liftM (_2 %~ (c*))
addM m = liftM (_1 %~ (m:))
