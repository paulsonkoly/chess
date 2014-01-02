{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Search
       ( negaScout
       , SearchState(..)
       , board
       ) where

import Control.Monad.State
import Control.Lens
import Data.Foldable

import Chess.Board
import Chess.TransPosCache
import Chess.Evaluation
import Chess.Move

import Debug.Trace

data SearchState = SearchState
                   { _board   :: Board
                   , _tpc     :: TransPosCache
                   , _tpcHit  :: ! Int
                   , _tpcMiss :: ! Int
                   }

type SearchResult = ( [ Move ], Int )


$(makeLenses ''SearchState)


negaScout
  :: Int -- ^ depth
  -> State SearchState SearchResult
negaScout d = do
  c <- liftM (\b -> direction (b^.next) 1) $ use board
  mulM c $ negaScout' d d (-inf) inf c


withMove :: forall (m :: * -> *) b . MonadState SearchState m => Move -> m b -> m b
withMove m ac = do
  board %= execState (doMoveM m)
  result <- ac
  board %= execState (undoMoveM m)
  return result
{-# INLINE withMove #-}


withTransPosCache :: forall (m :: * -> *) . MonadState SearchState m => Bool -> Int -> m SearchResult -> m SearchResult
withTransPosCache cond d ac =
  if cond
    then do
      b <- use board
      t <- use tpc
      case transPosCacheLookUp b d t of
        Just (cache', val) -> do
          tpc .= cache'
          tpcHit += 1
          return ([], val)
        Nothing -> do
          tpcMiss += 1
          insert
    else insert
  where insert = do
          result <- ac
          b <- use board
          tpc %= transPosCacheInsert b d (snd result)
          return result
{-# INLINE withTransPosCache #-}


hitRatio :: SearchState -> Int
hitRatio st = let s = st^.tpcHit + st^.tpcMiss
              in if s == 0 then 0 else (st^.tpcHit * 100) `div` s
        

negaScout'
  :: Int -- ^ depth
  -> Int -- ^ max depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> State SearchState SearchResult
negaScout' d mx alpha beta c = withTransPosCache (d < mx) d $ do
  mate <- liftM checkMate $ use board
  if mate
    then liftM (\b -> ([], c * evaluate b)) $ use board
    else if d == 0
          then quiscene alpha beta c
          else do
            ml <- liftM (\b -> zip (toList $ moves b) ([0, 1..] :: [Int])) (use board)
            go ([], alpha) ml

  where go l [] = return l
        go (pv, alpha') ((m, i) : ms) = info alpha' pv m $ do
          (pv', score) <- withMove m $ addM m $ if i > 0
                                                then do
                                                  (pv'', score') <- negM $ negaScout' (d - 1) mx (-alpha' - 1) (-alpha') (-c)
                                                  if alpha' < score' && score' < beta
                                                    then negM $ negaScout' (d - 1) mx (-beta) (-alpha') (-c)
                                                    else return (pv'', score')
                                                else negM $ negaScout' (d - 1) mx (-beta) (-alpha') (-c)
          let (pv'', alpha'') = greater (pv', score) (pv, alpha')
          if alpha'' >= beta
            then return (pv'', alpha'')
            else go (pv'', alpha'') ms
        info a pv m ac = if d == mx
                         then get >>= \st -> trace ("info " ++ Prelude.replicate d '>'
                                                    ++  "TPC : " ++ show (hitRatio st) ++ "% "
                                                    ++ show a
                                                    ++ " PV : " ++ unwords (map renderShortMove pv)
                                                    ++ " curr : " ++ renderShortMove m) ac
                         else ac

            

quiscene :: Int -> Int -> Int -> State SearchState SearchResult
quiscene alpha beta c = withTransPosCache True 0 $ do
  standPat <- liftM ((c*) . evaluate) (use board)
  if standPat >= beta
    then return ([], beta)
    else do
       ml <- liftM (toList . forcingMoves) (use board)
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
