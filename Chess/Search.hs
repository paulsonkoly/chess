{-# LANGUAGE TemplateHaskell #-}

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
                   { _board :: Board
                   , _tpc   :: TransPosCache
                   }

type SearchResult = ( [Move], Int )


$(makeLenses ''SearchState)


negaScout
  :: Int -- ^ depth
  -> State SearchState SearchResult
negaScout d = do
  c <- liftM (\b -> direction (b^.next) 1) $ use board
  mulM c $ negaScout' d (-inf) inf c


negaScout'
  :: Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> State SearchState SearchResult
negaScout' d alpha beta c = do
  mate <- liftM checkMate $ use board
  if mate
     then liftM (\b -> ([], c * evaluate b)) $ use board
     else if d == 0
          then quiscene alpha beta c
          else do
            ml <- liftM (\b -> zip (toList $ moves b) ([0, 1..] :: [Int])) (use board)
            go ([], alpha) ml

  where go l [] = trace "FIN" $ return l
        go (pv, alpha') ((m, i):ms) = trace ((if d == 4 then "info " else "") ++ (Prelude.replicate d '>') ++ (show alpha') ++ " PV : " ++ (unwords (map renderShortMove pv)) ++ "curr : " ++ (renderShortMove m)) $ do
          board %= execState (doMoveM m)
          (pv', score) <- addM m $ if i > 0
                                   then do
                                     (pv'', score') <- negM $ negaScout' (d - 1) (-alpha' - 1) (-alpha') (-c)
                                     if alpha' < score' && score' < beta
                                        then negM $ negaScout' (d - 1) (-beta) (-alpha') (-c)
                                        else return (pv'', score')
                                   else negM $ negaScout' (d - 1) (-beta) (-alpha') (-c)
          board %= execState (undoMoveM m)
          let (pv'', alpha'') = greater (pv', score) (pv, alpha')
          if alpha'' >= beta
            then trace "CUT" $ return (pv'', alpha'')
            else go (pv'', alpha'') ms
            
        
{-
--  st <- get
--  case transPosCacheLookUp (st^.board) d (st^.tpc) of
--    Just (cache', val) -> trace ("HIT!! " ++ show val) $ do
--      tpc .= cache'
--      return ([], val)
--    Nothing -> do
  val <- if d == 0
         then liftM (_2 %~ (c*)) $ quiscene alpha beta
         else do
           mate <- liftM checkMate (use board)
           if mate -- isTerminal (state^.board) 
              then liftM (\b -> ([], c * evaluate b)) (use board)
              else do
                 ml <- liftM (\l -> zip (toList $ moves l) ([0, 1..] :: [Int])) (use board)
                 go ([], alpha) ml
--  tpc %= transPosCacheInsert (st^.board) d (val^._2)
  return val

  where go (pv, alpha') ((m, i) : xs) = do
          board %= execState (doMoveM m)
          nRes <- addM m $ if i > 0
                          then do
                            (pv'', score') <- negM $ negaScout' (d - 1) (-alpha' - 1) (-alpha') (-c)
                            if alpha' < score' && score' < beta
                              then negM $ negaScout' (d - 1) (-beta) (-alpha') (-c)
                              else return (pv'', score')
                          else negM $ negaScout' (d - 1) (-beta) (-alpha') (-c)
          board %= execState (undoMoveM m)
          let tRes = greater nRes (pv, alpha')
          if snd tRes >= beta || null xs
            then return tRes
            else go tRes xs
-}

quiscene :: Int -> Int -> Int -> State SearchState SearchResult
quiscene alpha beta c = do
  standPat <- liftM ((c*) . evaluate) (use board)
  if standPat >= beta
    then return ([], beta)
    else do
       ml <- liftM (toList . forcingMoves) (use board)
       go ([], max alpha standPat) ml

  where go l [] = return l
        go (pv, alpha') (m:ms) = do
          board %= execState (doMoveM m)
          (pv', score) <- addM m $ negM $ quiscene (-beta) (-alpha') (-c)
          board %= execState (undoMoveM m)

          if score >= beta
            then return ([], beta)
            else go (greater (pv', score) (pv, alpha')) ms
             

greater :: SearchResult -> SearchResult -> SearchResult
greater (l1, v1) (l2, v2) = if v1 > v2 then (l1, v1) else (l2, v2)

inf :: Int
inf    = maxBound

negM   = mulM (-1)
mulM c = liftM (_2 %~ (c*))
addM m = liftM (_1 %~ (m:))
