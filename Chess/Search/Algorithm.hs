{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Chess.Search.Algorithm
       ( search
       ) where

import           Chess.Board
import qualified Chess.Board as B (hash)
import           Chess.Evaluation
import qualified Chess.Killer              as K
import           Chess.Move
import qualified Chess.Move                as M (calcHash)
import qualified Chess.PVStore             as PVS
import           Chess.Search.Search
import qualified Chess.Search.SearchResult as SR (moves)
import           Chess.Search.SearchResult hiding (moves)
import           Chess.Search.SearchState
import qualified Chess.Search.SearchState  as SS (hash)
import qualified Chess.TransPosCache       as TPC
import           Control.Applicative       ((<$>))
import           Control.Concurrent.STM    (readTVarIO)
import           Control.Lens              ((+=), (.=), (^.), (%=), makeLenses, use, view)
import           Control.Monad             (liftM, when)
import           Control.Monad.State       (get, liftIO)
import           Data.ChessTypes
import           Data.Foldable             (forM_)
import           Data.List.Extras
import           Data.Maybe                (fromJust, isJust)


data LoopResult = LoopResult
                  { _tpcEntryT :: ! TPC.TransPosCacheEntryType
                  , _line      :: ! SearchResult
                  }

$(makeLenses ''LoopResult)


-- | top level search
search
  :: Int -- ^ depth
  -> Search (Maybe SearchResult)
search d = do
  tpcHit   .= 0
  tpcMiss  .= 0
  nCnt     .= 0
--  tpc      %= TPC.transPosCacheDeflate
  mPonderHit <- use previous
  b          <- use board
  s <- if maybe False (\ph -> b^.B.hash == ph^.SS.hash) mPonderHit
       then do
         let ph = fromJust mPonderHit
         report $ "ponderhit @ " ++ (show $ ph^.depth) ++ " " ++ (renderVariation (ph^.result))
         pv  .= PVS.insert (ph^.result^.SR.moves)
         return $ (ph^.depth)
       else do
         report $ "pondermiss"
         return 1

  let c = direction (b^.next) 1
  r <- withIterativeDeepening d s $ \d' -> do
       r' <- (c*) <@> negaScout d' d' (-inf) inf c
       forM_ r' $ \r'' -> do
         pv .= PVS.insert (r''^.SR.moves)
         case M.calcHash b <$> (first r'') of
           Just h -> do
                   previous .= (Just $ Previous
                                { _depth = d' - 2
                                , _result = shift r''
                                , _hash = h
                                })
           Nothing -> return ()
       return r'
  return r


-- | searches with iterative deepening
withIterativeDeepening
  :: Int                        -- ^ max depth
  -> Int                        -- ^ start depth
  -> (Int -> Search (Maybe a))  -- ^ search of given depth
  -> Search (Maybe a)
withIterativeDeepening mx d s = do
  report $ "depth " ++ show d
  r <- s d
  maybe (return Nothing)
        (const $ if d >= mx
                 then return r
                 else withIterativeDeepening mx (d + 1) s)
        r


-- | executes search action with a moved played
withMove :: Move -> Search a -> Search a
withMove m ac = do
  old <- use board
  board %= makeMove m
  r <- ac
  board .= old
  return r
{-# INLINE withMove #-}


-- | executes a search action with a transpositional cache lookup
withTransPosCache
  :: Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> (Int -> Int -> Maybe Move -> Search (Maybe SearchResult))
  -> Search (Maybe SearchResult)
withTransPosCache d alpha beta f = do
  b <- use board
  t <- use tpc

  case TPC.transPosCacheLookUp b d t of
    
    Right (cache', entry) -> do
      tpc .= cache'
      tpcHit += 1
      case entry^.TPC.typ of

        TPC.Exact -> return $ Just $ entry^.TPC.result

        TPC.Lower -> let alpha' = max alpha $ entry^.TPC.result^.eval
                     in if alpha' >= beta
                        then return $ Just $ SearchResult [] alpha'
                        else f alpha' beta $ first $ entry^.TPC.result

        TPC.Upper -> let beta' = min beta $ entry^.TPC.result^.eval
                     in if alpha >= beta'
                        then return $ Just $ SearchResult [] alpha
                        else f alpha beta' $ first $ entry^.TPC.result
    Left mr -> do
      tpcMiss += 1
      f alpha beta mr
{-# INLINE withTransPosCache #-}


-- | iterates with killer heuristics
withKiller
  :: [ Move ] -- ^ move list
  -> Int      -- ^ max depth
  -> Int      -- ^ depth
  -> ( [ Move ] -> Search (Maybe LoopResult))
  -> Search (Maybe LoopResult)
withKiller ml mx d f = do
  ml' <- liftM (K.heuristics (mx - d) ml) (use kill)
  mlr <- f ml'
  when (isJust mlr) $ do
    let Just (LoopResult t res) = mlr
    when (t == TPC.Lower) $ kill %= K.insert (mx - d) (first res)
    kill %= K.clearLevel (mx - d + 1)
  return mlr
{-# INLINE withKiller #-}


-- | iterates with PV store heuristics
withPVStore
  :: [ Move ] -- ^ move list
  -> Int      -- ^ max depth
  -> Int      -- ^ depth
  -> ( [Move] -> Search (Maybe LoopResult ))
  -> Search (Maybe LoopResult)
withPVStore ml mx d f = do
  ml' <- liftM (PVS.heuristics (mx - d) ml) (use pv)
  r <- f ml'
  -- after the leftmost iteration on the level truncate the PV up to the level
  pv %= PVS.clearUpTo (mx - d)
  return r
{-# INLINE withPVStore #-}


-- | iterates with a Maybe Move first
withMaybeMove
  :: [ Move ]   -- ^ move list
  -> Maybe Move -- ^ a good move if any ( from the tt )
  -> ([Move] -> Search (Maybe LoopResult))
  -> Search (Maybe LoopResult)
withMaybeMove ml (Just m) f = f (toFront m ml)
withMaybeMove ml Nothing  f = f ml
{-# INLINE withMaybeMove #-}


-- | iterates with all store heuristics
withStores
  :: [ Move ]   -- ^ move list
  -> Maybe Move -- ^ a good move if any ( from the tt )
  -> Int        -- ^ max depth
  -> Int        -- ^ depth
  -> ( [Move] -> Search (Maybe LoopResult) )
  -> Search (Maybe LoopResult)
withStores ml mm mx d f = withKiller ml mx d $ \ml' -> withPVStore ml' mx d $ \ml'' -> withMaybeMove ml'' mm f
{-# INLINE withStores #-}


-- | iterates search on a move list
iterateMoves
  :: [ Move ]
  -> Int                                    -- ^ depth
  -> Int                                    -- ^ alpha
  -> Int                                    -- ^ beta
  -> Bool                                   -- ^ report info
  -> (Bool -> Move -> Int -> Int -> Search (Maybe SearchResult)) -- ^ True is fed in in the first iteration (for negascout)
  -> Search (Maybe LoopResult)
iterateMoves ml d alpha beta rep ac = do
  mlr <- go True (LoopResult TPC.Upper $ SearchResult [] alpha) ml
  case mlr of
    Just lr -> do
        b  <- use board
        tpc %= TPC.transPosCacheInsert b d (lr^.tpcEntryT) (lr^.line)
    Nothing -> return ()
  return $ mlr

  where go _ l [] = return $ Just l
        go f prev (m:ms) = do
          abortedtv <- use aborted
          abort     <- liftIO $ readTVarIO abortedtv
          if abort
          then return Nothing
          else do
            let prevVal = prev^.line^.eval
            mnxt <- ac f m prevVal beta
            maybe (return Nothing)
                  (\nxt -> if
                       | nxt^.eval >= beta   -> return $ Just $ LoopResult TPC.Lower $ SearchResult [m] beta
                       | nxt^.eval > prevVal -> info nxt m >> go False (LoopResult TPC.Exact nxt) ms
                       | otherwise           -> info (prev^.line) m >> go False prev ms)
                  mnxt

        info sr m = when rep $ do
                      st <- get
                      let s =  "TPC : " ++ show (hitRatio st) ++ "% "
                               ++ show (st^.nCnt `div` 1000) ++ "kn "
                               ++ " PV : " ++ renderVariation sr
                               ++ " curr : " ++ renderShortMove m
                      report $ s

percentage :: Int -> Int -> Int
percentage a b = if a == 0 then 0 else 100 * b `div` a


hitRatio :: SearchState -> Int
hitRatio st = percentage ((st^.tpcHit) + (st^.tpcMiss)) (st^.tpcHit)


-- | The negascout search with transpos cache and killer.
negaScout
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Search (Maybe SearchResult)
negaScout mx d alpha' beta' c = withTransPosCache d alpha' beta' $ \alpha beta mr -> do
  mate <- liftM (not . anyMove) $ use board
  if
    | mate -> do
      nCnt += 1
      liftM (\b -> Just $ SearchResult [] $ c * evaluate b) $ use board

    | d == 0 -> quiscene mx d alpha beta c

    | otherwise -> do
       ml <- liftM moves $ use board

       mr' <- withStores ml mr mx d
              $ \ml' -> iterateMoves ml' d alpha beta (mx == d)
                        $ \f m a b -> withMove m
                                      $ m <++>
                                        if not f
                                        then do
                                          mn <- ((-1)*) <@> negaScout mx (d - 1) (-a - 1) (-a) (-c)
                                          maybe (return Nothing)
                                                (\n -> if a < n^.eval && n^.eval < b
                                                       then ((-1)*) <@> negaScout mx (d - 1) (-b) (-a) (-c)
                                                       else return mn)
                                                mn

                                        else ((-1)*) <@> negaScout mx (d - 1) (-b) (-a) (-c)
       return $ (view line) <$> mr'


-- | quiscene search
quiscene
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Search (Maybe SearchResult)
quiscene mx d alpha' beta' c = withTransPosCache 0 alpha' beta' $ \alpha beta mr -> do
  standPat <- liftM ((c*) . evaluate) (use board)
  nCnt += 1
  if standPat >= beta
    then do
       b  <- use board
       tpc %= TPC.transPosCacheInsert b 0 TPC.Lower (SearchResult [] beta)
       return $ Just $ SearchResult [] beta
    else do
       ml <- liftM forcingMoves $ use board

       let alpha'' = max alpha standPat
       mr' <- withStores ml mr mx d
              $ \ml' ->  iterateMoves ml' 0 alpha'' beta False
                         $ \ _ m a b -> withMove m $ m <++> (((-1)*) <@> quiscene mx (d - 1) (-b) (-a) (-c))
       return $ (view line) <$> mr'


inf :: Int
inf = maxBound
