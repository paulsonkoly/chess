{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Chess.Search.Algorithm
       ( search
       ) where

------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (liftM, when)
import           Control.Monad.State       (get, liftIO)
import           Data.Foldable             (forM_)

import           Control.Lens              ((+=), (.=), (^.), (%=), use)
import           Data.Time.Clock           (getCurrentTime, diffUTCTime)

import           Chess.Board
import           Chess.Evaluation
import qualified Chess.Killer              as K
import           Chess.Move
import qualified Chess.PVStore             as PVS
import           Chess.Search.Search
import qualified Chess.Search.SearchResult as SR (moves)
import           Chess.Search.SearchResult hiding (moves)
import           Chess.Search.SearchState
import qualified Chess.TransPosCache       as TPC
import           Data.ChessTypes
import           Data.List.Extras



------------------------------------------------------------------------------
data LoopResult = Mere { score :: Int }
                | Cacheable
                  { score :: Int
                  , entry :: TPC.TransPosCacheEntryType
                  }


------------------------------------------------------------------------------
loopResultToSearchResult :: LoopResult -> SearchResult
loopResultToSearchResult (Mere i)                       = SearchResult [] i
loopResultToSearchResult (Cacheable i (TPC.Lower m))    = SearchResult [m] i
loopResultToSearchResult (Cacheable i (TPC.Exact m ms)) = SearchResult (m:ms) i
loopResultToSearchResult (Cacheable i TPC.Upper)        = SearchResult [] i


------------------------------------------------------------------------------
-- | top level search
search :: Search (Maybe SearchResult)
search = do
  tpcHit     .= 0
  tpcMiss    .= 0
  nCnt       .= 0
  tpc        %= TPC.transPosCacheDeflate
  pv         .= PVS.mkPVStore
  kill       .= K.mkKiller
  now        <- liftIO getCurrentTime
  clock      .= Just now
  b          <- use board
  let c = direction (b^.next) 1
  withIterativeDeepening 1 $ \d' -> do
    mbR <- (c*) <@> negaScout d' d' (-inf) inf c
    forM_ mbR $ \r -> pv .= PVS.insert (r^.SR.moves)
    return mbR


------------------------------------------------------------------------------
-- searches with iterative deepening
withIterativeDeepening
  :: Int                        -- ^ start depth
  -> (Int -> Search (Maybe a))  -- ^ search of given depth
  -> Search (Maybe a)
withIterativeDeepening d s = do
  report $ "depth " ++ show d
  r    <- s d
  mx   <- readMaxDepth
  maybe (return Nothing)
        (const $ if d >= mx
                 then return r
                 else withIterativeDeepening (d + 1) s)
        r


------------------------------------------------------------------------------
info :: Int -> SearchResult -> Search ()
info d sr = do
  st  <- get
  now <- liftIO getCurrentTime
  let Just backThen = st^.clock
      diffTimems    = truncate $ (10 ^ (3 :: Int))
                      * (now `diffUTCTime` backThen)
      s             = "depth "        ++ show d
                      ++ " score cp " ++ show (10 * sr^.eval)
                      ++ " nodes "    ++ show (st^.nCnt)
                      ++ " time "     ++ show (diffTimems :: Int)
                      ++ " pv "       ++ unwords (map renderShortMove
                                                  (sr^.SR.moves))
  report s


------------------------------------------------------------------------------
-- executes search action with a moved played
withMove :: Move -> Search a -> Search a
withMove m ac = do
  old <- use board
  board %= makeMove m
  r <- ac
  board .= old
  return r
{-# INLINE withMove #-}


------------------------------------------------------------------------------
-- executes a search action with a transpositional cache lookup
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
    
    Right (cache', e) -> do
      tpc .= cache'
      tpcHit += 1
      case e^.TPC.typ of

        TPC.Exact m ms -> let sr = SearchResult (m:ms) (e^.TPC.score)
                          in do
                            mx <- readMaxDepth
                            when (d == mx) (info d sr) >> return (Just sr)

        TPC.Lower m    -> let alpha' = max alpha $ e^.TPC.score
                          in if alpha' >= beta
                             then return $ Just $ SearchResult [] alpha'
                             else f alpha' beta $ Just m

        TPC.Upper      -> let beta' = min beta $ e^.TPC.score
                          in if alpha >= beta'
                             then return $ Just $ SearchResult [] alpha
                             else f alpha beta' Nothing
    Left mr -> do
      tpcMiss += 1
      f alpha beta mr
{-# INLINE withTransPosCache #-}


------------------------------------------------------------------------------
-- iterates with killer heuristics
withKiller
  :: [ Move ] -- ^ move list
  -> Int      -- ^ max depth
  -> Int      -- ^ depth
  -> ( [ Move ] -> Search (Maybe LoopResult))
  -> Search (Maybe LoopResult)
withKiller ml mx d f = do
  ml' <- liftM (K.heuristics (mx - d) ml) (use kill)
  mlr <- f ml'
  forM_ mlr $ \lr -> do
    case lr of
      Cacheable _ (TPC.Lower m) -> kill %= K.insert (mx - d) m
      _                         -> return ()
    kill %= K.clearLevel (mx - d + 1)
  return mlr
{-# INLINE withKiller #-}


------------------------------------------------------------------------------
-- iterates with PV store heuristics
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


------------------------------------------------------------------------------
-- iterates with a Maybe Move first
withMaybeMove
  :: [ Move ]   -- ^ move list
  -> Maybe Move -- ^ a good move if any ( from the tt )
  -> ([Move] -> Search (Maybe LoopResult))
  -> Search (Maybe LoopResult)
withMaybeMove ml (Just m) f = f (toFront m ml)
withMaybeMove ml Nothing  f = f ml
{-# INLINE withMaybeMove #-}


------------------------------------------------------------------------------
-- iterates with all store heuristics
withStores
  :: [ Move ]   -- ^ move list
  -> Maybe Move -- ^ a good move if any ( from the tt )
  -> Int        -- ^ max depth
  -> Int        -- ^ depth
  -> ( [Move] -> Search (Maybe LoopResult) )
  -> Search (Maybe LoopResult)
withStores ml mm mx d f =
  withKiller ml mx d $ \ml'    ->
  withPVStore ml' mx d $ \ml'' ->
  withMaybeMove ml'' mm f
{-# INLINE withStores #-}


------------------------------------------------------------------------------
-- iterates search on a move list
iterateMoves
  :: [ Move ]
  -> Int                                    -- ^ depth
  -> Int                                    -- ^ alpha
  -> Int                                    -- ^ beta
  -> Bool                                   -- ^ report info
  -> (Bool -> Move -> Int -> Int -> Search (Maybe SearchResult))
  -> Search (Maybe LoopResult)
iterateMoves ml d alpha beta rep ac = do
  mlr <- go (0::Int) (Cacheable alpha TPC.Upper) ml
  forM_ mlr $ \lr -> case lr of
    Cacheable s t -> do
      b <- use board
      tpc %= TPC.transPosCacheInsert b d s t
    _ -> return ()
  return mlr

  where go _ prev [] = return (Just prev)
        go n prev (m:ms) = abortable $ do
            let prevVal = score prev
            mnxt <- ac (n == 0) m prevVal beta
            maybe (return Nothing)
                  (\nxt -> if
                      
                       | nxt^.eval >= beta ->
                          return $ Just $ Cacheable beta (TPC.Lower m)

                       | nxt^.eval > prevVal ->
                          let this = if null (nxt^.SR.moves)
                                     then Mere (nxt^.eval)
                                     else Cacheable (nxt^.eval)
                                            (TPC.Exact (head $ nxt^.SR.moves)
                                                       (tail $ nxt^.SR.moves))
                          in when rep (info d nxt) >> go (n+1) this ms

                       | otherwise -> do
                          when rep $ report $ "currmove " ++ renderShortMove m
                             ++ " currmovenumber " ++ show n
                          go (n+1) prev ms)
                  mnxt

-- percentage :: Int -> Int -> Int
-- percentage a b = if a == 0 then 0 else 100 * b `div` a


-- hitRatio :: SearchState -> Int
-- hitRatio st = percentage ((st^.tpcHit) + (st^.tpcMiss)) (st^.tpcHit)


------------------------------------------------------------------------------
-- The negascout search with transpos cache and killer.
negaScout
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Search (Maybe SearchResult)
negaScout mx d alpha' beta' c =
  withTransPosCache d alpha' beta' $ \alpha beta mr -> do
    mate <- liftM (not . anyMove) $ use board
    if mate
      then do    
        nCnt += 1
        liftM (\b -> Just $ SearchResult [] $ c * evaluate b) $ use board
      else do
        ml <- liftM moves $ use board
        mr' <- withStores ml mr mx d $ \ml' ->
          iterateMoves ml' d alpha beta (mx == d) $ \f m a b ->
            withMove m $ m
              <++> if d == 1
                   then ((-1)*) <@> quiscene mx (d - 1) (-b) (-a) (-c) m
                   else
                     if not f
                     then do
                       mn <- ((-1)*) <@> negaScout mx (d - 1) (-a - 1) (-a) (-c)
                       maybe (return Nothing)
                         (\n ->
                           if a < n^.eval && n^.eval < b
                           then ((-1)*) <@> negaScout mx (d - 1) (-b) (-a) (-c)
                           else return mn)
                         mn
                     else ((-1)*) <@> negaScout mx (d - 1) (-b) (-a) (-c)
        return $ loopResultToSearchResult <$> mr'


------------------------------------------------------------------------------
-- quiscene search
quiscene
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Move
  -> Search (Maybe SearchResult)
quiscene mx d alpha' beta' c pm =
  withTransPosCache d alpha' beta' $ \alpha beta mr -> do
    standPat <- liftM ((c*) . evaluate) (use board)
    nCnt += 1
    brd <- use board
    if standPat >= beta
      then do
        tpc %= TPC.transPosCacheInsert brd d beta (TPC.Lower pm)
        return $ Just $ SearchResult [] beta
      else do
        let ml = if inCheck brd (brd^.next) && d > -10
                 then moves brd
                 else forcingMoves brd
            alpha'' = max alpha standPat
           
        mr' <- withStores ml mr mx d $ \ml' ->
          iterateMoves ml' d alpha'' beta False $ \ _ m a b ->
            withMove m $ m
              <++> (((-1)*) <@> quiscene mx (d - 1) (-b) (-a) (-c) m)
        return $ loopResultToSearchResult <$> mr'


------------------------------------------------------------------------------
inf :: Int
inf = maxBound
