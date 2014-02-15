{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Search
       ( negaScout
       , Search(..)
       , SearchState(..)
       , board
       ) where

import qualified Data.PQueue.Max as Q
import           Data.Maybe

import           Control.Monad.State
import           Control.Lens

import           Chess.Board
import qualified Chess.TransPosCache as TPC
import           Chess.TransPosCache (result, typ)
import           Chess.Evaluation
import           Chess.Move

data SearchState = SearchState
                   { _board   :: Board
                   , _tpc     :: TPC.TransPosCache
                   , _tpcHit  :: ! Int
                   , _tpcMiss :: ! Int
                   }


$(makeLenses ''SearchState)                   


newtype Search a = Search { runSearch :: StateT SearchState IO a }


instance Monad Search where
  return = Search . return
  (Search a) >>= f = Search $ a >>= runSearch . f


instance MonadIO Search where
  liftIO = Search . liftIO


instance MonadState SearchState Search where
  get = Search get
  put = Search . put
    

type Iterate = (TPC.TransPosCacheEntryType, SearchResult)


negaScout
  :: Int -- ^ depth
  -> Search SearchResult
negaScout d = do
  c <- liftM (\b -> direction (b^.next) 1) $ use board
  mulM c $ negaScout' d d (-inf) inf c



withMove :: Move -> Search a -> Search a
withMove m ac = do
  board %= execState (doMoveM m)
  r <- ac
  board %= execState (undoMoveM m)
  return r
{-# INLINE withMove #-}


renderVariation :: [ Move ] -> String
renderVariation = unwords . map renderShortMove


tryTransPosCache
  :: Bool                                               -- ^ condition
  -> Int -> Int -> Int -> Int                           -- ^ depth / alpha / beta / colour
  -> (Int -> Int -> Int -> Int -> Maybe Move -> Search SearchResult) -- ^ Maybe move is the TPC recommendation
  -> Search SearchResult
tryTransPosCache cond d alpha beta c f =
  if cond
    then do
      b <- use board
      t <- use tpc
      case TPC.transPosCacheLookUp b d t of
        Right (cache', entry) -> do
          tpc .= cache'
          tpcHit += 1
          case entry^.typ of
            TPC.Exact -> return $ entry^.result
            TPC.Lower -> let alpha' = max alpha $ snd $ entry^.result
                         in if alpha' >= beta
                            then return ([], alpha')
                            else f d alpha' beta c $ listToMaybe $ fst $ entry^.result
            TPC.Upper -> let beta' = min beta $ snd $ entry^.result
                         in if alpha >= beta'
                            then return ([], alpha)
                            else f d alpha beta' c $ listToMaybe $ fst $ entry^.result
        Left mr -> do
          tpcMiss += 1
          f d alpha beta c mr
    else f d alpha beta c Nothing
{-# INLINE tryTransPosCache #-}


updateTransPosCache :: Int -> Iterate -> Search SearchResult
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
  -> (Bool -> Move -> Int -> Int -> Search SearchResult) -- ^ True is fed in in the first iteration (for negascout)
  -> Search Iterate
iterateMoves ml alpha beta rep ac = go True (TPC.Upper, ([], alpha)) ml
  where go _ l [] = return l
        go f p@(_, pr@(_, a)) (m:ms) = do
          n@(_, score) <-  ac f m a beta
          let cont
                | a     >= beta = return (TPC.Upper, ([m], a))
                | score >= beta = return (TPC.Lower, ([m], beta))
                | score > a     = info n m >> go False (TPC.Exact, n) ms
                | otherwise     = info pr m >> go False p ms
          cont
        info (pv, score) m = when rep $
                             get >>= \st -> liftIO $ putStrLn ("info TPC : " ++ show (hitRatio st) ++ "% "
                                                               ++ show score
                                                               ++ " PV : " ++ renderVariation pv
                                                               ++ " curr : " ++ renderShortMove m)



hitRatio :: SearchState -> Int
hitRatio st = let s = st^.tpcHit + st^.tpcMiss
              in if s == 0 then 0 else (st^.tpcHit * 100) `div` s
        

negaScout'
  :: Int -- ^ max depth
  -> Int -- ^ depth
  -> Int -- ^ alpha
  -> Int -- ^ beta
  -> Int -- ^ colour
  -> Search SearchResult
negaScout' mx d' alpha' beta' c' = tryTransPosCache (d' /= mx) d' alpha' beta' c' $ \d alpha beta c mr -> do
  mate <- liftM checkMate $ use board
  if mate
    then liftM (\b -> ([], c * evaluate b)) $ use board
    else if d == 0
          then quiscene alpha beta c
          else do
            ml <- getMoveList mr moves
            r <- iterateMoves ml alpha beta (mx == d) $ \f m a b ->
              withMove m $ addM m $ if not f
                                    then do
                                      n@(_, score) <- negM $ negaScout' mx (d - 1) (-a - 1) (-a) (-c)
                                      if a < score && score < b
                                        then negM $ negaScout' mx (d - 1) (-b) (-a) (-c)
                                        else return n
                                    else negM $ negaScout' mx (d - 1) (-b) (-a) (-c)
            updateTransPosCache d r
            

quiscene :: Int -> Int -> Int -> Search SearchResult
quiscene alpha' beta' c' = tryTransPosCache True 0 alpha' beta' c' $ \_ alpha beta c mr -> do
  standPat <- liftM ((c*) . evaluate) (use board)
  if standPat >= beta
    then updateTransPosCache 0 (TPC.Lower, ([], beta))
    else do
       ml <- getMoveList mr forcingMoves
       let mx = max alpha standPat
       r  <- iterateMoves ml mx beta False $ \ _ m a b -> withMove m $ addM m $ negM $ quiscene (-b) (-a) (-c)
       updateTransPosCache 0 r


getMoveList :: Maybe Move -> (Board -> MoveQueue) -> Search [Move]
getMoveList mr mvs = liftM (addMaybeMove mr . map snd . Q.toDescList . mvs) $ use board
{-# INLINE getMoveList #-}


inf :: Int
inf = maxBound


addMaybeMove :: (Eq a) => Maybe a -> [a] -> [a]
addMaybeMove mr l = maybe l (\m -> m : filter (/= m) l) mr


negM   = mulM (-1)
mulM c = liftM (_2 %~ (c*))
addM m = liftM (_1 %~ (m:))

