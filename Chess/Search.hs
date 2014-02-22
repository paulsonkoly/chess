{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Search
       ( negaScout
       , Search(..)
       , SearchState(..)
       , mkSearchState
       , module Chess.SearchResult
       , board
       ) where

import qualified Data.PQueue.Max as Q
import           Data.Maybe

import           Control.Monad.State
import           Control.Lens

import           Chess.Board
import qualified Chess.TransPosCache as TPC
import           Chess.TransPosCache (result, typ)
import           Chess.SearchResult (SearchResult(..), eval, first, renderVariation)
import qualified Chess.SearchResult as SR ((<@>), (<++>))
import           Chess.Evaluation
import           Chess.Move

data SearchState = SearchState
                   { _board    :: Board
                   , _tpc      :: TPC.TransPosCache
                   , _tpcHit   :: ! Int
                   , _tpcMiss  :: ! Int
                   , _nCnt     :: ! Int
                   }

mkSearchState :: SearchState
mkSearchState = SearchState initialBoard TPC.mkTransPosCache 0 0 0


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

(<@>) :: (Int -> Int) -> Search SearchResult -> Search SearchResult
f <@> m = liftM (f SR.<@>) m
(<++>) :: Move -> Search SearchResult -> Search SearchResult
x <++> m = liftM (x SR.<++>) m
    
data LoopResult = LoopResult
                  { _tpcEntryT :: ! TPC.TransPosCacheEntryType
                  , _line      :: ! SearchResult
                  }

$(makeLenses ''LoopResult)


negaScout
  :: Int -- ^ depth
  -> Search SearchResult
negaScout d = do
  tpcHit   .= 0
  tpcMiss  .= 0
  nCnt     .= 0
  c <- liftM (\b -> direction (b^.next) 1) $ use board
  (c*) <@> negaScout' d d (-inf) inf c



withMove :: Move -> Search a -> Search a
withMove m ac = do
  old <- use board
  board %= makeMove m
  r <- ac
  board .= old
  return r
{-# INLINE withMove #-}

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
            TPC.Lower -> let alpha' = max alpha $ entry^.result^.eval
                         in if alpha' >= beta
                            then return $ SearchResult [] alpha'
                            else f d alpha' beta c $ first $ entry^.result
            TPC.Upper -> let beta' = min beta $ entry^.result^.eval
                         in if alpha >= beta'
                            then return $ SearchResult [] alpha
                            else f d alpha beta' c $ first $ entry^.result
        Left mr -> do
          tpcMiss += 1
          f d alpha beta c mr
    else f d alpha beta c Nothing
{-# INLINE tryTransPosCache #-}


finishLoop :: Int -> LoopResult -> Search SearchResult
finishLoop d lr = do
  b <- use board
  tpc %= TPC.transPosCacheInsert b d (lr^.tpcEntryT) (lr^.line)
  return $ lr^.line
{-# INLINE finishLoop #-}


iterateMoves
  :: [ Move ]
  -> Int                                    -- ^ alpha
  -> Int                                    -- ^ beta
  -> Bool                                   -- ^ Report
  -> (Bool -> Move -> Int -> Int -> Search SearchResult) -- ^ True is fed in in the first iteration (for negascout)
  -> Search LoopResult
iterateMoves ml alpha beta rep ac = go True (LoopResult TPC.Upper $ SearchResult [] alpha) ml
  where go _ l [] = return l
        go f prev (m:ms) = do
          let prevVal = prev^.line^.eval
          nxt <- ac f m prevVal beta
          let cont
                | nxt^.eval >= beta   = return $ LoopResult TPC.Lower $ SearchResult [m] beta
                | nxt^.eval > prevVal = info nxt m >> go False (LoopResult TPC.Exact nxt) ms
                | otherwise           = info (prev^.line) m >> go False prev ms
          cont
        info sr m = when rep $ get >>= \st -> liftIO $ putStrLn ("info TPC : " ++ show (hitRatio st) ++ "% "
                                                                 ++ show (st^.nCnt `div` 1000) ++ "kn "
                                                                 ++ " PV : " ++ renderVariation sr
                                                                 ++ " curr : " ++ renderShortMove m)

percentage :: Int -> Int -> Int
percentage a b = if a == 0 then 0 else 100 * b `div` a

hitRatio :: SearchState -> Int
hitRatio st = percentage ((st^.tpcHit) + (st^.tpcMiss)) (st^.tpcHit)


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
    then do
      nCnt += 1
      liftM (\b -> SearchResult [] $ c * evaluate b) $ use board
    else if d == 0
          then quiscene alpha beta c
          else do
            ml <- getMoveList mr moves
            r <- iterateMoves ml alpha beta (mx == d) $ \f m a b ->
              withMove m $ m <++> if not f
                                  then do
                                    n <- ((-1)*) <@> negaScout' mx (d - 1) (-a - 1) (-a) (-c)
                                    if a < n^.eval && n^.eval < b
                                      then ((-1)*) <@> negaScout' mx (d - 1) (-b) (-a) (-c)
                                      else return n
                                  else ((-1)*) <@> negaScout' mx (d - 1) (-b) (-a) (-c)
            finishLoop d r
            

quiscene :: Int -> Int -> Int -> Search SearchResult
quiscene alpha' beta' c' = tryTransPosCache True 0 alpha' beta' c' $ \_ alpha beta c mr -> do
  standPat <- liftM ((c*) . evaluate) (use board)
  nCnt += 1
  if standPat >= beta
    then finishLoop 0 $ LoopResult TPC.Lower $ SearchResult [] beta
    else do
       ml <- getMoveList mr forcingMoves
       let mx = max alpha standPat
       r  <- iterateMoves ml mx beta False $
             \ _ m a b -> withMove m $ m <++> (((-1)*) <@> quiscene (-b) (-a) (-c))
       finishLoop 0 r



getMoveList :: Maybe Move -> (Board -> MoveQueue) -> Search [Move]
getMoveList mr mvs = liftM (addMaybeMove mr . map snd . Q.toDescList . mvs) $ use board
{-# INLINE getMoveList #-}


inf :: Int
inf = maxBound


addMaybeMove :: (Eq a) => Maybe a -> [a] -> [a]
addMaybeMove mr l = maybe l (\m -> m : filter (/= m) l) mr
