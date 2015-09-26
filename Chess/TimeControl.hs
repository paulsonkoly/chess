{-# LANGUAGE MultiWayIf #-}
-- | The module calculates the allocated time for the search based on the
-- times remaining and other factors.
--
-- https://chessprogramming.wikispaces.com/Time+Management
--
-- The allocated time for the search is independent of the engine speed, 
-- and only depends on the remaining times, the moves played, and the state 
-- of the game. The search is supposed to call the 'timeControl' function 
-- before entering each level of iterative deepening, to decide whether to 
-- proceed or terminate.
--
-- We can terminate the search prematurely if the move to be played is
-- "obvious" ie the search is stable. For only legal moves or mate
-- combinations the Search should avoid unnecessary calculations regardless of
-- time control.
-- Move stability depends on the oscillation of the score between the
-- deepening iterations.
module Chess.TimeControl
       ( TimeControl(..)
       , TimeStats
       , mkTimeStats
       , addStats
       , timeControl
         -- * Setters
       , setWhiteTime
       , setBlackTime
       , setWhiteInc
       , setBlackInc
       , setMovesToGo
       ) where

------------------------------------------------------------------------------
import Control.Monad (join)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe, fromMaybe)

import Chess.Move
import Data.ChessTypes


type Time = Int


------------------------------------------------------------------------------
-- | The time control setup as received from the UCI.
data TimeControl = Timed
                   { whiteTime :: Time        -- ^ White player's time     
                   , whiteInc  :: Maybe Time  -- ^ White player's increment                   
                   , blackTime :: Time        -- ^ Black player's time      
                   , blackInc  :: Maybe Time  -- ^ Black player's increment
                   , movesToGo :: Maybe Int   -- ^ Moves until time controll
                   }
                 | DepthSpecified Int         -- ^ Ignore time, use this depth
                 | TimeSpecified Int          -- ^ Run for this amount of time         
                 | Infinite                   -- ^ Run for ever
                 deriving (Show)


------------------------------------------------------------------------------
-- | Sets white's time
setWhiteTime :: Time -> TimeControl -> TimeControl
setWhiteTime ot (Timed _ oi tt ti mtg) = Timed ot oi tt ti mtg
setWhiteTime ot _ = Timed ot Nothing 0 Nothing Nothing


------------------------------------------------------------------------------
-- | Sets black's time
setBlackTime :: Time -> TimeControl -> TimeControl
setBlackTime tt (Timed ot oi _ ti mtg) = Timed ot oi tt ti mtg
setBlackTime tt _ = Timed 0 Nothing tt Nothing Nothing


------------------------------------------------------------------------------
-- | Sets white's increment
setWhiteInc :: Time -> TimeControl -> TimeControl
setWhiteInc oi (Timed ot _ tt ti mtg) = Timed ot (Just oi) tt ti mtg
setWhiteInc oi _ = Timed 0 (Just oi) 0 Nothing Nothing


------------------------------------------------------------------------------
-- | Sets black's increment
setBlackInc :: Time -> TimeControl -> TimeControl
setBlackInc ti (Timed ot oi tt _ mtg) = Timed ot oi tt (Just ti) mtg
setBlackInc ti _ = Timed 0 Nothing 0 (Just ti) Nothing


------------------------------------------------------------------------------
-- | Sets the moves to go until the next time control
setMovesToGo :: Int -> TimeControl -> TimeControl
setMovesToGo mtg (Timed ot oi tt ti _) = Timed ot oi tt ti (Just mtg)
setMovesToGo mtg _ = Timed 0 Nothing 0 Nothing (Just mtg)


                            ---------------------
                            -- Time statistics --
                            ---------------------

------------------------------------------------------------------------------
-- | The store for the search statistics. Storing the time taken, the 
-- evaluation, and the best move per depth level.
newtype TimeStats = TimeStats [ (Time, Maybe (Move, Int)) ]


------------------------------------------------------------------------------
-- | Creates an empty 'TimeStats' instance
mkTimeStats :: TimeStats
mkTimeStats = TimeStats []


------------------------------------------------------------------------------
-- | Adds the next level to the statistics
addStats :: Time -> Maybe (Move, Int) -> TimeStats -> TimeStats
addStats tm mbMv (TimeStats l) = TimeStats $ (tm, mbMv) : l


                              ------------------
                              -- Time control --
                              ------------------

------------------------------------------------------------------------------
-- | Time control. Based on pondering, the UCI time control, the number of
-- remaining pieces, and our current time statistics decides whether we can
-- proceed to the next level.
timeControl :: Colour -> Bool -> Int -> TimeControl -> TimeStats -> Bool
timeControl c pondering pieceCnt tc ts = pondering || noPonder tc ts
  where
    noPonder Infinite _                       = True
    noPonder (DepthSpecified d) (TimeStats l) = length l < d
    noPonder (TimeSpecified t) _              = timeSpent ts < t 
    noPonder tc' (TimeStats l)                =
      timed (ourTime c tc') (ourInc c tc') (theirTime c tc') (theirInc c tc')
      (movesToGo tc') l

    timed ot oi _ _ mbMtg l =
      let increment = fromMaybe 0 oi

          -- is the search stable? (cheap deviation function on the last 3 
          -- scores). Also check that the last 3 moves were the same. 
          isStable =
            if length l >= 5
            then let first3' = (map snd $ take 3 l)
                 in if all isJust first3'
                    then let
                      first3  = map fromJust first3'
                      average = (sum (map snd first3) `div` 3)
                      mvs     = map fst first3
                      -- a fairly arbitrary number here, allow 1/3 pawn
                      -- deviation
                      in all (\x -> abs (snd x - average) <= 33) first3
                         && all (== head mvs) mvs
                    else False
            else False

          movesToGo'  = fromMaybe movesToGo'' mbMtg
          -- approximate the number of remaining moves with the absolute value
          -- of the score, and if the score is close to 0, with the remaining
          -- pieces.
          movesToGo'' = case latestScore ts of
            Just s -> if abs s <= 100
                      then pieceCnt
                      else 3000 `div` (abs s - 1) + 1
            _      -> pieceCnt

          allocTime = (ot + increment) `div` movesToGo'                             
              
      in if isStable
            -- if the search is stable, only continue if we have plenty of
            -- time.
         then timeSpent ts + requiredTime ts < allocTime
            -- positions seems to be critical. First let's make sure that we 
            -- are not loosing on time though.
         else timeSpent ts + 4 * requiredTime ts < ot


------------------------------------------------------------------------------
timeSpent :: TimeStats -> Time
timeSpent (TimeStats l) = sum $ map fst l


------------------------------------------------------------------------------
latestScore :: TimeStats -> Maybe Int
latestScore (TimeStats l) = 
  snd <$> join (listToMaybe $ dropWhile isNothing $ map snd l)


------------------------------------------------------------------------------
-- extrapolate the required time for the next level
requiredTime :: TimeStats -> Time
requiredTime (TimeStats l@(f:s:_:_)) =
  let y1 = fromIntegral $ fst f
      y0 = fromIntegral $ fst s
      x1 = fromIntegral $ length l
      x0 = fromIntegral $ length l - 1
      a  = (y0 ** (x1 / x0) / y1) ** x0 :: Float
      b  = log (y0 / a) / x0
  in floor $ a * exp(b * fromIntegral (length l + 1))
requiredTime _ = 1000     


------------------------------------------------------------------------------
ourTime :: Colour -> TimeControl -> Time
ourTime White tc = whiteTime tc
ourTime Black tc = blackTime tc


------------------------------------------------------------------------------
theirTime :: Colour -> TimeControl -> Time
theirTime White tc = blackTime tc
theirTime Black tc = whiteTime tc


------------------------------------------------------------------------------
ourInc :: Colour -> TimeControl -> Maybe Time
ourInc White tc = whiteInc tc
ourInc Black tc = blackInc tc


------------------------------------------------------------------------------
theirInc :: Colour -> TimeControl -> Maybe Time
theirInc White tc = blackInc tc
theirInc Black tc = whiteInc tc
