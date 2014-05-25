{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chess.Board.Arbitrary () where

import           Control.Monad
import           Control.Lens

import           Data.Maybe

import           Test.QuickCheck (arbitrary, suchThat)
import qualified Test.QuickCheck as Q

import           Data.ChessTypes
import           Data.BitBoard
import           Data.Square
import           Chess.Board.Board
import           Chess.Board.Attacks

instance Q.Arbitrary Board where
  -- TODO   enpassant
  --        casltes
  arbitrary = flip suchThat kingAttacksOk $ do
    wk <- liftM fromSquare arbitrary                    -- white king
    bk <- suchThat (liftM fromSquare arbitrary) (/= wk) -- black king
    let ks = wk .|. bk
    rs <- removing   ks                          -- rooks
    ns <- removing $ ks .|. rs                   -- knights
    bs <- removing $ ks .|. rs .|. ns            -- bishops
    qs <- removing $ ks .|. rs .|. ns .|. bs     -- queens
    ps <- removing $ ks .|. rs .|. ns .|. bs .|. qs .|. rankBB firstRank .|. rankBB eighthRank
    
    let occ = rs .|. ns .|. bs .|. qs .|. ps
    wpcs <- liftM (.&. occ) arbitrary
    let bpcs = occ .&. complement wpcs
    n <- Q.elements [ Black, White ]
    let b =   (whitePieces .~ wpcs .|. wk)
            $ (blackPieces .~ bpcs .|. bk)
            $ (rooks       .~ rs)
            $ (knights     .~ ns)
            $ (bishops     .~ bs)
            $ (queens      .~ qs)
            $ (kings       .~ wk .|. bk)
            $ (pawns       .~ ps)
            $ (next        .~ n)
            emptyBoard

    return $ (hash .~ calcHash b) b

    where removing f = liftM (.&. complement f) arbitrary

  -- remove 1 piece (except the king)
  shrink b = filter kingAttacksOk
             $ map ((\bb ->   (whitePieces %~ (.&. bb))
                            $ (blackPieces %~ (.&. bb))
                            $ (rooks       %~ (.&. bb))
                            $ (knights     %~ (.&. bb))
                            $ (bishops     %~ (.&. bb))
                            $ (queens      %~ (.&. bb))
                            $ (pawns       %~ (.&. bb)) b) . complement . fromSquare)
             $ filter goodSquare squares
    where goodSquare sq = let p = pieceAt b sq in isJust p && p /= Just King


-- | The king that's not next to move cannot be in check, and the
-- other king can only be in check by at most 2 pieces
kingAttacksOk :: Board -> Bool
kingAttacksOk b = let nKingPos = head $ toList $ piecesOf b (b^.next) King
                      oKingPos = head $ toList $ piecesOf b (b^.opponent) King
                      nAttackers = attackedFromBB b (occupancy b) (b^.opponent) nKingPos
                      oAttackers = attackedFromBB b (occupancy b) (b^.next)     oKingPos
                      numNAttackers = popCount nAttackers
                      numOAttackers = popCount oAttackers
                  in numOAttackers == 0 && numNAttackers <= 2
