{-# LANGUAGE TemplateHaskell #-}
-- | Various BitBoard values
module Data.BitBoard.Values
       ( neighbourFilesBB
       , neighbourRanksBB
       , knightAttackBB
       , kingAttackBB
       , pawnAttackBB
       , lineBB
       , pseudoAttackBB
       , centralSquares
       , largeCentralSquares
       , largeLargeCentralSquares
       , rimSquares
       , lightSquares
       , darkSquares
       , aheadBB
       , preMagics
       , sparseRandomBB
       , module P
       ) where

import           Data.Monoid

import qualified Data.Vector.Unboxed as V
import qualified Control.Monad.Random as R

import           Data.BitBoard.BitBoard
import           Data.BitBoard.TH
import           Data.BitBoard.Values.Private
import           Data.Square
import           Data.ChessTypes

import qualified Data.BitBoard.Values.Private as P (rankBB, fileBB)


$(toVecLookup "neighbourRanksBB" neighbourRanksBB')
$(toVecLookup "neighbourFilesBB" neighbourFilesBB')


lightSquares :: BitBoard
lightSquares = mconcat [ BitBoard $ bit i | i <- [ 1, 3 .. 63 ] ]


darkSquares :: BitBoard
darkSquares = mconcat [ BitBoard $ bit i | i <- [ 0, 2 .. 62 ] ]


-- | the ranks ahead of us ( from the specified colour's pov ) including the rank
$(toVecLookup2 "aheadBB" 2 aheadBB')                

$(toVecLookup "knightAttackBB" knightAttackBB')
$(toVecLookup "kingAttackBB" kingAttackBB')
$(toVecLookup2 "pawnAttackBB" 2 pawnAttackBB')
$(toVecLookup2 "lineBB" 64 lineBB')
$(toVecLookup2 "pseudoAttackBB" 64 pseudoAttackBB')


centralSquares :: BitBoard
centralSquares = mconcat [ fromSquare $ toSquare f r
                         | f <- [ dFile .. eFile ]
                         , r <- [ fourthRank .. fifthRank]
                         ]
{-# INLINE centralSquares #-}


largeCentralSquares :: BitBoard
largeCentralSquares = mconcat [ fromSquare $ toSquare f r
                              | f <- [ cFile .. fFile ]
                              , r <- [ thirdRank .. sixthRank ]
                              ]


largeLargeCentralSquares :: BitBoard
largeLargeCentralSquares = mconcat [ fromSquare $ toSquare f r
                                   | f <- [ bFile .. gFile ]
                                   , r <- [ secondRank .. seventhRank ]
                                   ]


rimSquares :: BitBoard
rimSquares = complement largeLargeCentralSquares


-- | Sparse random bitboard
sparseRandomBB :: (R.MonadRandom m) => m BitBoard
sparseRandomBB = do
   a <- R.getRandom
   b <- R.getRandom
   c <- R.getRandom
   return $ BitBoard $ a .&. b .&. c
{-# INLINE sparseRandomBB #-}


preMagics :: PieceType -> V.Vector BitBoard
preMagics Rook = V.fromList
   [ BitBoard 108086461930717186  , BitBoard 1170936184705802240  , BitBoard 2449977990650535945
   , BitBoard 9295464849692688640 , BitBoard 9871899179322970368  , BitBoard 36033197213089793
   , BitBoard 9511607464157584932 , BitBoard 72057871066595584    , BitBoard 140738033631234
   , BitBoard 703824884924544     , BitBoard 1018095060557758720  , BitBoard 3458905801333147648
   , BitBoard 10274128708239488   , BitBoard 36310280720089344    , BitBoard 281479271940608
   , BitBoard 288793327782920338  , BitBoard 612636334128890144   , BitBoard 18014536485834752
   , BitBoard 2342154381796835344 , BitBoard 432486851605958656   , BitBoard 2323998695034456064
   , BitBoard 563501856850944     , BitBoard 396884117761818890   , BitBoard 2307006292526628929
   , BitBoard 72127964929622048   , BitBoard 35185445851140       , BitBoard 630522098367865344
   , BitBoard 6917546621983000576 , BitBoard 10090314982370590752 , BitBoard 1153485562662094864
   , BitBoard 865254636754372676  , BitBoard 1729453184000214017  , BitBoard 2310417527357841664
   , BitBoard 36286047993864      , BitBoard 72198400254156800    , BitBoard 1351502135077900288
   , BitBoard 4758053023505189504 , BitBoard 140746086679552      , BitBoard 870921515456022544
   , BitBoard 1155177994558388353 , BitBoard 45176872274788352    , BitBoard 70370908504096
   , BitBoard 614776537807388696  , BitBoard 4508139408916489     , BitBoard 281492290863120
   , BitBoard 79657435589181464   , BitBoard 721156491125719048   , BitBoard 1169314414596
   , BitBoard 2594143756257075328 , BitBoard 364791844697018560   , BitBoard 144150373538468096
   , BitBoard 1301540326738002048 , BitBoard 1125934275101824     , BitBoard 72621093780979840
   , BitBoard 486389937794450432  , BitBoard 648659088126787712   , BitBoard 289436540686401537
   , BitBoard 2316046487857988641 , BitBoard 1154751230757587458  , BitBoard 9313725573108637721
   , BitBoard 1689159369761030    , BitBoard 291045745602135042   , BitBoard 9235765802807472132
   , BitBoard 3458777727560196354
   ]
preMagics Bishop = V.fromList
   [ BitBoard 4612253508295525120  , BitBoard 72629375669911808    , BitBoard 19149096707293184
   , BitBoard 14724943187017728    , BitBoard 1174331491509862400  , BitBoard 5476942305002913800
   , BitBoard 2594376855407165608  , BitBoard 2815866475448336     , BitBoard 14988049997375867008
   , BitBoard 17734591119424       , BitBoard 72235723516937728    , BitBoard 144133400918302720
   , BitBoard 902131719950305280   , BitBoard 4612708573653501964  , BitBoard 288514059547182098
   , BitBoard 35751509266944       , BitBoard 3541518226233494785  , BitBoard 146367273539732480
   , BitBoard 2307005296087924744  , BitBoard 587724493688938496   , BitBoard 1153625209243172896
   , BitBoard 378865396533501952   , BitBoard 5664692504758272     , BitBoard 378584119157785728
   , BitBoard 9225659022114533889  , BitBoard 4622981319230883208  , BitBoard 1228358997942732544
   , BitBoard 9818410275093643776  , BitBoard 10952899429375418384 , BitBoard 10130847637218279939
   , BitBoard 333561058889433344   , BitBoard 613615595274936832   , BitBoard 1297608723281216128
   , BitBoard 74313809083244820    , BitBoard 13585574263783460    , BitBoard 27023798943875201
   , BitBoard 126101889388445760   , BitBoard 9376494974486643716  , BitBoard 5146556267299845
   , BitBoard 91785034356280338    , BitBoard 1154048641766279168  , BitBoard 3459129895479707681
   , BitBoard 36592864754669568    , BitBoard 1128374080668160     , BitBoard 10092610749701622792
   , BitBoard 450360546856798336   , BitBoard 9579091399540864     , BitBoard 577657579371496964
   , BitBoard 288382967888220164   , BitBoard 6062269926677159936  , BitBoard 38357566952177800
   , BitBoard 25040766771208       , BitBoard 10394593847404658688 , BitBoard 324611025552868352
   , BitBoard 2310381802943955201  , BitBoard 74608478269876256    , BitBoard 563104589547776
   , BitBoard 18014613292450832    , BitBoard 4408788658176        , BitBoard 37717921761460736
   , BitBoard 306244775198196874   , BitBoard 281612449350156      , BitBoard 576485079199779216
   , BitBoard 13835638804127449216
   ]
preMagics _ = undefined
