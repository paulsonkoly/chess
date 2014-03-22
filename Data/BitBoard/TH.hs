{-| Template haskell functions -}
module Data.BitBoard.TH
       ( toVecLookup
       , toVecLookup2
       ) where

import           Prelude hiding (toInteger)
import           Language.Haskell.TH

import           Data.BitBoard.BitBoard


-- | Transforms a function that maps a Bounded Enum to BitBoards from pattern matching
--   (or whatever way the function is implemented) to a vector lookup.
toVecLookup
  :: (Enum a, Bounded a)
  => String             -- ^ name of the new function
  -> (a -> BitBoard)    -- ^ Enum to BitBoard function
  -> Q [ Dec ]          -- ^ Resulting splice
toVecLookup n f = do
  tvar <- newName "a"
  return [ mkSignature name [ tvar ]
         , FunD name [ Clause [] (NormalB body) [] ]
         , PragmaD $ InlineP name Inline FunLike AllPhases
         ]
  where name = mkName n
        body = InfixE (Just (InfixE (Just (AppE fromListV
                                           (ListE
                                            [ AppE bitBoardC (LitE (IntegerL $ toInteger $ f i))
                                            | i <- [ minBound .. maxBound ]
                                            ]))) (VarE $ mkName "V.!") Nothing)) (VarE $ mkName ".") (Just fromEnumV)


-- | Transforms a function that maps two Bounded Enums to BitBoards from pattern matching
--   (or whatever way the function is implemented) to a vector lookup.
toVecLookup2
  :: (Enum a, Bounded a, Enum b, Bounded b)
  => String                -- ^ Name of the function
  -> Integer               -- ^ TODO : remove this somehow - the maxBound + 1 of the 2nd enum
  -> (a -> b -> BitBoard)  -- ^ the tranformed function
  -> Q [ Dec ]             -- ^ resulting new splice
toVecLookup2 n mx f =  do
  vara <- newName "a"
  varb <- newName "b"
  let vars = [ vara, varb ]
  return [ mkSignature name vars
         , FunD name [ Clause [VarP var | var <- [ vara,  varb ] ] (NormalB $ body vara varb) [] ]
         , PragmaD $ InlineP name Inline FunLike AllPhases
         ]
  where name = mkName n
        body vvara vvarb = InfixE
                                 (Just (AppE fromListV
                                        (ListE
                                         [ AppE bitBoardC (LitE (IntegerL $ toInteger $ f i j))
                                         | i <- [ minBound .. maxBound ]
                                         , j <- [ minBound .. maxBound ]
                                         ])))
                                 (VarE $ mkName "V.!")
                                 (Just $ InfixE
                                        (Just $ InfixE
                                               (Just $ LitE $ IntegerL mx)
--                                               (Just (AppE fromEnumV (VarE $ mkName "maxBound")))
                                               (VarE $ mkName "*")
                                               (Just $ AppE fromEnumV $ VarE vvara))
                                        (VarE $ mkName "+")
                                        (Just $ AppE fromEnumV $ VarE vvarb))


fromListV, fromEnumV, bitBoardC :: Exp
fromListV = VarE $ mkName "V.fromList"
fromEnumV = VarE $ mkName "fromEnum"
bitBoardC = ConE $ mkName "BitBoard"


-- | Creates a signature of the form:
--
--   <name> :: forall [xxx, yyy ...].[Enum xxx, Bounded xxx, Enum yyy ...] => [xxx -> yyy -> ...] -> BitBoard
mkSignature :: Name -> [ Name ] -> Dec
mkSignature name ns = SigD name (ForallT
                                 [ PlainTV tvar| tvar <- ns ]
                                 [ ClassP (mkName nm) [ VarT tvar ]
                                 | tvar <- ns
                                 , nm <- ["Enum", "Bounded" ]
                                 ]
                                 (foldr (\v p -> (AppT (AppT ArrowT (VarT v)) p)) (ConT $ mkName "BitBoard") ns))
