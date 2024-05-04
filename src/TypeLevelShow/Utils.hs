{-# LANGUAGE UndecidableInstances #-}

module TypeLevelShow.Utils where

import GHC.TypeLits

-- | Surround the given 'Symbol' with parentheses when the 'Bool' parameter is
--   'True'.
type ShowParen :: Bool -> Symbol -> Symbol
type family ShowParen b p where
    ShowParen False p = p
    ShowParen True  p = ConsSymbol '(' p ++ ")"

type l ++ r = AppendSymbol l r
