{-# LANGUAGE UndecidableInstances #-}

module TypeLevelShow.Utils where

import GHC.TypeLits

-- | Surround the given 'Symbol' with parentheses when the 'Bool' parameter is
--   'True'.
type ShowParen :: Bool -> Symbol -> Symbol
type family ShowParen b p where
    ShowParen False p = p
    ShowParen True  p = ConsSymbol '(' p `AppendSymbol` ")"

-- | Surround the given 'Symbol' with parentheses if the given precedence is
--   greater than the given 'Natural'.
--
-- Type-level relational operations are a pain. This may be easier to use.
type ShowParenIfGt :: Natural -> Natural -> Symbol -> Symbol
type ShowParenIfGt n d p = ShowParen (OrderingLT (CmpNat d n)) p

type family OrderingLT (a :: Ordering) :: Bool where
    OrderingLT LT = True
    OrderingLT EQ = False
    OrderingLT GT = False

type ShowChar ch = ConsSymbol ch ""

type l ++ r = AppendSymbol l r
type l >  r = OrderingLT (CmpNat l r)
