{-# LANGUAGE UndecidableInstances #-}

module TypeLevelShow.Natural where

import GHC.TypeLits
import DeFun.Core ( type (~>), type App, type (@@) )

type ShowNatBin      n = ShowNatBase  2 ShowNatBinaryDigitSym   n
type ShowNatOct      n = ShowNatBase  8 ShowNatOctalDigitSym    n
type ShowNatDec      n = ShowNatBase 10 ShowNatDecimalDigitSym  n
type ShowNatHexLower n = ShowNatBase 16 ShowNatHexDigitLowerSym n
type ShowNatHexUpper n = ShowNatBase 16 ShowNatHexDigitUpperSym n

-- | Render a type-level 'Natural' in the given base using the given digit
--   printer.
type family ShowNatBase base showDigit n where
    -- | Our algorithm returns the empty 'Symbol' for 0, so handle it here.
    ShowNatBase base showDigit 0 = "0"

    ShowNatBase base showDigit n = ShowNatBase' base showDigit "" n

type ShowNatBase' :: Natural -> (Natural ~> Char) -> Symbol -> Natural -> Symbol
type family ShowNatBase' base showDigit acc n where
    ShowNatBase' base showDigit acc 0 = acc
    ShowNatBase' base showDigit acc n = ShowNatBase' base showDigit
        (ConsSymbol (showDigit @@ (n `Mod` base)) acc) (n `Div` base)

-- TODO add TypeErrors to invalid usages of these digit printers
-- should be free performance-wise so no harm
-- low priority because they're internals anyway

type family ShowNatBinaryDigit (d :: Natural) :: Char where
    ShowNatBinaryDigit 0 = '0'
    ShowNatBinaryDigit 1 = '1'

type ShowNatBinaryDigitSym :: Natural ~> Char
data ShowNatBinaryDigitSym d
type instance App ShowNatBinaryDigitSym d = ShowNatBinaryDigit d

type family ShowNatOctalDigit (d :: Natural) :: Char where
    ShowNatOctalDigit 0 = '0'
    ShowNatOctalDigit 1 = '1'
    ShowNatOctalDigit 2 = '2'
    ShowNatOctalDigit 3 = '3'
    ShowNatOctalDigit 4 = '4'
    ShowNatOctalDigit 5 = '5'
    ShowNatOctalDigit 6 = '6'
    ShowNatOctalDigit 7 = '7'

type ShowNatOctalDigitSym :: Natural ~> Char
data ShowNatOctalDigitSym d
type instance App ShowNatOctalDigitSym d = ShowNatOctalDigit d

type family ShowNatDecimalDigit (d :: Natural) :: Char where
    ShowNatDecimalDigit 0 = '0'
    ShowNatDecimalDigit 1 = '1'
    ShowNatDecimalDigit 2 = '2'
    ShowNatDecimalDigit 3 = '3'
    ShowNatDecimalDigit 4 = '4'
    ShowNatDecimalDigit 5 = '5'
    ShowNatDecimalDigit 6 = '6'
    ShowNatDecimalDigit 7 = '7'
    ShowNatDecimalDigit 8 = '8'
    ShowNatDecimalDigit 9 = '9'

type ShowNatDecimalDigitSym :: Natural ~> Char
data ShowNatDecimalDigitSym d
type instance App ShowNatDecimalDigitSym d = ShowNatDecimalDigit d

type family ShowNatHexDigitLower (d :: Natural) :: Char where
    ShowNatHexDigitLower  0 = '0'
    ShowNatHexDigitLower  1 = '1'
    ShowNatHexDigitLower  2 = '2'
    ShowNatHexDigitLower  3 = '3'
    ShowNatHexDigitLower  4 = '4'
    ShowNatHexDigitLower  5 = '5'
    ShowNatHexDigitLower  6 = '6'
    ShowNatHexDigitLower  7 = '7'
    ShowNatHexDigitLower  8 = '8'
    ShowNatHexDigitLower  9 = '9'
    ShowNatHexDigitLower 10 = 'a'
    ShowNatHexDigitLower 11 = 'b'
    ShowNatHexDigitLower 12 = 'c'
    ShowNatHexDigitLower 13 = 'd'
    ShowNatHexDigitLower 14 = 'e'
    ShowNatHexDigitLower 15 = 'f'

type ShowNatHexDigitLowerSym :: Natural ~> Char
data ShowNatHexDigitLowerSym d
type instance App ShowNatHexDigitLowerSym d = ShowNatHexDigitLower d

type family ShowNatHexDigitUpper (d :: Natural) :: Char where
    ShowNatHexDigitUpper  0 = '0'
    ShowNatHexDigitUpper  1 = '1'
    ShowNatHexDigitUpper  2 = '2'
    ShowNatHexDigitUpper  3 = '3'
    ShowNatHexDigitUpper  4 = '4'
    ShowNatHexDigitUpper  5 = '5'
    ShowNatHexDigitUpper  6 = '6'
    ShowNatHexDigitUpper  7 = '7'
    ShowNatHexDigitUpper  8 = '8'
    ShowNatHexDigitUpper  9 = '9'
    ShowNatHexDigitUpper 10 = 'A'
    ShowNatHexDigitUpper 11 = 'B'
    ShowNatHexDigitUpper 12 = 'C'
    ShowNatHexDigitUpper 13 = 'D'
    ShowNatHexDigitUpper 14 = 'E'
    ShowNatHexDigitUpper 15 = 'F'

type ShowNatHexDigitUpperSym :: Natural ~> Char
data ShowNatHexDigitUpperSym d
type instance App ShowNatHexDigitUpperSym d = ShowNatHexDigitUpper d
