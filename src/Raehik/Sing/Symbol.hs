module Raehik.Sing.Symbol where

import GHC.TypeLits
import Unsafe.Coerce ( unsafeCoerce )

sConsSymbol :: SChar ch -> SSymbol str -> SSymbol (ConsSymbol ch str)
sConsSymbol sch sstr = withSomeSSymbol (ch : str) unsafeCoerce
  where
    ch  = fromSChar   sch
    str = fromSSymbol sstr
