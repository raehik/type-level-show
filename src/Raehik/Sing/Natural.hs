module Raehik.Sing.Natural where

import GHC.TypeNats
import Unsafe.Coerce ( unsafeCoerce )

sMod :: SNat m -> SNat n -> SNat (Mod m n)
sMod sm sn = withSomeSNat (m `mod` n) unsafeCoerce
  where
    m = fromSNat sm
    n = fromSNat sn

sDiv :: SNat m -> SNat n -> SNat (Div m n)
sDiv sm sn = withSomeSNat (m `div` n) unsafeCoerce
  where
    m = fromSNat sm
    n = fromSNat sn
