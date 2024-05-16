{-# LANGUAGE AllowAmbiguousTypes #-} -- for reifying

module TypeLevelShow.Doc where

import GHC.TypeLits qualified as TE -- TE = TypeError
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )
import GHC.Exts ( proxy# )

-- | Simple pretty document ADT.
--
-- Designed to work on both type level (as a limited 'TE.ErrorMessage') and term
-- level (as a boring ADT).
--
-- Note that 'TE.ShowType' is magical (see
-- @compiler/GHC/Core/Type.hs#L1309@), so we need to remove it for term level.
--
-- singletons-base defines a version of this, but retains the 'TE.ShowType'
-- constructor and is in the singletons ecosystem.
data Doc s
  = Text s
  -- ^ plain ol' text
  | Doc s :<>: Doc s
  -- ^ append docs next to each other
  | Doc s :$$: Doc s
  -- ^ stack docs on top of each other (newline)
    deriving stock Show

-- | Promoted 'Doc'.
type PDoc = Doc Symbol

-- | Render a 'PDoc' as an 'ErrorMessage', for type-level error messages.
--
-- 'PDoc' is a subset of 'ErrorMessage', so this is very boring.
type RenderDoc :: PDoc -> TE.ErrorMessage
type family RenderDoc doc where
    RenderDoc (Text s)   = TE.Text s
    RenderDoc (l :<>: r) = RenderDoc l TE.:<>: RenderDoc r
    RenderDoc (l :$$: r) = RenderDoc l TE.:$$: RenderDoc r

-- | Reify a promoted 'Doc' to the corresponding term-level one.
class ReifyDoc (doc :: PDoc) where
    -- TODO do we want to do IsString directly in here? will it guarantee better
    --      performance, rather than mapping afterwards? hard to say
    reifyDoc :: Doc String

instance KnownSymbol s => ReifyDoc (Text s) where
    reifyDoc = Text $ symbolVal' (proxy# @s)

instance (ReifyDoc l, ReifyDoc r) => ReifyDoc (l :<>: r) where
    reifyDoc = reifyDoc @l :<>: reifyDoc @r

instance (ReifyDoc l, ReifyDoc r) => ReifyDoc (l :$$: r) where
    reifyDoc = reifyDoc @l :$$: reifyDoc @r
