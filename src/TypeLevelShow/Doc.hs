{-# LANGUAGE AllowAmbiguousTypes #-} -- for reifying

module TypeLevelShow.Doc where

import GHC.TypeLits qualified as TE -- TE = TypeError
import GHC.TypeLits hiding ( ErrorMessage(..) )

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
type RenderPDoc :: PDoc -> TE.ErrorMessage
type family RenderPDoc doc where
    RenderPDoc (Text s)   = TE.Text s
    RenderPDoc (l :<>: r) = RenderPDoc l TE.:<>: RenderPDoc r
    RenderPDoc (l :$$: r) = RenderPDoc l TE.:$$: RenderPDoc r

-- | Singleton 'Doc'.
data SDoc (doc :: PDoc) where
    SText   :: SSymbol s -> SDoc (Text s)
    (:$<>:) :: SDoc docl -> SDoc docr -> SDoc (docl :<>: docr)
    (:$$$:) :: SDoc docl -> SDoc docr -> SDoc (docl :$$: docr)

-- | Produce the singleton for the given promoted 'Doc'.
class SingDoc (doc :: PDoc) where
    singDoc :: SDoc doc

instance KnownSymbol s => SingDoc (Text s) where
    singDoc = SText (SSymbol @s)

instance (SingDoc l, SingDoc r) => SingDoc (l :<>: r) where
    singDoc = singDoc @l :$<>: singDoc @r

instance (SingDoc l, SingDoc r) => SingDoc (l :$$: r) where
    singDoc = singDoc @l :$$$: singDoc @r

-- | Demote an 'SDoc' singleton to the corresponding base 'Doc'.
demoteDoc :: SDoc doc -> Doc String
demoteDoc = \case
  SText s   -> Text $ fromSSymbol s
  l :$<>: r -> demoteDoc l :<>: demoteDoc r
  l :$$$: r -> demoteDoc l :$$: demoteDoc r

-- | Reify a promoted 'Doc' to the corresponding term-level one.
reifyDoc :: forall (doc :: PDoc). SingDoc doc => Doc String
reifyDoc = demoteDoc (singDoc @doc)
