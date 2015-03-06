{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures    #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction              #-}
{-# LANGUAGE PatternSynonyms, PolyKinds, ScopedTypeVariables               #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances                                          #-}
module Text.Yapan.Base
       ( -- * Basic Types
         BlockF(..), Block, InlineF(..), Inline,
         cataB, cataI, embedB, embedI,
         -- ** Smart constructors & destructors
         block, blockWith, inline, inlineWith,
         prjB, prjI, includeI, includeB,
         -- * Standard Grammars
         -- ** Blocks
         Header(..), header, Plain(..), plain,
         Para(..), para, Format(..), RawBlock(..),
         rawBlock, List(..), ListType(..), NumberStyle(..),
         list, DefinitionList(..), definitionList,
         -- ** Inlines
         Str (..), str,
         Emph(..), emph, RawInline(..), rawInline
       ) where
import Data.Bifunctor    (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Extensible   (Forall)
import Data.Extensible   (Membership)
import Data.OpenUnion2
import Prelude.Extras    (Eq2 (..), Ord2 (..), Read2 (..), Show2 (..))

newtype BlockF  b i = Block' { runBlockF :: b (BlockF b i) (InlineF b i) }
type Block  bs is = BlockF (Union2 bs) (Union2 is)

newtype InlineF b i = Inline' { runInlineF :: i (BlockF b i) (InlineF b i) }
type Inline bs is = InlineF (Union2 bs) (Union2 is)

cataB :: (Bifunctor is', Bifunctor bs')
       => (bs (BlockF bs is) (InlineF bs is)
           -> bs' (BlockF bs is) (InlineF bs is))
       -> (is (BlockF bs is) (InlineF bs is)
           -> is' (BlockF bs is) (InlineF bs is))
       -> BlockF bs is -> BlockF bs' is'
cataB upB upI (Block' b) = Block' $ bimap (cataB upB upI) (cataI upB upI) $ upB b

cataI :: (Bifunctor is', Bifunctor bs')
       => (bs (BlockF bs is) (InlineF bs is)
           -> bs' (BlockF bs is) (InlineF bs is))
       -> (is (BlockF bs is) (InlineF bs is)
           -> is' (BlockF bs is) (InlineF bs is))
       -> InlineF bs is -> InlineF bs' is'
cataI upB upI (Inline' i) = Inline' $ bimap (cataB upB upI) (cataI upB upI) $ upI i

embedB :: (Forall Bifunctor is', Forall Bifunctor bs',
           Subset bs bs', Subset is is')
       => Block bs is -> Block bs' is'
embedB = cataB embed embed

embedI :: (Forall Bifunctor is', Forall Bifunctor bs',
           Subset bs bs', Subset is is')
       => Inline bs is -> Inline bs' is'
embedI = cataI embed embed

{-
interpBB :: (Bifunctor b, Bifunctor (Union2 bs), Bifunctor is)
        => (b (BlockF (Union2 bs) is) (InlineF (Union2 bs) is)
            -> Union2 bs (BlockF (Union2 (b ': bs)) is) (InlineF (Union2 (b ': bs)) is))
        -> BlockF (Union2 (b ': bs)) is -> BlockF (Union2 bs) is
interpBB ints = cataB go id
  where
    go a =
      case decomp a of
        Left  c -> c
        Right b -> ints b
-}

-- | Smart constructor for @'Block'@. Most typical type is:
--
-- @
-- block :: Element b bs => b (Block bs is) (Inline bs is) -> Block bs is
-- @
block :: Element b bs => b (BlockF (Union2 bs) i) (InlineF (Union2 bs) i) -> BlockF (Union2 bs) i
block = Block' . inj

blockWith :: Membership bs b -> b (BlockF (Union2 bs) i) (InlineF (Union2 bs) i) -> BlockF (Union2 bs) i
blockWith = (Block' .) . injWith

-- | Smart constructor for @'Inline'@. Most typical type is:
--
-- @
-- inline :: Element i is => b (Block bs is) (Inline bs is) -> Block bs is
-- @
inline :: Element t ts => t (BlockF b (Union2 ts)) (InlineF b (Union2 ts)) -> InlineF b (Union2 ts)
inline = Inline' . inj

inlineWith :: Membership ts t -> t (BlockF b (Union2 ts)) (InlineF b (Union2 ts)) -> InlineF b (Union2 ts)
inlineWith mem = Inline' . injWith mem

includeI :: (Element t xs, Subset xs ts)
           => proxy xs -> t (BlockF b (Union2 ts)) (InlineF b (Union2 ts)) -> InlineF b (Union2 ts)
includeI pxy = inlineWith (super pxy)

includeB :: (Element t xs, Subset xs ts)
         => proxy xs -> t (BlockF (Union2 ts) i) (InlineF (Union2 ts) i) -> BlockF (Union2 ts) i
includeB pxy = blockWith (super pxy)

prjI :: Element h hs
     => InlineF b (Union2 hs) -> Maybe (h (BlockF b (Union2 hs)) (InlineF b (Union2 hs)))
prjI = prj . runInlineF

prjB :: Element h hs
     => BlockF (Union2 hs) is -> Maybe (h (BlockF (Union2 hs) is) (InlineF (Union2 hs) is))
prjB = prj . runBlockF

instance (Show2 f, Show2 g) => Show (BlockF f g) where
  showsPrec d (Block' f) = showsPrec2 d f

instance (Show2 f, Show2 g) => Show (InlineF f g) where
  showsPrec d (Inline' f) = showsPrec2 d f

instance (Eq2 f, Eq2 g) => Eq (BlockF f g) where
  Block' f == Block' g = f ==## g

instance (Eq2 f, Eq2 g) => Eq (InlineF f g) where
  Inline' f == Inline' g = f ==## g

instance (Ord2 f, Ord2 g) => Ord (BlockF f g) where
  Block' f `compare` Block' g = f `compare2` g

instance (Ord2 f, Ord2 g) => Ord (InlineF f g) where
  Inline' f `compare` Inline' g = f `compare2` g

data Header blk inl = Header' Int [inl]
                      deriving (Read, Show, Eq, Ord)

instance Ord2  Header
instance Show2 Header
instance Eq2   Header
instance Read2 Header
deriveBifunctor ''Header

header :: Element Header bs => Int -> [InlineF (Union2 bs) is] -> BlockF (Union2 bs) is
header lvl = block . Header' lvl

data Plain bs is = Plain' [is]
                 deriving (Read, Show, Eq, Ord)

instance Show2 Plain
instance Eq2 Plain
instance Ord2 Plain
instance Read2 Plain
deriveBifunctor ''Plain

plain :: Element Plain bs => [InlineF (Union2 bs) i] -> BlockF (Union2 bs) i
plain = block . Plain'

data Para bs is = Para' [is]
                 deriving (Read, Show, Eq, Ord)

instance Show2 Para
instance Eq2 Para
instance Ord2 Para
instance Read2 Para
deriveBifunctor ''Para

para :: Element Para bs => [InlineF (Union2 bs) i] -> BlockF (Union2 bs) i
para = block . Para'


newtype Format = Format String
               deriving (Read, Show, Eq, Ord)
data RawBlock bs is = RawBlock' Format String
                    deriving (Read, Show, Eq, Ord)


instance Show2 RawBlock
instance Eq2 RawBlock
instance Ord2 RawBlock
instance Read2 RawBlock
deriveBifunctor ''RawBlock

rawBlock :: Element RawBlock bs => Format -> String -> BlockF (Union2 bs) i
rawBlock = (block . ) . RawBlock'

data NumberStyle = Decimal
                 | Alphabetical
                   deriving (Read, Show, Eq, Ord)

data ListType = Unordered
              | Ordered Int NumberStyle
                deriving (Read, Show, Eq, Ord)

data List bs is = List' ListType [[bs]]
                deriving (Read, Show, Eq, Ord)

list :: Element List bs => ListType -> [[BlockF (Union2 bs) is]] -> BlockF (Union2 bs) is
list = (block .) . List'

instance Show2 List
instance Eq2 List
instance Ord2 List
instance Read2 List
deriveBifunctor ''List

newtype Str bs is = Str' String
                  deriving (Read, Show, Eq, Ord)

str :: Element Str is => String -> InlineF bs (Union2 is)
str = inline . Str'

instance Show2 Str
instance Eq2 Str
instance Ord2 Str
instance Read2 Str
deriveBifunctor ''Str

data Emph blk inl = Emph' [inl]
              deriving (Read, Show, Eq, Ord)

instance Eq2 Emph
instance Show2 Emph
instance Ord2 Emph
instance Read2 Emph
deriveBifunctor ''Emph

emph :: Element Emph is => [InlineF bs (Union2 is)] -> InlineF bs (Union2 is)
emph = inline . Emph'

data RawInline is bs = RawInline' Format String
                       deriving (Read, Show, Eq, Ord)

instance Eq2 RawInline
instance Show2 RawInline
instance Ord2 RawInline
instance Read2 RawInline
deriveBifunctor ''RawInline

rawInline :: Element RawInline is => Format -> String -> InlineF b (Union2 is)
rawInline = (inline .) . RawInline'

data DefinitionList bs is = DefinitionList' [([is], [[bs]])]
                            deriving (Read, Show, Eq, Ord)

instance Eq2 DefinitionList
instance Show2 DefinitionList
instance Ord2 DefinitionList
instance Read2 DefinitionList
deriveBifunctor ''DefinitionList

definitionList :: Element DefinitionList bs
               => [([InlineF (Union2 bs) is], [[BlockF (Union2 bs) is]])]
               -> BlockF (Union2 bs) is
definitionList = block . DefinitionList'
