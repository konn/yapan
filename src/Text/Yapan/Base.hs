{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures    #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction              #-}
{-# LANGUAGE PatternSynonyms, PolyKinds, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE TypeOperators, UndecidableInstances                           #-}
module Text.Yapan.Base
       ( -- * Basic Types
         BlockF(..), Block, InlineF(..), Inline,
         cataB, cataI, embedB, embedI,
         -- ** Smart constructors
         block, inline,
         -- * Standard Grammars
         -- ** Blocks
         Header(..), header, Plain(..), plain,
         Para(..), para,
         -- ** Inlines
         Str (..), str,
         Emph(..), emph,
       ) where
import Data.Bifunctor  (Bifunctor (..))
import Data.Extensible (Member)
import Data.Extensible (Forall)
import Data.OpenUnion2
import Prelude.Extras  (Eq2 (..), Ord2 (..), Read2 (..), Show2 (..))

type family Map f xs where
  Map f '[]       = '[]
  Map f (p ': ps) = f p ': Map f ps

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

-- | Smart constructor for @'Block'@. Most typical type is:
--
-- @
-- block :: Member b bs => b (Block bs is) (Inline bs is) -> Block bs is
-- @
block :: Member bs b => b (BlockF (Union2 bs) i) (InlineF (Union2 bs) i) -> BlockF (Union2 bs) i
block = Block' . inj

-- | Smart constructor for @'Inline'@. Most typical type is:
--
-- @
-- inline :: Member i is => b (Block bs is) (Inline bs is) -> Block bs is
-- @
inline :: Member ts t => t (BlockF b (Union2 ts)) (InlineF b (Union2 ts)) -> InlineF b (Union2 ts)
inline = Inline' . inj

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
instance Bifunctor Header where
  bimap _ f (Header' n iss) = Header' n $ map f iss
  first _ (Header' n iss)   = Header' n iss
  second f (Header' n iss)  = Header' n $ map f iss

header :: Member bs Header => Int -> [InlineF (Union2 bs) is] -> BlockF (Union2 bs) is
header lvl = block . Header' lvl

data Plain bs is = Plain' [is]
                 deriving (Read, Show, Eq, Ord)

instance Show2 Plain
instance Eq2 Plain
instance Ord2 Plain
instance Read2 Plain
instance Bifunctor Plain where
  bimap _ f (Plain' iss) = Plain' $ map f iss
  first _ (Plain' iss) = Plain' iss
  second f (Plain' iss) = Plain' $ map f iss

plain :: Member bs Plain => [InlineF (Union2 bs) i] -> BlockF (Union2 bs) i
plain = block . Plain'

data Para bs is = Para' [is]
                 deriving (Read, Show, Eq, Ord)

instance Show2 Para
instance Eq2 Para
instance Ord2 Para
instance Read2 Para
instance Bifunctor Para where
  bimap _ f (Para' iss) = Para' $ map f iss
  first _ (Para' iss) = Para' iss
  second f (Para' iss) = Para' $ map f iss

para :: Member bs Para => [InlineF (Union2 bs) i] -> BlockF (Union2 bs) i
para = block . Para'

newtype Str bs is = Str' String
                  deriving (Read, Show, Eq, Ord)

str :: Member is Str => String -> InlineF bs (Union2 is)
str = inline . Str'

instance Show2 Str
instance Eq2 Str
instance Ord2 Str
instance Read2 Str
instance Bifunctor Str where
  bimap  _ _ (Str' s) = Str' s
  first  _ (Str' s) = Str' s
  second _ (Str' s) = Str' s

data Emph blk inl = Emph' [inl]
              deriving (Read, Show, Eq, Ord)

instance Eq2 Emph
instance Show2 Emph
instance Ord2 Emph
instance Read2 Emph
instance Bifunctor Emph where
  bimap  _ g (Emph' iss) = Emph' $ map g iss
  first  _ (Emph' s) = Emph' s
  second g (Emph' s) = Emph' $ map g s


emph :: Member is Emph =>  [InlineF bs (Union2 is)] -> InlineF bs (Union2 is)
emph = inline . Emph'

