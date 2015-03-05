{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces               #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, IncoherentInstances              #-}
{-# LANGUAGE KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses   #-}
{-# LANGUAGE OverlappingInstances, PatternSynonyms, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies           #-}
{-# LANGUAGE TypeOperators, UndecidableInstances                          #-}
module Data.OpenUnion2
       (Element, Subset, Union2, embed, decomp, inj, prj, lift,
        extend, withWitness, withWitnesses, Refl(..))
       where
import           Control.Arrow            ((+++), (>>>))
import           Data.Bifunctor           (Bifunctor (..))
import           Data.Constraint          (Dict (..))
import           Data.Extensible          ((:*), (:|) (..), Forall, Include)
import           Data.Extensible          (Member, Membership, (<:|))
import qualified Data.Extensible          as E
import qualified Data.Extensible.Internal as E
import           Data.Proxy               (Proxy (..))
import           Prelude.Extras           (Eq2 (..), Ord2 (..), Show2 (..))

newtype K2 a b h = K2 { runK2 :: h a b }
                   deriving (Read, Show, Eq, Ord)

newtype Union2 hs a b = Union2 { runUnion2 :: K2 a b :| hs }

type Subset xs ys = Include ys xs
type Element x xs = Member xs x

withWitnesses :: Forall c xs => proxy c -> (E.Comp Dict c) :* xs
withWitnesses pxy = E.htabulateFor pxy $ \_ -> E.Comp Dict

withWitness :: Forall c xs => proxy c -> Membership xs x -> Dict (c x)
withWitness pxy = E.getComp . E.hindex (withWitnesses pxy)

instance Forall Show2 fs => Show2 (Union2 fs) where
  showsPrec2 d (Union2 (UnionAt mem (K2 f)))  =
    case withWitness (Proxy :: Proxy Show2) mem of
      Dict -> showsPrec2 d f

instance Eq2 (Union2 '[]) where
  _ ==## _ = True

instance (Eq2 x, Eq2 (Union2 xs))=> Eq2 (Union2 (x ': xs))  where
  u1 ==## u2 =
    case (decomp u1, decomp u2) of
      (Right a, Right b) ->  a ==## b
      (Left as, Left bs) -> as ==## bs
      _ -> False

instance Ord2 (Union2 '[]) where
  _ `compare2` _ = EQ

instance (Ord2 x, Ord2 (Union2 xs)) => Ord2 (Union2 (x ': xs)) where
  u1 `compare2` u2 =
    case (decomp u1, decomp u2) of
      (Right _, Left  _) -> LT
      (Left  _, Right _) -> GT
      (Right x, Right y) -> x `compare2` y
      (Left  x, Left  y) -> x `compare2` y

instance (Show a, Show b, Forall Show2 fs) => Show (Union2 fs a b) where
  showsPrec = showsPrec2

instance Forall Bifunctor ps => Bifunctor (Union2 ps) where
  bimap f g (Union2 (UnionAt mem (K2 hab))) =
    case withWitness (Proxy :: Proxy Bifunctor) mem of
      Dict -> Union2 $ UnionAt mem $ K2 $ bimap f g hab

inj :: Member hs h => h a b -> Union2 hs a b
inj f = Union2 $ E.embed $ K2 f

prj :: Member hs h => Union2 hs a b -> Maybe (h a b)
prj = undefined

decomp :: Union2 (h ': hs) a b -> Either (Union2 hs a b) (h a b)
decomp = runUnion2 >>>(Right <:| Left) >>> Union2 +++ runK2

embed :: Include gs fs => Union2 fs a b -> Union2 gs a b
embed (Union2 un) = Union2 $ E.spread un

lift :: Member fs f => (f a b -> f a b) -> Union2 fs a b -> Union2 fs a b
lift f x =
  case prj x of
    Just a -> inj $ f a
    Nothing -> x

class Refl xs where
  refl :: Membership xs :* xs

instance Refl '[] where
  refl = E.Nil

extend :: Union2 fs a b -> Union2 (f ': fs) a b
extend (Union2 (UnionAt mem f)) =
  Union2 $ UnionAt (E.navNext mem) f
