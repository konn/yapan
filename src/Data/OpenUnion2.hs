{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces             #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, IncoherentInstances            #-}
{-# LANGUAGE KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances, PatternSynonyms, PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies         #-}
{-# LANGUAGE TypeOperators, UndecidableInstances                        #-}
module Data.OpenUnion2 (Union2, embed, extend, decomp, inj, prj, Member, All, Subset, lift) where
import Data.Bifoldable   (Bifoldable (..))
import Data.Bifunctor    (Bifunctor (..))
import Data.Proxy        (Proxy (Proxy))
import Data.Type.Natural (Nat (..))
import GHC.Exts          (Constraint)
import Prelude.Extras    (Eq2 (..), Ord2 (..), Show2 (..))

data Union2 fs a b where
  Here2  :: f a b -> Union2 (f ': fs) a b
  There2 :: Union2 fs a b -> Union2 (f ': fs) a b

instance (Show a, Show b, All Show2 fs) => Show (Union2 fs a b) where
  showsPrec = showsPrec2

type family FindElem t r where
  FindElem t (t ': xs) = Z
  FindElem t (x ': xs) = S (FindElem t xs)

{-# INLINE decomp #-}
decomp :: Union2 (t ': rs) a b -> Either (Union2 rs a b) (t a b)
decomp (Here2 a) = Right a
decomp (There2 b) = Left b

class MemberN t ts (n :: Nat) where
  inj' :: Proxy n -> t a b -> Union2 ts a b
  prj' :: Proxy n -> Union2 ts a b -> Maybe (t a b)

instance MemberN t (t ': ts) Z where
  inj' _ = Here2
  prj' _ (Here2 a)  = Just a
  prj' _ (There2 _) = Nothing

instance (MemberN t ts n) => MemberN t (u ': ts) (S n) where
  inj' _ = There2 . inj' (Proxy :: Proxy n)
  prj' _ (Here2 _) = Nothing
  prj' _ (There2 x) = prj' (Proxy :: Proxy n) x

class MemberN t ts (FindElem t ts) => Member t ts where
  inj :: t a b -> Union2 ts a b
  prj :: Union2 ts a b -> Maybe (t a b)

instance MemberN t ts (FindElem t ts) => Member t ts where
  inj = inj' (Proxy :: Proxy (FindElem t ts))
  prj = prj' (Proxy :: Proxy (FindElem t ts))

-- instance (Subset (x ': xs) (x ': ys)) => IncludedR x (x ': xs) (x ': ys) Z where
--   includedR Proxy Proxy Proxy Proxy = Dict

-- instance IncludedR x xs ys n => IncludedR x xs (u ': ys) (S n) where
--   includedR Proxy Proxy Proxy Proxy = Dict

{-# INLINE extend #-}
extend :: Union2 fs a b -> Union2 (f ': fs) a b
extend = There2

instance All Bifunctor ps => Bifunctor (Union2 ps) where
  bimap f g (Here2 x)  = Here2 $ bimap f g x
  bimap f g (There2 x) = There2 $ bimap f g x

instance (All Bifoldable ps, All Bifunctor ps) => Bifoldable (Union2 ps) where
  bifoldMap f g (Here2 x) = bifoldMap f g x
  bifoldMap f g (There2 x) = bifoldMap f g x
  bifoldr f g c (Here2 x) = bifoldr f g c x
  bifoldr f g c (There2 x) = bifoldr f g c x
  bifold (Here2 x)  = bifold x
  bifold (There2 x) = bifold x
  bifoldl f g c (Here2 x) = bifoldl f g c x
  bifoldl f g c (There2 x) = bifoldl f g c x



type family All k xs :: Constraint where
  All k '[] = ()
  All k (x ': xs) = (k x, All k xs)

instance All Show2 fs => Show2 (Union2 fs) where
  showsPrec2 d (Here2 x)  = showsPrec2 d x
  showsPrec2 d (There2 x) = showsPrec2 d x

-- | Caution: This equality is homogeneous one.
--   Values of same type in the different position
--   are considered to be different.
instance All Eq2 fs => Eq2 (Union2 fs) where
  Here2  x ==## Here2  y = x ==## y
  There2 x ==## There2 y = x ==## y
  _ ==## _  = False

-- | @Here2 y@ is less than @There2 y@
instance (All Eq2 fs, All Ord2 fs) => Ord2 (Union2 fs) where
  Here2  x `compare2` Here2 y  = x `compare2` y
  Here2  _ `compare2` There2 _ = LT
  There2 x `compare2` There2 y = x `compare2` y
  There2 _ `compare2` Here2 _  = GT

type family Map f as where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family xs :! n where
  (x ': xs) :! Z   = x
  (x ': xs) :! S n = xs :! n

type family Indices as bs :: [Nat] where
  Indices '[]       ys = '[]
  Indices (x ': xs) ys = FindElem x ys ': Indices xs ys

class (us ~ Indices fs gs) => SubsetNs fs gs us where
  embed'    :: Proxy us -> Union2 fs a b -> Union2 gs a b

instance (ns ~ Indices xs xs) => SubsetNs xs xs ns where
  embed' _ = id

instance SubsetNs '[] fs '[] where
  embed'  Proxy _ = error "impossible!"

instance (SubsetNs fs gs ns, Member f gs, n ~ FindElem f gs)
         => SubsetNs (f ': fs) gs (n ': ns) where
  embed'  Proxy (Here2 x)   = inj' (Proxy :: Proxy n) x
  embed'  Proxy (There2 x)  = embed' (Proxy :: Proxy ns) x

class SubsetNs fs gs (Indices fs gs) => Subset fs gs where
  embed :: Union2 fs a b -> Union2 gs a b

instance SubsetNs fs gs (Indices fs gs) => Subset fs gs where
  embed = embed' (Proxy :: Proxy (Indices fs gs))

lift :: Member f fs => (f a b -> f a b) -> Union2 fs a b -> Union2 fs a b
lift f x =
  case prj x of
    Just a -> inj $ f a
    Nothing -> x

{-
data Boo a b = Boo a | Bar b
             deriving (Read, Show, Eq, Ord)
instance Show2 Boo
-}
{-
included :: (MemberN f fs ns, Subset fs gs)
         => pxy f -> pxy' fs -> pxy'' gs -> Dict (Member f gs)
included
-}
