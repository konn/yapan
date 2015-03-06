{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase    #-}
{-# LANGUAGE LiberalTypeSynonyms, MultiParamTypeClasses       #-}
{-# LANGUAGE NoMonomorphismRestriction, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables                              #-}
module Text.Yapan.Internal (Lens, Lens', lens, Prism, Prism',
                            Traversal, Traversal',view, review,
                            prism, prism', Iso, Iso', iso) where
import Control.Applicative   (Applicative)
import Control.Applicative   (pure)
import Control.Applicative   (Const (..))
import Control.Applicative   ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Profunctor       (Choice, Profunctor, dimap, right')
import Data.Tagged           (Tagged (..))

type Simple f s a =  f s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Simple Prism s a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

review :: forall s a. Prism' s a -> a -> s
review p = runIdentity . unTagged . p . Tagged . Identity
{-# INLINE review #-}

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> (f t)
type Lens' a b = Simple Lens a b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> (f t)
type Traversal' a b = Simple Lens a b

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Iso' s a = Simple Iso s a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

view :: Getting a s a -> s -> a
view p = getConst . p Const
{-# INLINE view #-}
