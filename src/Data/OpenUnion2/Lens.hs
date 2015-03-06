{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables                     #-}
module Data.OpenUnion2.Lens (summand, summand', summandWith) where
import Data.OpenUnion2
import Text.Yapan.Internal

import Data.Bifunctor  (Bifunctor (bimap))
import Data.Extensible (Forall)
import Data.Extensible (Member)
import Data.Extensible (Include)

-- | Pattern-matching on some direct summand of OpenUnion.
summand :: (Member hs h, Member hs' h', Include hs' hs)
        => Prism (Union2 hs a b) (Union2 hs' a b) (h a b) (h' a b)
summand = prism inj $ \a ->
  maybe (Left $ embed a) Right $ prj a
{-# INLINE summand #-}

-- | More flexible version of 'summand'.
summandWith :: (Forall Bifunctor hs,
                Member hs h, Member hs' h',
                Include hs' hs)
            => (a -> a') -> (b -> b')
            -> Prism (Union2 hs a b) (Union2 hs' a' b') (h a b) (h' a' b')
summandWith l r = prism inj $ \a ->
  maybe (Left $ embed $ bimap l r a) Right $ prj a
{-# INLINE summandWith #-}

-- | Simple version of 'summand'.
summand' :: Member hs h
         => Prism' (Union2 hs a b) (h a b)
summand' = prism inj $ \a ->  maybe (Left a) Right $ prj a

