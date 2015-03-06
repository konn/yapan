{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase    #-}
{-# LANGUAGE LiberalTypeSynonyms, MultiParamTypeClasses       #-}
{-# LANGUAGE NoMonomorphismRestriction, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables                              #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
module Text.Yapan.Lens
       ( -- * Prisms for standard elements
         YapanPrismic,
         BlockPrism, BlockPrism', InlinePrism, InlinePrism',
         InlineMonoPrism, InlineMonoPrism',
         BlockMonoPrism, BlockMonoPrism',
         Mono, Mono',
         _Str, _Str',
         _Emph, _Emph', _Header, _Header',
         _Plain, _Para, _Para', _List, _List',
         _DefinitionList, _DefinitionList',
         _RawInline, _RawInline', _RawBlock, _RawBlock',
         -- * Utility function to create prisms.
         mkGenPrism, mkGenPrism', mkPrismI, mkPrismI',
         mkPrismB, mkPrismB'
       ) where
import Control.Applicative (Applicative)
import Control.Applicative (pure)
import Data.Bifunctor      (Bifunctor)
import Data.Extensible     (Forall)
import Data.OpenUnion2
import Data.Profunctor     (Choice)
import Data.Profunctor     (right')
import Data.Profunctor     (dimap)
import Data.Proxy          (Proxy (..))
import Text.Yapan.Base

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

type Prism' s a = Prism s s a a

type YapanPrismic bs is bs' is' =
  (Forall Bifunctor bs', Forall Bifunctor is', Subset bs bs', Subset is is')

type BlockPrism b bs is bs' is' a a' =
  (YapanPrismic bs is bs' is', Element b bs)
  => Prism (Block bs is) (Block bs' is') a a'

type BlockPrism' b bs is a = (Element b bs) => Prism' (Block bs is) a

type InlinePrism i bs is bs' is' a a' =
  (YapanPrismic bs is bs' is', Element i is)
  => Prism (Inline bs is) (Inline bs' is') a a'

type InlinePrism' i bs is a = (Element i is) => Prism' (Inline bs is) a

type Mono  p a b c = forall bs is bs' is'. p a bs is bs' is' b c
type Mono' p a b   = forall bs is.         p a bs is         b

type InlineMonoPrism  i a b = Mono InlinePrism i a b
type InlineMonoPrism' i a   = Mono' InlinePrism' i a

type BlockMonoPrism  i a b = Mono BlockPrism i a b
type BlockMonoPrism' i a   = Mono' BlockPrism' i a

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

mkGenPrism :: (Subset is is', Subset bs bs')
           => (i (Block bs' is') (Inline bs' is') -> container (Union2 bs') (Union2 is'))
           -> (container (Union2 bs) (Union2 is)  -> container (Union2 bs') (Union2 is'))
           -> (container (Union2 bs) (Union2 is)  -> Maybe (i (Block bs is) (Inline bs is)))
           -> (b -> i (Block bs' is') (Inline bs' is'))
           -> (i (Block bs is) (Inline bs is) -> a)
           -> Prism (container (Union2 bs) (Union2 is)) (container (Union2 bs') (Union2 is')) a b
mkGenPrism inc emb pr constr destr =
  prism (inc . constr) $ \a -> maybe (Left $ emb a) (Right . destr) $ pr a

mkGenPrism' :: (i (Block bs is) (Inline bs is) -> container (Union2 bs) (Union2 is))
            -> (container (Union2 bs) (Union2 is)  -> Maybe (i (Block bs is) (Inline bs is)))
            -> (b -> i (Block bs is) (Inline bs is))
            -> (i (Block bs is) (Inline bs is) -> a)
            -> Prism (container (Union2 bs) (Union2 is)) (container (Union2 bs) (Union2 is)) a b
mkGenPrism' inc pr constr destr = prism' (inc . constr) $ fmap destr . pr

mkPrismI :: forall is is' bs bs' i a b.
         (Forall Bifunctor bs', Forall Bifunctor is',
          Element i is, Subset is is', Subset bs bs')
          => (b -> i (Block bs' is') (Inline bs' is'))
          -> (i (Block bs is) (Inline bs is) -> a)
          -> Prism (Inline bs is) (Inline bs' is') a b
mkPrismI = mkGenPrism (includeI (Proxy :: Proxy is)) embedI prjI

mkPrismI' :: (Element i is)
          => (a -> i (Block bs is) (Inline bs is))
          -> (i (Block bs is) (Inline bs is) -> a)
          -> Prism' (Inline bs is) a
mkPrismI' = mkGenPrism' inline prjI

mkPrismB :: forall is is' bs bs' b a a'.
         (Forall Bifunctor bs', Forall Bifunctor is',
          Element b bs, Subset is is', Subset bs bs')
          => (a' -> b (Block bs' is') (Inline bs' is'))
          -> (b (Block bs is) (Inline bs is) -> a)
          -> Prism (Block bs is) (Block bs' is') a a'
mkPrismB = mkGenPrism (includeB (Proxy :: Proxy bs)) embedB prjB

mkPrismB' :: (Element b bs)
          => (a -> b (Block bs is) (Inline bs is))
          -> (b (Block bs is) (Inline bs is) -> a)
          -> Prism' (Block bs is) a
mkPrismB' = mkGenPrism' block prjB

_Str :: InlineMonoPrism Str String String
_Str = mkPrismI Str' $ \case Str' a -> a

_Str' :: InlineMonoPrism' Str String
_Str' = mkPrismI' Str' $ \case Str' a -> a

_Emph :: InlinePrism Emph bs is bs' is' [Inline bs is] [Inline bs' is']
_Emph = mkPrismI Emph' $ \case Emph' a -> a

_Emph' :: InlinePrism' Emph bs is [Inline bs is]
_Emph' = mkPrismI' Emph' $ \case Emph' a -> a

_Header :: BlockPrism Header bs is bs' is' (Int, [Inline bs is]) (Int, [Inline bs' is'])
_Header = mkPrismB (uncurry Header') $ \case Header' a b -> (a, b)

_Header' :: BlockPrism' Header bs is (Int, [Inline bs is])
_Header' = mkPrismB' (uncurry Header') $ \case Header' a b -> (a, b)

_Plain :: BlockPrism Plain bs is bs' is' [Inline bs is] [Inline bs' is']
_Plain = mkPrismB Plain' $ \case Plain' a -> a

_Plain' :: BlockPrism' Plain bs is [Inline bs is]
_Plain' = mkPrismB' Plain' $ \case Plain' a -> a

_Para :: BlockPrism Para bs is bs' is' [Inline bs is] [Inline bs' is']
_Para = mkPrismB Para' $ \case Para' a -> a

_Para' :: BlockPrism' Para bs is [Inline bs is]
_Para' = mkPrismB' Para' $ \case Para' a -> a

_RawInline' :: InlineMonoPrism' RawInline (Format, String)
_RawInline' = mkPrismI' (uncurry RawInline') $ \ case (RawInline' fmt s) -> (fmt, s)

_RawInline :: InlineMonoPrism RawInline (Format, String) (Format, String)
_RawInline = mkPrismI (uncurry RawInline') $ \ case (RawInline' fmt s) -> (fmt, s)

_RawBlock' :: BlockMonoPrism' RawBlock (Format, String)
_RawBlock' = mkPrismB' (uncurry RawBlock') $ \ case (RawBlock' fmt s) -> (fmt, s)

_RawBlock :: BlockMonoPrism RawBlock (Format, String) (Format, String)
_RawBlock = mkPrismB (uncurry RawBlock') $ \ case (RawBlock' fmt s) -> (fmt, s)

_List :: BlockPrism List bs is bs' is' (ListType, [[Block bs is]]) (ListType, [[Block bs' is']])
_List = mkPrismB (uncurry List') $ \case (List' fmt s) -> (fmt, s)

_List' :: BlockPrism' List bs is (ListType, [[Block bs is]])
_List' = mkPrismB' (uncurry List') $ \case (List' fmt s) -> (fmt, s)

_DefinitionList :: BlockPrism DefinitionList bs is bs' is'
                              [([Inline bs is], [[Block bs is]])]
                              [([Inline bs' is'], [[Block bs' is']])]
_DefinitionList = mkPrismB DefinitionList' $
                  \case  (DefinitionList' f) -> f

_DefinitionList' :: BlockPrism' DefinitionList bs is
                              [([Inline bs is], [[Block bs is]])]
_DefinitionList' = mkPrismB' DefinitionList' $
                   \case  (DefinitionList' f) -> f
