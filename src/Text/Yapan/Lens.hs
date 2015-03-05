{-# LANGUAGE ConstraintKinds, FlexibleContexts, LiberalTypeSynonyms       #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes #-}
module Text.Yapan.Lens (_Str, _Emph, _Header, _Plain) where
import Control.Applicative (Applicative)
import Control.Applicative (pure)
import Data.Extensible     (Member)
import Data.OpenUnion2
import Data.Profunctor     (Choice)
import Data.Profunctor     (right')
import Data.Profunctor     (dimap)
import Text.Yapan.Base

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

type Prism' s a = Prism s s a a

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

_Str :: Member is Str => Prism' (Inline bs is) String
_Str = prism' str $ \(Inline' a) ->
  case prj a of
    Just (Str' s) -> Just s
    Nothing -> Nothing

_Emph :: Member is Emph => Prism' (Inline bs is) [Inline bs is]
_Emph = prism' emph $ \(Inline' a) ->
  case prj a of
    Just (Emph' s) -> Just s
    Nothing -> Nothing

_Header :: Member bs Header => Prism' (Block bs is) (Int, [Inline bs is])
_Header = prism' (uncurry header) $ \(Block' a) ->
  case prj a of
    Just (Header' n s) -> Just (n, s)
    Nothing -> Nothing

_Plain :: Member bs Plain => Prism' (Block bs is) [Inline bs is]
_Plain = prism' plain $ \(Block' a) ->
  case prj a of
    Just (Plain' s) -> Just s
    Nothing -> Nothing

_Para :: Member bs Para => Prism' (Block bs is) [Inline bs is]
_Para = prism' para $ \(Block' a) ->
  case prj a of
    Just (Para' s) -> Just s
    Nothing -> Nothing
