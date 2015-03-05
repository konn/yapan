{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction, RankNTypes                    #-}
module Text.Japandoc.Lens (_Str) where
import Control.Lens       ()
import Control.Lens       (Prism, prism)
import Data.Bifunctor     (Bifunctor)
import Data.OpenUnion2
import Text.Japandoc.Base

_Str :: (Member Str is, Subset is is', Subset b b', All Bifunctor is', All Bifunctor b')
      => Prism (Inline b is) (Inline b' is') String String
_Str = prism str $ \(Inline' a) ->
  case prj a of
    Just (Str' s) -> Right s
    Nothing -> Left $ embedI $ Inline' a

{-
_Emph :: Member Emph is => Prism' (InlineF b (Union2 is)) [InlineF b (Union2 is)]
_Emph = prism' emph $ \(Inline' a) ->
  case prj a of
    Just (Emph' s) -> Just s
    Nothing -> Nothing
-}

