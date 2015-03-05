{-# LANGUAGE DataKinds, ExplicitNamespaces, PatternSynonyms, ViewPatterns #-}
module Text.Japandoc (Block, Inline,
                      ) where
import Data.Monoid        ((<>))
import Data.OpenUnion2
import Prelude.Extras     (Eq2 (..), Ord2 (..), Show2 (..))
import Text.Japandoc.Base

