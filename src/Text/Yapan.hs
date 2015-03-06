{-# LANGUAGE DataKinds, ExplicitNamespaces, FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms, ViewPatterns #-}
module Text.Yapan
       ( module Text.Yapan.Base,
         module Data.OpenUnion2,
         PandocBlocks, PandocInlines,
         PandocBlock, PandocInline,
         Document(..), Metadata(..)
       ) where
import Data.Monoid         ((<>))
import Data.OpenUnion2
import Prelude.Extras      (Eq2 (..), Ord2 (..), Show2 (..))
import Text.Yapan.Base
import Text.Yapan.Patterns

data Metadata = Metadata { metaAuthor :: String
                         , metaTitle  :: String
                         }
              deriving (Read, Show, Eq, Ord)

type PandocBlocks  = [Header, Para, Plain, List, DefinitionList, RawBlock]
type PandocInlines = [Str, Emph, RawInline]

type PandocBlock  = Block  PandocBlocks PandocInlines
type PandocInline = Inline PandocBlocks PandocInlines

data Document bs is =
  Document { docMetadata :: Metadata
           , docBody     :: [Block bs is]
           }

instance (Show2 (Union2 bs), Show2 (Union2 is)) => Show (Document bs is) where
  showsPrec _d (Document m b) =
    showString "Document { docMetadata = " . shows m .
    showString ", docBody = " . shows b . showString "} "

instance (Eq2 (Union2 bs), Eq2 (Union2 is)) => Eq (Document bs is) where
  Document m b == Document m' b' =
    m == m' && b == b'

instance (Ord2 (Union2 bs), Ord2 (Union2 is)) => Ord (Document bs is) where
  Document m b `compare` Document m' b' =
    m `compare` m' <> b `compare` b'

