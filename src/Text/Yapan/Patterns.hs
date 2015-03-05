{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Text.Yapan.Patterns 
       (pattern Block,
        pattern Inline,
        pattern Str,
        pattern Emph,
        pattern Header,
        pattern Plain,
        pattern Para) where
import Text.Yapan.Base
import Data.OpenUnion2

pattern Block  a <- Block'  (prj -> Just a)
pattern Inline a <- Inline' (prj -> Just a)

pattern Str  s <- Inline (Str' s)
pattern Emph e <- Inline (Emph' e)

pattern Header i hs <- Block (Header' i hs)
pattern Plain  is   <- Block (Plain' is)
pattern Para   is   <- Block (Para' is)
