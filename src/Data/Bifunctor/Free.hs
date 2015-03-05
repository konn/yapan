{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Bifunctor.Free where
import Data.Bifunctor

data BiFree p a b where
  Bimap :: (a -> b) -> (c -> d) -> p a c -> BiFree p b d

instance Bifunctor (BiFree p) where
  bimap f g (Bimap h k ini) = Bimap (f . h) (g . k) ini

liftBi :: p a b -> BiFree p a b
liftBi = Bimap id id

retractBi :: Bifunctor p => BiFree p a b -> p a b
retractBi (Bimap f g a) = bimap f g a

universalBi :: Bifunctor q => (forall a b. p a b -> q a b) -> BiFree p c d -> q c d
universalBi tran (Bimap h k p) = bimap h k (tran p)

