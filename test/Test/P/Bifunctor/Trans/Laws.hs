{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Test.P.Bifunctor.Trans.Laws (
    bifunctorLaws
  ) where

import           Data.Functor.Identity (Identity(..))

import           P.Bifunctor.Trans

import           Test.QuickCheck
import           Test.QuickCheck.Function


bifunctorLaws
  :: forall
     (p :: * -> * -> *)
     (t :: * -> (* -> *) -> * -> *)
     (f :: * -> *)
     x y a b.
     (BifunctorTrans t, Functor f)
  => (Eq (p x a), Show (p x a))
  => (Eq (p y b), Show (p y b))
  => (forall z c. Identity (p z c) -> t z f c)
  -> (forall z c. t z f c -> Identity (p z c))
  -> p x a
  -> Fun x y
  -> Fun a b
  -> Property
bifunctorLaws mkT runT p (Fun _ f) (Fun _ g) =
  conjoin [
      functionEq mkT runT p (bimapT id id) id
    , functionEq mkT runT p (firstT id) id
    , functionEq mkT runT p (secondT id) id
    , functionEq mkT runT p (bimapT f g) (firstT f . secondT g)
    ]

functionEq
  :: (Eq q, Show q)
  => (Identity p -> t)
  -> (s -> Identity q)
  -> p
  -> (t -> s)
  -> (t -> s)
  -> Property
functionEq mkT runT p f g =
  runIdentity (runT (f (mkT (Identity p)))) ===
  runIdentity (runT (g (mkT (Identity p))))

