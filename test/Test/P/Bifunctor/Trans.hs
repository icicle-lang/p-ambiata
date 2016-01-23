{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.P.Bifunctor.Trans where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Functor.Identity (Identity(..))
import           Data.Tuple (swap)

import           P.Bifunctor.Trans

import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_bifunctor_ExceptT x f g =
  bifunctorLaws
    ExceptT
    runExceptT
    (x :: Either Int Int)
    (f :: Fun Int Int)
    (g :: Fun Int Int)

prop_bifunctor_StrictWriterT x f g =
  bifunctorLaws
    (Strict.WriterT . fmap swap)
    (fmap swap . Strict.runWriterT)
    (x :: (Int, Int))
    (f :: Fun Int Int)
    (g :: Fun Int Int)

prop_bifunctor_LazyWriterT x f g =
  bifunctorLaws
    (Lazy.WriterT . fmap swap)
    (fmap swap . Lazy.runWriterT)
    (x :: (Int, Int))
    (f :: Fun Int Int)
    (g :: Fun Int Int)

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

return []
tests :: IO Bool
tests = $quickCheckAll
