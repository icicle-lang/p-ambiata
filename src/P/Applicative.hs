module P.Applicative (
    ApplicativeMonoid (..)
  , valueOrEmpty
  , emptyOrValue
  , orEmpty
  , eitherA
  , (<<>>)
  ) where

import           Control.Applicative
import           Data.Semigroup
import           Prelude

valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a = if b then pure a else empty

emptyOrValue :: Alternative f => Bool -> a -> f a
emptyOrValue = valueOrEmpty . not

orEmpty :: (Alternative f, Monoid a) => f a -> f a
orEmpty f = f <|> pure mempty

-- | Combine two alternatives.
eitherA :: (Alternative f) => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)

-- | Applicative mappend
(<<>>) :: (Semigroup a, Applicative f) => f a -> f a -> f a
(<<>>) = liftA2 (<>)

-- | wrapper for monoids in an applicative context
newtype ApplicativeMonoid m a =
  ApplicativeMonoid { unApplicativeMonoid :: m a }
  deriving (Show, Eq)

instance (Semigroup a, Applicative m) => Semigroup (ApplicativeMonoid m a) where
  ApplicativeMonoid a <> ApplicativeMonoid b = ApplicativeMonoid (a <<>> b)

instance (Semigroup a, Monoid a, Applicative m) => Monoid (ApplicativeMonoid m a) where
  mempty = ApplicativeMonoid (pure mempty)
  mappend (ApplicativeMonoid a) (ApplicativeMonoid b) = ApplicativeMonoid (a <<>> b)
