{-# LANGUAGE NoImplicitPrelude #-}
module P.Either (
    maybeToLeft
  , maybeToRight
  , leftToMaybe
  , lefts
  , rightToMaybe
  , rights
  , ecase
  , flipEither
  , partitionEithers
  ) where

import           Data.Either (Either(..), either)
import qualified Data.Either as Either
import           Data.Foldable (Foldable, toList)
import           Data.Function ((.), flip, const)
import           Data.Maybe (Maybe(..), maybe)


maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

ecase :: Either l r -> (l -> b) -> (r -> b) -> b
ecase e l = flip (either l) e

flipEither :: Either a b -> Either b a
flipEither = either Right Left

lefts :: (Foldable f) => f (Either a b) -> [a]
lefts =
  Either.lefts . toList
{-# SPECIALIZE lefts :: [Either a b] -> [a] #-}

rights :: (Foldable f) => f (Either a b) -> [b]
rights =
  Either.rights . toList
{-# SPECIALIZE rights :: [Either a b] -> [b] #-}

partitionEithers :: (Foldable f) => f (Either a b) -> ([a], [b])
partitionEithers =
  Either.partitionEithers . toList
{-# SPECIALIZE partitionEithers :: [Either a b] -> ([a], [b]) #-}
