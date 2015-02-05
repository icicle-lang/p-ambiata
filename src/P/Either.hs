module P.Either (
    maybeToLeft
  , maybeToRight
  , leftToMaybe
  , leftMap
  , rightToMaybe
  ) where

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

leftMap :: (l -> l') -> Either l r -> Either l' r
leftMap f = either (Left . f) Right

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just
