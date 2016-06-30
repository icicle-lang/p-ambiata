module P.Either (
    maybeToLeft
  , maybeToRight
  , leftToMaybe
  , rightToMaybe
  , ecase
  ) where

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
