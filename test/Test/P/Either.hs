{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.P.Either where

import           Control.Monad (return)
import           Data.Either (Either(..))
import qualified Data.Either as E
import           Data.Function (($))
import           Data.Maybe (Maybe(..))

import           Prelude (String)
import           System.IO (IO)

import           P (Bool, Int)
import           P.Either

import           Test.QuickCheck


prop_maybeToLeft_just :: Int -> String -> Property
prop_maybeToLeft_just l r = maybeToLeft r (Just l) === Left l

prop_maybeToLeft_nothing :: Int -> Property
prop_maybeToLeft_nothing r = maybeToLeft r (Nothing :: Maybe String) === Right r

prop_maybeToRight_just :: Int -> String -> Property
prop_maybeToRight_just l r = maybeToRight l (Just r) === Right r

prop_maybeToRight_nothing :: Int -> Property
prop_maybeToRight_nothing l = maybeToRight l (Nothing :: Maybe String) === Left l

prop_rightToMaybe_right :: Int -> Property
prop_rightToMaybe_right r = rightToMaybe (Right r) === Just r

prop_rightToMaybe_left :: Int -> Property
prop_rightToMaybe_left r = rightToMaybe (Left r) === (Nothing :: Maybe String)

prop_leftToMaybe_right :: Int -> Property
prop_leftToMaybe_right r = leftToMaybe (Left r) === Just r

prop_leftToMaybe_left :: Int -> Property
prop_leftToMaybe_left r = leftToMaybe (Right r) === (Nothing :: Maybe String)

prop_right :: Int -> String -> Property
prop_right l r = maybeToRight l (rightToMaybe $ Right r) === Right r

prop_left :: Int -> String -> Property
prop_left l r = maybeToLeft r (leftToMaybe $ Left l) === Left l

prop_maybe :: Int -> String -> Property
prop_maybe l r = rightToMaybe (maybeToRight l $ Just r) === Just r

prop_nothing :: Int -> String -> Property
prop_nothing l r = leftToMaybe (maybeToLeft r $ Just l) === Just l

prop_flipEither :: Either Int String -> Property
prop_flipEither e@(Left x) = flipEither e === Right x
prop_flipEither e@(Right x) = flipEither e === Left x

prop_lefts_list :: [Either Int ()] -> Property
prop_lefts_list xs = lefts xs === E.lefts xs

prop_rights_list :: [Either () Int] -> Property
prop_rights_list xs = rights xs === E.rights xs


return []
tests :: IO Bool
tests = $quickCheckAll
