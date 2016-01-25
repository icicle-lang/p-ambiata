import           Control.Monad

import qualified Test.P.Applicative
import qualified Test.P.Bool
import qualified Test.P.Bifunctor.Trans
import qualified Test.P.Either
import qualified Test.P.Foldable
import qualified Test.P.Maybe
import qualified Test.P.Monoid
import qualified Test.P.Ord
import qualified Test.P.List
import qualified Test.P.Function

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.P.Applicative.tests
    , Test.P.Bifunctor.Trans.tests
    , Test.P.Bool.tests
    , Test.P.Either.tests
    , Test.P.Foldable.tests
    , Test.P.Maybe.tests
    , Test.P.Monoid.tests
    , Test.P.Ord.tests
    , Test.P.List.tests
    , Test.P.Function.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
