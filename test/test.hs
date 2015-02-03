import           Control.Monad

import qualified P.ApplicativeTest
import qualified P.BoolTest
import qualified P.MonoidTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      P.ApplicativeTest.tests
    , P.BoolTest.tests
    , P.MonoidTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
