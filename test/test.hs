import           Control.Monad

import qualified P.BoolTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      P.BoolTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
