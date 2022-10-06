module Main where

import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctorsTests
import Grisette.Lib.Control.Monad.CoroutineTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "grisette-monad-coroutine"
    [ testGroup
        "Grisette.Lib.Control.Monad"
        [ testGroup
            "Coroutine"
            [suspensionFunctorsTests],
          coroutineTests
        ]
    ]
