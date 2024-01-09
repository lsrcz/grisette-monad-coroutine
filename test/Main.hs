module Main where

import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctorsTests
import Grisette.Lib.Control.Monad.CoroutineTests
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ testGroup
        "Grisette.Lib.Control.Monad"
        [ testGroup
            "Coroutine"
            [suspensionFunctorsTests],
          coroutineTests
        ]
    ]
