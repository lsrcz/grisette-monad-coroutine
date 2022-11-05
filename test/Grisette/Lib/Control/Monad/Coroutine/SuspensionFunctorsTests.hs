{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctorsTests where

import Control.Monad.Coroutine.SuspensionFunctors
import Grisette.Core.Data.Class.Mergeable
import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors ()
import Grisette.TestUtils.Mergeable
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

suspensionFunctorsTests :: TestTree
suspensionFunctorsTests =
  testGroup
    "SuspensionFunctorsSpec"
    [ testGroup
        "Mergeable for suspension functors"
        [ testProperty "Yield Integer Integer" $
            ioProperty . \(x :: Integer, y :: Integer) -> do
              testMergeableSimpleEquivClass'
                (\(Yield a b) -> (a, b))
                (Yield x y)
                [DynamicSortedIdx x, DynamicSortedIdx y]
                [(SSBool "a", Yield x y, Yield x y, Yield x y)],
          testCase "Yield SBool SBool" $ do
            testMergeableSimpleEquivClass'
              (\(Yield a b) -> (a, b))
              (Yield (SSBool "a") (SSBool "b"))
              []
              [ ( SSBool "a",
                  Yield (SSBool "b") (SSBool "c"),
                  Yield (SSBool "d") (SSBool "e"),
                  Yield (ITE (SSBool "a") (SSBool "b") (SSBool "d")) (ITE (SSBool "a") (SSBool "c") (SSBool "e"))
                )
              ],
          testCase "Await SBool SBool" $ do
            let SimpleStrategy s = gmergingStrategy :: GMergingStrategy SBool (Await SBool SBool)
            let a1 = Await Not
            let a2 = Await (And (SSBool "a"))
            let Await a3 = s (SSBool "b") a1 a2
            a3 (SSBool "c") @=? ITE (SSBool "b") (Not (SSBool "c")) (And (SSBool "a") (SSBool "c")),
          testCase "Request SBool SBool SBool" $ do
            let SimpleStrategy s = gmergingStrategy :: GMergingStrategy SBool (Request SBool SBool SBool)
            let a1 = Request (SSBool "a") Not
            let a2 = Request (SSBool "b") (And (SSBool "c"))
            let Request v3 a3 = s (SSBool "d") a1 a2
            v3 @=? ITE (SSBool "d") (SSBool "a") (SSBool "b")
            a3 (SSBool "e") @=? ITE (SSBool "d") (Not (SSBool "e")) (And (SSBool "c") (SSBool "e"))
        ]
    ]
