{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctorsTests where

import Control.Monad.Coroutine.SuspensionFunctors
import Grisette
import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testMergeableSimpleEquivClass ::
  (HasCallStack, Mergeable x, Show x, Eq x) => x -> [DynamicSortedIdx] -> [(SymBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = resolveStrategy rootStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy rootStrategy t) @=? idxs
          fst (resolveStrategy rootStrategy f) @=? idxs
          fst (resolveStrategy rootStrategy r) @=? idxs
          m c t f @=? r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show x

testMergeableSimpleEquivClass' ::
  (HasCallStack, Mergeable x, Show y, Eq y) => (x -> y) -> x -> [DynamicSortedIdx] -> [(SymBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass' vis x idxs cases = do
  let (idxsT, s) = resolveStrategy rootStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy rootStrategy t) @=? idxs
          fst (resolveStrategy rootStrategy f) @=? idxs
          fst (resolveStrategy rootStrategy r) @=? idxs
          vis (m c t f) @=? vis r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show (vis x)

suspensionFunctorsTests :: TestTree
suspensionFunctorsTests =
  testGroup
    "SuspensionFunctorsSpec"
    [ testGroup
        "Mergeable for suspension functors"
        $ let visYield (Yield a b) = (a, b)
           in [ testProperty "Yield Integer Integer" $
                  ioProperty . \(x :: Integer, y :: Integer) -> do
                    let (idxs, SimpleStrategy s) = resolveStrategy rootStrategy (Yield x y)
                    idxs @=? [DynamicSortedIdx x, DynamicSortedIdx y]
                    visYield (s "a" (Yield x y) (Yield x y)) @=? visYield (Yield x y),
                testCase "Yield SymBool SymBool" $ do
                  testMergeableSimpleEquivClass'
                    (\(Yield a b) -> (a, b))
                    (Yield "a" "b" :: Yield SymBool SymBool)
                    []
                    [ ( "a",
                        Yield "b" "c",
                        Yield "d" "e",
                        Yield (ites "a" "b" "d") (ites "a" "c" "e")
                      )
                    ],
                testCase "Await SymBool SymBool" $ do
                  let SimpleStrategy s = rootStrategy :: MergingStrategy (Await SymBool SymBool)
                  let a1 = Await nots
                  let a2 = Await ("a" &&~)
                  let Await a3 = s "b" a1 a2
                  a3 "c" @=? ites "b" (nots "c") ("a" &&~ "c"),
                testCase "Request SymBool SymBool SymBool" $ do
                  let SimpleStrategy s = rootStrategy :: MergingStrategy (Request SymBool SymBool SymBool)
                  let a1 = Request "a" nots
                  let a2 = Request "b" ("c" &&~)
                  let Request v3 a3 = s "d" a1 a2
                  v3 @=? ites "d" "a" "b"
                  a3 "e" @=? ites "d" (nots "e") ("c" &&~ "e")
              ]
    ]
