{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Monad.CoroutineTests where

import Control.Monad.Coroutine hiding (merge)
import Control.Monad.Coroutine.SuspensionFunctors
import Grisette.Core
import Grisette.Lib.Control.Monad
import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors ()
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

coroutineTests :: TestTree
coroutineTests =
  testGroup
    "CoroutineTests"
    [ testGroup
        "MonadUnion for Coroutine"
        [ testCase "merge" $ do
            let Coroutine v =
                  merge
                    ( Coroutine $
                        unionIf
                          (SSBool "a")
                          (single $ Left $ Yield (SSBool "b") $ Coroutine $ single $ Right $ SSBool "c")
                          (single $ Left $ Yield (SSBool "d") $ Coroutine $ single $ Right $ SSBool "e") ::
                        Coroutine (Yield SBool) (UnionMBase SBool) SBool
                    )
            case v of
              SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
                x @=? ITE (SSBool "a") (SSBool "b") (SSBool "d")
                y @=? ITE (SSBool "a") (SSBool "c") (SSBool "e")
              _ -> assertFailure "Failed to merge Coroutine",
          testCase "mrgReturn" $ do
            case (mrgReturn 1 :: Coroutine (Yield SBool) (UnionMBase SBool) Integer) of
              Coroutine (SingleU (Right 1)) -> return ()
              _ -> assertFailure "mrgReturn for Coroutine is not working",
          testCase "mrgIf" $ do
            let Coroutine v =
                  mrgIf
                    (SSBool "a")
                    (Coroutine $ single $ Left $ Yield (SSBool "b") $ Coroutine $ single $ Right $ SSBool "c")
                    (Coroutine $ single $ Left $ Yield (SSBool "d") $ Coroutine $ single $ Right $ SSBool "e") ::
                    Coroutine (Yield SBool) (UnionMBase SBool) SBool
            case v of
              SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
                x @=? ITE (SSBool "a") (SSBool "b") (SSBool "d")
                y @=? ITE (SSBool "a") (SSBool "c") (SSBool "e")
              _ -> assertFailure "Failed to merge Coroutine"
        ],
      testCase "Mergeable for Coroutine" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (Coroutine (Yield SBool) (UnionMBase SBool) SBool)
        let a1 :: Coroutine (Yield SBool) (UnionMBase SBool) SBool =
              Coroutine (mrgReturn (Left (Yield (SSBool "a") (Coroutine (mrgReturn (Right $ SSBool "b"))))))
        let a2 :: Coroutine (Yield SBool) (UnionMBase SBool) SBool =
              Coroutine (mrgReturn (Left (Yield (SSBool "c") (Coroutine (mrgReturn (Right $ SSBool "d"))))))
        let Coroutine r = s (SSBool "e") a1 a2
        case r of
          SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
            x @=? ITE (SSBool "e") (SSBool "a") (SSBool "c")
            y @=? ITE (SSBool "e") (SSBool "b") (SSBool "d")
          _ -> assertFailure "Bad shape",
      testCase "SimpleMergeable for Coroutine" $ do
        let a1 :: Coroutine (Yield SBool) (UnionMBase SBool) SBool =
              Coroutine (mrgReturn (Left (Yield (SSBool "a") (Coroutine (mrgReturn (Right $ SSBool "b"))))))
        let a2 :: Coroutine (Yield SBool) (UnionMBase SBool) SBool =
              Coroutine (mrgReturn (Left (Yield (SSBool "c") (Coroutine (mrgReturn (Right $ SSBool "d"))))))
        let Coroutine r = mrgIte (SSBool "e") a1 a2
        let Coroutine r1 = mrgIte1 (SSBool "e") a1 a2
        let Coroutine ru1 = mrgIf (SSBool "e") a1 a2
        case r of
          SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
            x @=? ITE (SSBool "e") (SSBool "a") (SSBool "c")
            y @=? ITE (SSBool "e") (SSBool "b") (SSBool "d")
          _ -> assertFailure "Bad shape"
        case r1 of
          SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
            x @=? ITE (SSBool "e") (SSBool "a") (SSBool "c")
            y @=? ITE (SSBool "e") (SSBool "b") (SSBool "d")
          _ -> assertFailure "Bad shape"
        case ru1 of
          SingleU (Left (Yield x (Coroutine (SingleU (Right y))))) -> do
            x @=? ITE (SSBool "e") (SSBool "a") (SSBool "c")
            y @=? ITE (SSBool "e") (SSBool "b") (SSBool "d")
          _ -> assertFailure "Bad shape"
    ]
