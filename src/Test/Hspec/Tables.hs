{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: (c) 2020 Marcin Rzeźnicki
-- SPDX-License-Identifier: MIT
-- Maintainer: Marcin Rzeźnicki <marcin.rzeznicki@gmail.com>
--
-- Table-driven (by example) HSpec tests
module Test.Hspec.Tables
  ( byExample,
    testTable,
  )
where

import Test.Hspec.Core.Spec

class Table r where
  type Header r
  type Forall r p
  apply :: Forall r p -> r -> p

  showHeader :: Header r -> String
  default showHeader :: Show (Header r) => Header r -> String
  showHeader = show

instance Table (a, b) where
  type Header (a, b) = (String, String)
  type Forall (a, b) p = a -> b -> p
  apply = uncurry

instance Table (a, b, c) where
  type Header (a, b, c) = (String, String, String)
  type Forall (a, b, c) p = a -> b -> c -> p
  apply f (a, b, c) = f a b c

byExample ::
  forall a r.
  (Table r, Example a, Show r) =>
  Header r ->
  [r] ->
  Forall r a ->
  SpecWith (Arg a)
byExample header table test =
  describe (showHeader @r header) $ mapM_ (\row -> specify @a (show row) $ apply test row) table

testTable ::
  forall a r.
  (Table r, Example a, Show r) =>
  Header r ->
  [r] ->
  Forall r a ->
  SpecWith (Arg a)
testTable = byExample @a
