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
-- Table-driven (by example) HSpec tests.
--
-- Example usage:
--
-- > describe "multiplication table" $
-- >  byExample
-- >    ("x", "y", "result")
-- >    [ (0, 0, 0),
-- >      (0, 1, 0),
-- >      (0, 2, 0),
-- >      (1, 0, 0),
-- >      (1, 1, 1),
-- >      (1, 2, 2),
-- >      (2, 0, 0),
-- >      (2, 1, 2),
-- >      (2, 2, 4)
-- >    ]
-- >    (\a b expected -> a * b == expected)
--
-- > describe "reverse" $
-- >  byExample
-- >    ("list", "reversed")
-- >    [("abc", "cba"), ("", ""), ("0123456", "6543210")]
-- >    (shouldBe . reverse)
module Test.Hspec.Tables
  ( Table (..),
    byExample,
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

instance Table (a, b, c, d) where
  type Header (a, b, c, d) = (String, String, String, String)
  type Forall (a, b, c, d) p = a -> b -> c -> d -> p
  apply f (a, b, c, d) = f a b c d

instance Table (a, b, c, d, e) where
  type Header (a, b, c, d, e) = (String, String, String, String, String)
  type Forall (a, b, c, d, e) p = a -> b -> c -> d -> e -> p
  apply f (a, b, c, d, e) = f a b c d e

instance Table (a, b, c, d, e, f) where
  type Header (a, b, c, d, e, f) = (String, String, String, String, String, String)
  type Forall (a, b, c, d, e, f) p = a -> b -> c -> d -> e -> f -> p
  apply f (a, b, c, d, e, f_) = f a b c d e f_

instance Table (a, b, c, d, e, f, g) where
  type Header (a, b, c, d, e, f, g) = (String, String, String, String, String, String, String)
  type Forall (a, b, c, d, e, f, g) p = a -> b -> c -> d -> e -> f -> g -> p
  apply f (a, b, c, d, e, f_, g) = f a b c d e f_ g

-- | Creates a 'Spec' from the /table/ consisting of:
--
--    * header
--    * list of examples (/rows/)
--    * assertion
--
--  The resulting spec consists of one test per each /row/.
--  For example:
--
--  > byExample
--  >   ("list", "reversed")
--  >   [("abc", "cba"), ("", ""), ("0123456", "6543210")]
--  >  (shouldBe . reverse)
--
--  is equivalent to:
--
--  > describe (show ("list", "reversed")) $ do
--  >   specify (show ("abc", "cba")) $ reverse "abc" `shouldBe` "cba"
--  >   specify (show ("", "")) $ reverse "" `shouldBe` ""
--  >   specify (show ("0123456", "6543210")) $ reverse "0123456" `shouldBe` "6543210"
byExample ::
  forall a r.
  (Table r, Example a, Show r) =>
  -- | /header/ - tuple of strings (max 7 for now); used to 'describe' this spec, its arity == number of columns
  Header r ->
  -- | /rows/ - list of tuples of examples; arity of each tuple must match the number of columns (== arity of the /header/)
  [r] ->
  -- | /assertion/ - curried function from a row to an 'a' - if /row/ is @(b,c,d)@ then it must be @('Example' a) => b -> c -> d -> a@
  Forall r a ->
  SpecWith (Arg a)
byExample header table test =
  describe (showHeader @r header) $ mapM_ (\row -> specify @a (show row) $ apply test row) table

-- | Alias for 'byExample'
testTable ::
  forall a r.
  (Table r, Example a, Show r) =>
  Header r ->
  [r] ->
  Forall r a ->
  SpecWith (Arg a)
testTable = byExample @a
