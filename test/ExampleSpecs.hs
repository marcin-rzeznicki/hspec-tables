module ExampleSpecs (exampleSpecs) where

import Test.Hspec
import Test.Hspec.Tables

exampleSpecs :: Spec
exampleSpecs = describe "Example specs" $ do
  example1
  example2

example1 :: Spec
example1 =
  describe "multiplication table" $
    byExample
      ("x", "y", "result")
      [(0, 0, 0), (0, 1, 0), (0, 2, 0), (1, 0, 0), (1, 1, 1), (1, 2, 2), (2, 0, 0), (2, 1, 2), (2, 2, 4)]
      (\a b expected -> a * b == expected)

example2 :: Spec
example2 =
  describe "reverse" $
    byExample
      ("list", "reversed")
      [("abc", "cba"), ("", ""), ("0123456", "6543210")]
      (shouldBe . reverse)
