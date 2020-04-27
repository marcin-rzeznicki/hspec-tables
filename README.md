# hspec-tables

[![Hackage](https://img.shields.io/hackage/v/hspec-tables.svg?logo=haskell)](https://hackage.haskell.org/package/hspec-tables)
[![Stackage Lts](http://stackage.org/package/hspec-tables/badge/lts)](http://stackage.org/lts/package/hspec-tables)
[![Stackage Nightly](http://stackage.org/package/hspec-tables/badge/nightly)](http://stackage.org/nightly/package/hspec-tables)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/marcin-rzeznicki/hspec-tables.svg?branch=master)](https://travis-ci.com/marcin-rzeznicki/hspec-tables)

`Test.Hspec.Tables` allows you to define table-driven (or, _by-example_) [HSpec](https://hspec.github.io/) tests. For example:

```haskell
import Test.Hspec
import Test.Hspec.Tables

example1 :: Spec
example1 =
  describe "multiplication table" $
    byExample
      ("x", "y", "result")
      [ (0, 0, 0),
        (0, 1, 0),
        (0, 2, 0),
        (1, 0, 0),
        (1, 1, 1),
        (1, 2, 2),
        (2, 0, 0),
        (2, 1, 2),
        (2, 2, 4)
      ]
      (\a b expected -> a * b == expected)
```

or

```haskell
example2 :: Spec
example2 =
  describe "reverse" $
    byExample
      ("list", "reversed")
      [("abc", "cba"), ("", ""), ("0123456", "6543210")]
      (shouldBe . reverse)
```

When you run these, you'll see that each row becomes a seperate test labelled with `show row` (so the requirement for rows is to consist of elements that have a `Show` instance):

```
Example specs
  multiplication table
    ("x","y","result")
      (0,0,0)
      (0,1,0)
      (0,2,0)
      (1,0,0)
      (1,1,1)
      (1,2,2)
      (2,0,0)
      (2,1,2)
      (2,2,4)
  reverse
    ("list","reversed")
      ("abc","cba")
      ("","")
      ("0123456","6543210")

Finished in 0.0008 seconds
12 examples, 0 failures

```

The `byExample` method is type-safe. The table _header_ must be a 2- to 7- element tuple of String and you must provide the same number of "columns" or it doesn't compile. So if you attempted to write:

```haskell
example1 :: Spec
example1 =
  describe "THIS EXAMPLE DOES NOT COMPILE" $
    byExample
      ("x", "y", "result") -- 3 columns
      [ (0, 0, 0, 0) ]     -- 4 columns
      (\a b c expected -> a * b * c == expected)
```

then you should get the following error, hopefully guiding to what the problem is

```
error:
    • Couldn't match type ‘([Char], [Char], [Char])’
                     with ‘(String, String, String, String)’
      Expected type: Test.Hspec.Tables.Header
                       (Integer, Integer, Integer, Integer)
        Actual type: ([Char], [Char], [Char])

```

The assertion function will match the table type, so if your table is of shape `(a, b, c)` then the assertion is assumed to be of type `(Example e) => a -> b -> c -> e` (ie. it's always curried). [Example](https://hackage.haskell.org/package/hspec/docs/Test-Hspec.html#t:Example) comes from `HSpec`

## Caveats

* You can define tables up-to 7 columns (adding columns beyond that requires providing an instance of the `Table` type-class)
