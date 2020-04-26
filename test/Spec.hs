{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (join)
import Data.Foldable
import Data.Functor ((<&>))
import ExampleSpecs
import Test.Hspec
import Test.Hspec.Core.Runner (ColorMode (..), Config (..), Summary (..), defaultConfig, runSpec)
import Test.Hspec.Core.Spec (Item (..), runSpecM)
import Test.Hspec.Tables

main :: IO ()
main = hspec $ do
  spec
  exampleSpecs

spec :: Spec
spec = forM_ [spec2Bool, spec2BoolF, spec2Expectation, spec3, spec4, spec5, spec6, spec7] specMetaSpec

data MetaSpec b = forall a. Show a => MetaSpec {label :: String, def :: [a], thisSpec :: SpecWith b, expectedFailures :: Int}

withFailures :: MetaSpec a -> Int -> MetaSpec a
withFailures meta failures = meta {expectedFailures = failures}

describeMeta :: (Show a) => String -> [a] -> ([a] -> SpecWith b) -> MetaSpec b
describeMeta label def generateSpec = MetaSpec label def generatedSpec 0
  where
    generatedSpec = generateSpec def

spec2Bool :: MetaSpec ()
spec2Bool = describeMeta
  "spec2Bool"
  [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)]
  $ \examples -> byExample ("A", "expected") examples (\a expected -> expected == a * 2)

spec2BoolF :: MetaSpec ()
spec2BoolF =
  describeMeta
    "spec2BoolF"
    [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)]
    (\examples -> byExample ("A", "expected") examples (==))
    `withFailures` 4

spec2Expectation :: MetaSpec ()
spec2Expectation = describeMeta
  "spec2Expectation"
  [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)]
  $ \examples -> byExample ("A", "expected") examples (shouldBe . (* 2))

spec3 :: MetaSpec ()
spec3 = describeMeta
  "spec3"
  [(0, 0, 0), (1, 1, 2), (2, 2, 4), (3, 3, 6), (4, 4, 8), (5, 5, 10)]
  $ \examples -> byExample ("A", "B", "expected") examples (\a b expected -> a + b == expected)

spec4 :: MetaSpec ()
spec4 = describeMeta
  "spec4"
  [('a', 'b', 'c', "abc"), ('g', 'e', 'f', "gef"), ('x', 'y', 'z', "xyz")]
  $ \examples -> byExample ("A", "B", "C", "expected") examples (\a b c expected -> [a, b, c] `shouldBe` expected)

spec5 :: MetaSpec ()
spec5 = describeMeta
  "spec5"
  [('a', 'b', 'c', 'd', "abcd"), ('g', 'e', 'f', 'h', "gefh"), ('x', 'y', 'z', 'a', "xyza")]
  $ \examples -> byExample ("A", "B", "C", "D", "expected") examples (\a b c d expected -> [a, b, c, d] `shouldBe` expected)

spec6 :: MetaSpec ()
spec6 = describeMeta
  "spec6"
  [(1, 1, 1, 1, 1, 1), (2, 2, 2, 2, 2, 32)]
  $ \examples -> byExample ("A", "B", "C", "D", "E", "expected") examples (\a b c d e expected -> a * b * c * d * e == expected)

spec7 :: MetaSpec ()
spec7 = describeMeta
  "spec7"
  [(1, 1, 1, 1, 1, 1, 1), (2, 2, 2, 2, 2, 2, 64)]
  $ \examples -> byExample ("A", "B", "C", "D", "E", "F", "expected") examples (\a b c d e f expected -> a * b * c * d * e * f == expected)

_config :: Config
_config = defaultConfig {configIgnoreConfigFile = True, configColorMode = ColorNever}

specMetaSpec :: MetaSpec () -> Spec
specMetaSpec MetaSpec {..} = describe label $ do
  specify "Analysis" $ expectCorrectSpec def thisSpec
  let expectedSummary = Summary (length def) expectedFailures
  specify "Execution" $ runSpec thisSpec _config `shouldReturn` expectedSummary

expectCorrectSpec :: Show a => [a] -> SpecWith b -> Expectation
expectCorrectSpec def generatedSpec = runSpecM generatedSpec <&> getAllItems >>= (shouldMatchList labels . itemRequirements)
  where
    itemRequirements = map itemRequirement
    getAllItems = join . map toList
    labels = map show def
