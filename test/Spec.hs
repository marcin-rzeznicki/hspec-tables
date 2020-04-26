{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (join)
import Data.Foldable
import Data.Functor ((<&>))
import Test.Hspec
import Test.Hspec.Core.Runner (ColorMode (..), Config (..), Summary (..), defaultConfig, runSpec)
import Test.Hspec.Core.Spec (Item (..), runSpecM)
import Test.Hspec.Tables

main :: IO ()
main = hspec spec

spec :: Spec
spec = forM_ [spec2Bool, spec2BoolF, spec2Expectation, spec3] specMetaSpec

data MetaSpec b = forall a. Show a => MetaSpec {label :: String, def :: [a], thisSpec :: SpecWith b, expectedFailures :: Int}

withFailures :: MetaSpec a -> Int -> MetaSpec a
withFailures meta failures = meta {expectedFailures = failures}

describeMeta :: (Show a) => String -> [a] -> ([a] -> SpecWith b) -> MetaSpec b
describeMeta label def generateSpec = MetaSpec label def generatedSpec 0
  where
    generatedSpec = generateSpec def

spec2Bool :: MetaSpec ()
spec2Bool = describeMeta "spec2Bool" [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)] $ \examples -> byExample ("A", "expected") examples (\a expected -> expected == a * 2)

spec2BoolF :: MetaSpec ()
spec2BoolF = describeMeta "spec2BoolF" [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)] (\examples -> byExample ("A", "expected") examples (==)) `withFailures` 4

spec2Expectation :: MetaSpec ()
spec2Expectation = describeMeta "spec2Expectation" [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)] $ \examples -> byExample ("A", "expected") examples (shouldBe . (* 2))

spec3 :: MetaSpec ()
spec3 = describeMeta "spec3" [(0, 0, 0), (1, 1, 2), (2, 2, 4), (3, 3, 6), (4, 4, 8), (5, 5, 10)] $ \examples -> byExample ("A", "B", "expected") examples (\a b expected -> a + b == expected)

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
