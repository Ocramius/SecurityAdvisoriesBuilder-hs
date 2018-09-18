module Roave.SecurityAdvisories.Constraint
  ( Version(..)
  , VersionLimit(..)
  , VersionBoundary(..)
  , VersionRange(..)
  , VersionConstraintUnion(..)
  , makeVersionLimit
  , makeVersion
  , canMergeRanges
  ) where

import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as N
import           Numeric.Natural

data Version =
  Version (NonEmpty Natural)
  deriving (Eq, Show, Ord)

data VersionLimit
  = LessThanEquals
  | LessThan
  | Equals
  | GreaterThan
  | GreaterThanEquals
  deriving (Eq, Show)

data VersionBoundary =
  VersionBoundary VersionLimit
                  Version
  deriving (Eq, Show)

data VersionRange
  = Range VersionBoundary
          VersionBoundary
  | From VersionBoundary
  | Till VersionBoundary
  deriving (Eq, Show)

data VersionConstraintUnion =
  ConstraintUnion (NonEmpty VersionRange)
  deriving (Eq, Show)

-- | left side is an error message
makeVersionLimit :: String -> Either String VersionLimit
makeVersionLimit "<=" = Right LessThanEquals
makeVersionLimit "<" = Right LessThan
makeVersionLimit "=" = Right Equals
makeVersionLimit ">" = Right GreaterThan
makeVersionLimit ">=" = Right GreaterThanEquals
makeVersionLimit unknownDelimiter = Left $ "Unexpected version limit \"" ++ unknownDelimiter ++ "\" used"

makeVersion :: [Natural] -> Either String Version
makeVersion [] = Left "No version number provided"
makeVersion xs =
  case normalised of
    [] -> Right $ Version (fromList [0])
    _  -> Right $ Version (fromList normalised)
  where
    normalised = normalisedVersionNumbers xs

normalisedVersionNumbers :: [Natural] -> [Natural]
normalisedVersionNumbers = Prelude.reverse . Prelude.dropWhile (== 0) . Prelude.reverse

-- @TODO these rules seem a bit silly - can probably do with Ord + adjacency checks
versionLimitsAdjacent :: VersionLimit -> VersionLimit -> Bool
versionLimitsAdjacent LessThanEquals GreaterThan = True
versionLimitsAdjacent LessThan GreaterThanEquals = True
versionLimitsAdjacent GreaterThan LessThanEquals = True
versionLimitsAdjacent GreaterThanEquals LessThan = True
versionLimitsAdjacent GreaterThan Equals = True
versionLimitsAdjacent LessThan Equals = True
versionLimitsAdjacent Equals GreaterThan = True
versionLimitsAdjacent Equals LessThan = True
versionLimitsAdjacent _ _ = False

canMergeRanges :: VersionRange -> VersionRange -> Bool
canMergeRanges (From (VersionBoundary vl1 version1)) (Till (VersionBoundary vl2 version2)) = (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Till (VersionBoundary vl1 version1)) (From (VersionBoundary vl2 version2)) = (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Range (VersionBoundary vl1 version1) _) (Till (VersionBoundary vl2 version2)) = (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Range _ (VersionBoundary vl1 version1)) (From (VersionBoundary vl2 version2)) = (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges _ _ = False

rangeContainsRange :: VersionRange -> VersionRange -> Bool
rangeContainsRange = undefined

rangeOverlapsWithRange :: VersionRange -> VersionRange -> Bool
rangeOverlapsWithRange = undefined

rangeAdjacentToRange :: VersionRange -> VersionRange -> Bool
rangeAdjacentToRange = undefined