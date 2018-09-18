module Roave.SecurityAdvisories.Constraint
(
  Version(..),
  VersionLimit(..),
  VersionBoundary(..),
  VersionRange(..),
  VersionConstraintUnion(..),
  makeVersionLimit,
  makeVersion
)
where

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as N
import Numeric.Natural

data Version = Version (NonEmpty Natural)
  deriving (Eq, Show)

data VersionLimit = LessThanEquals | LessThan | Equals | GreaterThan | GreaterThanEquals
  deriving (Eq, Show)

data VersionBoundary = VersionBoundary VersionLimit Version
  deriving (Eq, Show)

data VersionRange = Range VersionBoundary VersionBoundary | From VersionBoundary | Till VersionBoundary
  deriving (Eq, Show)

data VersionConstraintUnion = ConstraintUnion (NonEmpty VersionRange)
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
  case normalised of [] -> Right $ Version (fromList [0])
                     _ -> Right $ Version (fromList normalised)
    where normalised = normalisedVersionNumbers xs

normalisedVersionNumbers :: [Natural] -> [Natural]
normalisedVersionNumbers = Prelude.reverse . Prelude.dropWhile (== 0) . Prelude.reverse

instance Ord Version where
  (Version v1) `compare` (Version v2) = compare v1 v2
