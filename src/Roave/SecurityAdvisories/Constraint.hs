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

-- not happy with this, as it is not a complete function
makeVersion :: [Natural] -> Maybe Version
makeVersion xs =
  case nonEmptyList of Nothing -> Nothing
                       Just nonEmptyNaturals -> Just $ Version nonEmptyNaturals
    where nonEmptyList = nonEmpty xs
