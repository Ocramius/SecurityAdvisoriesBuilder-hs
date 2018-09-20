module Roave.SecurityAdvisories.Constraint
  ( Version(..)
  , VersionLimit(..)
  , VersionBoundary(..)
  , VersionRange(..)
  , VersionConstraintUnion(..)
  , versionToString
  , canMergeRanges
  , stringToBoundary
  , stringToVersionLimit
  , stringToVersion
  ) where

import Data.List
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as N
import Numeric.Natural

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

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

versionToString :: Version -> String
versionToString (Version v) = intercalate "." (toList (N.map show v))

normalisedVersionNumbers :: [Natural] -> [Natural]
normalisedVersionNumbers toBeNormalised =
  case trimmed of [] -> [0]
                  xs -> xs
    where trimmed = Prelude.reverse . Prelude.dropWhile (== 0) . Prelude.reverse $ toBeNormalised

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
canMergeRanges (From (VersionBoundary vl1 version1)) (Till (VersionBoundary vl2 version2)) =
  (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Till (VersionBoundary vl1 version1)) (From (VersionBoundary vl2 version2)) =
  (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Range (VersionBoundary vl1 version1) _) (Till (VersionBoundary vl2 version2)) =
  (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges (Range _ (VersionBoundary vl1 version1)) (From (VersionBoundary vl2 version2)) =
  (vl1 `versionLimitsAdjacent` vl2) && version1 == version2
canMergeRanges _ _ = False

rangeContainsRange :: VersionRange -> VersionRange -> Bool
rangeContainsRange = undefined

rangeOverlapsWithRange :: VersionRange -> VersionRange -> Bool
rangeOverlapsWithRange = undefined

rangeAdjacentToRange :: VersionRange -> VersionRange -> Bool
rangeAdjacentToRange = undefined

type Parser = Parsec Void String

stringToBoundary :: String -> Maybe VersionBoundary
stringToBoundary = parseMaybe boundary

stringToVersionLimit :: String -> Maybe VersionLimit
stringToVersionLimit = parseMaybe versionLimit

stringToVersion :: String -> Maybe Version
stringToVersion = parseMaybe whileParser

omNomNom :: Parser ()
omNomNom = L.space empty empty empty

dot :: Parser String
dot = L.symbol omNomNom "."

whileParser :: Parser Version
whileParser = between omNomNom eof parseVersion

parseVersion :: Parser Version
parseVersion = f <$> sepBy1 (L.lexeme omNomNom L.decimal) dot
  where
    f l = Version (fromList $ normalisedVersionNumbers l)

versionLimit :: Parser VersionLimit
versionLimit =
  LessThanEquals <$ string "<=" <|>
  GreaterThanEquals <$ string ">=" <|>
  LessThan <$ string "<" <|>
  Equals <$ string "=" <|>
  GreaterThan <$ string ">"

boundary :: Parser VersionBoundary
boundary = do
  l <- versionLimit
  v <- parseVersion
  return (VersionBoundary l v)