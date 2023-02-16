{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : AMQP.OrphanInstances
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.TH (
  compileDerivingDecs,
  compileRoundTripSpecDecs,
) where

import Data.Char (toTitle)
import Data.GenValidity (GenValid (..))
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (
  Dec (StandaloneDerivD),
  DerivStrategy (AnyclassStrategy),
  Exp (..),
  Name,
  Q,
  Stmt (..),
  Type (..),
  mkName,
  runIO,
 )
import Protocol.AMQP.Extracted (
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  extractInfo,
 )
import Test.Validity.ParserOf (roundtripSpecFor')


compileRoundTripSpecDecs :: Q Exp
compileRoundTripSpecDecs = do
  (classInfos, _basicPropInfo) <- runIO extractInfo
  pure $ DoE $ map genRoundtripStmt $ concatMap derivDataTypeNamesOf classInfos


genRoundtripStmt :: Name -> Stmt
genRoundtripStmt x = NoBindS $ AppTypeE (VarE 'roundtripSpecFor') $ ConT x


compileDerivingDecs :: Q [Dec]
compileDerivingDecs = do
  (classInfos, _basicPropInfo) <- runIO extractInfo
  pure $ map genvalidDeriv $ concatMap derivDataTypeNamesOf classInfos


genvalidDeriv :: Name -> Dec
genvalidDeriv x = StandaloneDerivD (Just AnyclassStrategy) [] $ AppT (ConT ''GenValid) (ConT x)


derivDataTypeNamesOf :: ClassInfo -> [Name]
derivDataTypeNamesOf ci =
  let innerDataNameOf x | length (miFields $ xmiInfo x) < 2 = Nothing
      innerDataNameOf x = Just $ xmiDataName x
      innerDataNames = catMaybes . map innerDataNameOf . ciMethods
   in map mkName $ pascalCase (ciName ci) : innerDataNames ci


pascalCase :: String -> String
pascalCase = concatMap titleCase . splitOn "-"


titleCase :: String -> String
titleCase (x : xs) = toTitle x : xs
titleCase xs = xs
