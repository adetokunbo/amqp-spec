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
  DerivStrategy (..),
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
  basicName,
  extractInfo,
  methodName,
 )
import Test.Validity.ParserOf (roundtripSpecFor')


compileRoundTripSpecDecs :: Q Exp
compileRoundTripSpecDecs = do
  (classInfos, _basicHdrTyInfo) <- runIO extractInfo
  let commandTypes = concatMap derivDataTypeNamesOf classInfos
      basicHdrTypes = map mkName $ methodName : basicName : map (pascalCase . fst) _basicHdrTyInfo
  pure $ DoE Nothing $ map genRoundtripStmt $ commandTypes <> basicHdrTypes


genRoundtripStmt :: Name -> Stmt
genRoundtripStmt x = NoBindS $ AppTypeE (VarE 'roundtripSpecFor') $ ConT x


compileDerivingDecs :: Q [Dec]
compileDerivingDecs = do
  (classInfos, basicHdrTyInfo) <- runIO extractInfo
  let commandTypes = concatMap derivDataTypeNamesOf classInfos
      allTypes = mkName methodName : mkName basicName : commandTypes
      commandDecs = genvalidDeriv <$> allTypes
      basicHdrDecs = map (uncurry genvalidViaDeriv) basicHdrTyInfo
  pure $ commandDecs <> basicHdrDecs


genvalidDeriv :: Name -> Dec
genvalidDeriv x = StandaloneDerivD (Just AnyclassStrategy) [] $ AppT (ConT ''GenValid) (ConT x)


genvalidViaDeriv :: String -> Name -> Dec
genvalidViaDeriv x origTy =
  let theInstance = AppT (ConT ''GenValid) $ ConT $ mkName $ pascalCase x
   in StandaloneDerivD (Just (ViaStrategy $ ConT origTy)) [] theInstance


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
