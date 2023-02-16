{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : AMQP.OrphanInstances
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.TH (
  compileDerivingDecs,
) where

import Data.Char (toTitle)
import Data.GenValidity (GenValid (..))
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (
  Dec (StandaloneDerivD),
  DerivStrategy (AnyclassStrategy),
  Name,
  Q,
  Type (AppT, ConT),
  mkName,
  runIO,
 )
import Protocol.AMQP.Extracted (
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  extractInfo,
 )


compileDerivingDecs :: Q [Dec]
compileDerivingDecs = do
  (classInfos, _basicPropInfo) <- runIO extractInfo
  let innerDataNameOf x | length (miFields $ xmiInfo x) < 2 = Nothing
      innerDataNameOf x = Just $ xmiDataName x
      innerDataNames = catMaybes . map innerDataNameOf . ciMethods
      derivingNamesOf x = pascalCase (ciName x) : innerDataNames x
      decs = map (genvalidDeriv . mkName) $ concatMap derivingNamesOf classInfos
  pure decs


genvalidDeriv :: Name -> Dec
genvalidDeriv x = StandaloneDerivD (Just AnyclassStrategy) [] $ AppT (ConT ''GenValid) (ConT x)


pascalCase :: String -> String
pascalCase = concatMap titleCase . splitOn "-"


titleCase :: String -> String
titleCase (x : xs) = toTitle x : xs
titleCase xs = xs
