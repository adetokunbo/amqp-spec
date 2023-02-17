{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Protocol.AMQP.TH
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Defines @Template Haskell@ combinators used to transform the XML representation
of commands into Haskell data types with instances of 'ParserOf' and 'ToBuilder'
that allow them to encoded and decoded from a byte stream in accordance with the
specification.
-}
module Protocol.AMQP.TH (
  -- * main compiler
  compileXml,

  -- * construct inner datatypes
  basicName,
  mkBasicHdrType,
  builderInstanceD,
  mkInnerDataDecl,
  parserOfInstanceD,

  -- * declare "BitIndexed" newtypes and instances
  newTypeDerivingD,
  bitIndexDecsOf,
  mkBitIndexDecs,
  bitIndexTyInstDecs,
  anyBitIndexedMbConE,
) where

import qualified Data.ByteString.Builder as BB
import Data.Char (toTitle)
import Data.Foldable (msum)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.Validity (Validity (..))
import Data.Word (Word16)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Protocol.AMQP.Attoparsec (choice, with2Prefixes, word16Pre)
import Protocol.AMQP.Bits
import Protocol.AMQP.Extracted (
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  basicName,
  extractInfo,
  methodName,
 )
import Protocol.AMQP.FieldValue


compileXml :: Q [Dec]
compileXml = do
  (classInfos, basicHdrTyInfo) <- runIO extractInfo
  classes <- fmap msum $ mapM mkClassDecs classInfos
  basicHdrs <- mkBasicHdrType basicHdrTyInfo
  let methodDecs = methodSumTyDecs classInfos
  pure $ methodDecs <> classes <> basicHdrs


asParserOfExp :: ClassInfo -> Exp
asParserOfExp ci =
  let pairsExp = ListE $ map toPairExp $ map asMatchTwoPair $ ciMethods ci
      toPairExp (x, y) = TupE [Just (LitE $ IntegerL $ toInteger x), Just y]
      firstApp = AppE (VarE 'with2Prefixes) (LitE $ IntegerL $ toInteger $ ciPrefix ci)
   in AppE firstApp pairsExp


methodSumTyDecs :: [ClassInfo] -> [Dec]
methodSumTyDecs classInfos =
  let ci2sumTy ci = (ciSumTyName ci, [mkName $ pascalCase $ ciName ci])
      conName ci = mkName $ ciSumTyName ci
      parserOfExp ci = AppE (AppE (VarE 'fmap) (ConE $ conName ci)) (VarE 'parserOf)
      sumTyDec = sumTypeD methodName $ map ci2sumTy classInfos
      parserOfExps = parserOfExp <$> classInfos
      parserOfDec = parserOfInstanceD' methodName $ AppE (VarE 'choice) $ ListE parserOfExps
      toBuilderDec = builderInstanceD methodName $ toBuilderPatExp <$> classInfos
      toBuilderOfX = AppE (VarE 'toBuilder) (VarE xAsVar)
      toBuilderPatExp ci = (ConP (conName ci) [VarP xAsVar], toBuilderOfX)
   in [sumTyDec, parserOfDec, toBuilderDec]


mkClassDecs :: ClassInfo -> DecsQ
mkClassDecs ci@ClassInfo {ciMethods = methods} = do
  let patExps = map (mkToBuilderPatExp (ciPrefix ci)) (ciMethods ci)
      mkInnerD x =
        if ((length $ miFields $ xmiInfo x) < 2)
          then pure []
          else pure $ mkInnerDataDecl (xmiDataName x) (xmiDataFields x)

      tyName = pascalCase $ ciName ci
      tyFields = map asSumConstr $ ciMethods ci
      sumTy = sumTypeD tyName tyFields
      parserOfExp = asParserOfExp ci
      toBuilderDec = builderInstanceD (pascalCase $ ciName ci) patExps
      parserOfDec = parserOfInstanceD' tyName parserOfExp
  innerTypes <- msum <$> mapM mkInnerD methods
  pure (sumTy : parserOfDec : toBuilderDec : innerTypes)


asSumConstr :: XMethodInfo -> (String, [Name])
asSumConstr xmi@XMethodInfo {xmiConstrName = con} = case xmiDataFields xmi of
  [] -> (con, [])
  [(_, name)] -> (con, [name])
  _gtThan1 -> (con, [mkName $ xmiDataName xmi])


mkToBuilderPatExp :: Word16 -> XMethodInfo -> (Pat, Exp)
mkToBuilderPatExp classPre xmi =
  let asLit x = LitE $ IntegerL $ toInteger x
      nameX = mkName "x"
      appClassLit = flip AppE $ asLit classPre
      appMethodLit = flip AppE $ asLit $ miPrefix $ xmiInfo xmi
      onlyPrefixesE = appMethodLit $ appClassLit $ VarE 'onlyPrefixes
      coreWithE = appMethodLit $ appClassLit $ VarE 'withPrefixes
      withPrefixesE = AppE coreWithE $ VarE nameX
      conName = mkName $ xmiConstrName xmi
   in case xmiDataFields xmi of
        [] -> (ConP conName [], onlyPrefixesE)
        _gtThan0 -> (ConP conName [VarP nameX], withPrefixesE)


asMatchTwoPair :: XMethodInfo -> (Word16, Exp)
asMatchTwoPair xmi@XMethodInfo {xmiConstrName = con} =
  let prefix = miPrefix $ xmiInfo xmi
      conExp = ConE $ mkName con
      parserExp = case xmiDataFields xmi of
        [] -> AppE (VarE 'pure) conExp
        _gtThan0 -> InfixE (Just conExp) (VarE '(<$>)) (Just $ VarE 'parserOf)
   in (prefix, parserExp)


mkBitIndexDecs :: [(String, Name)] -> DecsQ
mkBitIndexDecs fields =
  let indexed = zip [0 ..] fields
      mk (pos, (raw, original)) = bitIndexDecsOf (pascalCase raw) pos original
   in fmap concat $ traverse mk indexed


{- | Generates the BasicHdr data definition

data BasicHdr = BasicHdr {
  bhField1 :: !(Maybe NewtypeForField1)
  bhField2 :: !(Maybe NewtypeForField2)
  ...
  bhFieldN :: !(Maybe NewtypeForFieldN)
}

Every field in BasicHdr has a type that is a newtype registered with a BitIndex
-}
mkBasicHdrType :: [(String, Name)] -> DecsQ
mkBasicHdrType fields =
  let mkBasicFields = map asField
      asField (x, _y) = (nameOf x, typeOf x)
      nameOf = mkName . camelCase . ("bh-" ++)
      typeOf = maybeOf . mkName . pascalCase
      maybeOf x = AppT (ConT ''Maybe) (ConT x)
      mainRec = recordAdtDec (mkName basicName) (mkBasicFields fields)
      builderDec = mkBuildBasicHdrType fields
   in do
        bitIndexDecs <- mkBitIndexDecs fields
        parserDecs <- mkParseBasicHdrType fields
        pure $ bitIndexDecs <> [mainRec] <> [builderDec] <> parserDecs


mkInnerDataParserOfDoE :: String -> [(String, Name)] -> Exp
mkInnerDataParserOfDoE constr fields =
  let constrName = mkName constr
      packedName = mkName "packed"
      mkBoundName n = mkName $ n ++ "B"
      bitAtE = AppE (VarE 'bitAt) (VarE packedName)
      packedBitE n = AppE bitAtE (LitE $ IntegerL n)
      letD (pos, (n, _exp)) = ValD (VarP $ mkBoundName n) (NormalB $ packedBitE pos) []
      bindOf ((n, _e) :| []) = [BindS (VarP $ mkBoundName n) $ VarE 'parserOf]
      bindOf (x :| xs) =
        [ BindS (VarP packedName) (VarE 'parserOf)
        , LetS $ map letD $ zip [0 ..] (x : xs)
        ]
      boundVarsE (n, _e) = (mkName n, VarE $ mkBoundName n)
      recordConE = AppE (VarE 'pure) (RecConE constrName $ map boundVarsE fields)
   in DoE $ concatMap bindOf (groupBitFields fields) <> [NoBindS recordConE]


groupBitFields :: Foldable f => f (String, Name) -> [NE.NonEmpty (String, Name)]
groupBitFields =
  let grouper = \x y -> snd x == ''Bit && snd y == ''Bit
   in NE.groupBy grouper


mkParseBasicHdrType :: [(String, Name)] -> DecsQ
mkParseBasicHdrType fieldNames = do
  let argCount = length fieldNames
      withWord16Pre f = pure $ AppE (VarE 'word16Pre) f
  constrExp <- anyBitIndexedMbConE (mkName basicName) argCount
  parserOfInstanceD basicName $ withWord16Pre constrExp


-- fail if argCount < 2 ??
anyBitIndexedMbConE :: Name -> Int -> Q Exp
anyBitIndexedMbConE constrName argCount = do
  x <- newName "x"
  inv <- [|(anyBitIndexedMb $(varE x))|]
  let invs = inv :| replicate (argCount - 1) inv
  pure $ LamE [VarP x] $ applicativeConE constrName invs


mkBuildBasicHdrType :: [(String, Name)] -> Dec
mkBuildBasicHdrType fieldNames =
  let nameX = mkName "x"
      justCanBuildOf = justCanBuildConE nameX . camelCase . ("bh-" ++)
      accessors = map (\(x, _y) -> justCanBuildOf x) fieldNames
      applyBuildWithPrefix xs = AppE (VarE 'withBitIndexPrefix) $ ListE xs
   in builderInstanceD basicName [(VarP nameX, applyBuildWithPrefix accessors)]


justCanBuildConE :: Name -> String -> Exp
justCanBuildConE argName funcName =
  let asVarE = VarE $ mkName funcName
      fmapCanBuild = AppE (VarE 'fmap) (ConE 'CanBuild)
      withCanBuild = InfixE (Just fmapCanBuild) (VarE '(.)) (Just asVarE)
   in AppE withCanBuild (VarE argName)


bitIndexDecsOf :: String -> Integer -> Name -> DecsQ
bitIndexDecsOf wrapperName pos original = do
  let x = newTypeDerivingD wrapperName original [''ParserOf, ''Validity]
      z = builderForNewTyDecs wrapperName
  y <- bitIndexTyInstDecs wrapperName pos
  pure $ x : z : y


bitIndexTyInstDecs :: String -> Integer -> DecsQ
bitIndexTyInstDecs wrapperName pos =
  let name = mkName wrapperName
      decs =
        [d|
          type instance BitIndex $(conT name) = $(litT (numTyLit pos))
          |]
   in decs


builderForNewTyDecs :: String -> Dec
builderForNewTyDecs wrapperName =
  let name = mkName wrapperName
      nameX = mkName "x"
      constrP = ConP name [VarP nameX]
      instanceE = AppE (VarE 'toBuilder) (VarE nameX)
   in builderInstanceD wrapperName [(constrP, instanceE)]


builderInstanceD :: String -> [(Pat, Exp)] -> Dec
builderInstanceD instanceName patExps =
  let classT = ConT ''ToBuilder
      instanceT = ConT $ mkName instanceName
      builderT = ConT ''BB.Builder
      fullInstanceT = AppT (AppT classT instanceT) builderT
      theFunc = FunD 'toBuilder $ map clauseFrom patExps
      clauseFrom (pat, expr) = Clause [pat] (NormalB expr) []
   in InstanceD Nothing [] fullInstanceT [theFunc]


parserOfInstanceD :: String -> ExpQ -> Q [Dec]
parserOfInstanceD instanceName instanceExp =
  let name = mkName instanceName
   in [d|
        instance ParserOf $(conT name) where
          parserOf = $instanceExp
        |]


parserOfInstanceD' :: String -> Exp -> Dec
parserOfInstanceD' instanceName instanceExp =
  let classT = ConT ''ParserOf
      instanceT = ConT $ mkName instanceName
      fullInstanceT = AppT classT instanceT
      theClause = Clause [] (NormalB instanceExp) []
      theFunc = FunD 'parserOf [theClause]
   in InstanceD Nothing [] fullInstanceT [theFunc]


mkInnerDataDecl :: String -> [(String, Name)] -> [Dec]
mkInnerDataDecl name fields =
  let dataDecl = recordAdtDec' name fields
      builderDecl = mkInnerDataToBuilderDecs name fields
      doExp = mkInnerDataParserOfDoE name fields
      parseOfDecl = parserOfInstanceD' name doExp
   in [dataDecl, builderDecl, parseOfDecl]


mkInnerDataToBuilderDecs :: String -> [(String, Name)] -> Dec
mkInnerDataToBuilderDecs name fields =
  let nameX = mkName "x"
      invOf ((n, _e) :| []) = invToBuilderConE nameX n
      invOf (x :| xs) = invBuildBitsE nameX $ x : xs
      accessors = map invOf $ groupBitFields fields
      applyMconcat xs = AppE (VarE 'mconcat) $ ListE xs
   in builderInstanceD name [(VarP nameX, applyMconcat accessors)]


invBuildBitsE :: Name -> [(String, Name)] -> Exp
invBuildBitsE nameX fields =
  let mkAccessor (n, _e) = AppE (VarE $ mkName n) (VarE $ nameX)
   in AppE (VarE 'buildBits) $ ListE $ map mkAccessor fields


invToBuilderConE :: Name -> String -> Exp
invToBuilderConE argName funcName =
  let asVarE = VarE $ mkName funcName
      inner = AppE asVarE $ VarE argName
   in AppE (VarE 'toBuilder) inner


recordAdtDec' :: String -> [(String, Name)] -> Dec
recordAdtDec' typeName xs =
  let fields = map (\(x, y) -> (mkName x, ConT y)) xs
      tyName = mkName typeName
      con = RecC tyName $ (\(name, t) -> (name, fieldBang, t)) <$> fields
   in DataD [] tyName [] Nothing [con] [eqShowGenDeriv, validityDeriv]


recordAdtDec :: Name -> [(Name, Type)] -> Dec
recordAdtDec typeName fields =
  let con = RecC typeName $ (\(name, t) -> (name, fieldBang, t)) <$> fields
   in DataD [] typeName [] Nothing [con] [eqShowGenDeriv, validityDeriv]


sumTypeD :: String -> [(String, [Name])] -> Dec
sumTypeD typeName xs =
  let constrs = map (\(x, y) -> (mkName x, map ConT y)) xs
      derivs = [eqShowGenDeriv, validityDeriv]
   in DataD [] (mkName typeName) [] Nothing (fmap (uncurry sumCon) constrs) derivs


applicativeConE :: Name -> NonEmpty Exp -> Exp
applicativeConE constrName fieldExps =
  let exp0 :| otherExps = fieldExps
      startExp = InfixE (Just $ ConE constrName) (VarE '(<$>)) (Just exp0)
      step anExp acc = InfixE (Just acc) (VarE '(<*>)) (Just anExp)
   in foldr step startExp otherExps


newTypeDerivingD :: String -> Name -> [Name] -> Dec
newTypeDerivingD wrapperName original xs =
  let origTy = ConT original
      name = mkName wrapperName
      constr = NormalC name [(emptyBang, origTy)]
   in NewtypeD
        []
        name
        []
        Nothing
        constr
        [ eqShowDeriv
        , DerivClause (Just (ViaStrategy origTy)) $ map ConT xs
        ]


eqShowDeriv :: DerivClause
eqShowDeriv = DerivClause (Just StockStrategy) (map ConT [''Eq, ''Show])


eqShowGenDeriv :: DerivClause
eqShowGenDeriv = DerivClause (Just StockStrategy) (map ConT [''Eq, ''Show, ''Generic])


validityDeriv :: DerivClause
validityDeriv = DerivClause (Just AnyclassStrategy) [ConT ''Validity]


sumCon :: Name -> [Type] -> Con
sumCon a b = NormalC a $ fmap (fieldBang,) b


fieldBang :: Bang
fieldBang = Bang NoSourceUnpackedness SourceStrict


emptyBang :: Bang
emptyBang = Bang NoSourceUnpackedness NoSourceStrictness


xAsVar :: Name
xAsVar = mkName "x"


camelCase :: String -> String
camelCase = concat . titleCaseTail . splitOn "-"


pascalCase :: String -> String
pascalCase = concatMap titleCase . splitOn "-"


titleCaseTail :: [String] -> [String]
titleCaseTail (x : xs) = x : map titleCase xs
titleCaseTail y = y


titleCase :: String -> String
titleCase (x : xs) = toTitle x : xs
titleCase xs = xs

-- data TrialConfirm -- prefix 85
--   = SelectOk -- prefix 10
--   | Select !Bit} -- prefix 11
--   deriving (Eq, Show)

-- instance ToBuilder TrialConfirm BB.Builder where
--   toBuilder SelectOk = onlyPrefixes 85 10
--   toBuilder (Select x) = withPrefixes 85 11 x

-- instance ParserOf TrialConfirm where
--   parserOf =
--     matchTwoPrefixes
--       85
--       [ (10, pure SelectOk)
--       , (11, Select <$> parserOf)
--       ]

-- data DaNackData = DaNackData
--   { dnDeliveryTag :: !LongLongInt
--   , dnMultiple :: !Bit
--   , dnRequeue :: !Bit
--   }
--   deriving (Eq, Show)

-- instance ToBuilder DaNackData BB.Builder where
--   toBuilder x =
--     mconcat
--       [ toBuilder (dnDeliveryTag x)
--       , buildBits [dnMultiple x, dnRequeue x]
--       ]

-- instance ParserOf DaNackData where
--   parserOf = do
--     dnDeliveryTag <- parserOf
--     packed <- parserOf
--     let dnMultiple = bitAt packed 0
--         dnRequeue = bitAt packed 1
--     pure $ DaNackData {dnMultiple, dnRequeue, dnDeliveryTag}
