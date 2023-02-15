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
  -- * functions
  compileXml,
  mkManyClassDecs,
  mkBasicProperties,
  builderInstanceD,

  -- * construct inner datatypes
  mkMethodInnerData,
  mkInnerDataDecl,
  mkInnerDataToBuilderDecs,
  mkParserOfInstance,

  -- * declare "BitIndexed" newtypes and instances
  newParserOfType,
  bitIndexDecsOf,
  mkBitIndexDecs,
  bitIndexTyInstDecs,
  anyBitIndexedMbConE,

  -- * focussed TH constructors
  sumAdtDec,
  sumAdtDec',
  recordAdtDec,
  recordAdtDec',
) where

import qualified Data.ByteString.Builder as BB
import Data.Char (toTitle)
import Data.Foldable (msum)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.Word (Word16)
import Language.Haskell.TH
import Protocol.AMQP.Attoparsec (with2Prefixes, word16Pre)
import Protocol.AMQP.Bits
import Protocol.AMQP.Extracted (
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  extractInfo,
 )
import Protocol.AMQP.FieldValue


compileXml :: Q [Dec]
compileXml = do
  (classInfos, basicPropInfo) <- runIO extractInfo
  classes <- mkManyClassDecs classInfos
  basicProps <- mkBasicProperties basicPropInfo
  pure $ classes <> basicProps


asToBuilderDec :: ClassInfo -> Dec
asToBuilderDec ci =
  let patExps = map (mkToBuilderPatExp (ciPrefix ci)) (ciMethods ci)
   in builderInstanceD (pascalCase $ ciName ci) patExps


asParserOfExp :: ClassInfo -> Exp
asParserOfExp ci =
  let pairsExp = ListE $ map toPairExp $ map asMatchTwoPair $ ciMethods ci
      toPairExp (x, y) = TupE [Just (LitE $ IntegerL $ toInteger x), Just y]
      firstApp = AppE (VarE 'with2Prefixes) (LitE $ IntegerL $ toInteger $ ciPrefix ci)
   in AppE firstApp pairsExp


asSumTy :: ClassInfo -> Dec
asSumTy ci =
  let sums = map asSumConstr $ ciMethods ci
   in sumAdtDec' (pascalCase $ ciName ci) sums


mkClassDecs :: ClassInfo -> DecsQ
mkClassDecs ci@ClassInfo {ciMethods = methods} = do
  let mkInnerD x =
        if ((length $ miFields $ xmiInfo x) < 2)
          then pure []
          else mkInnerDataDecl (xmiDataName x) (xmiDataFields x)

      tyName = pascalCase $ ciName ci
      sumTy = asSumTy ci
      parserOfExp = asParserOfExp ci
      toBuilderDec = asToBuilderDec ci
  innerTypes <- msum <$> mapM mkInnerD methods
  parserOfInst <- mkParserOfInstance tyName $ pure parserOfExp
  pure (sumTy : (innerTypes <> parserOfInst <> [toBuilderDec]))


mkManyClassDecs :: [ClassInfo] -> DecsQ
mkManyClassDecs = fmap msum . mapM mkClassDecs


mkMethodInnerData :: XMethodInfo -> DecsQ
mkMethodInnerData x = mkInnerDataDecl (xmiDataName x) (xmiDataFields x)


asSumConstr :: XMethodInfo -> (String, [Name])
asSumConstr xmi@XMethodInfo {xmiConstrName = con} = case xmiDataFields xmi of
  [] -> (con, [])
  [(_, name)] -> (con, [name])
  _anythingElse -> (con, [mkName $ xmiDataName xmi])


mkToBuilderPatExp :: Word16 -> XMethodInfo -> (Pat, Exp)
mkToBuilderPatExp classPre xmi =
  let asLit x = LitE $ IntegerL $ toInteger x
      xName = mkName "x"
      appClassLit = flip AppE $ asLit classPre
      appMethodLit = flip AppE $ asLit $ miPrefix $ xmiInfo xmi
      onlyPrefixesE = appMethodLit $ appClassLit $ VarE 'onlyPrefixes
      coreWithE = appMethodLit $ appClassLit $ VarE 'withPrefixes
      withPrefixesE = AppE coreWithE $ VarE xName
      conName = mkName $ xmiConstrName xmi
   in case xmiDataFields xmi of
        [] -> (ConP conName [], onlyPrefixesE)
        _anythingElse -> (ConP conName [VarP xName], withPrefixesE)


asMatchTwoPair :: XMethodInfo -> (Word16, Exp)
asMatchTwoPair xmi@XMethodInfo {xmiConstrName = con} =
  let prefix = miPrefix $ xmiInfo xmi
      conExp = ConE $ mkName con
      parserExp = case xmiDataFields xmi of
        [] -> AppE (VarE 'pure) conExp
        _anythingElse -> InfixE (Just conExp) (VarE '(<$>)) (Just $ VarE 'parserOf)
   in (prefix, parserExp)


mkBitIndexDecs :: [(String, Name)] -> DecsQ
mkBitIndexDecs fields =
  let indexed = zip [1 ..] fields
      mk (pos, (raw, original)) = bitIndexDecsOf (pascalCase raw) pos original
   in fmap concat $ traverse mk indexed


{- | Generates the BasicProperties data definition

data BasicProperties = BasicProperties {
  bpField1 :: !(Maybe NewtypeForField1)
  bpField2 :: !(Maybe NewtypeForField2)
  ...
  bpFieldN :: !(Maybe NewtypeForFieldN)
}

Every field in BasicProperties has a type that is a newtype registered with a BitIndex
-}
mkBasicProperties :: [(String, Name)] -> DecsQ
mkBasicProperties fieldNames =
  let mkBasicFields = map asField
      asField (x, _y) = (nameOf x, typeOf x)
      nameOf = mkName . camelCase . ("bp-" ++)
      typeOf = maybeOf . mkName . pascalCase
      maybeOf x = AppT (ConT ''Maybe) (ConT x)
      mainRec = recordAdtDec (mkName "BasicProperties") (mkBasicFields fieldNames)
   in do
        bitIndexDecs <- mkBitIndexDecs fieldNames
        builderDecs <- mkBuildBasicProperties fieldNames
        parserDecs <- mkParseBasicProperties fieldNames
        pure $ bitIndexDecs <> [mainRec] <> builderDecs <> parserDecs


groupBitFields :: Foldable f => f (String, Name) -> [NE.NonEmpty (String, Name)]
groupBitFields =
  let grouper = \x y -> snd x == ''Bit && snd y == ''Bit
   in NE.groupBy grouper


mkRecParserOfInstance :: String -> [(String, Name)] -> DecsQ
mkRecParserOfInstance recName fields = do
  mkParserOfInstance recName $ pure $ mkInnerDataParserOfDoE recName fields


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


mkParseBasicProperties :: [(String, Name)] -> DecsQ
mkParseBasicProperties fieldNames = do
  let argCount = length fieldNames
      instanceName = "BasicProperties"
      withWord16Pre f = pure $ AppE (VarE 'word16Pre) f
  constrExp <- anyBitIndexedMbConE (mkName instanceName) argCount
  mkParserOfInstance instanceName $ withWord16Pre constrExp


-- fail if argCount < 2 ??
anyBitIndexedMbConE :: Name -> Int -> Q Exp
anyBitIndexedMbConE constrName argCount = do
  x <- newName "x"
  inv <- [|(anyBitIndexedMb $(varE x))|]
  let invs = inv :| replicate (argCount - 1) inv
  pure $ LamE [VarP x] $ applicativeConE constrName invs


-- mkBuildBasicProperties' :: [(String, Name)] -> DecsQ
-- mkBuildBasicProperties' fieldNames = do
--   nameX <- newName "x"
--   let justCanBuildOf = justCanBuildConE nameX . camelCase . ("bp-" ++)
--       accessors = map (\(x, _y) -> justCanBuildOf x) fieldNames
--       funName = mkName "buildBasicProperties"
--       applyBuildWithPrefix xs = AppE (VarE 'buildWithPrefix) $ ListE xs
--       lambda = LamE [VarP nameX] $ applyBuildWithPrefix accessors
--   pure $ [FunD funName [Clause [] (NormalB lambda) []]]

mkBuildBasicProperties :: [(String, Name)] -> DecsQ
mkBuildBasicProperties fieldNames = do
  nameX <- newName "x"
  let justCanBuildOf = justCanBuildConE nameX . camelCase . ("bp-" ++)
      accessors = map (\(x, _y) -> justCanBuildOf x) fieldNames
      applyBuildWithPrefix xs = AppE (VarE 'buildWithPrefix) $ ListE xs
  mkBuilderInstance "BasicProperties" (varP nameX) (pure $ applyBuildWithPrefix accessors)


justCanBuildConE :: Name -> String -> Exp
justCanBuildConE argName funcName =
  let asVarE = VarE $ mkName funcName
      fmapCanBuild = AppE (VarE 'fmap) (ConE 'CanBuild)
      withCanBuild = InfixE (Just fmapCanBuild) (VarE '(.)) (Just asVarE)
   in AppE withCanBuild (VarE argName)


bitIndexDecsOf :: String -> Integer -> Name -> DecsQ
bitIndexDecsOf wrapperName pos original = do
  x <- newParserOfType wrapperName original
  y <- bitIndexTyInstDecs wrapperName pos
  z <- builderForNewTyDecs wrapperName
  pure $ x <> y <> z


newParserOfType :: String -> Name -> DecsQ
newParserOfType wrapperName original =
  let name = mkName wrapperName
      origTy = ConT original
      constr = NormalC name [(emptyBang, origTy)]
   in pure
        [ NewtypeD
            []
            name
            []
            Nothing
            constr
            [ eqShowDeriv
            , DerivClause (Just (ViaStrategy origTy)) [ConT ''ParserOf]
            ]
        ]


eqShowDeriv :: DerivClause
eqShowDeriv = DerivClause (Just StockStrategy) (map ConT [''Eq, ''Show])


bitIndexTyInstDecs :: String -> Integer -> DecsQ
bitIndexTyInstDecs wrapperName pos =
  let name = mkName wrapperName
      decs =
        [d|
          type instance BitIndex $(conT name) = $(litT (numTyLit pos))
          |]
   in decs


builderForNewTyDecs :: String -> Q [Dec]
builderForNewTyDecs wrapperName =
  let name = mkName wrapperName
      xName = mkName "x"
      varX = varP xName
      instanceExp = [|toBuilder $(varE xName)|]
      patt = [p|$(conP name [varX])|]
   in mkBuilderInstance wrapperName patt instanceExp


mkBuilderInstance :: String -> PatQ -> ExpQ -> Q [Dec]
mkBuilderInstance wrapperName patt instanceExp = do
  patt' <- patt
  instanceExp' <- instanceExp
  pure [builderInstanceD wrapperName [(patt', instanceExp')]]


builderInstanceD :: String -> [(Pat, Exp)] -> Dec
builderInstanceD instanceName patExps =
  let classT = ConT ''ToBuilder
      instanceT = ConT $ mkName instanceName
      builderT = ConT ''BB.Builder
      fullInstanceT = AppT (AppT classT instanceT) builderT
      theFunc = FunD 'toBuilder $ map clauseFrom patExps
      clauseFrom (pat, expr) = Clause [pat] (NormalB expr) []
   in InstanceD Nothing [] fullInstanceT [theFunc]


mkParserOfInstance :: String -> ExpQ -> Q [Dec]
mkParserOfInstance instanceName instanceExp =
  let name = mkName instanceName
   in [d|
        instance ParserOf $(conT name) where
          parserOf = $instanceExp
        |]


mkInnerDataDecl :: String -> [(String, Name)] -> DecsQ
mkInnerDataDecl name fields = do
  let dataDecl = recordAdtDec' name fields
  builderDecl <- mkInnerDataToBuilderDecs name fields
  parseOfDecl <- mkRecParserOfInstance name fields
  pure $ dataDecl : builderDecl <> parseOfDecl


mkInnerDataToBuilderDecs :: String -> [(String, Name)] -> DecsQ
mkInnerDataToBuilderDecs name fields = do
  nameX <- newName "x"
  let invOf ((n, _e) :| []) = invToBuilderConE nameX n
      invOf (x :| xs) = invBuildBitsE nameX $ x : xs
      accessors = map invOf $ groupBitFields fields
      applyMconcat xs = AppE (VarE 'mconcat) $ ListE xs
  mkBuilderInstance name (varP nameX) (pure $ applyMconcat accessors)


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
   in recordAdtDec (mkName typeName) fields


recordAdtDec :: Name -> [(Name, Type)] -> Dec
recordAdtDec typeName fields =
  let con = RecC typeName $ (\(name, t) -> (name, fieldBang, t)) <$> fields
   in DataD [] typeName [] Nothing [con] [eqShowDeriv]


sumAdtDec' :: String -> [(String, [Name])] -> Dec
sumAdtDec' typeName xs =
  let constrs = map (\(x, y) -> (mkName x, map ConT y)) xs
   in sumAdtDec (mkName typeName) constrs


sumAdtDec :: Name -> [(Name, [Type])] -> Dec
sumAdtDec a b = DataD [] a [] Nothing (fmap (uncurry sumCon) b) []


sumCon :: Name -> [Type] -> Con
sumCon a b = NormalC a $ fmap (fieldBang,) b


fieldBang :: Bang
fieldBang = Bang NoSourceUnpackedness SourceStrict


applicativeConE :: Name -> NonEmpty Exp -> Exp
applicativeConE constrName fieldExps =
  let exp0 :| otherExps = fieldExps
      startExp = mkFmap constrName exp0
      step anExp acc = InfixE (Just acc) (VarE '(<*>)) (Just anExp)
   in foldr step startExp otherExps


mkFmap :: Name -> Exp -> Exp
mkFmap constrName fmapped =
  let constr = ConE constrName
   in InfixE (Just constr) (VarE '(<$>)) (Just fmapped)


emptyBang :: Bang
emptyBang = Bang NoSourceUnpackedness NoSourceStrictness


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
--   | Select {selectNoWait :: !Bit} -- prefix 11
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
