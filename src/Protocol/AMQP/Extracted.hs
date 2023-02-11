{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.Extracted (
  -- * data types
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  FieldInfo (..),

  -- * functions
  compileXml,
  mkClassDecs,
  mkManyClassDecs,
  mkMethodInnerData,
  xmlSpecPath,
  loadBasicPropInfo,
  loadClassInfos,
  loadXml,
  isDomain,
  nameAndType,
  toClassInfos,
  asSumTy,
  module Text.XML.Light,
) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Char (isUpper, toLower, toTitle)
import Data.Foldable (msum)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word16)
import Language.Haskell.TH
import Paths_amqp_base
import Protocol.AMQP.FieldValue
import Protocol.AMQP.TH
import Text.Read (readMaybe)
import Text.XML.Light


compileXml' :: Q [Dec]
compileXml' = do
  xs <- runIO loadClassInfos
  mkManyClassDecs xs


compileXml :: Q [Dec]
compileXml = do
  (classInfos, basicPropInfo) <- runIO loadCompilerInfo
  classes <- mkManyClassDecs classInfos
  basicProps <- mkBasicProperties basicPropInfo
  pure $ classes <> basicProps


loadCompilerInfo :: IO ([ClassInfo], [(String, Name)])
loadCompilerInfo = do
  xmlDoc <- (parseXMLDoc <$> loadXml)
  let classInfos = maybe [] toClassInfos xmlDoc
      basicPropInfo = maybe [] toBasicPropInfo xmlDoc
  pure (classInfos, basicPropInfo)


specPath :: FilePath
specPath = "spec/amqp0-9-1.xml"


readXml' :: FilePath -> IO Text
readXml' = fmap decodeUtf8 . BS.readFile


xmlSpecPath :: IO FilePath
xmlSpecPath = getDataFileName specPath


loadBasicPropInfo :: IO [(String, Name)]
loadBasicPropInfo = do
  (parseXMLDoc <$> loadXml) >>= pure . maybe [] toBasicPropInfo


toBasicPropInfo :: Element -> [(String, Name)]
toBasicPropInfo el =
  let go = selectAll "field" nameAndDomain
      isBasicClass e = (qName $ elName e) == "class" && attrNamed "name" e == Just "basic"
   in maybe [] go $ filterElement isBasicClass el


nameAndDomain :: Element -> Maybe (String, Name)
nameAndDomain e =
  let name = findAttrBy ((== "name") . qName) e
      elementaryType = findAttrBy ((== "domain") . qName) e
      typeName = elementaryType >>= flip lookup elementaryTypes
   in (,) <$> name <*> typeName


loadClassInfos :: IO [ClassInfo]
loadClassInfos = (parseXMLDoc <$> loadXml) >>= pure . maybe [] toClassInfos


loadXml :: IO Text
loadXml = xmlSpecPath >>= readXml'


isDomain :: QName -> Bool
isDomain = (== "domain") . qName


nameAndType :: Element -> Maybe (String, Name)
nameAndType e =
  let name = findAttrBy ((== "name") . qName) e
      elementaryType = findAttrBy ((== "type") . qName) e
      typeName = elementaryType >>= flip lookup elementaryTypes
   in (,) <$> name <*> typeName


toClassInfos :: Element -> [ClassInfo]
toClassInfos e =
  let infos = selectAll "class" (flip fromElement namedDomains) e
      namedDomains = selectAll "domain" nameAndType e
   in infos


data ClassInfo = ClassInfo
  { ciName :: !String
  , ciPrefix :: !Word16
  , ciLabel :: !String
  , ciMethods :: ![XMethodInfo]
  }
  deriving (Eq, Show)


instance FromElement ClassInfo where
  fromElement e xs =
    let ciName = nameAttr e
        ciPrefix = prefixAttr' e
        ciLabel = labelAttr e
        ciMethods = case ciName of
          Nothing -> pure []
          Just x -> pure $ map (xMethodInfo x) theMethods
        theMethods = selectAll "method" (flip fromElement xs) e
     in ClassInfo <$> ciName <*> ciPrefix <*> ciLabel <*> ciMethods


asToBuilderDec :: ClassInfo -> Dec
asToBuilderDec ci =
  let patExps = map (mkToBuilderPatExp (ciPrefix ci)) (ciMethods ci)
   in builderInstanceD (pascalCase $ ciName ci) patExps


asParserOfExp :: ClassInfo -> Exp
asParserOfExp ci =
  let pairsExp = mkMatchTwoPairList $ map asMatchTwoPair $ ciMethods ci
      firstApp = AppE (VarE 'matchTwoPrefixes) (LitE $ IntegerL $ toInteger $ ciPrefix ci)
   in AppE firstApp pairsExp


mkMatchTwoPairList :: [(Word16, Exp)] -> Exp
mkMatchTwoPairList pairs =
  let toPairExp (x, y) = TupE [Just (LitE $ IntegerL $ toInteger x), Just y]
   in ListE $ map toPairExp pairs


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


data MethodInfo = MethodInfo
  { miName :: !String
  , miPrefix :: !Word16
  , miLabel :: !String
  , miFields :: ![FieldInfo]
  }
  deriving (Eq, Show)


instance FromElement MethodInfo where
  fromElement e xs =
    let miName = nameAttr e
        miPrefix = prefixAttr' e
        miLabel = labelAttr e
        miFields = pure $ selectAll "field" (flip fromElement xs) e
     in MethodInfo <$> miName <*> miPrefix <*> miLabel <*> miFields


xMethodInfo :: String -> MethodInfo -> XMethodInfo
xMethodInfo className xmiInfo =
  let xmiConstrName = methodPreOf className ++ (uniqifyMethod $ miName xmiInfo)
      xmiDataName = xmiConstrName ++ "Data"
      xmiDataPrefix = capsOf xmiConstrName
      xmiDataFields = dataFieldsOf xmiDataPrefix (miFields xmiInfo)
   in XMethodInfo
        { xmiInfo
        , xmiDataName
        , xmiDataPrefix
        , xmiDataFields
        , xmiConstrName
        }


data XMethodInfo = XMethodInfo
  { xmiInfo :: !MethodInfo
  , xmiConstrName :: !String
  , xmiDataName :: !String
  , xmiDataPrefix :: !String
  , xmiDataFields :: ![(String, Name)]
  }
  deriving (Eq, Show)


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


data FieldInfo = FieldInfo
  { fiName :: !String
  , fiDomain :: !String
  , fiLabel :: !String
  , fiTypeName :: !Name
  }
  deriving (Eq, Show)


instance FromElement FieldInfo where
  fromElement e xs =
    let fiName = nameAttr e
        fiDomain = fieldTypeAttr e
        fiLabel = labelAttr e
        fiTypeName = fiDomain >>= flip lookup xs
     in FieldInfo <$> fiName <*> fiDomain <*> fiLabel <*> fiTypeName


class FromElement a where
  fromElement :: Element -> [(String, Name)] -> Maybe a


dataFieldsOf :: String -> [FieldInfo] -> [(String, Name)]
dataFieldsOf dataPrefix xs =
  let typeOf = fiTypeName
      nameOf x = dataPrefix ++ (pascalCase $ fiName x)
   in map (\x -> (nameOf x, typeOf x)) xs


attrNamed :: String -> Element -> Maybe String
attrNamed name = findAttrBy ((== name) . qName)


labelAttr, nameAttr, prefixAttr, fieldTypeAttr :: Element -> Maybe String
labelAttr e = attrNamed "label" e <|> pure ""
nameAttr = attrNamed "name"
prefixAttr = attrNamed "index"
fieldTypeAttr e = attrNamed "domain" e <|> attrNamed "type" e


prefixAttr' :: Element -> Maybe Word16
prefixAttr' el = prefixAttr el >>= readMaybe


selectAll :: String -> (Element -> Maybe a) -> Element -> [a]
selectAll elemName mk el = catMaybes $ map mk $ kids elemName el


kids :: String -> Element -> [Element]
kids name el = filterChildrenName ((== name) . qName) el


methodPreOf :: String -> String
methodPreOf "confirm" = "Fi"
methodPreOf "channel" = "Av"
methodPreOf x = titleCase $ take 2 x


uniqifyMethod :: String -> String
uniqifyMethod =
  let go = renameReject . renameReturn . renameDelete . renameConsume . pascalCase
      renameReject = replace "Reject" "Skip"
      renameReturn = replace "Return" "Yield"
      renameDelete = replace "Delete" "Remove"
      renameConsume = replace "Consume" "Employ"
      replace x y = Text.unpack . Text.replace x y . Text.pack
   in go


elementaryTypes :: [(String, Name)]
elementaryTypes =
  [ ("bit", ''Bit)
  , ("octet", ''Octet)
  , ("short", ''ShortInt)
  , ("long", ''LongInt)
  , ("longlong", ''LongLongInt)
  , ("shortstr", ''ShortString)
  , ("longstr", ''LongString)
  , ("timestamp", ''LongLongInt)
  , ("table", ''FieldTable)
  ]


pascalCase :: String -> String
pascalCase = concatMap titleCase . splitOn "-"


titleCase :: String -> String
titleCase (x : xs) = toTitle x : xs
titleCase xs = xs


capsOf :: String -> String
capsOf = map toLower . filter isUpper
