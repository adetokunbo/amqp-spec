{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Protocol.AMQP.Extracted
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Declares data types that reflect the command definitions declared in the XML
specification and template haskell functions used to transform them into haskell
data types and type classes.
-}
module Protocol.AMQP.Extracted (
  -- * Types reflecting the XML spec metadata
  ClassInfo (..),
  MethodInfo (..),
  XMethodInfo (..),
  FieldInfo (..),

  -- * parsing the spec 
  extractInfo,
  loadClassInfos,
  loadXml,

  -- * constants
  basicName,
  xmlSpecPath,
) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Char (isUpper, toLower, toTitle)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word16)
import Language.Haskell.TH
import Paths_amqp_compiled
import Protocol.AMQP.FieldValue
import Text.Read (readMaybe)
import Text.XML.Light




basicName :: String
basicName = "BasicHdr"

extractInfo :: IO ([ClassInfo], [(String, Name)])
extractInfo = do
  xmlDoc <- (parseXMLDoc <$> loadXml)
  let classInfos = maybe [] toClassInfos xmlDoc
      basicPropInfo = maybe [] toBasicPropInfo xmlDoc
  pure (classInfos, basicPropInfo)


specPath :: FilePath
specPath = "spec/amqp0-9-1.xml"


readXml :: FilePath -> IO Text
readXml = fmap decodeUtf8 . BS.readFile


xmlSpecPath :: IO FilePath
xmlSpecPath = getDataFileName specPath


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
loadXml = xmlSpecPath >>= readXml


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
