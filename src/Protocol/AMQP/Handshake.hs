{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.Handshake (
  -- * Authentication using SASL
  SASLMechanism (..),
  Handshake (..),
  plain,
  amqplain,

  -- * functions
  runHandshake,
) where

import Control.Exception (Exception, throwIO)
import Data.Attoparsec.Framer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import Data.Word (Word16)
import Paths_amqp_compiled (version)
import Protocol.AMQP.Elementary (
  Bit (..),
  LongInt (..),
  LongString (..),
  ParserOf (..),
  ShortInt (..),
  ShortString,
  ToBuilder (..),
  unsafeMkShortString,
 )
import Protocol.AMQP.FieldValue (FieldTable (..), FieldValue (..))
import Protocol.AMQP.Frame
import Protocol.AMQP.Translated


type ByteSink = BS.ByteString -> IO ()


data Handshake = Handshake
  { hsConnectionName :: !Text
  , hsVirtualHost :: !ShortString
  , hsMechanisms :: ![SASLMechanism]
  , hsChannelMax :: !(Maybe ShortInt)
  , hsHeartbeat :: !(Maybe ShortInt)
  , hsFrameMax :: !(Maybe LongInt)
  , hsSink :: !ByteSink
  , hsOnShaken :: !(CoStartData -> CoTuneOkData -> IO ())
  }


reply :: ToBuilder payload BB.Builder => Handshake -> payload -> IO ()
reply hs p = hsSink hs $ LBS.toStrict $ BB.toLazyByteString $ toBuilder p


reply' :: Handshake -> InnerFrame Method -> IO ()
reply' hs = reply hs . Frame . Frame'


orBadHandshake :: Framer IO a -> Framer IO a
orBadHandshake = setOnBadParse onParseError . setOnClosed (throwIO ServerClosed)


runHandshake :: ByteSource IO -> Handshake -> IO ()
runHandshake src hs = do
  reply hs connectionHeader
  -- read Server: runHandshake
  runFramer $ stepFramer src hs $ onStart src


onStart :: ByteSource IO -> Handshake -> InnerFrame Method -> IO ()
onStart src hs method =
  let spaceSplit = Text.split (== ' ') . Text.decodeUtf8 . coerce
      supportedOf = spaceSplit . csMechanisms
      determineAuth x = find (flip elem (supportedOf x) . smName) $ hsMechanisms hs
   in case asStartData method of
        Left e -> throwIO e
        Right coStart -> case determineAuth coStart of
          Nothing -> throwIO NoSupportedAuth
          Just sm -> do
            hs `reply'` mkCoStartOk (hsConnectionName hs) sm
            -- read Server: tune or Server: secure
            runFramer $ stepFramer src hs $ onChallengeOrTune coStart sm src


asStartData :: InnerFrame Method -> Either BadHandshake CoStartData
asStartData (InnerFrame 0 (ModusConnection (CoStart x))) = Right x
asStartData f = notForHandshake f WantedCoStart


mkCoStartOk :: Text -> SASLMechanism -> InnerFrame Method
mkCoStartOk name SASLMechanism {smName, smInitialResponse} =
  connFrame $
    CoStartOk $
      CoStartOkData
        { csoClientProperties = mkClientProperties name
        , csoLocale = unsafeMkShortString "en_US"
        , csoResponse = coerce smInitialResponse
        , csoMechanism = unsafeMkShortString smName
        }


onChallengeOrTune ::
  CoStartData ->
  SASLMechanism ->
  ByteSource IO ->
  Handshake ->
  InnerFrame Method ->
  IO ()
onChallengeOrTune csd sm src hs method = flip (either throwIO) (asChallengeOrTune method) $ \case
  Left tune -> do
    let tunedOk = mkCoTuneOk hs tune
    hs `reply'` (connFrame $ CoTuneOk tunedOk)
    hs `reply'` mkCoOpen hs
    -- read Server: open_ok
    runFramer $ stepFramer src hs $ onOpenOk csd tunedOk
  Right challenge -> case smChallenge sm of
    Nothing -> throwIO CannotChallenge
    Just mkResponse -> do
      secureOk <- (connFrame . CoSecureOk . coerce) <$> mkResponse challenge
      hs `reply'` secureOk
      -- next step should be tuning; but the challenge may repeat so use @onChallengeOrTune@
      runFramer $ stepFramer src hs $ onChallengeOrTune csd sm src


asChallengeOrTune :: InnerFrame Method -> Either BadHandshake (Either CoTuneData BS.ByteString)
asChallengeOrTune (InnerFrame 0 (ModusConnection (CoSecure x))) = Right $ Right $ coerce x
asChallengeOrTune (InnerFrame 0 (ModusConnection (CoTune x))) = Right $ Left x
asChallengeOrTune f = notForHandshake f WantedCoSecureOrTune


mkCoTuneOk :: Handshake -> CoTuneData -> CoTuneOkData
mkCoTuneOk hs ct =
  let min' a b = maybe a (min a) b
      disallow0 0 = 65535
      disallow0 x = x
      ctoChannelMax = min' (disallow0 $ ctChannelMax ct) (fmap disallow0 $ hsChannelMax hs)
      ctoFrameMax = min' (ctFrameMax ct) (hsFrameMax hs)
      ctoHeartbeat = min' (ctHeartbeat ct) (hsHeartbeat hs)
   in CoTuneOkData {ctoChannelMax, ctoFrameMax, ctoHeartbeat}


mkCoOpen :: Handshake -> InnerFrame Method
mkCoOpen Handshake {hsVirtualHost} =
  connFrame $
    CoOpen $
      CoOpenData
        { coReserved2 = coerce False
        , coReserved1 = unsafeMkShortString ""
        , coVirtualHost = hsVirtualHost
        }


onOpenOk ::
  CoStartData ->
  CoTuneOkData ->
  Handshake ->
  InnerFrame Method ->
  IO ()
onOpenOk startData tunedOk hs method = case asOpenOk method of
  Left err -> throwIO err
  Right _ -> hsOnShaken hs startData tunedOk


asOpenOk :: InnerFrame Method -> Either BadHandshake ShortString
asOpenOk (InnerFrame 0 (ModusConnection (CoOpenOk x))) = Right x
asOpenOk f = notForHandshake f WantedCoOpenOk


type AuthMechanism = Text


{- | A 'SASLMechanism' is described by its name ('saslName'), its initial response ('saslInitialResponse'), and an optional function ('saslChallengeFunc') that
 transforms a security challenge provided by the server into response, which is then sent back to the server for verification.
-}
data SASLMechanism = SASLMechanism
  { -- | mechanism name
    smName :: !AuthMechanism
  , -- | initial response
    smInitialResponse :: !BS.ByteString
  , -- | challenge processing function
    smChallenge :: !(Maybe (BS.ByteString -> IO BS.ByteString))
  }


-- | The @PLAIN@ SASL mechanism. See <http://tools.ietf.org/html/rfc4616 RFC4616>
plain :: Text -> Text -> SASLMechanism
plain name password =
  let nul = '\0'
      smInitialResponse = Text.encodeUtf8 $ Text.cons nul name <> Text.cons nul password
   in SASLMechanism {smName = "PLAIN", smInitialResponse, smChallenge = Nothing}


-- | The @AMQPLAIN@ SASL mechanism.
amqplain :: Text -> Text -> SASLMechanism
amqplain name password =
  let amqpPair x y = toAmqpPair x $ FString $ coerce $ Text.encodeUtf8 y
      concatBuilders = mconcat . map (\(x, y) -> toBuilder x <> toBuilder y)
      fields = [amqpPair "LOGIN" name, amqpPair "PASSWORD" password]
      smInitialResponse = LBS.toStrict $ BB.toLazyByteString $ concatBuilders fields
   in SASLMechanism {smName = "AMQPLAIN", smInitialResponse, smChallenge = Nothing}


mkClientProperties :: Text -> FieldTable
mkClientProperties name =
  FieldTable
    [ toAmqpPair "platform" $ stringValue "Haskell"
    , toAmqpPair "version" $ stringValue currentVersion
    , toAmqpPair "capabilities" $ FTable clientCaps
    , toAmqpPair "connection_name" $ stringValue $ Text.encodeUtf8 name
    ]


clientCaps :: FieldTable
clientCaps =
  FieldTable
    [ toAmqpPair "consumer_cancel_notify" $ FBool $ coerce True
    , toAmqpPair "connection.blocked" $ FBool $ coerce True
    ]


notForHandshake :: InnerFrame a -> BadHandshake -> Either BadHandshake b
notForHandshake (InnerFrame 0 _) x = Left x
notForHandshake (InnerFrame x _) _ = Left $ WantedChan0 x


data BadHandshake
  = WantedChan0 !Word16
  | WantedCoStart
  | WantedCoSecureOrTune
  | WantedCoOpenOk
  | NoSupportedAuth
  | ProtocolError !Text
  | CannotChallenge
  | ServerClosed
  deriving (Eq, Show)


instance Exception BadHandshake


onParseError :: Text -> IO ()
onParseError x = throwIO $ ProtocolError x


stepFramer ::
  ByteSource IO ->
  t ->
  (t -> InnerFrame Method -> IO ()) ->
  Framer IO (Frame Method)
stepFramer src hs step = orBadHandshake $ mkFramer' parserOf (run1Method $ step hs) src


run1Method :: (InnerFrame Method -> IO ()) -> Frame Method -> IO Progression
run1Method f (Frame (Frame' inner)) = do
  f inner
  pure Stop


connFrame :: Connection -> InnerFrame Method
connFrame = InnerFrame 0 . ModusConnection


toAmqpPair :: Text -> b -> (ShortString, b)
toAmqpPair x y = (unsafeMkShortString x, y)


stringValue :: BS.ByteString -> FieldValue
stringValue = FString . coerce


currentVersion :: BS.ByteString
currentVersion = Text.encodeUtf8 $ Text.pack $ showVersion version


connectionHeader :: BB.Builder
connectionHeader = BB.string7 "AMQP" <> (mconcat $ (map BB.word8 [1, 1, 0, 9]))
