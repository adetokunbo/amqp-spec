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
  -- * Perform the handshake
  Handshake (..),
  ByteSink,
  runHandshake,

  -- * SASL Authentication
  SASLMechanism (..),
  plain,
  amqplain,
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..), throwM)
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


type ByteSink m = BS.ByteString -> m ()


{--| Represents connection configuration choices. -}
data Options = Options
  { opVirtualHost :: !ShortString
  , opChannelMax :: !(Maybe ShortInt)
  , opConnectionName :: !Text
  , opHeartbeat :: !(Maybe ShortInt)
  , opFrameMax :: !(Maybe LongInt)
  }
  deriving (Eq, Show)


data Handshake m = Handshake
  { hsMechanisms :: ![SASLMechanism m]
  , hsSink :: !(ByteSink m)
  , hsSource :: !(ByteSource m)
  , hsOnShaken :: !(CoStartData -> CoTuneOkData -> m ())
  , hsOptions :: !Options
  }


reply :: (MonadThrow m, ToBuilder payload BB.Builder) => Handshake m -> payload -> m ()
reply hs p = hsSink hs $ LBS.toStrict $ BB.toLazyByteString $ toBuilder p


reply' :: MonadThrow m => Handshake m -> InnerFrame Method -> m ()
reply' hs = reply hs . Frame . Frame'


orBadHandshake :: MonadThrow m => Framer m a -> Framer m a
orBadHandshake = setOnBadParse onParseError . setOnClosed (throwM ServerClosed)


runHandshake :: MonadThrow m => Handshake m -> m ()
runHandshake hs = do
  reply hs connectionHeader
  -- read Server: runHandshake
  runFramer $ stepFramer hs onStart


onStart :: MonadThrow m => Handshake m -> InnerFrame Method -> m ()
onStart hs method =
  let spaceSplit = Text.split (== ' ') . Text.decodeUtf8 . coerce
      supportedOf = spaceSplit . csMechanisms
      determineAuth x = find (flip elem (supportedOf x) . smName) $ hsMechanisms hs
   in case asStartData method of
        Left e -> throwM e
        Right coStart -> case determineAuth coStart of
          Nothing -> throwM NoSupportedAuth
          Just sm -> do
            hs `reply'` mkCoStartOk (opConnectionName $ hsOptions hs) sm
            -- read Server: tune or Server: secure
            runFramer $ stepFramer hs $ onChallengeOrTune coStart sm


asStartData :: InnerFrame Method -> Either BadHandshake CoStartData
asStartData (InnerFrame 0 (ModusConnection (CoStart x))) = Right x
asStartData f = notForHandshake f WantedCoStart


mkCoStartOk :: Text -> SASLMechanism m -> InnerFrame Method
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
  MonadThrow m =>
  CoStartData ->
  SASLMechanism m ->
  Handshake m ->
  InnerFrame Method ->
  m ()
onChallengeOrTune csd sm hs method = flip (either throwM) (asChallengeOrTune method) $ \case
  Left tune -> do
    let tunedOk = mkCoTuneOk hs tune
    hs `reply'` (connFrame $ CoTuneOk tunedOk)
    hs `reply'` mkCoOpen hs
    -- read Server: open_ok
    runFramer $ stepFramer hs $ onOpenOk csd tunedOk
  Right challenge -> case smChallenge sm of
    Nothing -> throwM CannotChallenge
    Just mkResponse -> do
      secureOk <- (connFrame . CoSecureOk . coerce) <$> mkResponse challenge
      hs `reply'` secureOk
      -- next step should be tuning; but the challenge may repeat so use @onChallengeOrTune@
      runFramer $ stepFramer hs $ onChallengeOrTune csd sm


asChallengeOrTune :: InnerFrame Method -> Either BadHandshake (Either CoTuneData BS.ByteString)
asChallengeOrTune (InnerFrame 0 (ModusConnection (CoSecure x))) = Right $ Right $ coerce x
asChallengeOrTune (InnerFrame 0 (ModusConnection (CoTune x))) = Right $ Left x
asChallengeOrTune f = notForHandshake f WantedCoSecureOrTune


mkCoTuneOk :: Handshake m -> CoTuneData -> CoTuneOkData
mkCoTuneOk hs ct =
  let min' a b = maybe a (min a) b
      disallow0 0 = 65535
      disallow0 x = x
      ctoChannelMax =
        min'
          (disallow0 $ ctChannelMax ct)
          (fmap disallow0 $ opChannelMax $ hsOptions hs)
      ctoFrameMax = min' (ctFrameMax ct) (opFrameMax $ hsOptions hs)
      ctoHeartbeat = min' (ctHeartbeat ct) (opHeartbeat $ hsOptions hs)
   in CoTuneOkData {ctoChannelMax, ctoFrameMax, ctoHeartbeat}


mkCoOpen :: Handshake m -> InnerFrame Method
mkCoOpen Handshake {hsOptions = Options {opVirtualHost}} =
  connFrame $
    CoOpen $
      CoOpenData
        { coReserved2 = coerce False
        , coReserved1 = unsafeMkShortString ""
        , coVirtualHost = opVirtualHost
        }


onOpenOk ::
  MonadThrow m =>
  CoStartData ->
  CoTuneOkData ->
  Handshake m ->
  InnerFrame Method ->
  m ()
onOpenOk startData tunedOk hs method = case asOpenOk method of
  Left err -> throwM err
  Right _ -> hsOnShaken hs startData tunedOk


asOpenOk :: InnerFrame Method -> Either BadHandshake ShortString
asOpenOk (InnerFrame 0 (ModusConnection (CoOpenOk x))) = Right x
asOpenOk f = notForHandshake f WantedCoOpenOk


type AuthMechanism = Text


{- | A 'SASLMechanism' is described by its name ('saslName'), its initial response ('saslInitialResponse'), and an optional function ('saslChallengeFunc') that
 transforms a security challenge provided by the server into response, which is then sent back to the server for verification.
-}
data SASLMechanism m = SASLMechanism
  { -- | mechanism name
    smName :: !AuthMechanism
  , -- | initial response
    smInitialResponse :: !BS.ByteString
  , -- | challenge processing function
    smChallenge :: !(Maybe (BS.ByteString -> m BS.ByteString))
  }


-- | The @PLAIN@ SASL mechanism. See <http://tools.ietf.org/html/rfc4616 RFC4616>
plain :: Text -> Text -> SASLMechanism m
plain name password =
  let nul = '\0'
      smInitialResponse = Text.encodeUtf8 $ Text.cons nul name <> Text.cons nul password
   in SASLMechanism {smName = "PLAIN", smInitialResponse, smChallenge = Nothing}


-- | The @AMQPLAIN@ SASL mechanism.
amqplain :: Text -> Text -> SASLMechanism m
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


onParseError :: MonadThrow m => Text -> m ()
onParseError x = throwM $ ProtocolError x


stepFramer ::
  MonadThrow m =>
  Handshake m ->
  (Handshake m -> InnerFrame Method -> m ()) ->
  Framer m (Frame Method)
stepFramer hs step = orBadHandshake $ mkFramer' parserOf (run1Method $ step hs) $ hsSource hs


run1Method :: MonadThrow m => (InnerFrame Method -> m ()) -> Frame Method -> m Progression
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
