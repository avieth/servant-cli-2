{-|
Module      : Servant.CLI.Parser
Description : Evaluation of a CLI form to an optparse-applicative parser
Copyright   : (c) Alexander Vieth, 2020
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Servant.CLI.Parser
  ( CLI
  , toParser

  , Endpoint (..)
  , endpoint
  , alterEndpointRequest

  , Some (..)
  , getSome
  ) where

import Control.Applicative ((<|>), empty, optional, many)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI (mk)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Monoid (Endo (..))
import qualified Data.Sequence as Seq ((|>))
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Network.HTTP.Media.MediaType as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc)
import qualified Options.Applicative.Help.Pretty as Doc
import Servant.API (type (:>), type (:<|>))
import Servant.API.BasicAuth (BasicAuthData (..))
import qualified Servant.Client.Core as Servant
import Servant.Client.Core.BasicAuth (basicAuthReq)

import Servant.CLI.Form
import Servant.CLI.Modifier
import Servant.CLI.Routes

type CLI api r = Fix Undetermined api r

-- | 'Endpoint r' but with a phantom api type, so that we can use it in
-- reucrsion on Form.
data EndpointParser api r = EndpointParser
  { getEndpointParser :: Opt.Parser (Endpoint r) }

applyEndpointParser
  :: Opt.Parser (Endo (Endpoint r))
  -> EndpointParser api r
  -> EndpointParser api r
applyEndpointParser optParser = EndpointParser
  . (\x -> appEndo <$> optParser <*> x)
  . getEndpointParser

applyEndpointParserRequest
  :: Opt.Parser (Endo Servant.Request)
  -> EndpointParser api r
  -> EndpointParser api r
applyEndpointParserRequest = applyEndpointParser . fmap alterEndpointRequest

sequenceEndpointParser :: EndpointParser api r -> EndpointParser (a :> api) r
sequenceEndpointParser = coerce

chooseLeftEndpointParser :: EndpointParser left r -> EndpointParser (left :<|> right) r
chooseLeftEndpointParser = coerce

chooseRightEndpointParser :: EndpointParser right r -> EndpointParser (left :<|> right) r
chooseRightEndpointParser = coerce

mergeEndpointParsers
  :: EndpointParser  left             r
  -> EndpointParser            right  r
  -> EndpointParser (left :<|> right) r
mergeEndpointParsers left right = EndpointParser
  { getEndpointParser = getEndpointParser left <|> getEndpointParser right }

data DeterminedEndpointParser api r = DeterminedEndpointParser
  { determinedName           :: String
  , determinedDescription    :: String
  , determinedMethod         :: HTTP.Method
  , determinedEndpointParser :: EndpointParser api r
  }

applyDeterminedEndpointParser
  :: (EndpointParser api r -> EndpointParser (l :> api) r)
  -> DeterminedEndpointParser       api  r
  -> DeterminedEndpointParser (l :> api) r
applyDeterminedEndpointParser f dep = dep { determinedEndpointParser = f (determinedEndpointParser dep) }

determinedLeft :: DeterminedEndpointParser left r -> DeterminedEndpointParser (left :<|> right) r
determinedLeft sl = sl { determinedEndpointParser = chooseLeftEndpointParser (determinedEndpointParser sl) }

determinedRight :: DeterminedEndpointParser right r -> DeterminedEndpointParser (left :<|> right) r
determinedRight sl = sl { determinedEndpointParser = chooseRightEndpointParser (determinedEndpointParser sl) }

data SelectedEndpointParser api r = SelectedEndpointParser
  { selectedDisambiguator  :: String
  , selectedDescription    :: String
  , selectedEndpointParser :: EndpointParser api r
  }

applySelectedEndpointParser
  :: (EndpointParser api r -> EndpointParser (l :> api) r)
  -> SelectedEndpointParser       api  r
  -> SelectedEndpointParser (l :> api) r
applySelectedEndpointParser f sep = sep { selectedEndpointParser = f (selectedEndpointParser sep) }

selectedLeft :: SelectedEndpointParser left r -> SelectedEndpointParser (left :<|> right) r
selectedLeft sl = sl { selectedEndpointParser = chooseLeftEndpointParser (selectedEndpointParser sl) }

selectedRight :: SelectedEndpointParser right r -> SelectedEndpointParser (left :<|> right) r
selectedRight sl = sl { selectedEndpointParser = chooseRightEndpointParser (selectedEndpointParser sl) }

-- | This is what will be produced by an optparse-applicative parser.
data Endpoint r = Endpoint
  { makeRequest  :: Endo Servant.Request
  , takeResponse :: Servant.Request -> Either Servant.ClientError Servant.Response -> r
  }

endpoint :: (Servant.Request -> Either Servant.ClientError Servant.Response -> r) -> Endpoint r
endpoint k = Endpoint
  { makeRequest  = mempty
  , takeResponse = k
  }

endpointMethod
  :: HTTP.Method
  -> (Servant.Request -> Either Servant.ClientError Servant.Response -> r)
  -> Endpoint r
endpointMethod m k = applyEndpointRequestEndo setMethod (endpoint k)
  where
  setMethod = Endo $ \req -> req { Servant.requestMethod = m }

-- | Crucial to apply setRequestMethod on the right, because the Endo
-- semigroup is defined such that the left function is run after the
-- right function, but we want this function to go first (due to how we use it
-- when building up a parser; deeper levels are evaluated first).
alterEndpointRequest :: Endo Servant.Request -> Endo (Endpoint r)
alterEndpointRequest endo = Endo $ \ep -> ep { makeRequest = makeRequest ep <> endo }

applyEndpointRequestEndo :: Endo Servant.Request -> Endpoint r -> Endpoint r
applyEndpointRequestEndo = appEndo . alterEndpointRequest

evalModifier
  :: (forall k (l :: k) api r . (EndpointParser api r -> EndpointParser (l :> api) r) -> ep api r -> ep (l :> api) r)
  -> Modifier ep (l :> api) r
  ->          ep (l :> api) r

evalModifier f (Description desc ep) = f sequenceEndpointParser $ ep (knownText desc)
evalModifier f (Summary     smry ep) = f sequenceEndpointParser $ ep (knownText smry)

-- Static path pieces are not arguments, they are subparser commands. This is
-- essential, otherwise they would not work well in alternatives.
evalModifier f (Path name ep) = f makeSubcommand ep

  where

  makeSubcommand ep = EndpointParser $ Opt.hsubparser $ Opt.command str $
    Opt.info (getEndpointParser (applyEndpointParserRequest (pure modifyRequest) ep)) Opt.fullDesc

  modifyRequest = Endo (Servant.appendToPath (Text.pack str))

  str = pathString name

-- Use the capture name parser to parse a Text, and then use _that_ to
-- modify the requests of all of the recursively-defined promptStructure by
-- adding it to the path of the request of every endpoint therein.
evalModifier f (Capture name help meta read ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$> captureParser)) ep

  where

  modifyRequest :: Text -> Endo Servant.Request
  modifyRequest = Endo . Servant.appendToPath

  captureParser = Opt.argument read $
       Opt.help help
    <> Opt.metavar meta
    <> Opt.style Doc.braces

  strName = captureName name

evalModifier f (QueryFlag name ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$ flagParser)) ep

  where

  modifyRequest :: Endo Servant.Request
  modifyRequest = Endo (Servant.appendToQueryString txt Nothing)

  flagParser = Opt.switch $ Opt.help (queryFlagName name)

  txt = Text.pack (queryFlagName name)

evalModifier f (QueryParam name read ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$> optionParser)) ep

  where

  modifyRequest :: Maybe Text -> Endo Servant.Request
  modifyRequest = Endo . Servant.appendToQueryString txt

  optionParser = optional $ Opt.option read $
    Opt.help (queryParamName name) <> Opt.long (queryParamName name)

  txt = Text.pack (queryParamName name)

evalModifier f (QueryParams name read ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$> optionParser)) ep

  where

  modifyRequest :: [Text] -> Endo Servant.Request
  modifyRequest txts = Endo $ \req ->
    foldr (Servant.appendToQueryString txt . Just) req txts

  optionParser = many $ Opt.option read $
    Opt.help (queryParamName name) <> Opt.long (queryParamName name)

  txt = Text.pack (queryParamName name)

evalModifier f (Header name read ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$> optionParser)) ep

  where

  modifyRequest :: ByteString -> Endo Servant.Request
  modifyRequest = Endo . addHeader

  optionParser = Opt.option read $
    Opt.help (headerName name) <> Opt.long (headerName name)

  -- Cannot use Servant.addHeader because that one uses ToHttpApiData
  addHeader :: ByteString -> Servant.Request -> Servant.Request
  addHeader val req = req
    { Servant.requestHeaders = Servant.requestHeaders req Seq.|> (hdr, val) }

  hdr = CI.mk (B8.pack (headerName name))

evalModifier f (ReqBody read ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (uncurry modifyRequest <$> bodyParser)) ep

  where


  modifyRequest :: Servant.RequestBody -> HTTP.MediaType -> Endo Servant.Request
  modifyRequest body mtype = Endo (Servant.setRequestBody body mtype)

  bodyParser = Opt.argument read $
    Opt.help "body" <> Opt.metavar "REQUEST_BODY"

-- Basic auth is always optional; will default to an empty username and password.
evalModifier f (BasicAuth readUser readPass ep) =
  f (sequenceEndpointParser . applyEndpointParserRequest (modifyRequest <$> userParser <*> passParser)) ep

  where

  modifyRequest :: ByteString -> ByteString -> Endo Servant.Request
  modifyRequest user pass = Endo (basicAuthReq (BasicAuthData user pass))

  userParser = Opt.option readUser $
    Opt.help "Username for basic auth" <> Opt.long "basic-auth-username" <> Opt.value mempty

  passParser = Opt.option readPass $
    Opt.help "Password for basic auth" <> Opt.long "basic-auth-password" <> Opt.value mempty



evalSelected
  :: forall api r .
     Selected SelectedEndpointParser api r
  ->          SelectedEndpointParser api r

evalSelected (Sequence mod) = evalModifier applySelectedEndpointParser mod

evalSelected (Choice (L l)) = selectedLeft  l
evalSelected (Choice (R r)) = selectedRight r

-- TODO require the pathstring as an argument?
evalSelected (Terminal (SubPath path desc undet)) = SelectedEndpointParser
  { selectedDisambiguator  = pathString path
  , selectedDescription    = desc
  , selectedEndpointParser = sequenceEndpointParser (undeterminedToEndpointParser undet)
  }


evalDetermined
  :: forall api r .
     Determined DeterminedEndpointParser api r
  ->            DeterminedEndpointParser api r

evalDetermined (Sequence mod) = evalModifier applyDeterminedEndpointParser mod

evalDetermined (Choice (L l)) = determinedLeft  l
evalDetermined (Choice (R r)) = determinedRight r

evalDetermined (Terminal (Request name desc m k)) = DeterminedEndpointParser
  { determinedName        = name
  , determinedDescription = desc
  , determinedMethod      = method m
  , determinedEndpointParser = EndpointParser (pure (endpointMethod (method m) k))
  }


methodStyle :: HTTP.Method -> String -> Doc -> Doc
methodStyle m name _ = methodColour m (Doc.text name Doc.<+> Doc.text (B8.unpack m))

methodColour :: HTTP.Method -> Doc -> Doc
methodColour m
  | m == HTTP.methodGet    = Doc.blue
  | m == HTTP.methodPost   = Doc.green
  | m == HTTP.methodPut    = Doc.yellow
  | m == HTTP.methodDelete = Doc.red
  | otherwise              = id


evalUndetermined
  :: forall api r .
     Undetermined EndpointParser api r
  ->              EndpointParser api r

evalUndetermined (Sequence mod) = evalModifier ($) mod

evalUndetermined (Choice (Both left right)) = mergeEndpointParsers left right

evalUndetermined (Terminal (Prompt str methods subs)) = EndpointParser $

  -- TBD maybe this is not the best way to compose subcommands?
  -- optparse-applicative doesn't make this too clear to me.
  --Opt.subparser methodCommands <|> Opt.subparser subCommands
  (foldl (<|>) empty methodCommands) <|> (foldl (<|>) empty subCommands)

  where

  methodGroups :: [GroupOf (HTTP.Method, String, Opt.Mod Opt.CommandFields (Endpoint r))]
  methodGroups = flip fmap methods $ \group -> flip fmap group $ \member ->
    let parser = determinedToEndpointParser member
        desc = determinedDescription parser
        m = determinedMethod parser
        parserInfo = Opt.info (getEndpointParser (determinedEndpointParser parser) Opt.<**> Opt.helper) $ mconcat
          [ Opt.progDesc desc
          , Opt.fullDesc
          ]
    in  (m, determinedName parser, Opt.command (determinedName parser) parserInfo)

  methodCommands :: [Opt.Parser (Endpoint r)]
  methodCommands = do
    group <- methodGroups
    (m, name, cmd) <- groupMembers group
    pure $ Opt.hsubparser $ mconcat
      [ Opt.commandGroup (groupName group)
      , cmd
      -- Use the Opt.style modifier to actually change the help text completely
      -- so that it shows a coloured method indicator and the command name.
      , Opt.style (methodStyle m name)
      ]

  subGroups :: [GroupOf (String, Opt.Mod Opt.CommandFields (Endpoint r))]
  subGroups = flip fmap subs $ \group -> flip fmap group $ \member ->
    let parser = selectedToEndpointParser member
        desc = selectedDescription parser
        disamb = selectedDisambiguator parser
        parserInfo = Opt.info (getEndpointParser (selectedEndpointParser parser) Opt.<**> Opt.helper) $ mconcat
          [ Opt.progDesc desc
          , Opt.fullDesc
          ]
    in  (disamb, Opt.command disamb parserInfo)

  subCommands :: [Opt.Parser (Endpoint r)]
  subCommands = do
    group <- subGroups
    (name, cmd) <- groupMembers group
    pure $ Opt.hsubparser $ mconcat
      [ Opt.commandGroup (groupName group)
      , cmd
      , Opt.style (const (Doc.text name))
      ]

undeterminedToEndpointParser :: Fix Undetermined api r -> EndpointParser api r
undeterminedToEndpointParser =
  runFix (recurseForm recurseModifier recurseBoth recursePrompt) evalUndetermined

determinedToEndpointParser :: Fix Determined api r -> DeterminedEndpointParser api r
determinedToEndpointParser =
  runFix (recurseForm recurseModifier recurseChoose recurseRequest) evalDetermined

selectedToEndpointParser :: Fix Selected api r -> SelectedEndpointParser api r
selectedToEndpointParser =
  runFix (recurseForm recurseModifier recurseChoose recurseSubPath) evalSelected

toParser :: CLI api r -> Opt.Parser (Endpoint r)
toParser = getEndpointParser . undeterminedToEndpointParser


data Some (f :: Type -> Type) (api :: k) (r :: Type) where
  Some :: f r -> Some f api r

getSome :: Some f api r -> f r
getSome (Some it) = it
