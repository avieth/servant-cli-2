{-|
Module      : Servant.CLI
Description : 
Copyright   : (c) Alexander Vieth, 2020
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Servant.CLI
  ( CLI
  , parseRequest
  , parseAndRunRequest

  , Endpoint

  -- | Servant route forms
  , Fix
  , Form
  , Choose
  , Both
  , F
  , Servant.CLI.Form.sequence
  , choice
  , terminal
  , left
  , right
  , (.<|>)

  , Undetermined
  , Determined
  , Selected
  , Request (..)
  , request
  , Prompt (..)
  , prompt
  , SubPath (..)
  , subPath
  , GroupOf (..)
  , groupOf
  , groupOfOne

  -- | Modifiers
  , Modifier (..)
  , modifier
  , description
  , summary
  , path
  , capture
  , queryFlag
  , queryParam
  , queryParams
  , header
  , body
  , basicAuth

  , KnownText (..)
  , knownText
  , PathString (..)
  , pathString
  , CaptureName (..)
  , captureName
  , QueryFlagName (..)
  , queryFlagName
  , QueryParamName (..)
  , queryParamName
  , HeaderName (..)
  , headerName
  , Method (..)
  , method

  , module Opt

  , Servant.RequestF
  , Servant.requestMethod
  , Servant.requestPath
  , Servant.requestQueryString
  , Servant.requestAccept
  , Servant.requestHeaders
  , Servant.requestHttpVersion
  ) where

import Data.Monoid (appEndo)
import qualified Network.HTTP.Client as HTTP.Client
import qualified Options.Applicative as Opt
import qualified Servant.Client as Servant
import Servant.Client.Core.BaseUrl as Servant (BaseUrl (..), Scheme (Http), parseBaseUrl)
import qualified Servant.Client.Core as Servant
import qualified Servant.Client.Core.RunClient as Servant (runRequest)

import Servant.CLI.Form
import Servant.CLI.Modifier
import Servant.CLI.Routes
import Servant.CLI.Parser

parseRequest :: CLI api r -> Opt.Parser (Endpoint r)
parseRequest = toParser

baseUrlParser :: Opt.Parser BaseUrl
baseUrlParser = Opt.argument (Opt.maybeReader parseBaseUrl) $
     Opt.help "Base URL"
  <> Opt.metavar "{scheme}://{host}:{port}{/path}"

-- | Parse the request and run it using servant-client.
--
-- Prepends a base URL parser to the request parser.
--
-- Currently it prints the HTTP response or error. Does not attempt to parse
-- it into the `r` type.
parseAndRunRequest :: CLI api r -> Opt.InfoMod (BaseUrl, Endpoint r) -> IO r
parseAndRunRequest cli infoMod = do

  (baseUrl, ep) <- Opt.execParser $ Opt.info
    (((,) <$> baseUrlParser <*> parseRequest cli) Opt.<**> Opt.helper)
    infoMod

  let req = appEndo (makeRequest ep) Servant.defaultRequest

  manager <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
  let env = Servant.mkClientEnv manager baseUrl

  -- runRequest is from the Servant.Client.Core.RunClient.RunClient class
  -- We specialize it to ClientM from servant-client.
  errOrRes <- Servant.runClientM (Servant.runRequest req) env

  pure (takeResponse ep req errOrRes)
