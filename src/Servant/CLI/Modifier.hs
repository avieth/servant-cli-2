{-|
Module      : Servant.CLI.Modifier
Description : Definition of servant route modifiers for CLI parsers.
Copyright   : (c) Alexander Vieth, 2020
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Servant.CLI.Modifier
  ( Modifier (..)
  , recurseModifier
  , elimModifier

  , modifier
  , description
  , summary
  , path
  , capture
  , captureRaw
  , queryFlag
  , queryParam
  , queryParamRaw
  , queryParams
  , queryParamsRaw
  , header
  , headerRaw
  , body
  , Member (..)
  , bodyRaw
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
  ) where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Servant.API (type (:>))
import qualified Servant.API as Servant
import qualified Servant.Client.Core as Servant
import qualified Network.HTTP.Media.MediaType as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Options.Applicative as Opt

import Servant.CLI.Form

data KnownText text where
  KnownText :: KnownSymbol text => KnownText text

knownText :: forall text . KnownText text -> String
knownText KnownText = symbolVal (Proxy :: Proxy text)

data PathString path where
  PathString :: KnownSymbol path => PathString path

pathString :: forall path . PathString path -> String
pathString PathString = symbolVal (Proxy :: Proxy path)

data CaptureName name where
  CaptureName :: KnownSymbol name => CaptureName name

captureName :: forall name . CaptureName name -> String
captureName CaptureName = symbolVal (Proxy :: Proxy name)

data QueryParamName name where
  QueryParamName :: KnownSymbol name => QueryParamName name

queryParamName :: forall name . QueryParamName name -> String
queryParamName QueryParamName = symbolVal (Proxy :: Proxy name)

data QueryFlagName name where
  QueryFlagName :: KnownSymbol name => QueryFlagName name

queryFlagName :: forall name . QueryFlagName name -> String
queryFlagName QueryFlagName = symbolVal (Proxy :: Proxy name)

data HeaderName name where
  HeaderName :: KnownSymbol name => HeaderName name

headerName :: forall name . HeaderName name -> String
headerName HeaderName = symbolVal (Proxy :: Proxy name)

data Method method where
  Method :: Servant.ReflectMethod method => Method method

method :: forall method . Method method -> HTTP.Method
method Method = Servant.reflectMethod (Proxy :: Proxy method)

-- | Corresponds to the servant route piece types which modify an HTTP
-- request.
data Modifier :: F where

  -- Description and summary route constructors may be used in any way; they
  -- need not be included in the CLI.

  Description
    :: KnownText text
    -> (String -> f api r)
    -> Modifier f (Servant.Description text :> api) r

  Summary
    :: KnownText text
    -> (String -> f api r)
    -> Modifier f (Servant.Summary text :> api) r

  -- The following constructors correspond to API pieces which modify the
  -- request.

  -- appendToPath
  Path
    :: PathString path
    -> f api r
    -> Modifier f (path :> api) r

  -- appendToPath
  Capture
    :: CaptureName name
    -> String -- ^ help
    -> String -- ^ metavar
    -> Opt.ReadM Text
    -> f api r
    -> Modifier f (Servant.Capture' mods name param :> api) r

  -- appendToQueryString
  QueryFlag
    :: QueryFlagName qfname
    -> f api r
    -> Modifier f (Servant.QueryFlag qfname :> api) r

  -- appendToQueryString
  --
  -- TODO support modifiers for optional/required
  QueryParam
    :: QueryParamName name
    -> Opt.ReadM Text
    -> f api r
    -> Modifier f (Servant.QueryParam' mods name param :> api) r

  QueryParams
    :: QueryParamName name
    -> Opt.ReadM Text
    -> f api r
    -> Modifier f (Servant.QueryParams name param :> api) r

  -- addHeader
  Header
    :: HeaderName name
    -> Opt.ReadM ByteString
    -> f api r
    -> Modifier f (Servant.Header' mods name :> api) r

  -- setRequestBody
  ReqBody
    :: Opt.ReadM (Servant.RequestBody, HTTP.MediaType)
    -> f api r
    -> Modifier f (Servant.ReqBody' mods contentTypes request :> api) r

  -- basicAuthReq
  BasicAuth
    :: Opt.ReadM ByteString
    -> Opt.ReadM ByteString
    -> f api r
    -> Modifier f (Servant.BasicAuth realm userData :> api) r

recurseModifier :: Recurse Modifier
recurseModifier f (Description text k)    = Description text (f . k)
recurseModifier f (Summary text k)        = Summary text (f . k)
recurseModifier f (Path name t)           = Path name (f t)
recurseModifier f (Capture name h m rd t) = Capture name h m rd (f t)
recurseModifier f (QueryFlag name t)      = QueryFlag name (f t)
recurseModifier f (QueryParam name rd t)  = QueryParam name rd (f t)
recurseModifier f (QueryParams name rd t) = QueryParams name rd (f t)
recurseModifier f (Header name rd t)      = Header name rd (f t)
recurseModifier f (ReqBody rd t)          = ReqBody rd (f t)
recurseModifier f (BasicAuth u p t)       = BasicAuth u p (f t)

elimModifier :: Modifier f (l :> api) r -> f api r
elimModifier (Description desc k) = k (knownText desc)
elimModifier (Summary     smry k) = k (knownText smry)
elimModifier (Path           _ t) = t
elimModifier (Capture  _ _ _ _ t) = t
elimModifier (QueryFlag      _ t) = t
elimModifier (QueryParam   _ _ t) = t
elimModifier (QueryParams  _ _ t) = t
elimModifier (Header       _ _ t) = t
elimModifier (ReqBody        _ t) = t
elimModifier (BasicAuth    _ _ t) = t

modifier
  :: Modifier (Fix (Form Modifier choice terminal)) (l :> api) r
  ->           Fix (Form Modifier choice terminal)  (l :> api) r
modifier = Fix . Sequence

description
  :: KnownSymbol desc
  => (String -> f api r)
  -> Modifier f (Servant.Description desc :> api) r
description = Description KnownText

summary
  :: KnownSymbol desc
  => (String -> f api r)
  -> Modifier f (Servant.Summary desc :> api) r
summary = Summary KnownText

path
  :: KnownSymbol path
  => f api r
  -> Modifier f (path :> api) r
path = Path PathString

capture
  :: (KnownSymbol name, Servant.ToHttpApiData t)
  => String -- ^ help
  -> String -- ^ metavar
  -> Opt.ReadM t
  -> f api r
  -> Modifier f (Servant.Capture name t :> api) r
capture h m readM = captureRaw h m (fmap Servant.toUrlPiece readM)

-- | Any Text can be used. The `t` in the `Servant.Capture name t` type is
-- ignored. To go by way of a `ReadM t` followed by `ToHttpApiData `t, use
-- `capture`
captureRaw
  :: KnownSymbol name
  => String -- ^ help
  -> String -- ^ metavar
  -> Opt.ReadM Text
  -> f api r
  -> Modifier f (Servant.Capture name t :> api) r
captureRaw = Capture CaptureName

queryFlag
  :: KnownSymbol name
  => f api r
  -> Modifier f (Servant.QueryFlag name :> api) r
queryFlag = QueryFlag QueryFlagName

queryParam
  :: (KnownSymbol name, Servant.ToHttpApiData t)
  => Opt.ReadM t
  -> f api r
  -> Modifier f (Servant.QueryParam' mods name param :> api) r
queryParam readM = queryParamRaw (fmap Servant.toQueryParam readM)

queryParamRaw
  :: KnownSymbol name
  => Opt.ReadM Text
  -> f api r
  -> Modifier f (Servant.QueryParam' mods name param :> api) r
queryParamRaw = QueryParam QueryParamName

queryParams
  :: (KnownSymbol name, Servant.ToHttpApiData t)
  => Opt.ReadM t
  -> f api r
  -> Modifier f (Servant.QueryParams name param :> api) r
queryParams readM = queryParamsRaw (fmap Servant.toQueryParam readM)

queryParamsRaw
  :: KnownSymbol name
  => Opt.ReadM Text
  -> f api r
  -> Modifier f (Servant.QueryParams name param :> api) r
queryParamsRaw = QueryParams QueryParamName

header
  :: (KnownSymbol name, Servant.ToHttpApiData t)
  => Opt.ReadM t
  -> f api r
  -> Modifier f (Servant.Header' mods name :> api) r
header readM = headerRaw (fmap Servant.toHeader readM)

headerRaw
  :: KnownSymbol name
  => Opt.ReadM ByteString
  -> f api r
  -> Modifier f (Servant.Header' mods name :> api) r
headerRaw = Header HeaderName

body
  :: forall f api r body mods contentTypes ctype .
     (Servant.MimeRender ctype body)
  => Member ctype contentTypes
  -> Opt.ReadM body
  -> f api r
  -> Modifier f (Servant.ReqBody' mods contentTypes body :> api) r
body _ readM = bodyRaw (fmap render readM)
  where
  ctypeProxy :: Proxy ctype
  ctypeProxy = Proxy
  render :: body -> (Servant.RequestBody, HTTP.MediaType)
  render b =
    ( Servant.RequestBodyLBS (Servant.mimeRender ctypeProxy b)
    , Servant.contentType ctypeProxy
    )

bodyRaw
  :: Opt.ReadM (Servant.RequestBody, HTTP.MediaType)
  -> f api r
  -> Modifier f (Servant.ReqBody' mods contentTypes request :> api) r
bodyRaw = ReqBody

basicAuth
  :: Opt.ReadM ByteString
  -> Opt.ReadM ByteString
  -> f api r
  -> Modifier f (Servant.BasicAuth realm userData :> api) r
basicAuth = BasicAuth

data Member (x :: k) (xs :: [k]) where
  Here  :: Member x (x ': xs)
  There :: Member x xs -> Member x (y ': xs)
