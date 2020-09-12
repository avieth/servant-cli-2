{-|
Module      : Servant.CLI.Termini
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
{-# LANGUAGE StandaloneKindSignatures #-}

module Servant.CLI.Routes
  ( Undetermined
  , Determined
  , Selected

  , Request (..)
  , recurseRequest
  , request

  , Prompt (..)
  , recursePrompt
  , prompt

  , SubPath (..)
  , recurseSubPath
  , subPath

  , GroupOf (..)
  , groupOf
  , groupOfOne
  , forgetGroupOf
  , concatGroup
  ) where

import GHC.TypeLits (KnownSymbol)

import Servant.API (type (:>))
import qualified Servant.API as Servant
import qualified Servant.Client.Core as Servant

import Servant.CLI.Form
import Servant.CLI.Modifier

-- What we want: you are forced to do a subcommand/prompt whenever a difference
-- in the path comes up (a <|>). That's to say, we do not want Both

type Undetermined :: F
type Undetermined = Form Modifier Both Prompt

type Determined :: F
type Determined = Form Modifier Choose Request

type Selected :: F
type Selected = Form Modifier Choose SubPath

data GroupOf t = GroupOf
  { groupName    :: String
  , groupMembers :: [t]
  }

instance Functor GroupOf where
  fmap f groupOf = groupOf { groupMembers = fmap f (groupMembers groupOf) }

groupOfOne :: String -> t -> GroupOf t
groupOfOne str t = GroupOf
  { groupName = str
  , groupMembers = [t]
  }

forgetGroupOf :: GroupOf t -> [t]
forgetGroupOf = groupMembers

groupOf :: String -> [t] -> GroupOf t
groupOf str ts = GroupOf { groupName = str, groupMembers = ts }

concatGroup :: GroupOf [t] -> GroupOf t
concatGroup grp = grp { groupMembers = concat (groupMembers grp) }


data Request :: F where
  Request :: String -- ^ Identifier (name)
          -> String -- ^ Description
          -> Method method
          -- TODO parse 'response' from the HTTP response.
          -> (Servant.Request -> Either Servant.ClientError Servant.Response -> r)
          -> Request f (Servant.Verb method statusCode contentTypes response) r

recurseRequest :: Recurse Request
recurseRequest _ (Request name desc req k) = Request name desc req k

request
  :: Servant.ReflectMethod method
  => String
  -> String -- ^ Description of the request.
  -> (Servant.Request -> Either Servant.ClientError Servant.Response -> r)
  -> Request f (Servant.Verb method statusCode contentTypes response) r
request name desc  = Request name desc Method

data Prompt :: F where
  -- | A prompt is 0 or more determined routes (ending in verbs) and 0 or
  -- more disambiguated routes.
  Prompt :: String -- ^ Determines the METAVAR for this prompt.
         -> [GroupOf (Fix Determined api r)]
         -> [GroupOf (Fix Selected   api r)]
         -> Prompt f api r

recursePrompt :: Recurse Prompt
recursePrompt _ (Prompt str methods prompts) = Prompt str methods prompts

prompt
  :: String
  -> [GroupOf (Fix Determined api r)]
  -> [GroupOf (Fix Selected api r)]
  -> Prompt f api r
prompt = Prompt

-- TODO allow for a customization of the name? Shouldn't need to be the path
-- string necessarily.
data SubPath :: F where
  SubPath :: PathString path
          -> String -- ^ Description
          -> Fix Undetermined api r
          -> SubPath f (path :> api) r

recurseSubPath :: Recurse SubPath
recurseSubPath _ (SubPath path desc next) = SubPath path desc next

subPath
  :: KnownSymbol path
  => String -- ^ Description of this path.
  -> Fix Undetermined api r
  -> SubPath f (path :> api) r
subPath = SubPath PathString
