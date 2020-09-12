{-|
Module      : Servant.CLI.Form
Description : GADTs for forms of servant CLI parsers
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

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Servant.CLI.Form
  ( Fix (..)
  , runFix
  , getFix

  , Form (..)
  , recurseForm
  , sequence
  , choice
  , terminal

  , Choose (..)
  , recurseChoose
  , left
  , right

  , Both (..)
  , recurseBoth
  , (.<|>)

  , F
  , Recurse
  ) where

import Prelude hiding (sequence, sequence_)
import Data.Kind (Type)

import Servant.API (type (:<|>), type (:>))

type F = (Type -> Type -> Type) -> Type -> Type -> Type

type Recurse (t :: F) = forall f g api r .
  (forall api' x . f api' x -> g api' x) -> t f api r -> t g api r

newtype Fix :: F -> Type -> Type -> Type where
  Fix :: f (Fix f) api r -> Fix f api r

getFix :: forall f api r . Fix f api r -> f (Fix f) api r
getFix (Fix it) = it

runFix :: forall f g api t .
          Recurse f
       -> (forall api' x . f g api' x -> g api' x)
       -> Fix f api t
       ->     g api t
runFix recurse f t = f (recurse (runFix recurse f) (getFix t))

data Form :: F -> F -> F -> F where
  Sequence
    :: forall (sequence :: F) (choice :: F) (terminal :: F) f a b r .
       sequence f (a :> b) r
    -> Form sequence choice terminal f (a :> b) r
  Choice
    :: forall (sequence :: F) (choice :: F) (terminal :: F) f a b r .
       choice f (a :<|> b) r
    -> Form sequence choice terminal f (a :<|> b) r
  Terminal
    :: forall (sequence :: F) (choice :: F) (terminal :: F) f t r .
       terminal f t r
    -> Form sequence choice terminal f t r

recurseForm
  :: Recurse sequence
  -> Recurse choice
  -> Recurse terminal
  -> Recurse (Form sequence choice terminal)
recurseForm s _ _ f (Sequence it) = Sequence (s f it)
recurseForm _ c _ f (Choice   it) = Choice   (c f it)
recurseForm _ _ t f (Terminal it) = Terminal (t f it)

elimForm
  :: forall sequence choice terminal f t r z .
     (forall k (a :: k) b . sequence f (a :> b)   r -> z)
  -> (forall    a       b . choice   f (a :<|> b) r -> z)
  -> (forall    a         . terminal f a          r -> z)
  -> Form sequence choice terminal f t r
  -> z
elimForm elimS elimC elimT form = case form of
  Sequence s -> elimS s
  Choice   c -> elimC c
  Terminal t -> elimT t

sequence
  :: sequence (Fix (Form sequence choice terminal)) (l :> api) r
  ->           Fix (Form sequence choice terminal)  (l :> api) r
sequence = Fix . Sequence

choice
  :: choice (Fix (Form sequence choice terminal)) (left :<|> right) r
  ->         Fix (Form sequence choice terminal)  (left :<|> right) r
choice = Fix . Choice

terminal
  :: terminal (Fix (Form sequence choice terminal)) t r
  ->           Fix (Form sequence choice terminal)  t r
terminal = Fix . Terminal

data Choose :: F where
  L :: f left  r -> Choose f (left :<|> right) r
  R :: f right r -> Choose f (left :<|> right) r

recurseChoose :: Recurse Choose
recurseChoose f (L t) = L (f t)
recurseChoose f (R t) = R (f t)

left :: Fix (Form sequence Choose terminal)  left             r
     -> Fix (Form sequence Choose terminal) (left :<|> right) r
left = Fix . Choice . L

right :: Fix (Form sequence Choose terminal)            right  r
      -> Fix (Form sequence Choose terminal) (left :<|> right) r
right = Fix . Choice . R

data Both :: F where
  Both :: f left r -> f right r -> Both f (left :<|> right) r

recurseBoth :: Recurse Both
recurseBoth f (Both l r) = Both (f l) (f r)

(.<|>) :: Fix (Form sequence Both terminal)  left             r
       -> Fix (Form sequence Both terminal)            right  r
       -> Fix (Form sequence Both terminal) (left :<|> right) r
l .<|> r = Fix $ Choice (Both l r)
