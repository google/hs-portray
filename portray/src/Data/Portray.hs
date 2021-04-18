-- Copyright 2020-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Portray
         ( Portrayal
             ( Atom, Apply, Binop, Tuple, List
             , Mconcat, Record, TyApp, TySig
             , Quot, Unlines, Nest
             , ..
             )
         , PortrayalF(..), Portray(..)
         , Assoc(..), Infixity(..), FactorPortrayal(..)
         , ShowAtom(..), showAtom, strAtom, strQuot, strBinop
         , Fix(..), cata
         , portrayCallStack
         ) where

import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text as T
import GHC.Exts (IsList, proxy#)
import qualified GHC.Exts as Exts
import GHC.Generics
         ( (:*:)(..), (:+:)(..)
         , Generic(..), Rep
         , U1(..), K1(..), M1(..)
         , Meta(..), D1, C1, S1
         , Constructor, conName, conFixity
         , Selector, selName
         , Fixity(..), Associativity(..)
         )
import GHC.Stack (CallStack, SrcLoc, getCallStack, prettySrcLoc)
import GHC.TypeLits (KnownSymbol, symbolVal')
import Numeric.Natural (Natural)

import Data.Wrapped (Wrapped(..))

data Assoc = AssocL | AssocR | AssocNope
  deriving (Read, Show, Eq, Ord, Generic)
  deriving Portray via Wrapped Generic Assoc

data Infixity = Infixity !Assoc !Rational
  deriving (Read, Show, Eq, Ord, Generic)
  deriving Portray via Wrapped Generic Infixity

-- | The portrayal of a Haskell runtime value as a pseudo-Haskell syntax tree.
--
-- This can be rendered to various pretty-printing libraries' document types
-- relatively easily; as such, it provides a /lingua franca/ for integrating
-- with pretty-printers, without incurring heavyweight dependencies.
data PortrayalF a
  = AtomF !Text
    -- ^ Render this text directly.
  | ApplyF !a [a]
    -- ^ Render a function application to several arguments.
  | BinopF !Text !Infixity !a !a
    -- ^ Render a binary infix operator application to two arguments.
  | TupleF [a]
    -- ^ Render a tuple of sub-values.
  | ListF [a]
    -- ^ Render a list of sub-values.
  | MconcatF [a]
    -- ^ Render a @<>@-separated list of "monoidy" values.
  | RecordF !a [FactorPortrayal a]
    -- ^ Render a record construction/update syntax.
  | TyAppF !a !a
    -- ^ Render a TypeApplication.
  | TySigF !a !a
    -- ^ Render a term with explicit type signature.
  | QuotF !Text !a
    -- ^ Render a quasiquoter term with the given name.
  | UnlinesF [a]
    -- ^ Render a collection of vertically-aligned lines
    --
    -- This is meant for use inside 'QuotF's, since it needn't produce valid
    -- Haskell syntax.
  | NestF !Int !a
    -- ^ Indent the subdocument by the given number of columns.
    --
    -- This is meant for use inside 'QuotF's, since it needn't produce valid
    -- Haskell syntax.
  deriving (Read, Show, Functor, Foldable, Traversable, Generic)
  deriving Portray via Wrapped Generic (PortrayalF a)

instance IsString (PortrayalF a) where fromString = AtomF . T.pack

newtype Fix f = Fix (f (Fix f))
  deriving Generic

deriving newtype
  instance (forall a. Portray a => Portray (f a)) => Portray (Fix f)

deriving stock
  instance (forall a. Show a => Show (f a)) => Show (Fix f)

-- This is a newtype solely so that we can export pattern synonyms with it.
newtype Portrayal = Portrayal { unPortrayal :: Fix PortrayalF }
  deriving stock Generic
  deriving newtype (Portray, Show)

instance IsString Portrayal where fromString = Atom . T.pack

{-# COMPLETE
      Atom, Apply, Binop, List, Tuple,
      Mconcat, Record, TyApp, TySig, Quot
  #-}

-- N.B.: lol, I did not expect this to work.
pattern Coerced :: Coercible a b => a -> b
pattern Coerced x <- (coerce -> x)
 where
  Coerced x = coerce x

-- A collection of pattern synonyms to hide the fact that we're using Fix
-- internally.

pattern Atom :: Text -> Portrayal
pattern Atom txt = Portrayal (Fix (AtomF txt))

pattern Apply :: Portrayal -> [Portrayal] -> Portrayal
pattern Apply f xs = Portrayal (Fix (ApplyF (Coerced f) (Coerced xs)))

pattern Binop :: Text -> Infixity -> Portrayal -> Portrayal -> Portrayal
pattern Binop nm inf x y =
  Portrayal (Fix (BinopF nm inf (Coerced x) (Coerced y)))

pattern List, Tuple, Mconcat :: [Portrayal] -> Portrayal
pattern Tuple xs = Portrayal (Fix (TupleF (Coerced xs)))
pattern List xs = Portrayal (Fix (ListF (Coerced xs)))
pattern Mconcat xs = Portrayal (Fix (MconcatF (Coerced xs)))

pattern Record :: Portrayal -> [FactorPortrayal Portrayal] -> Portrayal
pattern Record x xs = Portrayal (Fix (RecordF (Coerced x) (Coerced xs)))

pattern TyApp, TySig :: Portrayal -> Portrayal -> Portrayal
pattern TyApp x t = Portrayal (Fix (TyAppF (Coerced x) (Coerced t)))
pattern TySig x t = Portrayal (Fix (TySigF (Coerced x) (Coerced t)))

pattern Quot :: Text -> Portrayal -> Portrayal
pattern Quot t x = Portrayal (Fix (QuotF t (Coerced x)))

pattern Unlines :: [Portrayal] -> Portrayal
pattern Unlines xs = Portrayal (Fix (UnlinesF (Coerced xs)))

pattern Nest :: Int -> Portrayal -> Portrayal
pattern Nest n x = Portrayal (Fix (NestF n (Coerced x)))

-- | A class providing rendering to pseudo-Haskell syntax.
--
-- Instances should guarantee that they produce output that could, in
-- principle, be parsed as Haskell source that evaluates to a value equal to
-- the one being printed, provided the right functions, quasiquoters, plugins,
-- extensions, etc. are available.  Note this doesn't require you to /actually
-- implement/ these functions, quasiquoters, etc; just that it would be
-- feasible to do so.
--
-- Most of the time, this requirement is dispatched simply by portraying the
-- datum as its actual tree of data constructors.  However, since this can
-- sometimes be unwieldy, you might wish to have more stylized portrayals.
--
-- The most basic form of stylized portrayal is to retract the datum through a
-- function, e.g. portraying @4 :| [2] :: NonEmpty a@ as @fromList [4, 2]@.
--
-- For cases where you actually want to escape the Haskell syntax, you can use
-- (or pretend to use) quasiquoter syntax, e.g. portray
-- @EAdd (ELit 2) (EVar a)@ as @[expr| 2 + a |]@.
class Portray a where
  portray :: a -> Portrayal

showAtom :: Show a => a -> Portrayal
showAtom = strAtom . show

strAtom :: String -> Portrayal
strAtom = Atom . T.pack

strQuot :: String -> Portrayal -> Portrayal
strQuot = Quot . T.pack

strBinop :: String -> Infixity -> Portrayal -> Portrayal -> Portrayal
strBinop = Binop . T.pack

data FactorPortrayal a = FactorPortrayal
  { _fpFieldName :: !Text
  , _fpPortrayal :: !a
  }
  deriving (Read, Show, Functor, Foldable, Traversable, Generic)
  deriving Portray via Wrapped Generic (FactorPortrayal a)

class GPortrayProduct f where
  gportrayProduct
    :: f a -> [FactorPortrayal Portrayal] -> [FactorPortrayal Portrayal]

instance GPortrayProduct U1 where
  gportrayProduct U1 = id

instance (Selector s, Portray a) => GPortrayProduct (S1 s (K1 i a)) where
  gportrayProduct (M1 (K1 x)) =
    (FactorPortrayal (T.pack $ selName @s undefined) (portray x) :)

instance (GPortrayProduct f, GPortrayProduct g)
      => GPortrayProduct (f :*: g) where
  gportrayProduct (f :*: g) = gportrayProduct f . gportrayProduct g

class GPortray f where
  gportray :: f a -> Portrayal

instance GPortray f => GPortray (D1 d f) where
  gportray (M1 x) = gportray x

instance (GPortray f, GPortray g) => GPortray (f :+: g) where
  gportray (L1 f) = gportray f
  gportray (R1 g) = gportray g

-- Wrap operator constructor names (which must start with a colon) in parens,
-- for use in function application context.  This arises in four scenarios:
--
-- - The constructor has fewer than two arguments: @(:%) :: Int -> Thing@ gives
-- e.g. "(:%) 42".
-- - The constructor has more than two arguments:
-- @(:%) :: Int -> Int -> Int -> Thing@ gives e.g. "(:%) 2 4 6".
-- - The constructor is declared in prefix form or GADT syntax and has no
-- fixity declaration: @data Thing = (:%) Int Int@ gives e.g. "(:%) 2 4".
-- - The constructor is declared in record notation:
-- @data Thing = (:%) { _x :: Int, _y :: Int }@ gives e.g.
-- "(:%) { _x = 2, _y = 4 }".
formatPrefixCon :: String -> String
formatPrefixCon (':' : rest) = "(:" ++ rest ++ ")"
formatPrefixCon con = con

-- Wrap alphanumeric constructor names in backquotes, for use in infix operator
-- context.  This only arises from datatypes with alphanumeric constructors
-- declared in infix syntax, e.g. "data Thing = Int `Thing` Int".
formatInfixCon :: String -> String
formatInfixCon (':' : rest) = ':' : rest
formatInfixCon con = '`' : con ++ "`"

toAssoc :: Associativity -> Assoc
toAssoc = \case
  LeftAssociative -> AssocL
  RightAssociative -> AssocR
  NotAssociative -> AssocNope

instance (KnownSymbol n, GPortrayProduct f)
      => GPortray (C1 ('MetaCons n fx 'True) f) where
  gportray (M1 x) = Record
    (strAtom (formatPrefixCon $ symbolVal' @n proxy#))
    (gportrayProduct x [])

instance (Constructor ('MetaCons n fx 'False), GPortrayProduct f)
      => GPortray (C1 ('MetaCons n fx 'False) f) where
  gportray (M1 x0) =
    case (nm, conFixity @('MetaCons n fx 'False) undefined, args) of
      ('(' : ',' : _, _, _) -> Tuple args
      (_, Infix lr p, [x, y]) -> Binop
        (T.pack $ formatInfixCon nm)
        (Infixity (toAssoc lr) (toRational p))
        x
        y
      _ -> Apply (strAtom (formatPrefixCon nm)) args
   where
    args = _fpPortrayal <$> gportrayProduct x0 []
    nm = conName @('MetaCons n fx 'False) undefined

instance (Generic a, GPortray (Rep a)) => Portray (Wrapped Generic a) where
  portray (Wrapped x) = gportray (from x)

newtype ShowAtom a = ShowAtom { unShowAtom :: a }

instance Show a => Portray (ShowAtom a) where
  portray = showAtom . unShowAtom

deriving via ShowAtom Int       instance Portray Int
deriving via ShowAtom Int8      instance Portray Int8
deriving via ShowAtom Int16     instance Portray Int16
deriving via ShowAtom Int32     instance Portray Int32
deriving via ShowAtom Int64     instance Portray Int64
deriving via ShowAtom Integer   instance Portray Integer

deriving via ShowAtom Word      instance Portray Word
deriving via ShowAtom Word8     instance Portray Word8
deriving via ShowAtom Word16    instance Portray Word16
deriving via ShowAtom Word32    instance Portray Word32
deriving via ShowAtom Word64    instance Portray Word64
deriving via ShowAtom Natural   instance Portray Natural

deriving via ShowAtom Float     instance Portray Float
deriving via ShowAtom Double    instance Portray Double
deriving via ShowAtom Char      instance Portray Char
deriving via ShowAtom Text      instance Portray Text
deriving via ShowAtom Bool      instance Portray Bool
deriving via ShowAtom ()        instance Portray ()
deriving via ShowAtom (Ratio a) instance Show a => Portray (Ratio a)

deriving via Wrapped Generic (a, b)
  instance (Portray a, Portray b) => Portray (a, b)
deriving via Wrapped Generic (a, b, c)
  instance (Portray a, Portray b, Portray c) => Portray (a, b, c)
deriving via Wrapped Generic (a, b, c, d)
  instance (Portray a, Portray b, Portray c, Portray d) => Portray (a, b, c, d)
deriving via Wrapped Generic (a, b, c, d, e)
  instance (Portray a, Portray b, Portray c, Portray d, Portray e) => Portray (a, b, c, d, e)
deriving via Wrapped Generic (Identity a)
  instance Portray a => Portray (Identity a)
deriving via Wrapped Generic (Const a b)
  instance Portray a => Portray (Const a b)
deriving via Wrapped Generic (Maybe a)
  instance Portray a => Portray (Maybe a)
deriving via Wrapped Generic (Either a b)
  instance (Portray a, Portray b) => Portray (Either a b)

instance Portray a => Portray [a] where
  portray = List . map portray

-- | Portray a list-like type as "fromList [...]".
instance (IsList a, Portray (Exts.Item a))
      => Portray (Wrapped IsList a) where
  portray = Apply (strAtom "fromList") . pure . portray . Exts.toList

deriving via Wrapped IsList (IntMap a)
  instance Portray a => Portray (IntMap a)
deriving via Wrapped IsList (Map k a)
  instance (Ord k, Portray k, Portray a) => Portray (Map k a)
deriving via Wrapped IsList (Set a)
  instance (Ord a, Portray a) => Portray (Set a)
deriving via Wrapped IsList (Seq a)
  instance Portray a => Portray (Seq a)
deriving via Wrapped IsList (NonEmpty a)
  instance Portray a => Portray (NonEmpty a)

-- Note: intentionally no instance for @'Wrapped1' 'Foldable'@, since that
-- doesn't ensure that 'fromList' is actually a valid way to construct @f a@.

portrayCallStack :: [(String, SrcLoc)] -> Portrayal
portrayCallStack xs = Unlines
  [ strAtom "GHC.Stack.CallStack:"
  , Nest 2 $ Unlines
      [ strAtom (func ++ ", called at " ++ prettySrcLoc loc)
      | (func, loc) <- xs
      ]
  ]

instance Portray CallStack where
  portray cs = case getCallStack cs of
    [] -> strAtom "emptyCallStack"
    xs -> strQuot "callStack" $ portrayCallStack xs

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = go
 where
  go (Fix fa) = f $ go <$> fa
