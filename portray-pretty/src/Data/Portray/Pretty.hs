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

-- | Provides rendering of 'Portrayal' to 'Doc'.
--
-- There are two intended uses of this module: firstly, to use @pretty@'s
-- layout and rendering algorithms to render 'Portray' instances, 'Diff's, or
-- other 'Portrayal's; and secondly, to derive 'Pretty' instances based on
-- existing 'Portray' instances.  I find the former more ergonomic, but in
-- established codebases that want to benefit from deriving, the latter may be
-- more achievable.
--
-- The first usage is for codebases with pervasive use of 'Portray', and
-- involves using e.g. 'pp' and 'ppd' in GHCi, or 'showPortrayal' or 'showDiff'
-- in application code.  With this usage, anything you want to pretty-print
-- needs a 'Portray' instance, and the typeclass 'Pretty' is not involved in
-- any way.  With this approach, pretty-printable types and the types they
-- include should derive only 'Portray', and pretty-printing should be done
-- with the aforementioned utility functions:
--
-- @
-- data MyRecord = MyRecord { anInt :: Int, anotherRecord :: MyOtherRecord }
--   deriving Generic
--   deriving Portray via Wrapped Generic MyRecord
--
-- example = 'showPortrayal' (MyRecord 2 ...)
-- @
--
-- The second usage is to use @portray@'s generic deriving to provide derived
-- 'Pretty' instances, in a codebase that uses 'Pretty' as the preferred
-- typeclass for pretty-printable values.  With this usage, things you want to
-- pretty-print need 'Pretty' instances, and 'Portray' is needed for the
-- transitive closure of types included in types you want to derive 'Pretty'
-- instances for.  This may result in many types needing both instances of both
-- 'Pretty' (for direct pretty-printing) and 'Portray' (for deriving 'Portray'
-- on downstream types) instances.  Note that with this approach, types that
-- derive their 'Pretty' instances via 'Portray' will ignore any custom
-- 'Pretty' instances of nested types, since they recurse to nested 'Portray'
-- instances instead.
--
-- To derive an instance for a pretty-printable type, the type itself should
-- look like the following:
--
-- @
-- data MyRecord = MyRecord { anInt :: Int, anotherRecord :: MyOtherRecord }
--   deriving Generic
--   deriving Portray via Wrapped Generic MyRecord
--   deriving Pretty via WrappedPortray MyRecord
--
-- example = 'Text.PrettyPrint.HughesPJClass.prettyShow' (MyRecord 2 ...)
-- @
--
-- And any types transitively included in it should look like the following:
--
-- @
-- data MyOtherRecord = MyOtherRecord
--   deriving Generic
--   deriving Portray via Wrapped Generic MyRecord
-- @
--
-- This module also exports the underlying rendering functionality in a variety
-- of forms for more esoteric uses.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Portray.Pretty
         ( -- * Pretty-Printing
           showPortrayal, pp
           -- * Diffing
         , showDiff, ppd
           -- * DerivingVia wrapper
         , WrappedPortray(..)
           -- * Rendering Functions
           -- ** With Associativity
         , DocAssocPrec, toDocAssocPrecF, toDocAssocPrec
           -- ** With Precedence
         , portrayalToDocPrecF, portrayalToDocPrec
           -- ** Convenience Functions
         , portrayalToDoc
         , prettyShowPortrayal
         , pPrintPortrayal
         ) where

import Data.Functor ((<&>))

import qualified Data.Text as T
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P
import qualified Text.PrettyPrint.HughesPJ as P (maybeParens)
import Text.PrettyPrint.HughesPJClass (Pretty(..), PrettyLevel, prettyNormal)

import Data.Portray
         ( Assoc(..), Infixity(..), FactorPortrayal(..)
         , Portray, Portrayal(..), PortrayalF(..)
         , cata, portray
         )
import Data.Portray.Diff (Diff(..))

-- | Pretty-print a value to stdout using its 'Portray' instance.
pp :: Portray a => a -> IO ()
pp = putStrLn . showPortrayal

-- | Pretty-print a value using its 'Portray' instance.
showPortrayal :: Portray a => a -> String
showPortrayal = prettyShowPortrayal . portray

-- | Pretty-print a diff between two values to stdout using a 'Diff' instance.
ppd :: Diff a => a -> a -> IO ()
ppd x = putStrLn . showDiff x

-- | Pretty-print a diff between two values using a 'Diff' instance.
showDiff :: Diff a => a -> a -> String
showDiff x = maybe "_" prettyShowPortrayal . diff x

-- | A 'Doc' that varies according to associativity and precedence context.
type DocAssocPrec = Assoc -> Rational -> Doc

fixityCompatible :: Infixity -> Assoc -> Rational -> Bool
fixityCompatible (Infixity assoc p) assoc' p' = case compare p' p of
  GT -> False  -- Context has higher precedence than this binop.
  EQ -> assoc == assoc'
  LT -> True

matchCtx :: Assoc -> Assoc -> Assoc
matchCtx ctx assoc
  | ctx == assoc = ctx
  | otherwise = AssocNope

-- | Convert a 'Portrayal' to a 'Doc'.
portrayalToDoc :: Portrayal -> Doc
portrayalToDoc t = portrayalToDocPrec t prettyNormal (-1)

ppBinop
  :: String
  -> Infixity
  -> DocAssocPrec -> DocAssocPrec -> DocAssocPrec
ppBinop nm fx@(Infixity assoc opPrec) x y lr p =
  P.maybeParens (not $ fixityCompatible fx lr p) $ P.sep
    [ x (matchCtx AssocL assoc) opPrec P.<+> P.text nm
    , P.nest 2 $ y (matchCtx AssocR assoc) opPrec
    ]

ppBulletList
  :: Doc -- ^ Open brace,  e.g. {  [  {  (
  -> Doc -- ^ Separator,   e.g. ;  ,  ,  ,
  -> Doc -- ^ Close brace, e.g. }  ]  }  )
  -> [Doc]
  -> Doc
ppBulletList opener _         closer []   = opener <> closer
ppBulletList opener separator closer docs =
  P.sep $
    [ P.fcat $ zipWith (P.<+>) (opener : repeat separator) docs
    , closer
    ]

-- | Render one layer of 'PortrayalF' to 'DocAssocPrec'.
toDocAssocPrecF :: PortrayalF DocAssocPrec -> DocAssocPrec
toDocAssocPrecF = \case
  AtomF txt -> \_ _ -> P.text (T.unpack txt)
  ApplyF fn [] -> \_ _ -> fn AssocL 10
  ApplyF fn xs -> \lr p ->
    P.maybeParens (not $ fixityCompatible (Infixity AssocL 10) lr p) $
      P.sep
        [ fn AssocL 10
        , P.nest 2 $ P.sep $ xs <&> \docprec -> docprec AssocR 10
        ]
  BinopF nm fx x y -> ppBinop (T.unpack nm) fx x y
  TupleF xs -> \_ _ -> ppBulletList "(" "," ")" $ xs <&> \x -> x AssocNope (-1)
  ListF xs -> \_ _ -> ppBulletList "[" "," "]" $ xs <&> \x -> x AssocNope (-1)
  LambdaCaseF xs -> \_ p ->
    P.maybeParens (p >= 10) $
      P.sep
        [ "\\case"
        , P.nest 2 $ ppBulletList "{" ";" "}"
            [ P.sep $
                [ pat AssocNope 0 P.<+> "->"
                , P.nest 2 $ val AssocNope 0
                ]
            | (pat, val) <- xs
            ]
        ]
  RecordF con sels -> \_ _ -> case sels of
    [] -> con AssocNope (-1)
    _  -> P.sep
      [ con AssocNope 10
      , P.nest 2 $ ppBulletList "{" "," "}"
          [ P.sep
              [ P.text (T.unpack sel) P.<+> "="
              , P.nest 2 $ val AssocNope 0
              ]
          | FactorPortrayal sel val <- sels
          ]
      ]
  TyAppF val ty -> \_ _ ->
    P.sep [val AssocNope 10, P.nest 2 $ "@" <> ty AssocNope 10]
  TySigF val ty -> \_ p -> P.maybeParens (p >= 0) $
    P.sep [val AssocNope 0, P.nest 2 $ "::" P.<+> ty AssocNope 0]
  QuotF nm content -> \_ _ ->
    P.sep
      [ P.char '[' <> P.text (T.unpack nm) <> P.char '|'
      , P.nest 2 $ content AssocNope (-1)
      , "|]"
      ]
  UnlinesF ls -> \_ _ -> P.vcat (ls <&> \l -> l AssocNope (-1))
  NestF n x -> \_ _ -> P.nest n (x AssocNope (-1))

toDocPrec :: DocAssocPrec -> PrettyLevel -> Rational -> Doc
toDocPrec dap _l = dap AssocNope . subtract 1

-- | Render a 'PortrayalF' to a 'Doc'.
portrayalToDocPrecF
  :: PortrayalF DocAssocPrec -> PrettyLevel -> Rational -> Doc
portrayalToDocPrecF = toDocPrec . toDocAssocPrecF

-- | Render a 'Portrayal' to a 'Doc' with support for operator associativity.
toDocAssocPrec :: Portrayal -> DocAssocPrec
toDocAssocPrec = cata toDocAssocPrecF . unPortrayal

-- | Render a 'Portrayal' to a 'Doc' with only operator precedence.
portrayalToDocPrec :: Portrayal -> PrettyLevel -> Rational -> Doc
portrayalToDocPrec = toDocPrec . toDocAssocPrec

-- | 'portrayalToDocPrec' with arguments ordered for use in 'pPrintPrec'.
pPrintPortrayal :: PrettyLevel -> Rational -> Portrayal -> Doc
pPrintPortrayal l p x = portrayalToDocPrec x l p

-- | Convenience function for rendering a 'Portrayal' to a 'String'.
prettyShowPortrayal :: Portrayal -> String
prettyShowPortrayal p = show (toDocAssocPrec p AssocNope (-1))

-- | A newtype providing a 'Pretty' instance via 'Portray', for @DerivingVia@.
--
-- Sadly we can't use @Wrapped@ since it would be an orphan instance.  Oh well.
-- We'll just define a unique 'WrappedPortray' newtype in each
-- pretty-printer-integration package.
newtype WrappedPortray a = WrappedPortray { unWrappedPortray :: a }
  deriving newtype (Eq, Ord, Show)

-- | Provide an instance for 'Pretty' by way of 'Portray'.
instance Portray a => Pretty (WrappedPortray a) where
  pPrintPrec l p x = portrayalToDocPrec (portray $ unWrappedPortray x) l p
