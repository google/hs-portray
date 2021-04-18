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

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Data.Portray.Pretty
         ( portrayalToDocPrecF, portrayalToDocPrec, portrayalToDoc
         , pPrintParen
         , DocAssocPrec, toDocAssocPrecF, toDocAssocPrec
         , WrappedPortray(..)
         , prettyShowPortrayal
         , pPrintPortrayal
         ) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P
import Text.PrettyPrint.HughesPJClass
         ( Pretty(..), PrettyLevel, prettyNormal
         )

import Data.Portray
         ( Assoc(..), Infixity(..), FactorPortrayal(..)
         , Portray, Portrayal(..), PortrayalF(..)
         , cata, portray
         )

type DocAssocPrec = Assoc -> Rational -> P.Doc

fixityCompatible :: Infixity -> Assoc -> Rational -> Bool
fixityCompatible (Infixity assoc p) assoc' p' = case compare p' p of
  GT -> False  -- Context has higher precedence than this binop.
  EQ -> assoc == assoc'
  LT -> True

matchCtx :: Assoc -> Assoc -> Assoc
matchCtx ctx assoc
  | ctx == assoc = ctx
  | otherwise = AssocNope

portrayalToDoc :: Portrayal -> P.Doc
portrayalToDoc t = portrayalToDocPrec t prettyNormal (-1)

pPrintParen :: Bool -> Doc -> Doc
pPrintParen b = if b then P.parens else id

ppBinop
  :: String
  -> Infixity
  -> DocAssocPrec -> DocAssocPrec -> DocAssocPrec
ppBinop nm fx@(Infixity assoc opPrec) x y lr p =
  pPrintParen (not $ fixityCompatible fx lr p) $ P.sep
    [ x (matchCtx AssocL assoc) opPrec P.<+> P.text nm
    , P.nest 2 $ y (matchCtx AssocR assoc) opPrec
    ]

toDocAssocPrecF :: PortrayalF DocAssocPrec -> DocAssocPrec
toDocAssocPrecF = \case
  AtomF txt -> \_ _ -> P.text (T.unpack txt)
  ApplyF fn xs -> \lr p ->
    pPrintParen (not $ fixityCompatible (Infixity AssocL 10) lr p) $
      P.sep
        [ fn AssocL 10
        , P.nest 2 $ P.sep $ xs <&> \docprec -> docprec AssocR 10
        ]
  BinopF nm fx x y -> ppBinop (T.unpack nm) fx x y
  TupleF xs -> \_ _ ->
    P.parens . P.fsep . P.punctuate P.comma $
    xs <&> \x -> x AssocNope (-1)
  ListF xs -> \_ _ ->
    P.brackets . P.fsep . P.punctuate P.comma $
    xs <&> \x -> x AssocNope (-1)
  -- TODO remove?
  MconcatF xs ->
    let g
          :: DocAssocPrec
          -> Maybe DocAssocPrec -> Maybe DocAssocPrec
        g l mr = Just $ maybe l (ppBinop "<>" fixity l) mr
        mempty_ _ _ = P.text "mempty"
        fixity = Infixity AssocR 6
    in  fromMaybe mempty_ $ foldr g Nothing xs
  RecordF con sels -> \_ _ -> case sels of
    [] -> con AssocNope (-1)
    _  -> P.sep
      [ con AssocNope 10 P.<+> P.lbrace
      , P.nest 2 $ P.sep $ P.punctuate P.comma
          [ P.sep
              [ P.text (T.unpack sel) P.<+> P.text "="
              , P.nest 2 $ val AssocNope 0
              ]
          | FactorPortrayal sel val <- sels
          ]
      , P.rbrace
      ]
  TyAppF val ty -> \_ _ ->
    P.sep [val AssocNope 10, P.nest 2 $ P.text "@" <> ty AssocNope 10]
  TySigF val ty -> \_ p -> pPrintParen (p >= 0) $
    P.sep [val AssocNope 0, P.nest 2 $ P.text "::" P.<+> ty AssocNope 0]
  QuotF nm content -> \_ _ ->
    P.sep
      [ P.char '[' <> P.text (T.unpack nm) <> P.char '|'
      , P.nest 2 $ content AssocNope (-1)
      , P.text "|]"
      ]
  UnlinesF ls -> \_ _ -> P.vcat (ls <&> \l -> l AssocNope (-1))
  NestF n x -> \_ _ -> P.nest n (x AssocNope (-1))

toDocPrec :: DocAssocPrec -> PrettyLevel -> Rational -> Doc
toDocPrec dap _l = dap AssocNope . subtract 1

portrayalToDocPrecF
  :: PortrayalF DocAssocPrec -> PrettyLevel -> Rational -> Doc
portrayalToDocPrecF = toDocPrec . toDocAssocPrecF

toDocAssocPrec :: Portrayal -> DocAssocPrec
toDocAssocPrec = cata toDocAssocPrecF . unPortrayal

portrayalToDocPrec :: Portrayal -> PrettyLevel -> Rational -> Doc
portrayalToDocPrec = toDocPrec . toDocAssocPrec

pPrintPortrayal :: PrettyLevel -> Rational -> Portrayal -> Doc
pPrintPortrayal l p x = portrayalToDocPrec x l p

prettyShowPortrayal :: Portrayal -> String
prettyShowPortrayal p = show (toDocAssocPrec p AssocNope 0)

-- Sadly we can't use 'Wrapped' since it would be an orphan instance.  Oh well.
-- We'll just define a unique 'WrappedPortray' newtype in each
-- pretty-printer-integration package.
newtype WrappedPortray a = WrappedPortray { unWrappedPortray :: a }
  deriving newtype (Eq, Ord, Show)

-- | Provide an instance for 'Pretty' by way of 'Portray'.
instance Portray a => Pretty (WrappedPortray a) where
  pPrintPrec l p x = portrayalToDocPrec (portray $ unWrappedPortray x) l p
