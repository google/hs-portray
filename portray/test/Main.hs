-- Copyright 2021 Google LLC
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Type.Reflection (typeRep)

import Data.Wrapped (Wrapped(..))
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Data.Portray

data NormalCon = NormalCon Int Bool
  deriving Generic
  deriving Portray via Wrapped Generic NormalCon

infixl 5 :?
data InfixOperatorCon = Int :? Bool
  deriving Generic
  deriving Portray via Wrapped Generic InfixOperatorCon

data PrefixOperatorCon = (:??) Int Bool
  deriving Generic
  deriving Portray via Wrapped Generic PrefixOperatorCon

data RecordCon = RecordCon { _rcInt :: Int, _rcBool :: Bool }
  deriving Generic
  deriving Portray via Wrapped Generic RecordCon

-- More question marks == more "why would you do this?"
data OperatorRecordCon = (:???) { _orcInt :: Int, _orcBool :: Bool }
  deriving Generic
  deriving Portray via Wrapped Generic OperatorRecordCon

-- Yes, this is possible.
data InfixCon = Int `InfixCon` Bool
  deriving Generic
  deriving Portray via Wrapped Generic InfixCon

sel :: String -> Ident
sel = Ident VarIdent . T.pack

main :: IO ()
main = defaultMain
  [ testGroup "atoms"
      [ testCase "portray @Int" $ portray @Int 0 @?= LitInt 0
      , testCase "portray @Bool" $ portray True @?= Name (Ident ConIdent "True")
      , testCase "portray @Float" $ portray @Float 0.0 @?= LitRat 0
      , testCase "portray @Char" $ portray 'a' @?= LitChar 'a'
      , testCase "portray @Text" $ portray @Text "aoeu" @?= LitStr "aoeu"
      , testCase "portray @()" $ portray () @?= Tuple []
      ]

  , testGroup "Maybe"
      [ testCase "portray Nothing" $
          portray (Nothing @Int) @?= Name (Ident ConIdent "Nothing")
      , testCase "portray Just" $
          portray (Just ()) @?= Apply (Name (Ident ConIdent "Just")) [Tuple []]
      ]

  , testCase "portray String" $ portray ("aoeu" :: String) @?= LitStr "aoeu"

  , testCase "portray Void" $ const () (\x -> portray @Void x) @?= ()

  , testGroup "tuples"
      [ testCase "portray (,)" $
          portray ('a', 'b') @?= Tuple [LitChar 'a', LitChar 'b']
      , testCase "portray (,,)" $
          portray ('a', 'b', 'c') @?=
            Tuple [LitChar 'a', LitChar 'b', LitChar 'c']
      ]

  , testGroup "reflection"
      [ testCase "portray Int" $
          portray (typeRep @Int) @?=
            TyApp
              (Name (Ident VarIdent "typeRep"))
              (Name (Ident ConIdent "Int"))
      , testCase "portray ->" $
          portray (typeRep @(Int -> Int)) @?=
            TyApp
              (Name (Ident VarIdent "typeRep"))
              (Binop
                (Ident OpIdent "->")
                (infixr_ (-1))
                (Name (Ident ConIdent "Int"))
                (Name (Ident ConIdent "Int")))
      , testCase "portray Either" $
          portray (typeRep @(Either Bool Int)) @?=
            TyApp
              (Name (Ident VarIdent "typeRep"))
              (Apply
                (Apply
                  (Name (Ident ConIdent "Either"))
                  [Name (Ident ConIdent "Bool")])
                [Name (Ident ConIdent "Int")])
      , testCase "portray DataKinds" $
          portray (typeRep @'True) @?=
            TyApp
              (Name (Ident VarIdent "typeRep"))
              (Opaque "'True")
      , testCase "portray TypeNats" $
          portray (typeRep @4) @?=
            TyApp (Name (Ident VarIdent "typeRep")) (Opaque "4")
      , testCase "portray Symbol" $
          portray (typeRep @"hi") @?=
            TyApp (Name (Ident VarIdent "typeRep")) (Opaque "\"hi\"")
      ]

  , testGroup "lists"
      [ testCase "portray []" $ portray @[()] [] @?= List []
      , testCase "portray [True]" $
          portray [True] @?= List [Name (Ident ConIdent "True")]
      ]

  , testGroup "IsList"
      [ testCase "portray NonEmpty" $
          portray (True :| [False]) @?=
            Apply (Name (Ident VarIdent "fromList"))
            [List
               [ Name (Ident ConIdent "True")
               , Name (Ident ConIdent "False")
               ]]
      ]

  , testGroup "Generic"
      [ testCase "portray NormalCon" $
          portray (NormalCon 2 True) @?=
            Apply
              (Name (Ident ConIdent "NormalCon"))
              [LitInt 2, Name (Ident ConIdent "True")]
      , testCase "portray InfixOperatorCon" $
          portray (2 :? True) @?=
            Binop
              (Ident OpConIdent ":?")
              (infixl_ 5)
              (LitInt 2)
              (Name (Ident ConIdent "True"))
      , testCase "portray PrefixOperatorCon" $
          portray (2 :?? True) @?=
            Apply
              (Name (Ident OpConIdent ":??"))
              [LitInt 2, Name (Ident ConIdent "True")]
      , testCase "portray RecordCon" $
          portray (RecordCon 2 True) @?=
            Record (Name (Ident ConIdent "RecordCon"))
              [ FactorPortrayal (sel "_rcInt") (LitInt 2)
              , FactorPortrayal (sel "_rcBool") (Name (Ident ConIdent "True"))
              ]
      , testCase "portray OperatorRecordCon" $
          portray (2 :??? True) @?=
            Record (Name (Ident OpConIdent ":???"))
              [ FactorPortrayal (sel "_orcInt") (LitInt 2)
              , FactorPortrayal (sel "_orcBool") (Name (Ident ConIdent "True"))
              ]
      , testCase "portray InfixCon" $
          portray (InfixCon 2 True) @?=
            Binop
              (Ident ConIdent "InfixCon")
              (infixl_ 9)
              (LitInt 2)
              (Name (Ident ConIdent "True"))
      -- Covered basic sum types and nullary constructors with Maybe and Void.
      ]
  ]
