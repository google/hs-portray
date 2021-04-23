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
import Data.Void (Void)
import GHC.Generics (Generic)
import Type.Reflection (typeRep)

import Data.Wrapped (Wrapped(..))
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

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

main :: IO ()
main = defaultMain
  [ testGroup "atoms"
      [ testCase "portray @Int" $ portray @Int 0 @=? "0"
      , testCase "portray @Bool" $ portray True @=? "True"
      , testCase "portray @Float" $ portray @Float 0.0 @=? "0.0"
      , testCase "portray @Char" $ portray 'a' @=? "'a'"
      , testCase "portray @Text" $ portray @Text "aoeu" @=? "\"aoeu\""
      , testCase "portray @()" $ portray () @=? "()"
      ]

  , testGroup "Maybe"
      [ testCase "portray Nothing" $ portray (Nothing @Int) @=? "Nothing"
      , testCase "portray Just" $ portray (Just ()) @=? Apply "Just" ["()"]
      ]

  , testCase "portray Void" $ const () (\x -> portray @Void x) @=? ()

  , testGroup "tuples"
      [ testCase "portray (,)" $ portray ('a', 'b') @=? Tuple ["'a'", "'b'"]
      , testCase "portray (,,)" $
          portray ('a', 'b', 'c') @=? Tuple ["'a'", "'b'", "'c'"]
      ]

  , testGroup "reflection"
      [ testCase "portray Int" $
          portray (typeRep @Int) @=? TyApp "typeRep" "Int"
      , testCase "portray ->" $
          portray (typeRep @(Int -> Int)) @=?
            TyApp "typeRep" (Binop "->" (infixr_ (-1)) "Int" "Int")
      , testCase "portray Either" $
          portray (typeRep @(Either Bool Int)) @=?
            TyApp "typeRep" (Apply (Apply "Either" ["Bool"]) ["Int"])
      , testCase "portray DataKinds" $
          portray (typeRep @'True) @=? TyApp "typeRep" "'True"
      , testCase "portray TypeNats" $
          portray (typeRep @4) @=? TyApp "typeRep" "4"
      , testCase "portray Symbol" $
          portray (typeRep @"hi") @=? TyApp "typeRep" "\"hi\""
      ]

  , testGroup "lists"
      [ testCase "portray []" $ portray @[()] [] @=? List []
      , testCase "portray [True]" $ portray [True] @=? List ["True"]
      ]

  , testGroup "IsList"
      [ testCase "portray NonEmpty" $
          portray (True :| [False]) @=?
            Apply "fromList" [List ["True", "False"]]
      ]

  , testGroup "Generic"
      [ testCase "portray NormalCon" $
          portray (NormalCon 2 True) @=? Apply "NormalCon" ["2", "True"]
      , testCase "portray InfixOperatorCon" $
          portray (2 :? True) @=? Binop ":?" (infixl_ 5) "2" "True"
      , testCase "portray PrefixOperatorCon" $
          portray (2 :?? True) @=? Apply "(:??)" ["2", "True"]
      , testCase "portray RecordCon" $
          portray (RecordCon 2 True) @=?
            Record "RecordCon"
              [ FactorPortrayal "_rcInt" "2"
              , FactorPortrayal "_rcBool" "True"
              ]
      , testCase "portray OperatorRecordCon" $
          portray (2 :??? True) @=?
            Record "(:???)"
              [ FactorPortrayal "_orcInt" "2"
              , FactorPortrayal "_orcBool" "True"
              ]
      -- Covered basic sum types and nullary constructors with Maybe and Void.
      ]
  ]
