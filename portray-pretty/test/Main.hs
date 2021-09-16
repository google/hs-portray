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

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Data.Portray
import Data.Portray.Pretty

main :: IO ()
main = defaultMain
  [ testGroup "Atom"
      [ testCase "()" $ prettyShowPortrayal (Tuple []) @?= "()"
      , testCase "2" $ prettyShowPortrayal (LitInt 2) @?= "2"
      ]

  , testGroup "Apply"
      [ testCase "nullary" $
          prettyShowPortrayal (Apply (Name "Nothing") []) @?= "Nothing"
      , testCase "nullary 2" $
          prettyShowPortrayal
              (Apply (Name "id") [Apply (Name "Nothing") []]) @?=
            "id Nothing"
      , testCase "unary" $
          prettyShowPortrayal (Apply (Name "Just") [LitInt 2]) @?= "Just 2"
      , testCase "parens" $
          prettyShowPortrayal
              (Apply (Name "Just") [Apply (Name "Just") [LitInt 2]]) @?=
            "Just (Just 2)"
      , testCase "binary" $
          prettyShowPortrayal (Apply (Name "These") [LitInt 2, LitInt 4]) @?=
            "These 2 4"
      , testCase "nested" $
          prettyShowPortrayal
              (Apply (Apply (Name "These") [LitInt 2]) [LitInt 4]) @?=
            "These 2 4"
      ]

  , testGroup "Binop"
      [ testCase "operator" $
          prettyShowPortrayal
              (Binop ":|" (infixr_ 5) (LitInt 5) (List [])) @?=
            "5 :| []"

      , testCase "con" $
          prettyShowPortrayal 
              (Binop
                (Ident ConIdent "InfixCon")
                (infixl_ 9)
                (LitInt 2)
                (Name "True")) @?=
            "2 `InfixCon` True"

      , testCase "nest prec" $
          prettyShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (Binop "*" (infixl_ 7) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "2 * 4 + 6"

      , testCase "nest anti-prec" $
          prettyShowPortrayal
              (Binop
                "*"
                (infixl_ 7)
                (Binop "+" (infixl_ 6) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "(2 + 4) * 6"

      , testCase "nest assoc" $
          prettyShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (Binop "+" (infixl_ 6) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "2 + 4 + 6"

      , testCase "nest anti-assoc" $
          prettyShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (LitInt 2)
                (Binop "+" (infixl_ 6) (LitInt 4) (LitInt 6))) @?=
            "2 + (4 + 6)"
      ]

  , testGroup "Tuple"
      [ testCase "pair" $
          prettyShowPortrayal (Tuple [LitInt 2, LitInt 4]) @?= "( 2, 4 )"
      , testCase "triple" $
          prettyShowPortrayal (Tuple [LitInt 2, LitInt 4, LitInt 6]) @?=
            "( 2, 4, 6 )"
      , testCase "line-break" $
          prettyShowPortrayal
              (Tuple [strAtom "222", strAtom (replicate 61 '2')]) @?=
            "( 222\n\
            \, 2222222222222222222222222222222222222222222222222222222222222\n\
            \)"
      ]

  , testGroup "List"
      [ testCase "empty" $ prettyShowPortrayal (List []) @?= "[]"
      , testCase "singleton" $ prettyShowPortrayal (List [LitInt 2]) @?=
          "[ 2 ]"
      ]

  , testGroup "LambdaCase"
      [ testCase "empty" $ prettyShowPortrayal (LambdaCase []) @?= "\\case {}"
      , testCase "singleton" $
          prettyShowPortrayal (LambdaCase [(Tuple [], LitInt 2)]) @?=
            "\\case { () -> 2 }"
      , testCase "two" $
          prettyShowPortrayal
              (LambdaCase
                [(Name "True", LitInt 2), (Name "False", LitInt 4)]) @?=
            "\\case { True -> 2; False -> 4 }"
      , testCase "line-break" $
          prettyShowPortrayal
              (LambdaCase
                [ (Name "True", strAtom (replicate 25 '2'))
                , (Name "False", strAtom (replicate 25 '4'))
                ]) @?=
            "\\case\n\
            \  { True -> 2222222222222222222222222\n\
            \  ; False -> 4444444444444444444444444\n\
            \  }"
      , testCase "no-parens" $
          prettyShowPortrayal
              (LambdaCase
                [( Apply (Name "Just") [LitInt 2]
                 , Apply (Name "Just") [LitInt 4]
                 )]) @?=
            "\\case { Just 2 -> Just 4 }"
      ]

  , testGroup "Record"
      [ testCase "empty" $ prettyShowPortrayal (Record (Name "Nothing") []) @?=
          "Nothing"
      , testCase "singleton" $
          prettyShowPortrayal
              (Record (Name "Just") [FactorPortrayal "it" (LitInt 2)]) @?=
            "Just { it = 2 }"
      , testCase "two" $
          prettyShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal "l" (LitInt 2)
                , FactorPortrayal "r" (LitInt 4)
                ]) @?=
            "These { l = 2, r = 4 }"
      , testCase "line-break" $
          prettyShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal "l" (portray @[Int] [0..10])
                , FactorPortrayal "r" (portray @[Int] [0..10])
                ]) @?=
            "These\n\
            \  { l = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]\n\
            \  , r = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]\n\
            \  }"
      , testCase "break-equals" $
          prettyShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal
                    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                    (Name "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                ]) @?=
            "These\n\
            \  { aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa =\n\
            \      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
            \  }"
      ]

  , testGroup "TyApp"
      [ testCase "con" $
          prettyShowPortrayal (TyApp (Name "typeRep") (Name "Int")) @?=
            "typeRep @Int"
      , testCase "parens" $
          prettyShowPortrayal
              (TyApp (Name "typeRep") (Apply (Name "Maybe") [Name "Int"])) @?=
            "typeRep @(Maybe Int)"
      , testCase "line-break" $
          prettyShowPortrayal
              (TyApp
                (strAtom $ replicate 50 'a')
                (strAtom $ replicate 50 'a')) @?=
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
            \  @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      ]

  , testGroup "TySig"
      [ testCase "con" $ prettyShowPortrayal (TySig (LitInt 2) (Name "Int")) @?=
          "2 :: Int"
      , testCase "no-parens" $
          prettyShowPortrayal
              (TySig
                (Apply (Name "Just") [LitInt 2])
                (Apply (Name "Maybe") [Name "Int"])) @?=
            "Just 2 :: Maybe Int"
      , testCase "line-break" $
          prettyShowPortrayal
              (TySig
                (strAtom $ replicate 50 'a')
                (strAtom $ replicate 50 'a')) @?=
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
            \  :: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      , testCase "parens" $
          prettyShowPortrayal
              (Apply (Name "Just") [TySig (LitInt 2) (Name "Int")]) @?=
            "Just (2 :: Int)"
      ]
  ]
