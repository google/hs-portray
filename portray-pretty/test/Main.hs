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
      [ testCase "()" $ prettyShowPortrayal "()" @?= "()"
      , testCase "2" $ prettyShowPortrayal "2" @?= "2"
      ]

  , testGroup "Apply"
      [ testCase "nullary" $
          prettyShowPortrayal (Apply "Nothing" []) @?= "Nothing"
      , testCase "nullary 2" $
          prettyShowPortrayal (Apply "id" [Apply "Nothing" []]) @?= "id Nothing"
      , testCase "unary" $
          prettyShowPortrayal (Apply "Just" ["2"]) @?= "Just 2"
      , testCase "parens" $
          prettyShowPortrayal (Apply "Just" [Apply "Just" ["2"]]) @?=
            "Just (Just 2)"
      , testCase "binary" $
          prettyShowPortrayal (Apply "These" ["2", "4"]) @?=
            "These 2 4"
      , testCase "nested" $
          prettyShowPortrayal (Apply (Apply "These" ["2"]) ["4"]) @?=
            "These 2 4"
      ]

  , testGroup "Binop"
      [ testCase "operator" $
          prettyShowPortrayal (Binop ":|" (infixr_ 5) "5" (List [])) @?=
            "5 :| []"

      , testCase "con" $
          prettyShowPortrayal (Binop "`InfixCon`" (infixl_ 9) "2" "True") @?=
            "2 `InfixCon` True"

      , testCase "nest prec" $
          prettyShowPortrayal
              (Binop "+" (infixl_ 6) (Binop "*" (infixl_ 7) "2" "4") "6") @?=
            "2 * 4 + 6"

      , testCase "nest anti-prec" $
          prettyShowPortrayal
              (Binop "*" (infixl_ 7) (Binop "+" (infixl_ 6) "2" "4") "6") @?=
            "(2 + 4) * 6"

      , testCase "nest assoc" $
          prettyShowPortrayal
              (Binop "+" (infixl_ 6) (Binop "+" (infixl_ 6) "2" "4") "6") @?=
            "2 + 4 + 6"

      , testCase "nest anti-assoc" $
          prettyShowPortrayal
              (Binop "+" (infixl_ 6) "2" (Binop "+" (infixl_ 6) "4" "6")) @?=
            "2 + (4 + 6)"
      ]

  , testGroup "Tuple"
      [ testCase "pair" $
          prettyShowPortrayal (Tuple ["2", "4"]) @?= "( 2, 4 )"
      , testCase "triple" $
          prettyShowPortrayal (Tuple ["2", "4", "6"]) @?= "( 2, 4, 6 )"
      , testCase "line-break" $
          prettyShowPortrayal (Tuple ["222", strAtom (replicate 61 '2')]) @?=
            "( 222\n\
            \, 2222222222222222222222222222222222222222222222222222222222222\n\
            \)"
      ]

  , testGroup "List"
      [ testCase "empty" $ prettyShowPortrayal (List []) @?= "[]"
      , testCase "singleton" $ prettyShowPortrayal (List ["2"]) @?= "[ 2 ]"
      ]

  , testGroup "LambdaCase"
      [ testCase "empty" $ prettyShowPortrayal (LambdaCase []) @?= "\\case {}"
      , testCase "singleton" $
          prettyShowPortrayal (LambdaCase [("()", "2")]) @?=
            "\\case { () -> 2 }"
      , testCase "two" $
          prettyShowPortrayal (LambdaCase [("True", "2"), ("False", "4")]) @?=
            "\\case { True -> 2; False -> 4 }"
      , testCase "line-break" $
          prettyShowPortrayal
              (LambdaCase
                [ ("True", strAtom (replicate 25 '2'))
                , ("False", strAtom (replicate 25 '4'))
                ]) @?=
            "\\case\n\
            \  { True -> 2222222222222222222222222\n\
            \  ; False -> 4444444444444444444444444\n\
            \  }"
      , testCase "no-parens" $
          prettyShowPortrayal
              (LambdaCase [(Apply "Just" ["2"], Apply "Just" ["4"])]) @?=
            "\\case { Just 2 -> Just 4 }"
      ]

  , testGroup "Record"
      [ testCase "empty" $ prettyShowPortrayal (Record "Nothing" []) @?=
          "Nothing"
      , testCase "singleton" $
            prettyShowPortrayal (Record "Just" [FactorPortrayal "it" "2"]) @?=
          "Just { it = 2 }"
      , testCase "two" $
            prettyShowPortrayal
              (Record "These"
                [FactorPortrayal "l" "2", FactorPortrayal "r" "4"]) @?=
          "These { l = 2, r = 4 }"
      , testCase "line-break" $
            prettyShowPortrayal
              (Record "These"
                [ FactorPortrayal "l" (portray @[Int] [0..10])
                , FactorPortrayal "r" (portray @[Int] [0..10])
                ]) @?=
          "These\n\
          \  { l = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]\n\
          \  , r = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]\n\
          \  }"
      ]

  , testGroup "TyApp"
      [ testCase "con" $
          prettyShowPortrayal (TyApp "typeRep" "Int") @?= "typeRep @Int"
      , testCase "parens" $
          prettyShowPortrayal (TyApp "typeRep" (Apply "Maybe" ["Int"])) @?=
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
      [ testCase "con" $ prettyShowPortrayal (TySig "2" "Int") @?= "2 :: Int"
      , testCase "no-parens" $
          prettyShowPortrayal
              (TySig (Apply "Just" ["2"]) (Apply "Maybe" ["Int"])) @?=
            "Just 2 :: Maybe Int"
      , testCase "line-break" $
          prettyShowPortrayal
              (TySig
                (strAtom $ replicate 50 'a')
                (strAtom $ replicate 50 'a')) @?=
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
            \  :: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      , testCase "parens" $
          prettyShowPortrayal (Apply "Just" [TySig "2" "Int"]) @?=
            "Just (2 :: Int)"
      ]
  ]
