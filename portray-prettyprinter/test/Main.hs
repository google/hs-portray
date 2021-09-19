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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Read (readLitChar)
import GHC.Show (showLitChar)

#if MIN_VERSION_prettyprinter(1, 7, 0)
#define Prettyprinter_ Prettyprinter
#else
#define Prettyprinter_ Data.Text.Prettyprint.Doc
#endif

import qualified Prettyprinter_ as P
import qualified Prettyprinter_.Render.Text as R
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck ((===), Property)

import Data.Portray
import Data.Portray.Prettyprinter

-- 'read' doesn't implement this feature of Haskell string literals, so strip
-- it out ourselves.
stripEscapedWhitespace :: String -> String
stripEscapedWhitespace = \case
  [] -> []
  '\\' : c : rest
    | c `elem` [' ', '\t', '\n'] ->
        stripEscapedWhitespace $ tail $ dropWhile (/= '\\') rest
  -- showLitChar doesn't have to escape '"', but we do.
  ('\\' : '"' : rest) -> '\\' : '"' : stripEscapedWhitespace rest
  ('\\' : s) | ((c, rest):_) <- readLitChar ('\\' : s) ->
        showLitChar c (stripEscapedWhitespace rest)
  c:rest -> c : stripEscapedWhitespace rest

showPortrayalCfg :: Portray a => Config -> a -> Text
showPortrayalCfg cfg x =
  R.renderStrict $ P.layoutPretty P.defaultLayoutOptions $
  toDocAssocPrec cfg (portray x) AssocNope (-1)

propTextRoundTrips :: Config -> String -> Property
propTextRoundTrips cfg t =
  t ===
    read (stripEscapedWhitespace $ T.unpack $ showPortrayalCfg cfg $ T.pack t)

propStringRoundTrips :: Config -> String -> Property
propStringRoundTrips cfg t =
  t === read (stripEscapedWhitespace $ T.unpack $ showPortrayalCfg cfg t)

propCharRoundTrips :: Config -> Char -> Property
propCharRoundTrips cfg c = c === read (T.unpack $ showPortrayalCfg cfg c)

main :: IO ()
main = defaultMain
  [ testGroup "Atom"
      [ testCase "()" $ basicShowPortrayal (Tuple []) @?= "()"
      , testCase "2" $ basicShowPortrayal (LitInt 2) @?= "2"
      ]

  , testGroup "Apply"
      [ testCase "nullary" $
          basicShowPortrayal (Apply (Name "Nothing") []) @?= "Nothing"
      , testCase "nullary 2" $
          basicShowPortrayal
              (Apply (Name "id") [Apply (Name "Nothing") []]) @?=
            "id Nothing"
      , testCase "unary" $
          basicShowPortrayal (Apply (Name "Just") [LitInt 2]) @?= "Just 2"
      , testCase "parens" $
          basicShowPortrayal
              (Apply (Name "Just") [Apply (Name "Just") [LitInt 2]]) @?=
            "Just (Just 2)"
      , testCase "binary" $
          basicShowPortrayal (Apply (Name "These") [LitInt 2, LitInt 4]) @?=
            "These 2 4"
      , testCase "nested" $
          basicShowPortrayal
              (Apply (Apply (Name "These") [LitInt 2]) [LitInt 4]) @?=
            "These 2 4"
      ]

  , testGroup "Binop"
      [ testCase "operator" $
          basicShowPortrayal
              (Binop ":|" (infixr_ 5) (LitInt 5) (List [])) @?=
            "5 :| []"

      , testCase "con" $
          basicShowPortrayal
              (Binop
                (Ident ConIdent "InfixCon")
                (infixl_ 9)
                (LitInt 2)
                (Name "True")) @?=
            "2 `InfixCon` True"

      , testCase "nest prec" $
          basicShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (Binop "*" (infixl_ 7) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "2 * 4 + 6"

      , testCase "nest anti-prec" $
          basicShowPortrayal
              (Binop
                "*"
                (infixl_ 7)
                (Binop "+" (infixl_ 6) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "(2 + 4) * 6"

      , testCase "nest assoc" $
          basicShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (Binop "+" (infixl_ 6) (LitInt 2) (LitInt 4))
                (LitInt 6)) @?=
            "2 + 4 + 6"

      , testCase "nest anti-assoc" $
          basicShowPortrayal
              (Binop
                "+"
                (infixl_ 6)
                (LitInt 2)
                (Binop "+" (infixl_ 6) (LitInt 4) (LitInt 6))) @?=
            "2 + (4 + 6)"
      ]

  , testGroup "Tuple"
      [ testCase "pair" $
          basicShowPortrayal (Tuple [LitInt 2, LitInt 4]) @?= "(2, 4)"
      , testCase "triple" $
          basicShowPortrayal (Tuple [LitInt 2, LitInt 4, LitInt 6]) @?=
            "(2, 4, 6)"
      , testCase "line-break" $
          basicShowPortrayal
              (Tuple [strAtom "222", strAtom (replicate 80 '2')]) @?=
            "( 222\n\\
            \, 2222222222222222222222222222222222222222222222222222222222222\\
            \2222222222222222222\n\\
            \)"
      ]

  , testGroup "List"
      [ testCase "empty" $ basicShowPortrayal (List []) @?= "[]"
      , testCase "singleton" $ basicShowPortrayal (List [LitInt 2]) @?=
          "[2]"
      ]

  , testGroup "LambdaCase"
      [ testCase "empty" $ basicShowPortrayal (LambdaCase []) @?= "\\case {}"
      , testCase "singleton" $
          basicShowPortrayal (LambdaCase [(Tuple [], LitInt 2)]) @?=
            "\\case {() -> 2}"
      , testCase "two" $
          basicShowPortrayal
              (LambdaCase
                [(Name "True", LitInt 2), (Name "False", LitInt 4)]) @?=
            "\\case {True -> 2; False -> 4}"
      , testCase "line-break" $
          basicShowPortrayal
              (LambdaCase
                [ (Name "True", strAtom (replicate 40 '2'))
                , (Name "False", strAtom (replicate 40 '4'))
                ]) @?=
            "\\case\n\\
            \  { True -> 2222222222222222222222222222222222222222\n\\
            \  ; False -> 4444444444444444444444444444444444444444\n\\
            \  }"
      , testCase "no-parens" $
          basicShowPortrayal
              (LambdaCase
                [( Apply (Name "Just") [LitInt 2]
                 , Apply (Name "Just") [LitInt 4]
                 )]) @?=
            "\\case {Just 2 -> Just 4}"
      ]

  , testGroup "Record"
      [ testCase "empty" $ basicShowPortrayal (Record (Name "Nothing") []) @?=
          "Nothing"
      , testCase "singleton" $
            basicShowPortrayal
              (Record
                (Name "Just")
                [FactorPortrayal "it" (LitInt 2)]) @?=
          "Just {it = 2}"
      , testCase "two" $
            basicShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal "l" (LitInt 2)
                , FactorPortrayal "r" (LitInt 4)
                ]) @?=
          "These {l = 2, r = 4}"
      , testCase "line-break" $
            basicShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal "l" (portray @[Int] [0..10])
                , FactorPortrayal "r" (portray @[Int] [0..10])
                ]) @?=
          "These\n\\
          \  { l = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\\
          \  , r = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\\
          \  }"
      , testCase "break-equals" $
          basicShowPortrayal
              (Record (Name "These")
                [ FactorPortrayal
                    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                    (Name "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                ]) @?=
            "These\n\\
            \  { aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa =\n\\
            \      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\\
            \  }"
      ]

  , testGroup "TyApp"
      [ testCase "con" $
          basicShowPortrayal (TyApp (Name "typeRep") (Name "Int")) @?=
            "typeRep @Int"
      , testCase "parens" $
          basicShowPortrayal
              (TyApp (Name "typeRep") (Apply (Name "Maybe") [Name "Int"])) @?=
            "typeRep @(Maybe Int)"
      , testCase "line-break" $
          basicShowPortrayal
              (TyApp
                (strAtom $ replicate 50 'a')
                (strAtom $ replicate 50 'a')) @?=
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\\
            \  @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      ]

  , testGroup "TySig"
      [ testCase "con" $ basicShowPortrayal (TySig (LitInt 2) (Name "Int")) @?=
          "2 :: Int"
      , testCase "no-parens" $
          basicShowPortrayal
              (TySig
                (Apply (Name "Just") [LitInt 2])
                (Apply (Name "Maybe") [Name "Int"])) @?=
            "Just 2 :: Maybe Int"
      , testCase "line-break" $
          basicShowPortrayal
              (TySig
                (strAtom $ replicate 50 'a')
                (strAtom $ replicate 50 'a')) @?=
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\\
            \  :: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      , testCase "parens" $
          basicShowPortrayal
              (Apply (Name "Just") [TySig (LitInt 2) (Name "Int")]) @?=
            "Just (2 :: Int)"
      ]

  , testGroup "StrLit"
      [ testProperty "Text round-trips ascii"
          (propTextRoundTrips defaultConfig)
      , testProperty "String round-trips ascii"
          (propStringRoundTrips defaultConfig)
      , testProperty "Char round-trips ascii"
          (propCharRoundTrips defaultConfig)

      , testProperty "Text round-trips unicode"
          (propTextRoundTrips unicodeConfig)
      , testProperty "String round-trips unicode"
          (propStringRoundTrips unicodeConfig)
      , testProperty "Char round-trips unicode"
          (propCharRoundTrips unicodeConfig)
      ]
  ]
 where
  unicodeConfig = defaultConfig & setShouldEscapeChar escapeSpecialOnly
