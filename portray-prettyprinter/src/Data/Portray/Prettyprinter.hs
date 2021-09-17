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

-- | Provides rendering of 'Portrayal' to 'Doc'.
--
-- There are two intended uses of this module: firstly, to use @prettyprinter@'s
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
-- This usage provides colorized pretty-printing by default with 'pp'.  Note if
-- you don't like the default choice of colors or don't want colors at all, you
-- can roll your own 'pp' function with 'portray', 'toDocAssocPrec' and your
-- @prettyprinter@ rendering backend of choice.
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
-- example = 'R.renderStrict' $ 'pretty' (MyRecord 2 ...)
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
-- Since the 'Pretty' class requires a universally-quantified annotation type,
-- its instances cannot provide any annotations.  As such, this usage cannot
-- provide automatic colorization.
--
-- This module also exports the underlying rendering functionality in a variety
-- of forms for more esoteric uses.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Portray.Prettyprinter
         ( -- * Pretty-Printing
           showPortrayal, pp
           -- * Diffing
         , showDiff, ppd
           -- * DerivingVia wrapper
         , WrappedPortray(..)
           -- * Rendering
           -- ** Configuration
         , Config, defaultConfig
           -- *** Escape Sequences
         , setShouldEscapeChar, escapeNonASCII, allowUnicode
           -- ** Colorization
         , defaultStyling, SyntaxClass(..)
           -- ** With Associativity
         , DocAssocPrec, toDocAssocPrecF, toDocAssocPrec
           -- ** Convenience Functions
         , portrayalToDoc, prettyShowPortrayal, basicShowPortrayal
         ) where

import Data.Char (isAscii, isDigit, isPrint)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import GHC.Show (showLitChar)

import Prettyprinter (Doc, Pretty(..))
import qualified Prettyprinter.Render.Terminal as A -- for ANSI
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as R

import Data.Portray
         ( Assoc(..), Infixity(..), FactorPortrayal(..)
         , Ident(..), IdentKind(..)
         , Portray, Portrayal(..), PortrayalF(..)
         , cata, portray
         )
import Data.Portray.Diff (Diff(..))

-- | Pretty-print a value to stdout using its 'Portray' instance.
--
-- This uses ANSI color codes, so take care not to use it in contexts where it
-- might output to something other than a terminal.
pp :: Portray a => a -> IO ()
pp = T.putStrLn . prettyShowPortrayal . portray

-- | Pretty-print a value using its 'Portray' instance.
--
-- This uses no ANSI terminal escape codes and escapes all non-ASCII characters.
showPortrayal :: Portray a => a -> Text
showPortrayal = basicShowPortrayal . portray

-- | Pretty-print a diff between two values to stdout using a 'Diff' instance.
--
-- This uses ANSI color codes, so take care not to use it in contexts where it
-- might output to something other than a terminal.
ppd :: Diff a => a -> a -> IO ()
ppd x = T.putStrLn . maybe "_" prettyShowPortrayal . diff x

-- | Pretty-print a diff between two values using a 'Diff' instance.
--
-- This uses no ANSI terminal escape codes and escapes all non-ASCII characters.
showDiff :: Diff a => a -> a -> Text
showDiff x = maybe "_" basicShowPortrayal . diff x

-- | A 'Doc' that varies according to associativity and precedence context.
type DocAssocPrec ann = Assoc -> Rational -> Doc ann

fixityCompatible :: Infixity -> Assoc -> Rational -> Bool
fixityCompatible (Infixity assoc p) assoc' p' = case compare p' p of
  GT -> False  -- Context has higher precedence than this binop.
  EQ -> assoc == assoc'
  LT -> True

matchCtx :: Assoc -> Assoc -> Assoc
matchCtx ctx assoc
  | ctx == assoc = ctx
  | otherwise = AssocNope

data LitKind = IntLit | RatLit | CharLit | StrLit

data SyntaxClass
  = Identifier IdentKind
  | Literal LitKind
  | EscapeSequence
  | Keyword
  | Structural -- ^ Parens, brackets, and other fixed syntactic symbols.

-- | A fairly arbitrary colorization style based on what looked good to me.
defaultStyling :: SyntaxClass -> A.AnsiStyle
defaultStyling = \case
  Identifier k -> case k of
    OpConIdent -> A.color A.Magenta
    OpIdent -> A.colorDull A.Yellow
    ConIdent -> mempty
    VarIdent -> mempty
  Literal k -> case k of
    StrLit -> A.colorDull A.Blue
    _      -> A.colorDull A.Cyan
  EscapeSequence -> A.colorDull A.Red
  Keyword -> A.colorDull A.Green
  Structural -> mempty

-- | An escape-sequence predicate to escape any non-ASCII character.
escapeNonASCII :: Char -> Bool
escapeNonASCII = not . isAscii

-- | An escape-sequence predicate to escape as little as possible.
allowUnicode :: Char -> Bool
allowUnicode = const False

-- | Configuration for the conversion to 'Doc'.
--
-- Includes the following:
--
-- * 'setShouldEscapeChar', a function determining whether an escapable
-- character should be escaped in a string or character literal.  Unprintable
-- characters, backslashes, and the relevant quote for the current literal type
-- are always escaped, and anything that wouldn't be escaped by 'Show' is never
-- escaped.
--
-- For forwards-compatibility reasons, the field selectors and constructor of
-- this type are hidden; use the provided setters to build a config.  For
-- example:
--
-- @
-- config =
--   defaultConfig
--     & setShouldEscapeChar (const True) -- Escape everything we can.
-- @
data Config = Config
  { _shouldEscapeChar :: Char -> Bool
  }

-- | Set the predicate for whether to escape a given character; see 'Config'.
setShouldEscapeChar :: (Char -> Bool) -> Config -> Config
setShouldEscapeChar f _ = Config f

-- | A sensible default configuration to build on.
--
-- Uses 'escapeNonASCII'.
defaultConfig :: Config
defaultConfig = Config escapeNonASCII

-- | Convert a 'Portrayal' to a 'Doc'.
portrayalToDoc :: Config -> Portrayal -> Doc SyntaxClass
portrayalToDoc cfg t = toDocAssocPrec cfg t AssocNope (-1)

parens :: Doc SyntaxClass -> Doc SyntaxClass
parens d =
  P.annotate Structural (char '(') <> d <> P.annotate Structural (char ')')

-- Conditionally wrap a document in parentheses.
maybeParens :: Bool -> Doc SyntaxClass -> Doc SyntaxClass
maybeParens = \case True -> parens; False -> id

-- Convert Text to a document; 'pretty' specialized to 'Text'.
text :: Text -> Doc ann
text = pretty

-- Convert a Char to a document; 'pretty' specialized to 'Char'.
char :: Char -> Doc ann
char = pretty

ppInfix :: Ident -> Doc SyntaxClass
ppInfix (Ident k nm) = case k of
  OpConIdent -> nmDoc
  OpIdent -> nmDoc
  VarIdent -> wrappedNm
  ConIdent -> wrappedNm
 where
  backquote = P.annotate Structural (char '`')
  nmDoc = P.annotate (Identifier k) (text nm)
  wrappedNm = backquote <> nmDoc <> backquote

ppPrefix :: Ident -> Doc SyntaxClass
ppPrefix (Ident k nm) = case k of
  OpConIdent -> wrappedNm
  OpIdent -> wrappedNm
  VarIdent -> nmDoc
  ConIdent -> nmDoc
 where
  nmDoc = P.annotate (Identifier k) (text nm)
  wrappedNm = parens nmDoc

ppBinop
  :: Ident
  -> Infixity
  -> DocAssocPrec SyntaxClass
  -> DocAssocPrec SyntaxClass
  -> DocAssocPrec SyntaxClass
ppBinop nm fx@(Infixity assoc opPrec) x y lr p =
  maybeParens (not $ fixityCompatible fx lr p) $ P.nest 2 $ P.sep
    [ x (matchCtx AssocL assoc) opPrec P.<+> ppInfix nm
    , y (matchCtx AssocR assoc) opPrec
    ]

ppBulletList
  :: Doc SyntaxClass -- ^ Open brace,  e.g. {  [  {  (
  -> Doc SyntaxClass -- ^ Separator,   e.g. ;  ,  ,  ,
  -> Doc SyntaxClass -- ^ Close brace, e.g. }  ]  }  )
  -> [Doc SyntaxClass]
  -> Doc SyntaxClass
ppBulletList o s c = \case
  []         -> opener <> closer
  (doc:docs) ->
    P.group $
      foldl01
        (\x y -> x <> P.group (P.line' <> y))
        id
        mempty
        (opener <> P.flatAlt " " "" <> doc :
          zipWith (P.<+>) (repeat separator) docs) <>
      P.line' <> closer
 where
  f = P.annotate Structural
  opener = f o
  separator = f s
  closer = f c

foldl01 :: (b -> a -> b) -> (a -> b) -> b -> [a] -> b
foldl01 f g z = \case
  [] -> z
  (x:xs) -> foldl f (g x) xs

-- 'T.words' coalesces adjacent spaces, so it's not suitable for use in
-- 'ppStrLit'; roll our own that considers adjacent spaces to have empty words
-- between them.
wordsSep :: Text -> [(Text, Text)]
wordsSep "" = []
wordsSep s =
  let isWhitespace = (`elem` [' ', '\t'])
      (word, rest) = T.break isWhitespace s
      (sep, rest') = T.span isWhitespace rest
  in  (word, sep) : wordsSep rest'

-- 'T.lines' also fails to distinguish trailing newlines... ugh.
linesSep :: Text -> [Text]
linesSep "" = []
linesSep s0 = go s0
 where
  go s =
    let (line, rest) = T.break (== '\n') s
    in  line : if T.null rest then [] else go (T.tail rest)

-- Returns True for characters that must always be escaped regardless of
-- configuration; this is unprintable characters and backlashes.
charAlwaysEscaped :: Char -> Bool
charAlwaysEscaped c = not (isPrint c) || c == '\\'

shouldEscapeChar :: Config -> Char -> Bool
shouldEscapeChar cfg c = charAlwaysEscaped c || _shouldEscapeChar cfg c

showLitEscapesChar :: Char -> Bool
showLitEscapesChar c = [c] /= showLitChar c ""

litCharIsEscaped :: Config -> Char -> Bool
litCharIsEscaped cfg = \case
  '\'' -> True
  c    -> showLitEscapesChar c && shouldEscapeChar cfg c

strCharIsEscaped :: Config -> Char -> Bool
strCharIsEscaped cfg = \case
  '"' -> True
  c   -> showLitEscapesChar c && shouldEscapeChar cfg c

-- After a numeric escape, we need a \&; to that end, detect numeric escapes.
needsEmptyEscape :: Config -> Char -> Char -> Bool
needsEmptyEscape cfg c0 c1 =
  strCharIsEscaped cfg c0 &&
  case showLitChar c0 "" of
    "\\SO" -> c1 == 'H'
    ('\\' : c : _) -> isDigit c && isDigit c1
    _ -> False

escapeChar :: Config -> Char -> Text
escapeChar cfg c
  | shouldEscapeChar cfg c = T.pack (showLitChar c "")
  | otherwise              = T.singleton c

escapeLitChar :: Config -> Char -> Text
escapeLitChar cfg = \case
  '\'' -> "\\'"
  c -> escapeChar cfg c

escapeStrChar :: Config -> Char -> Text
escapeStrChar cfg = \case
  '"' -> "\\\""
  c -> escapeChar cfg c

ppStrLit :: Config -> Text -> Doc SyntaxClass
ppStrLit cfg unescaped =
  P.annotate (Literal StrLit) $
  P.group $ -- Prefer putting the whole thing on this line whenever possible.
  P.enclose "\"" "\"" $
  -- Then prefer breaking on newlines when the next line doesn't fit.
  foldl01
    (\x l ->
      x <> P.group (P.flatAlt (nl <> backslashBreak <> l) (nl <> l)))
    id
    mempty
    (ppLine <$> escapedLinesOfWords)
 where
  nl = P.annotate EscapeSequence "\\n"

  ppWord :: Text -> Doc SyntaxClass
  ppWord "" = mempty
  ppWord w =
    let (toEscape, rest) = T.span (strCharIsEscaped cfg) w
        (plain, rest') = T.break (strCharIsEscaped cfg) rest
        sep =
          -- Do we need to insert a \& to separate a numeric escape from a
          -- following digit?
          if not (T.null toEscape) &&
               not (T.null plain) &&
               needsEmptyEscape cfg (T.last toEscape) (T.head plain)
             then "\\&"
             else mempty
        escaped = pretty (T.concatMap (escapeStrChar cfg) toEscape) <> sep
    in  P.annotate EscapeSequence escaped <> text plain <> ppWord rest'

  escapedLinesOfWords :: [[(Doc SyntaxClass, Doc SyntaxClass)]]
  escapedLinesOfWords =
    map
        (\ (w, ws) -> (ppWord w, ppWhitespace ws)) .
      wordsSep <$>
    linesSep unescaped

  ppWhitespace :: Text -> Doc SyntaxClass
  ppWhitespace =
    P.annotate EscapeSequence .
    text . T.concatMap (escapeStrChar cfg)

  ppLine :: [(Doc SyntaxClass, Doc SyntaxClass)] -> Doc SyntaxClass
  ppLine ws =
    -- Finally, break at word boundaries if the next word doesn't fit.
    P.group $ uncurry (<>) $ foldl01
      (\(line, space) (w, space') ->
        ( line <> P.group (P.flatAlt (space <> backslashBreak) space <> w)
        , space'
        ))
      id
      mempty
      ws

  backslashBreak = P.annotate EscapeSequence $ "\\" <> P.line' <> "\\"

-- | Render one layer of 'PortrayalF' to 'DocAssocPrec'.
toDocAssocPrecF
  :: Config
  -> PortrayalF (DocAssocPrec SyntaxClass)
  -> DocAssocPrec SyntaxClass
toDocAssocPrecF cfg = \case
  NameF nm -> \_ _ -> ppPrefix nm
  LitIntF x -> \_ _ -> P.annotate (Literal IntLit) $ pretty x
  LitRatF x -> \_ _ ->
    P.annotate (Literal RatLit) $ pretty (fromRational x :: Double)
  LitStrF x -> \_ _ -> ppStrLit cfg x
  LitCharF x -> \_ _ ->
    -- Likewise Char
    P.annotate (Literal CharLit) $
    P.enclose "'" "'" $
    (if litCharIsEscaped cfg x then P.annotate EscapeSequence else id) $
    text $ escapeLitChar cfg x
  OpaqueF txt -> \_ _ -> text txt
  ApplyF fn [] -> \_ _ -> fn AssocL 10
  ApplyF fn xs -> \lr p ->
    maybeParens (not $ fixityCompatible (Infixity AssocL 10) lr p) $
      P.nest 2 $ P.sep
        [ fn AssocL 10
        , P.sep $ xs <&> \docprec -> docprec AssocR 10
        ]
  BinopF nm fx x y -> ppBinop nm fx x y
  TupleF xs -> \_ _ -> ppBulletList "(" "," ")" $ xs <&> \x -> x AssocNope (-1)
  ListF xs -> \_ _ -> ppBulletList "[" "," "]" $ xs <&> \x -> x AssocNope (-1)
  LambdaCaseF xs -> \_ p ->
    maybeParens (p >= 10) $
      P.nest 2 $ P.sep
        [ P.annotate Structural "\\" <> P.annotate Keyword "case"
        , ppBulletList "{" ";" "}"
            [ P.nest 2 $ P.sep $
                [ pat AssocNope 0 P.<+> P.annotate Structural "->"
                , val AssocNope 0
                ]
            | (pat, val) <- xs
            ]
        ]
  RecordF con sels -> \_ _ -> case sels of
    [] -> con AssocNope (-1)
    _  -> P.nest 2 $ P.sep
      [ con AssocNope 10
      , ppBulletList "{" "," "}"
          [ P.nest 4 $ P.sep
              [ ppPrefix sel P.<+> P.annotate Structural "="
              , val AssocNope 0
              ]
          | FactorPortrayal sel val <- sels
          ]
      ]
  TyAppF val ty -> \_ _ ->
    P.nest 2 $ P.sep
      [ val AssocNope 10
      , P.annotate Structural "@" <> ty AssocNope 10
      ]
  TySigF val ty -> \_ p -> maybeParens (p >= 0) $
    P.nest 2 $ P.sep
      [ val AssocNope 0
      , P.annotate Structural "::" P.<+> ty AssocNope 0
      ]
  QuotF nm content -> \_ _ ->
    P.nest 2 $ P.sep
      [ P.annotate Structural "[" <>
          P.annotate (Identifier VarIdent) (text nm) <>
          P.annotate Structural "|"
      , content AssocNope (-1)
      , P.annotate Structural "|]"
      ]
  UnlinesF ls -> \_ _ -> P.vcat (ls <&> \l -> l AssocNope (-1))
  NestF n x -> \_ _ -> P.nest n (x AssocNope (-1))

-- | Render a 'Portrayal' to a 'Doc' with support for operator associativity.
toDocAssocPrec :: Config -> Portrayal -> DocAssocPrec SyntaxClass
toDocAssocPrec cfg = cata (toDocAssocPrecF cfg) . unPortrayal

-- | Convenience function for rendering a 'Portrayal' to a 'Text'.
basicShowPortrayal :: Portrayal -> Text
basicShowPortrayal p =
  R.renderStrict $ P.layoutPretty P.defaultLayoutOptions $
  toDocAssocPrec defaultConfig p AssocNope (-1)

-- | Convenience function for rendering a 'Portrayal' to colorized 'Text'.
prettyShowPortrayal :: Portrayal -> Text
prettyShowPortrayal p =
  A.renderStrict $ fmap defaultStyling $
  P.layoutPretty P.defaultLayoutOptions $
  toDocAssocPrec
    (defaultConfig & setShouldEscapeChar allowUnicode)
    p
    AssocNope
    (-1)

-- | A newtype providing a 'Pretty' instance via 'Portray', for @DerivingVia@.
--
-- Sadly we can't use @Wrapped@ since it would be an orphan instance.  Oh well.
-- We'll just define a unique 'WrappedPortray' newtype in each
-- pretty-printer-integration package.
newtype WrappedPortray a = WrappedPortray { unWrappedPortray :: a }
  deriving newtype (Eq, Ord, Show)

-- | Provide an instance for 'Pretty' by way of 'Portray'.
instance Portray a => Pretty (WrappedPortray a) where
  pretty x =
    P.unAnnotate $ portrayalToDoc defaultConfig (portray $ unWrappedPortray x)
