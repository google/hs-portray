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

-- | Assertion functions for HUnit based on 'Diff'.

module Data.Portray.Diff.HUnit (assertNoDiff, (@?-), (@-?)) where

import GHC.Stack (HasCallStack)
import Text.PrettyPrint.HughesPJ (($+$), colon, empty, render, text)

import Test.HUnit (Assertion, assertFailure)

import Data.Portray.Diff (Diff(..))
import Data.Portray.Pretty (portrayalToDoc)

-- | Same as 'assertEqual', but using 'Diff' to compare and report errors.
assertNoDiff :: (HasCallStack, Diff a) => String -> a -> a -> Assertion
assertNoDiff msg a b = case diff a b of
  Nothing -> pure ()
  Just d -> assertFailure . render $
    (if null msg then empty else text msg <> colon) $+$
    portrayalToDoc d

-- | Same as ('@?='), but using 'Diff' to compare and report errors.
(@?-) :: (HasCallStack, Diff a) => a -> a -> Assertion
a @?- b = assertNoDiff "" a b

-- | Same as ('@-?'), but using 'Diff' to compare and report errors.
(@-?) :: (HasCallStack, Diff a) => a -> a -> Assertion
a @-? b = assertNoDiff "" b a
