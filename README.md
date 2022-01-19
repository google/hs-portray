# portray

A compatibility layer between Haskell-like syntax and pretty-printer backends.

[![Stack CI](https://github.com/google/hs-portray/actions/workflows/stack-ci.yml/badge.svg)](https://github.com/google/hs-portray/actions/workflows/stack-ci.yml)

## Disclaimer

This is not an officially supported Google product.

## Hackage Status

* [![portray](https://badgen.net/runkit/awpr/hackage/v/portray?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray?cache=600)
* [![portray-diff](https://badgen.net/runkit/awpr/hackage/v/portray-diff?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray-diff)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray-diff?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray-diff?cache=600)
* [![portray-pretty](https://badgen.net/runkit/awpr/hackage/v/portray-pretty?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray-pretty)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray-pretty?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray-pretty?cache=600)
* [![portray-prettyprinter](https://badgen.net/runkit/awpr/hackage/v/portray-prettyprinter?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray-prettyprinter)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray-prettyprinter?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray-prettyprinter?cache=600)
* [![portray-diff-hunit](https://badgen.net/runkit/awpr/hackage/v/portray-diff-hunit?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray-diff-hunit)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray-diff-hunit?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray-diff-hunit?cache=600)
* [![portray-diff-quickcheck](https://badgen.net/runkit/awpr/hackage/v/portray-diff-quickcheck?icon=haskell&cache=600)](https://hackage.haskell.org/package/portray-diff-quickcheck)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/portray-diff-quickcheck?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/portray-diff-quickcheck?cache=600)

## Problem Statement

The Haskell ecosystem doesn't have consistently-available pretty-printing
functionality for debugging purposes.  Many pretty-printers exist providing
rendering and typeclasses of pretty-printable types; each has its own advantages
and set of devotees, but few libraries on Hackage actually provide instances for
any of them.  Providing instances for all of the pretty-printers is too onerous
and incurs too many dependencies; and no single pretty-printer has sufficient
critical mass to convince package maintainers to support it over the others.
So, in practice, nothing comes with any pretty-printing support.

In most application codebases of nontrivial size, one of the first things
Haskellers end up doing is picking a pretty-printer, writing orphan instances
for the whole world, and maintaining instances for their own types alongside the
orphans.  Doing this from scratch in every new application codebase is a waste
of effort.  The alternative, even worse, is to throw up one's hands, declare it
not worth the effort, and use Show, poring over many-kilobyte-long lines of
text.

### Bonus Problem

`Text.PrettyPrint.HughesPJ.Doc` has an `NFData` instance, but the type relies
critically on laziness, and its complete structure grows exponentially with the
length of the document, so calling `rnf` on a document of even relatively modest
size has disastrous performance consequences (e.g. inexplicably consume all of
your memory and lock up your machine).  Laziness-based pretty-printer documents
are ill-suited as a data representation; they're primarily a control structure.

## Solution

There are 15 competing standards.

## No, Really

Create a package providing a type of pseudo-Haskell-syntax terms and a typeclass
for values that can be rendered to it; minimize its dependency weight to make it
palatable to library maintainers.  Provide (in separate packages) renderings of
this syntax into various pretty-printer backends.  By incurring a small
dependency and deriving a single instance, library maintainers can provide
support for a wide variety of debugging use cases.  Application authors can
either use this class as their pretty-printing ecosystem, or use it to derive
(rather than hand-code) the orphan instances for any other pretty-printing
ecosystem; either way, they save significant effort.

## Usage

### For Library Maintainers

Depend on `portray` and optionally `wrapped`; derive instances for your types
with `DerivingVia`, or hand-write them.

```
data These a b = This a | That b | These a b
  deriving Generic
  deriving Portray via Wrapped Generic (These a b)
```

### For Application Authors

Depend on `portray` and a rendering backend, e.g. `portray-pretty`.  From here,
you have two main options: either use `Portray` as the primary carrier of
pretty-printing functionality by using `showPortrayal` and `pp`, or use
`Portray` to derive instances for your pretty-printing class of choice.

To do the former:

```
import Data.Portray.Pretty (pp)
...

data MyType = MyType { _mtInt :: Int, _mtBool :: Bool }
  deriving Generic
  deriving Portray via Wrapped Generic MyType

main = do
  ...
  pp (MyType 2 True)
  -- outputs "MyType { _mtInt = 2, _mtBool = True }"
  ...
```

To do the latter:

```
import Data.Portray.Pretty (WrappedPortray(..))
import Text.PrettyPrint.HughesPJClass (prettyShow)
...

data MyType = MyType { _mtInt :: Int, _mtBool :: Bool }
  deriving Generic
  deriving Portray via Wrapped Generic MyType
  deriving Pretty via WrappedPortray MyType

main = do
  ...
  putStrLn $ prettyShow (MyType 2 True)
  -- outputs "MyType { _mtInt = 2, _mtBool = True }"
  ...
```
