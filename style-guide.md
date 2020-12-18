# Haskell Style Guide

> Forked from the style guide used in Kowainik:
> https://github.com/kowainik/org/blob/main/style-guide.md

This document is a collection of best-practices inspired by commercial and free open source Haskell libraries and applications.

## Style guide goals

The purpose of this document is to help developers and people working on
Haskell code-bases to have a smoother experience while dealing with code in different
situations. This style guide aims to increase productivity by defining the
following goals:

1. Make code **easier to understand:**  ideas for solutions should not be hidden
   behind complex and obscure code.
2. Make code **easier to read:** code arrangement should be immediately apparent
   after looking at the existing code. Names of functions & variables should be transparent and obvious.
3. Make code **easier to write:** developers should think about code formatting
   rules as little as possible.
4. Make code **easier to maintain:** this style guide aims to reduce the burden
   of maintaining packages using version control systems unless this conflicts
   with the previous points.

## Rule of thumb when working with existing source code

The general rule is to stick to the same coding style that is already used in the
file you are editing. If you must make significant style modifications, then commit them
independently from the functional changes so that someone looking back through the
changelog can easily distinguish between them.

## Indentation

Indent code blocks with _2 spaces_.

Indent `where` keywords with _2 spaces_ and always put a `where` keyword on a new line.

```haskell
showDouble :: Double -> String
showDouble n
  | isNaN n      = "NaN"
  | isInfinite n = "Infinity"
  | otherwise    = show n

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ greeting name
  where
    greeting :: Text -> Text
    greeting name = "Hey " <> name <> "!"
```

## Line length

The maximum allowed line length is _80 characters_. If your line of code exceeds
this limit, try to split code into smaller chunks or break long lines over
multiple shorter ones.

## Whitespaces

**No trailing whitespaces** (use some tools to automatically cleanup trailing
whitespaces).

Surround binary operators with a single space on either side.

## Alignment

Use _comma-leading_ style for formatting module exports, lists, tuples, records, etc.

```haskell
answers :: [Maybe Int]
answers =
  [ Just 42
  , Just 7
  , Nothing
  ]
```

If a function definition doesn't fit the line limit then align multiple lines
according to the same separator like `::`, `=>`, `->`.

```haskell
printQuestion
  :: Show a
  => Text  -- ^ Question text
  -> [a]   -- ^ List of available answers
  -> IO ()
```

Align records with every field on a separate line with leading commas.

```haskell
data Foo = Foo
  { fooBar  :: Bar
  , fooBaz  :: Baz
  , fooQuux :: Quux
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

Align sum types with every constructor on its own line with leading `=` and `|`.

```haskell
data TrafficLight
  = Red
  | Yellow
  | Green
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Ix)
```

> **The indentation of a line should not depend on the length of any identifier in preceding lines.**

Try to follow the above rule inside function definitions but without fanatism:

```haskell
-- + Good
createFoo = Foo
  <$> veryLongBar
  <*> veryLongBaz

-- - Bad
createFoo = Foo <$> veryLongBar
                <*> veryLongBaz

-- - Meh
createFoo =
  Foo  -- there's no need to put the constructor on a separate line and have an extra line
  <$> veryLongBar
  <*> veryLongBaz
```

Basically, it is often possible to join consequent lines without introducing
alignment dependency. Try not to span multiple short lines unnecessarily.

If a function application must spawn multiple lines to fit within the maximum line
length, then write one argument on each line following the head, indented by one
level:

```haskell
veryLongProductionName
  firstArgumentOfThisFunction
  secondArgumentOfThisFunction
  (DummyDatatype withDummyField1 andDummyField2)
  lastArgumentOfThisFunction
```

## Naming

### Functions and variables

+ **lowerCamelCase** for function and variable names.
+ **UpperCamelCase** for data types, typeclasses and constructors.

Try not to create new operators.

```haskell
-- What does this 'mouse operator' mean? 🤔
(~@@^>) :: Functor f => (a -> b) -> (a -> c -> d) -> (b -> f c) -> a -> f d
```

Do not use ultra-short or indescriptive names like `a`, `par`, `g` unless the types of these variables are general enough.

```haskell
-- + Good
mapSelect :: forall a . (a -> Bool) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapSelect test ifTrue ifFalse = go
  where
    go :: [a] -> [a]
    go [] = []
    go (x:xs) = if test x
        then ifTrue  x : go xs
        else ifFalse x : go xs

-- - Bad
mapSelect :: forall a . (a -> Bool) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapSelect p f g = go
  where
    go :: [a] -> [a]
    go [] = []
    go (x:xs) = if p x
        then f x : go xs
        else g x : go xs
```

Do not introduce unnecessarily long names for variables.

```haskell
-- + Good
map :: (a -> b) -> [a] -> [b]
map f = \case
  [] ->
    []
  x : xs ->
    f x : map f xs

-- - Bad
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function (firstElement:remainingList) =
    function firstElement : map function remainingList
```

For readability reasons, do not capitalize all letters when using an
abbreviation as a part of a longer name. For example, write `TomlException` instead
of `TOMLException`.

Unicode symbols are allowed only in modules that already use unicode symbols. If
you create a unicode name, you should also create a non-unicode one as an alias.

### Data types

Creating data types is extremely easy in Haskell. It is usually a good idea to
introduce a custom data type (enum or `newtype`) instead of using a commonly used
data type (like `Int`, `String`, `Set Text`, etc.).

`type` aliases are allowed only for specializing general types:

```haskell
-- + Good
data StateT s m a
type State s = StateT s Identity

-- - Bad
type Size = Int
```

Use the data type name as the constructor name for `data` with single
constructor and `newtype`.

```haskell
data User = User
  { userId   :: Int
  , userName :: String
  }
```

The field name for a `newtype` must be prefixed by `un` followed by the type name.

```haskell
newtype Size = Size
  { unSize :: Int
  }

newtype App a = App
  { unApp :: ReaderT Context IO a
  }
```

Field names for the record data type should start with the full name of the data type.

```haskell
-- + Good
data HealthReading = HealthReading
  { healthReadingDate        :: UTCTime
  , healthReadingMeasurement :: Double
  }
```

It is acceptable to use an abbreviation as the field prefix if the data type name is
too long.

```haskell
-- + Acceptable
data HealthReading = HealthReading
  { hrDate        :: UTCTime
  , hrMeasurement :: Double
  }
```

## Comments

Separate end-of-line comments from the code with _2 spaces_.

```haskell
newtype Measure = Measure
  { unMeasure :: Double  -- ^ See how 2 spaces separate this comment
  }
```

Write [Haddock documentation](https://github.com/aisamanra/haddock-cheatsheet/blob/master/haddocks.pdf)
for the top-level functions, function arguments
and data type fields. The documentation should give enough
information to apply the function without looking at its definition.

Use block comment style (`{- |` and `-}`) for Haddock in multiple line comments.

```haskell
-- + Good
{- | Example of multi-line block comment which is very long
and doesn't fit single line.
-}
foo :: Int -> [a] -> [a]

-- + Also good
-- | Single-line short comment.
foo :: Int -> [a] -> [a]

-- ~ Bad
-- | Example of multi-line block comment which is very long
-- and doesn't fit single line.
foo :: Int -> [a] -> [a]
```

For commenting function arguments, data type constructors and their fields,
you are allowed to use end-of-line Haddock comments if they fit line length
limit. Otherwise, use block style comments. It is _allowed_ to align end-of-line
comments with each other. But it is _forbidden_ to use comments of different
types (pre or post) for the function arguments, data type constructors, and fields.

```haskell
-- + Good
{- | 'replicate' @n x@ returns list of length @n@ with @x@ as the value of
every element. This function is lazy in its returned value.
-}
replicate
  :: Int  -- ^ Length of returned list
  -> a    -- ^ Element to populate list
  -> [a]

-- - Bad
{- | 'replicate' @n x@ returns list of length @n@ with @x@ as the value of
every element. This function is lazy in its returned value.
-}
replicate
  :: Int  -- ^ Length of returned list
  {- | Element to populate list -}
  -> a
  -> [a]
```

If possible, include typeclass laws and function usage examples into the
documentation.

```haskell
{- | The class of semigroups (types with an associative binary operation).

Instances should satisfy the associativity law:

* @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
-}
class Semigroup a where
  (<>) :: a -> a -> a


{- | The 'intersperse' function takes a character and places it
between the characters of a 'Text'.

>>> Text.intersperse '.' "SHIELD"
"S.H.I.E.L.D"
-}
intersperse :: Char -> Text -> Text
```

## Guideline for module formatting

Use these tools for automatic module formatting:

* [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)
  (with a relevant [`.stylish-haskell.yaml`](https://github.com/kowainik/org/blob/main/.stylish-haskell.yaml)):
  for formatting the import section and for alignment.

### {-# LANGUAGE #-}

Put `OPTIONS_GHC` pragma before `LANGUAGE` pragmas in a separate section.

Write each `LANGUAGE` pragma on its own line and sort them alphabetically.
Do not align finishing braces.

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
```

### Default extensions

Put away commonly-used language extensions into `default-extensions` in the package file.

Until there's some kind of `Haskell2220` there are some that we expect to be available when
writing new code for GHC >= 8.10:

```yaml
default-extensions:
- ApplicativeDo
- BlockArguments
- ConstraintKinds
- DataKinds
- DefaultSignatures
- DeriveDataTypeable
- DeriveGeneric
- DeriveGeneric
- DeriveTraversable
- DerivingStrategies
- DuplicateRecordFields
- FlexibleContexts
- FlexibleInstances
- FunctionalDependencies
- GeneralizedNewtypeDeriving
- ImportQualifiedPost
- InstanceSigs
- LambdaCase
- LiberalTypeSynonyms
- MultiParamTypeClasses
- NamedFieldPuns
- NumericUnderscores
- OverloadedStrings
- PatternSynonyms
- QuasiQuotes
- RankNTypes
- RecordWildCards
- ScopedTypeVariables
- StandaloneDeriving
- StandaloneKindSignatures
- StrictData
- TupleSections
- TypeApplications
- TypeFamilies
- TypeOperators
- ViewPatterns
```

Other extensions, like `TemplateHaskell` and `CPP` are to be put in file headers.

### Export lists

Use the following rules to format the export section:

1. **Always write** an explicit export list.
2. Indent the export list by _2 spaces_.
3. You can split the export list into sections. Use Haddock to assign names to
   these sections.
4. Classes, data types and type aliases should be written before functions in
   each section.

```haskell
module Data.ExtraFast.Map
  ( -- * Data type
    Map
  , Key
  , empty

    -- * Update
  , insert
  , insertWith
  , alter
  ) where
```

### Imports

Always use **explicit import lists** or **qualified imports**.

> __Exception:__ modules that only reexport other entire modules.

Use your judgement to choose between explicit import lists or
qualified imports. However, qualified imports are recommended in the
following situations:

* Name conflicts
* Long export lists
* A library is designed for qualified imports, e.g. `tomland`

This import policy makes the code more maintainable and robust against
changes in dependent libraries.

Use postpositive qualified imports.
Put `qualified` signifier before `as`, not after `import`.

Choose the reasonable names for `qualified` imports:

```haskell
-- + Good
import Data.Text qualified as Text
import Data.ByteString.Lazy qualified as BSL
import Toml qualified

-- - Bad
import GitHub qualified as G
import App.Server qualified as Srv
import App.Service qualified as Svc
```

Imports should be grouped in the following order:

1. Prelude, if not implicit
2. 3rd party packages (inlcuding `base`).
3. Project-packages (like `nk-extra`).
4. Package modules

Put a blank line between each group of imports.

The imports in each group should be sorted alphabetically by module name.

```haskell
module MyProject.Foo
  ( Foo (..)
  ) where

import Control.Exception (catch, try)
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)

import MyProject.Ansi (errorMessage, infoMessage)
import MyProject.BigModule qualified as Big

data Foo
...
```

### Data declaration

Refer to the [Alignment section](#alignment) to see how to format data type
declarations.

Records for data types with multiple constructors are forbidden.

```haskell
-- - Bad
data Foo
  = Bar { bar1 :: Int, bar2 :: Double }
  | Baz { baz1 :: Int, baz2 :: Double, baz3 :: Text }

-- + Good
data Foo
  = FooBar Bar
  | FooBaz Baz

data Bar = Bar
  { bar1 :: Int
  , bar2 :: Double
  }

data Baz = Baz
  { baz1 :: Int
  , baz2 :: Double
  , baz3 :: Text
  }

-- + Also acceptable
data Foo
  = Bar Int Double
  | Baz Int Double Text
```

### Strictness

With package-enabled [StrictData] constructorss have their fields strict by default.

This helps to avoid space leaks and gives you an error instead of a
warning in case you forget to initialize some fields.

```haskell
-- + Good (and have StrictData enabled)
data Settings = Settings
  { settingsHasTravis  :: Bool
  , settingsConfigPath :: FilePath
  , settingsRetryCount :: Int
  }

-- - Bad (unless StrictData is enabled)
data Settings = Settings
  { settingsHasTravis  :: Bool
  , settingsConfigPath :: FilePath
  , settingsRetryCount :: Int
  }
```

[StrictData]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-data-types

### Deriving

Always specify a deriving strategy for each deriving clause.
Use [DerivingStrategies](https://kowainik.github.io/posts/deriving)
to explicitly specify the way you want to derive type classes.

Type classes in the deriving section should always be surrounded by parentheses.

Derive `Show` and `Eq` instances for all introduced data types where possible.

Derive `Ord`, `Bounded`, `Enum` for enumeration types without constructor fields.

For `newtype`s prefer to use __newtype__ strategy of deriving.

```haskell
newtype Id a = Id
  { unId :: Int
  }
  deriving stock    (Show, Generic)
  deriving newtype  (Eq, Ord, Hashable)
  deriving anyclass (FromJSON, ToJSON)
```

### Function declaration

All top-level functions _must_ have type signatures.

Surround `.` after `forall` in type signatures with spaces.

```haskell
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
```

If the function type signature is very long, then place the type of each argument under
its own line with respect to alignment.

```haskell
sendEmail
  :: forall env m
  .  ( MonadLog m
      , MonadEmail m
      , WithDb env m
      )
  => Email
  -> Subject
  -> Body
  -> Template
  -> m ()
```

If the line with argument names is too big, then it's a code smell that a function is too complex.

```haskell
-- - Bad
sendEmail
  toEmail
  subject@(Subject subj)
  body
  Template{..}
  = do
  <code goes here>
```

In other cases, place an `=` sign on the same line where the function definition is.

Put operator fixity before operator signature:

```haskell
-- | Flipped version of '<$>'.
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
```

Put pragmas preceding the function they apply to.

```haskell
-- | Lifted version of 'Text.putStrLn'.
{-# INLINE putTextLn #-}
{-# SPECIALIZE putTextLn :: Text -> IO () #-}
putTextLn :: MonadIO io => Text -> io ()
putTextLn = liftIO . Text.putStrLn
```

In case of data type definitions, you must put the pragma before
the type it applies to. Example:

```haskell
data TypeRepMap (f :: k -> Type) = TypeRepMap
  { fingerprintAs :: {-# UNPACK #-} (PrimArray Word64)
  , fingerprintBs :: {-# UNPACK #-} (PrimArray Word64)
  , trAnys        :: {-# UNPACK #-} (Array Any)
  , trKeys        :: {-# UNPACK #-} (Array Any)
  }
```

### if-then-else clauses

Use python-style ifsss:

```haskell
digitOrNumber :: Int -> Text
digitOrNumber i =
  if i >= 0 && i < 10 then
    "This is a digit"
  else
    "This is a number"
```

Do the same inside `do` blocks.
Leave the `do` keyword on the line before the block:

```haskell
choose
  :: Text          -- ^ Question text.
  -> NonEmpty Text -- ^ List of available options.
  -> IO Text       -- ^ The chosen option.
choose question choices = do
  printQuestion question choices
  answer <- prompt
  if null answer then do
    putStrLn "No answer, using default choice"
    pure (head choices)
  else
    pure answer
```

Avoid if-oneliners as they hide the branches in the syntax noise:

```haskell
-- - Bad
shiftInts :: [Int] -> [Int]
shiftInts = map \n -> if even n then n + 1 else n - 1

-- + Good
shiftInts :: [Int] -> [Int]
shiftInts =
  map \n ->
    if even n then
      -- XXX: you can even put some comments in there!
      n + 1
    else
      n - 1
```

### Case expressions

Align the `->` arrows in the alternatives when the expression is a tabular form.

```haskell
-- + Good
firstOrDefault :: [a] -> a -> a
firstOrDefault list def =
  case list of
    []    -> def
    x : _ -> x

-- - Bad
foo :: IO ()
foo = getArgs >>= \case
  []                          -> do
    putStrLn "No arguments provided"
    runWithNoArgs
  firstArg : secondArg : rest -> do
    putStrLn $ "The first argument is " <> firstArg
    putStrLn $ "The second argument is " <> secondArg
  _                           -> pure ()
```

Use the `LambdaCase` extension when you perform pattern matching over the last
argument of the function:

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe v = \case
  Nothing -> v
  Just x  -> x
```

### let expressions

In pure functions try to avoid `let`. Instead, use `where`.

```haskell
-- - Bad
isLimitedBy :: Integer -> Natural -> Bool
isLimitedBy n limit =
  let intLimit = toInteger limit
  in n <= intLimit

-- - Better
isLimitedBy :: Integer -> Natural -> Bool
isLimitedBy n limit =
  let
    intLimit = toInteger limit
  in
    n <= intLimit

-- + Good
isLimitedBy :: Integer -> Natural -> Bool
isLimitedBy n limit = n <= intLimit
  where
    intLimit = toInteger limit
```

In a `do` block group let-bindings if they're related.
For a series of short definitions a tabular form is allowed.
Separate groups with empty line.

Put away pure expressions (those not depending on procedure results) under a `where`.

```haskell
mainIsh = do
  (ctx, thingy) <- getSomeThing initial

  let
    foo  = getFoo thingy
    baz  = getBaz thingy
    quux = getQuux thingy

  let
    next = case pickNext ctx of
      Nothing -> mempty
      Just  r -> r

  proceed next foo baz quux
  where
    initial = SomeConfig True
```

## General recommendations

Try to split code into separate modules to show data dependencies.
Don't go full one-module-per-type though.

Avoid abusing point-free style. Sometimes code is clearer when **not** written
in point-free style:

```haskell
-- + Good
foo :: Int -> a -> Int
foo n x = length $ replicate n x

-- - Bad
foo :: Int -> a -> Int
foo = (length . ) . replicate
```

Don't use `return`, as we're approaching `Monad Of No Return` point in GHC evolution.

Use `-XApplicativeDo` in combination with `-XRecordWildCards` to prevent
position-sensitive errors where possible.

Avoid using operators where a function would suffice.

Avoid `<$>` in general. It could look good with `<*>`, but with `ApplicativeDo` it's time is over.

Avoid partial functions like `Prelude.head`. Use data types that provide total alternatives.

Avoid functions that silently discard data, state your assumptions explicitly.

```haskell
-- + Good
foo :: Int -> a -> Int
foo n x =
  Map.fromListWith (assert "source elements are distinct") $
    zip [0..n] (repeat x)

-- + Acceptable
foo :: Ord a => Set a -> Int
foo xs =
  Map.fromList $
    zip (Set.toList xs) (repeat 0)

-- - Bad
foo :: Ord a => [a] -> Int
foo xs =
  Map.fromList $
    -- FIXME: there's no such guarantee in the type
    zip xs (repeat 0)
```

On a related note, avoid the `void`. It has some merit in one-liners, but in big blocks
you have the luxury of having named discards.

```haskell
-- + Good
something todo = do
  _taskStatus <- getThings todo finish
  somethingElse 42

-- + Acceptable
something_ todo = void $ getThings todo finish

-- - Bad
something todo = do
  void $ getThings todo finish -- What's going on? 🤔
  somethingElse 42
```

Avoid left-facing effectful pipelines (`=<<` etc.).
Pure code flows right-to-left with `(.)`, effects flow left-to-right with `>>=`.

```haskell
-- + Good
something todo = do
  x <- getThings todo $
    finish >=> collect
  somethingElse x 42

-- - Bad
something todo = do
  x <- getThings todo $ collect <=< finish
  somethingElse x 42
```

Dangling arrows in `do` blocks are a code smell, time to extract the code.

Don't use `String`. If you really need a stack of unicode characters, then write it as it is - `[Char]`.
It's okay to use string literals though, if the type matches.

Use strict `Text` for, well, textual data.
Use `ByteString` for IO.
Convert between the two explicitly.
Don't use `printf`.

Prefer `traverse`/`traverse_` over `mapM`/`mapM_`. Use `for`/`for_` sparingly.
With a good prelude they are readily availablable.

Use a good prelude. Currently - RIO.

Run a full `stack build --test --pedantic` before submitting your code.

## GHC options

Code should be compilable with the following ghc options without warnings:

* `-Wall`
* `-Wcompat`
* `-Widentities`
* `-Wincomplete-uni-patterns`
* `-Wincomplete-record-updates`
* `-Wredundant-constraints`
* `-Wmissing-export-lists`
* `-Wpartial-fields`
* `-Wmissing-deriving-strategies`
* `-Wunused-packages`

Enable `-fhide-source-paths` and `-freverse-errors` for cleaner compiler output.
