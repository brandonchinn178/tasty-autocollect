# tasty-autocollect

[![](https://img.shields.io/github/actions/workflow/status/brandonchinn178/tasty-autocollect/ci.yml?branch=main)](https://github.com/brandonchinn178/tasty-autocollect/actions)
[![](https://img.shields.io/codecov/c/gh/brandonchinn178/tasty-autocollect)](https://app.codecov.io/gh/brandonchinn178/tasty-autocollect)
[![](https://img.shields.io/hackage/v/tasty-autocollect)](https://hackage.haskell.org/package/tasty-autocollect)

A preprocessor/compiler plugin that will automatically collect Tasty tests and generate a main file to run all the tests.

Design goals:
* Don't use any weird syntax so that syntax highlighters, linters, and formatters still work
* Avoid universally exporting the whole test module, so that GHC can warn about unused test helpers
* Support arbitrary test functions (e.g. user-defined test helpers or third-party tasty libraries)
* Minimal dependencies
  * Only uses boot libraries, with two exceptions: `tasty` + `tasty-expected-failure`

## Quickstart

1. Add the following to your `package.yaml` or `.cabal` file:

    ```yaml
    tests:
      my-library-tests:
        ghc-options: -F -pgmF=tasty-autocollect
        build-tools:
          - tasty-autocollect:tasty-autocollect
        ...
        dependencies:
          - tasty-autocollect
          - ...
    ```
    ```cabal
    test-suite my-library-tests
      ghc-options: -F -pgmF=tasty-autocollect
      build-tool-depends:
        tasty-autocollect:tasty-autocollect
      ...
      build-depends:
        tasty-autocollect
        ...
    ```

1. Write your main file to contain just:

    ```hs
    {- AUTOCOLLECT.MAIN -}
    ```

1. Write your tests:

    ```hs
    {- AUTOCOLLECT.TEST -}

    module MyTest (
      {- AUTOCOLLECT.TEST.export -}
    ) where

    import Test.Tasty.Golden
    import Test.Tasty.HUnit
    import Test.Tasty.QuickCheck

    test =
      testCase "Addition" $ do
        1 + 1 @?= (2 :: Int)
        2 + 2 @?= (4 :: Int)

    -- See the "Integration with QuickCheck/SmallCheck/etc." section
    -- for a more seamless integration
    test =
      testProperty "reverse . reverse === id" $ \xs ->
        (reverse . reverse) xs === id (xs :: [Int])

    test =
      goldenVsString "Example golden test" "test/golden/example.golden" $
        pure "example"

    test =
      testGroup "manually defining a test group"
        [ testCase "some test" $ pure ()
        , testCase "some other test" $ pure ()
        ]
    ```

## Features

In addition to automatically collecting tests, this library also provides some additional functionality out-of-the-box, to make writing + managing tests a seamless experience.

### Integration with QuickCheck/SmallCheck/etc.

Property test frameworks like QuickCheck or SmallCheck work better when defining the types of arguments instead of using lambdas. So there's a special syntax for defining properties:

```hs
test_prop :: [Int] -> Property
test_prop "reverse . reverse === id" xs = (reverse . reverse) xs === id xs
```

This will be rewritten to the equivalent of:

```hs
test =
  testProperty
    "reverse . reverse === id"
    ( (\xs -> (reverse . reverse) xs === id xs)
        :: [Int] -> Property
    )
```

### Marking tests as "TODO"

If you're of the Test Driven Development (TDD) mentality, you might want to specify what tests you want to write before actually writing any code. In this workflow, you might not even know what kind of test you want to write (e.g. HUnit, QuickCheck, etc.).

With `tasty-autocollect`, you can use `test_todo` to write down tests you'd like to write. By default, they'll pass with a "TODO" message, but you can also pass `--fail-todos` at runtime to make them fail instead.

```hs
test_todo = "a test to implement later"
```

### Defining batches of tests

With `tasty-autocollect`, you can write a set of tests in one definition without needing to nest them within a test group. For example,

```hs
test_batch =
  [ testCase ("test #" ++ show x) $ pure ()
  | x <- [1, 5, 10 :: Int]
  ]
```

is equivalent to writing:

```hs
test = testCase "test #1" $ pure ()

test = testCase "test #5" $ pure ()

test = testCase "test #10" $ pure ()
```

### Integration with `tasty-expected-failures`

If you need to mark a test as an expected failure or just unconditionally skip a test, you can add an appropriate suffix to your test. For example:

```hs
test_expectFail = testCase "this test should fail" $ ...

test_expectFailBecause "Issue #123" = testCase "this test should fail" $ ...

test_ignoreTest = testCase "this test is skipped" $ ...

test_ignoreTestBecause "Issue #123" = testCase "this test is skipped" $ ...
```

The last example will be converted into the equivalent of:

```hs
test =
  ignoreTestBecause "Issue #123" $
    testCase "this test is skipped" $ ...
```

It also works in combination with other test types, e.g. with `test_batch` to skip the entire batch of tests:

```hs
test_batch_expectFail =
  [ testCase ("this test should fail: " ++ show x) ...
  | x <- ...
  ]

test_prop_expectFailBecause :: Int -> Property
test_prop_expectFailBecause "Issue #123" "some property" x = x === x
```

## Configuration

`tasty-autocollect` can be configured by adding `k = v` lines to the same block comment as `AUTOCOLLECT.MAIN`; e.g.

```hs
{- AUTOCOLLECT.MAIN
suite_name = foo

# comments can start with a hash symbol
group_type = flat
-}
```

* `import`: A comma separated list of files (relative to the Main file) containing configuration to import
    * Recommended file extension: `.conf`
    * Configuration in files later in the list override configuration in files earlier in the list
    * Configuration in the Main file override imported configuration

* `suite_name`: The name to use in the `testGroup` at the root of the test suite `TestTree` (defaults to the path of the main file)

* `group_type`: How the tests should be grouped (defaults to `modules`)
    * `flat`: All the tests are in the same namespace
        ```
        Main.hs
          test 1: OK
          test 2: OK
          test 3: OK
        ```

    * `modules`: Tests are grouped by their module
        ```
        Main.hs
          Test.Module1
            test1: OK
            test2: OK
          Test.Module2
            test3: OK
        ```

    * `tree`: Tests are grouped by their module, which is broken out into a tree
        ```
        Main.hs
          Test
            Module1
              test1: OK
              test2: OK
            Module2
              test3: OK
        ```

* `strip_suffix`: The suffix to strip from a test module, e.g. `strip_suffix = Test` will relabel a `Foo.BarTest` module to `Foo.Bar`

* `ingredients`: A comma-separated list of extra tasty ingredients to include, e.g.

    ```
    ingredients = SomeLibrary.ingredient1, SomeLibrary.ingredient2
    ```

* `ingredients_override`: By default, `ingredients` will add the ingredients in front of the default `tasty` ingredients. When `true`, does not automatically include the default `tasty` ingredients, for complete control over the ingredient order.

* `custom_main`: If you'd like fine-grained control over how the `Main` module is generated (e.g. if you're using `NoImplicitPrelude` or custom preludes), set this to `true`. When set, it will do the following replacements:
    * `{- AUTOCOLLECT.MAIN.imports -}`: replaced with the import lines needed for the tests.
    * `{- AUTOCOLLECT.MAIN.tests -}`: replaced with the `[TestTree]` list, all on one line. If you're using a formatter or linter, it might be helpful to do `tests = id {- AUTOCOLLECT.MAIN.tests -}` so that the code still parses.

    Due to current limitations, the above comments need to be matched exactly (e.g. not with `--` comments or extra whitespace).

## Comparison with other libraries

Advantages:
* Avoids hardcoding blessed test frameworks
  * Both `tasty-discover` and `tasty-th` prefix tests with hardcoded prefixes like `unit_` or `prop_`. While they both provide a mechanism to specify arbitrary `TestTree`s with `test_`, it makes for a less seamless integration with unblessed test frameworks, such as `tasty-golden`:
    ```hs
    test_do_something :: TestTree
    test_do_something =
      goldenVsString "do something" "goldens/do_something.golden" $ ...
    ```
  * With `tasty-autocollect`, all tests are specified the same way (modulo some syntax sugar niceties like `test_prop`)
* Avoids rewriting test label twice in function name
  * Both `tasty-discover` and `tasty-th` force you to repeat long test names twice:
    ```hs
    unit_this_is_a_test :: Assertion
    unit_this_is_a_test = do
      ...
    ```
* Avoids test name restrictions
  * Because `tasty-discover` and `tasty-th` couple the function name with the test label, you can't do things like use punctuation in the test label. So these frameworks don't allow writing the equivalent of `testProperty "reverse . reverse === id"`.
* More features out-of-the-box (see "Features" section)
* More configurable
  * More configuration options
  * Configuration is more extensible, since configuration is parsed from a comment in the main module instead of as preprocessor arguments (for `tasty-discover`)
  * `tasty-th` isn't configurable at all
* Automatically generates the `Main.hs` file that discovers + imports all test modules in the directory
  * `tasty-discover` does this, but `tasty-th` does not; `tasty-th` provides `defaultMainGenerator`, but it only discovers tests in the same module, so if you have tests in multiple files, you'd still have to manually import all of them into a `Main.hs` file

Disadvantages:
* Uses both a preprocessor and a GHC plugin
    * `tasty-discover` only uses a preprocessor, while `tasty-th` uses Template Haskell
    * Haven't tested performance yet, but I wouldn't be surprised if compilation takes longer

## Appendix

### Debugging 

To inspect the generated Main module, build with `-keep-tmp-files`, and look in `$TMPDIR` for a `ghc_1.hspp` file. (Upstream tickets: [GHC](https://gitlab.haskell.org/ghc/ghc/-/issues/22258), [Stack](https://github.com/commercialhaskell/stack/issues/5892))

To inspect the converted test modules, build with `-ddump-rn -ddump-to-file` and look for the `.dump-rn` files in `.stack-work/` or `dist-newstyle/`.

### Note for Ormolu/Fourmolu

If you're using Ormolu < 0.7 or Fourmolu < 0.13, use `-- $AUTOCOLLECT.TEST.export$` instead; otherwise, [the comment will be moved out of the export list](https://github.com/tweag/ormolu/issues/906).

This works around the issue by reusing Haddock's named section syntax, but it shouldn't be an issue because you shouldn't be building Haddocks for test modules. If this becomes an issue, upgrade Ormolu or Fourmolu.

### How it works

The `package.yaml`/`.cabal` snippet registers `tasty-autocollect` as a preprocessor, which does one of three things at the very beginning of compilation:

1. If the file contains `{- AUTOCOLLECT.MAIN -}`, find all test modules and generate a main module.
2. If the file contains `{- AUTOCOLLECT.TEST -}`, register the `tasty-autocollect` GHC plugin to rewrite tests (see below).
3. Otherwise, do nothing

In a test file, the plugin will search for any functions named `test`. It will then rename the function to `tasty_test_N`, where `N` is an autoincrementing, unique number. Then it will collect all the tests into a `tasty_tests :: [TestTree]` binding, which is exported at the location of the `{- AUTOCOLLECT.TEST.export -}` comment.
