# Unreleased

* Add support for GHC 9.6

# v0.4.0

* Drop support for GHC 8.10
* Added support for where clauses in `test_prop`, where the where clause may reference the generated arguments in `test_prop`
* Don't autocollect invalid module names
* Enable importing configuration from other files

# v0.3.2.0

* Added support for GHC 9.4

# v0.3.1.0

* Fix bug where errors in Main module would show the path of the temporary file instead of the original file
* Ignore binary files in test directory

# v0.3.0.0

* Fix bug when omitting signature after specifying a signature for a prior test
* Add support for `_expectFail`, `_expectFailBecause`, `_ignoreTest`, `_ignoreTestBecause` suffixes

# v0.2.0.0

* Fix build for GHC 8.10.2
* Greatly simplify framework by replacing `test_testCase` with just `test = testCase ...`

# v0.1.0.0

Initial release
