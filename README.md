# tasty-autocollect

TODO

Design goals:
* Don't use any weird syntax so that syntax highlighters, linters, and formatters still work
* Support test functions with multiple arguments like `tasty-golden`'s API (which `tasty-discover` doesn't easily support)
* Avoid universally exporting the whole test module, so that GHC can warn about unused test helpers
* Don't add any of the tasty plugins as explicit dependencies

## Usage

TODO
