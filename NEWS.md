# zutils (development version)

* Ensure `NULL` can be feeded into `call_full()`.

# zutils 0.0.9

## New Features

* Added `...` to `call_full()` to pass additional arguments to the function.

# zutils 0.0.8

## New Features

* Added `cautiouly()`, a warning version of `possibly()`.

# zutils 0.0.7

## New Features

* Support drop components by specifiying `NA` in `names` argument of `separate_wider_dsv()`.

# zutils 0.0.6

## Bug fixes

* Fixed a bug introduced in 0.0.4 that `separate_wider_dsv()` does not work when `patterns` is specified.

# zutils 0.0.5

## Breaking changes

* Removed `bind_rows_meta()` and added `separate_wider_dsv()` which solves a similar problem but in a more general way.

# zutils 0.0.4

## Bug fixes

* Fixed a bug in `call_fun()` which is introduced in 0.0.3.

# zutils 0.0.3

## Breaking changes

* Removed `.fun_pre` and `.fun_post` arguments from `bind_rows_meta()`. These two arguments are not expected to be used in this function.

# zutils 0.0.2

# zutils 0.0.1

* First github release.
