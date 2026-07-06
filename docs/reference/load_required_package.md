# Check whether a required package is available

Thin wrapper around
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) that returns
a boolean. Packages a jamovi module needs must be listed in
`DESCRIPTION` `Imports:` so they are installed at install time - this
helper does NOT install anything (auto- installing dependencies at
runtime is a CRAN policy violation and a security hazard).

## Usage

``` r
load_required_package(package_name, install_if_missing = FALSE)
```

## Arguments

- package_name:

  Character string with package name.

- install_if_missing:

  Ignored. Retained for backwards compatibility with earlier signatures
  of this function. A warning is issued when set to anything other than
  its default.

## Value

Logical indicating whether the package is available.
