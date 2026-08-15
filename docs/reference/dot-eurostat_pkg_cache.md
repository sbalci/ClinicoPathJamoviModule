# Package-local cache environment

Internal environment used to cache downloaded Eurostat data across runs,
avoiding assignment to `.GlobalEnv`.

## Usage

``` r
.eurostat_pkg_cache
```

## Value

An environment used internally to cache Eurostat data between runs.
