# Package startup message

Returns the package author / website banner. Called by `.onAttach()`
(see `R/zzz.R`) via
[`packageStartupMessage()`](https://rdrr.io/r/base/message.html), which
routes to the message stream and respects
[`suppressPackageStartupMessages()`](https://rdrr.io/r/base/message.html).
Available as an exported function so users can print the banner
explicitly.

## Usage

``` r
clinicopath_startup_message()
```

## Value

Invisible NULL (called for side effects).
