# Initialize ClinicoPath Error Handling System

Initialize error handling for a ClinicoPath function

## Usage

``` r
clinicopath_init(function_name, context = "")
```

## Arguments

- function_name:

  Name of the function being initialized

- context:

  Additional context information

## Value

Invisibly returns `TRUE`; called for its side effect of resetting
counters and pushing the function onto the tracking stack.
