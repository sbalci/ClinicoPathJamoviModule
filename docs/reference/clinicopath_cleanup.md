# Function Cleanup

Clean up function execution context

## Usage

``` r
clinicopath_cleanup(function_name)
```

## Arguments

- function_name:

  Name of the function being cleaned up

## Value

Invisibly returns `TRUE`; called for its side effect of popping the
function from the tracking stack.
