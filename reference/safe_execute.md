# Safe Execution Wrapper

Safely execute code with enhanced error/warning capture. The argument is
a zero-argument function (a "thunk"), not an unevaluated expression.
Earlier versions accepted an unevaluated expression and ran the runtime
evaluator on it; that contract made the helper a string-to-code pathway
whenever a caller was tempted to feed it a parsed user string. The
current contract removes that risk by requiring callers to commit to a
function value at the call site.

## Usage

``` r
safe_execute(
  fn,
  function_name = "unknown",
  clinical_context = "",
  default_value = NULL
)
```

## Arguments

- fn:

  A zero-argument function whose body is the code to run safely. Pass
  `function() { ... your code ... }`.

- function_name:

  Name of the calling function (for log messages).

- clinical_context:

  Clinical context for the operation (for log messages).

- default_value:

  Value to place in the `result` slot on error.

## Value

A list with `success`, `result`, and either `warnings`/`errors` counters
(on success) or `error_info` (on failure).
