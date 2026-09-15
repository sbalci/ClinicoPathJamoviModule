# Build the context the engine functions read

Build the context the engine functions read

## Usage

``` r
recist_context(
  baselineTimepoint = 0,
  confirmationInterval = 4,
  maxTargetLesions = 5,
  maxLesionsPerOrgan = 2,
  nonTargetResponseVar = NULL,
  targetSelectionVar = NULL,
  notify = NULL
)
```

## Arguments

- baselineTimepoint:

  Visit time treated as baseline.

- confirmationInterval:

  Minimum gap (weeks) before a CR/PR can be confirmed.

- maxTargetLesions, maxLesionsPerOrgan:

  RECIST target-selection limits.

- nonTargetResponseVar:

  Name of the column holding the radiologist's non-target assessment, or
  NULL to fall back to the lesion-count heuristic.

- notify:

  Optional `function(type, title, content)` used to raise notices. When
  NULL the engine stays silent, which is what a non-interactive caller
  wants.
