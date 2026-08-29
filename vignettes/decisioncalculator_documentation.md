# Medical Decision Calculator - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `decisioncalculator`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `TP` | UI Control `TP` | `self$options$TP` | Output item / Table |
| `TN` | UI Control `TN` | `self$options$TN` | Output item / Table |
| `FP` | UI Control `FP` | `self$options$FP` | Output item / Table |
| `FN` | UI Control `FN` | `self$options$FN` | Output item / Table |
| `pp` | UI Control `pp` | `self$options$pp` | Output item / Table |
| `pprob` | UI Control `pprob` | `self$options$pprob` | Output item / Table |
| `fnote` | UI Control `fnote` | `self$options$fnote` | Output item / Table |
| `ci` | UI Control `ci` | `self$options$ci` | Output item / Table |
| `fagan` | UI Control `fagan` | `self$options$fagan` | Output item / Table |
| `showWelcome` | UI Control `showWelcome` | `self$options$showWelcome` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showAbout` | UI Control `showAbout` | `self$options$showAbout` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |
| `multiplecuts` | UI Control `multiplecuts` | `self$options$multiplecuts` | Output item / Table |
| `cutoff1` | UI Control `cutoff1` | `self$options$cutoff1` | Output item / Table |
| `tp1` | UI Control `tp1` | `self$options$tp1` | Output item / Table |
| `fp1` | UI Control `fp1` | `self$options$fp1` | Output item / Table |
| `tn1` | UI Control `tn1` | `self$options$tn1` | Output item / Table |
| `fn1` | UI Control `fn1` | `self$options$fn1` | Output item / Table |
| `cutoff2` | UI Control `cutoff2` | `self$options$cutoff2` | Output item / Table |
| `tp2` | UI Control `tp2` | `self$options$tp2` | Output item / Table |
| `fp2` | UI Control `fp2` | `self$options$fp2` | Output item / Table |
| `tn2` | UI Control `tn2` | `self$options$tn2` | Output item / Table |
| `fn2` | UI Control `fn2` | `self$options$fn2` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/decisioncalculator.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

