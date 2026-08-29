# Variable Tree - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `vartree`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `percvar` | UI Control `percvar` | `self$options$percvar` | Output item / Table |
| `percvarLevel` | UI Control `percvarLevel` | `self$options$percvarLevel` | Output item / Table |
| `summaryvar` | UI Control `summaryvar` | `self$options$summaryvar` | Output item / Table |
| `summarylocation` | UI Control `summarylocation` | `self$options$summarylocation` | Output item / Table |
| `style` | UI Control `style` | `self$options$style` | Output item / Table |
| `prunebelow` | UI Control `prunebelow` | `self$options$prunebelow` | Output item / Table |
| `pruneLevel1` | UI Control `pruneLevel1` | `self$options$pruneLevel1` | Output item / Table |
| `pruneLevel2` | UI Control `pruneLevel2` | `self$options$pruneLevel2` | Output item / Table |
| `follow` | UI Control `follow` | `self$options$follow` | Output item / Table |
| `followLevel1` | UI Control `followLevel1` | `self$options$followLevel1` | Output item / Table |
| `followLevel2` | UI Control `followLevel2` | `self$options$followLevel2` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `vp` | UI Control `vp` | `self$options$vp` | Output item / Table |
| `horizontal` | UI Control `horizontal` | `self$options$horizontal` | Output item / Table |
| `sline` | UI Control `sline` | `self$options$sline` | Output item / Table |
| `varnames` | UI Control `varnames` | `self$options$varnames` | Output item / Table |
| `nodelabel` | UI Control `nodelabel` | `self$options$nodelabel` | Output item / Table |
| `pct` | UI Control `pct` | `self$options$pct` | Output item / Table |
| `showcount` | UI Control `showcount` | `self$options$showcount` | Output item / Table |
| `legend` | UI Control `legend` | `self$options$legend` | Output item / Table |
| `pattern` | UI Control `pattern` | `self$options$pattern` | Output item / Table |
| `sequence` | UI Control `sequence` | `self$options$sequence` | Output item / Table |
| `ptable` | UI Control `ptable` | `self$options$ptable` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `useprunesmaller` | UI Control `useprunesmaller` | `self$options$useprunesmaller` | Output item / Table |
| `prunesmaller` | UI Control `prunesmaller` | `self$options$prunesmaller` | Output item / Table |
| `showInterpretation` | UI Control `showInterpretation` | `self$options$showInterpretation` | Output item / Table |
| `maxwidth` | UI Control `maxwidth` | `self$options$maxwidth` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/vartree.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

