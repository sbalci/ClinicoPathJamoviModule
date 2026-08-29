# Arc Diagram - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjarcdiagram`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `source` | UI Control `source` | `self$options$source` | Output item / Table |
| `target` | UI Control `target` | `self$options$target` | Output item / Table |
| `weight` | UI Control `weight` | `self$options$weight` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `analysisPreset` | UI Control `analysisPreset` | `self$options$analysisPreset` | Output item / Table |
| `showNodes` | UI Control `showNodes` | `self$options$showNodes` | Output item / Table |
| `nodeSize` | UI Control `nodeSize` | `self$options$nodeSize` | Output item / Table |
| `nodeSizeValue` | UI Control `nodeSizeValue` | `self$options$nodeSizeValue` | Output item / Table |
| `sortNodes` | UI Control `sortNodes` | `self$options$sortNodes` | Output item / Table |
| `sortDecreasing` | UI Control `sortDecreasing` | `self$options$sortDecreasing` | Output item / Table |
| `horizontal` | UI Control `horizontal` | `self$options$horizontal` | Output item / Table |
| `arcWidth` | UI Control `arcWidth` | `self$options$arcWidth` | Output item / Table |
| `arcWidthValue` | UI Control `arcWidthValue` | `self$options$arcWidthValue` | Output item / Table |
| `arcTransparency` | UI Control `arcTransparency` | `self$options$arcTransparency` | Output item / Table |
| `directed` | UI Control `directed` | `self$options$directed` | Output item / Table |
| `aggregateEdges` | UI Control `aggregateEdges` | `self$options$aggregateEdges` | Output item / Table |
| `weightMode` | UI Control `weightMode` | `self$options$weightMode` | Output item / Table |
| `arcColorMode` | UI Control `arcColorMode` | `self$options$arcColorMode` | Output item / Table |
| `colorByGroup` | UI Control `colorByGroup` | `self$options$colorByGroup` | Output item / Table |
| `showStats` | UI Control `showStats` | `self$options$showStats` | Output item / Table |
| `showLegend` | UI Control `showLegend` | `self$options$showLegend` | Output item / Table |
| `labelSize` | UI Control `labelSize` | `self$options$labelSize` | Output item / Table |
| `plotTitle` | UI Control `plotTitle` | `self$options$plotTitle` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showAssumptions` | UI Control `showAssumptions` | `self$options$showAssumptions` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjarcdiagram.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

