# Venn Diagram - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `venn`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `var1` | UI Control `var1` | `self$options$var1` | Output item / Table |
| `var1true` | UI Control `var1true` | `self$options$var1true` | Output item / Table |
| `var2` | UI Control `var2` | `self$options$var2` | Output item / Table |
| `var2true` | UI Control `var2true` | `self$options$var2true` | Output item / Table |
| `var3` | UI Control `var3` | `self$options$var3` | Output item / Table |
| `var3true` | UI Control `var3true` | `self$options$var3true` | Output item / Table |
| `var4` | UI Control `var4` | `self$options$var4` | Output item / Table |
| `var4true` | UI Control `var4true` | `self$options$var4true` | Output item / Table |
| `var5` | UI Control `var5` | `self$options$var5` | Output item / Table |
| `var5true` | UI Control `var5true` | `self$options$var5true` | Output item / Table |
| `var6` | UI Control `var6` | `self$options$var6` | Output item / Table |
| `var6true` | UI Control `var6true` | `self$options$var6true` | Output item / Table |
| `var7` | UI Control `var7` | `self$options$var7` | Output item / Table |
| `var7true` | UI Control `var7true` | `self$options$var7true` | Output item / Table |
| `show_upsetR` | UI Control `show_upsetR` | `self$options$show_upsetR` | Output item / Table |
| `show_complexUpset` | UI Control `show_complexUpset` | `self$options$show_complexUpset` | Output item / Table |
| `show_ggvenn` | UI Control `show_ggvenn` | `self$options$show_ggvenn` | Output item / Table |
| `show_ggVennDiagram` | UI Control `show_ggVennDiagram` | `self$options$show_ggVennDiagram` | Output item / Table |
| `sortBy` | UI Control `sortBy` | `self$options$sortBy` | Output item / Table |
| `minSize` | UI Control `minSize` | `self$options$minSize` | Output item / Table |
| `showAnnotations` | UI Control `showAnnotations` | `self$options$showAnnotations` | Output item / Table |
| `explanatory` | UI Control `explanatory` | `self$options$explanatory` | Output item / Table |
| `aboutAnalysis` | UI Control `aboutAnalysis` | `self$options$aboutAnalysis` | Output item / Table |
| `clinicalSummary` | UI Control `clinicalSummary` | `self$options$clinicalSummary` | Output item / Table |
| `reportSentences` | UI Control `reportSentences` | `self$options$reportSentences` | Output item / Table |
| `assumptions` | UI Control `assumptions` | `self$options$assumptions` | Output item / Table |
| `shapeType` | UI Control `shapeType` | `self$options$shapeType` | Output item / Table |
| `regionLabels` | UI Control `regionLabels` | `self$options$regionLabels` | Output item / Table |
| `labelGeometry` | UI Control `labelGeometry` | `self$options$labelGeometry` | Output item / Table |
| `labelPrecisionDigits` | UI Control `labelPrecisionDigits` | `self$options$labelPrecisionDigits` | Output item / Table |
| `setNameSize` | UI Control `setNameSize` | `self$options$setNameSize` | Output item / Table |
| `labelSize` | UI Control `labelSize` | `self$options$labelSize` | Output item / Table |
| `edgeSize` | UI Control `edgeSize` | `self$options$edgeSize` | Output item / Table |
| `edgeColor` | UI Control `edgeColor` | `self$options$edgeColor` | Output item / Table |
| `edgeLineType` | UI Control `edgeLineType` | `self$options$edgeLineType` | Output item / Table |
| `edgeAlpha` | UI Control `edgeAlpha` | `self$options$edgeAlpha` | Output item / Table |
| `fillAlpha` | UI Control `fillAlpha` | `self$options$fillAlpha` | Output item / Table |
| `showSetLabels` | UI Control `showSetLabels` | `self$options$showSetLabels` | Output item / Table |
| `setLabelColor` | UI Control `setLabelColor` | `self$options$setLabelColor` | Output item / Table |
| `fillColorMapping` | UI Control `fillColorMapping` | `self$options$fillColorMapping` | Output item / Table |
| `colorPalette` | UI Control `colorPalette` | `self$options$colorPalette` | Output item / Table |
| `showSetCalculations` | UI Control `showSetCalculations` | `self$options$showSetCalculations` | Output item / Table |
| `calculateOverlap` | UI Control `calculateOverlap` | `self$options$calculateOverlap` | Output item / Table |
| `calculateDiscern` | UI Control `calculateDiscern` | `self$options$calculateDiscern` | Output item / Table |
| `calculateUnite` | UI Control `calculateUnite` | `self$options$calculateUnite` | Output item / Table |
| `showMembershipTable` | UI Control `showMembershipTable` | `self$options$showMembershipTable` | Output item / Table |
| `membershipGroups` | UI Control `membershipGroups` | `self$options$membershipGroups` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/venn.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

