# Swimmer Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `swimmerplot`
- **Module**: `OncoPath`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `patientID` | UI Control `patientID` | `self$options$patientID` | Output item / Table |
| `startTime` | UI Control `startTime` | `self$options$startTime` | Output item / Table |
| `endTime` | UI Control `endTime` | `self$options$endTime` | Output item / Table |
| `responseVar` | UI Control `responseVar` | `self$options$responseVar` | Output item / Table |
| `censorVar` | UI Control `censorVar` | `self$options$censorVar` | Output item / Table |
| `groupVar` | UI Control `groupVar` | `self$options$groupVar` | Output item / Table |
| `timeType` | UI Control `timeType` | `self$options$timeType` | Output item / Table |
| `dateFormat` | UI Control `dateFormat` | `self$options$dateFormat` | Output item / Table |
| `timeUnit` | UI Control `timeUnit` | `self$options$timeUnit` | Output item / Table |
| `timeDisplay` | UI Control `timeDisplay` | `self$options$timeDisplay` | Output item / Table |
| `maxMilestones` | UI Control `maxMilestones` | `self$options$maxMilestones` | Output item / Table |
| `milestone1Name` | UI Control `milestone1Name` | `self$options$milestone1Name` | Output item / Table |
| `milestone1Date` | UI Control `milestone1Date` | `self$options$milestone1Date` | Output item / Table |
| `milestone2Name` | UI Control `milestone2Name` | `self$options$milestone2Name` | Output item / Table |
| `milestone2Date` | UI Control `milestone2Date` | `self$options$milestone2Date` | Output item / Table |
| `milestone3Name` | UI Control `milestone3Name` | `self$options$milestone3Name` | Output item / Table |
| `milestone3Date` | UI Control `milestone3Date` | `self$options$milestone3Date` | Output item / Table |
| `milestone4Name` | UI Control `milestone4Name` | `self$options$milestone4Name` | Output item / Table |
| `milestone4Date` | UI Control `milestone4Date` | `self$options$milestone4Date` | Output item / Table |
| `milestone5Name` | UI Control `milestone5Name` | `self$options$milestone5Name` | Output item / Table |
| `milestone5Date` | UI Control `milestone5Date` | `self$options$milestone5Date` | Output item / Table |
| `showEventMarkers` | UI Control `showEventMarkers` | `self$options$showEventMarkers` | Output item / Table |
| `eventVar` | UI Control `eventVar` | `self$options$eventVar` | Output item / Table |
| `eventTimeVar` | UI Control `eventTimeVar` | `self$options$eventTimeVar` | Output item / Table |
| `laneWidth` | UI Control `laneWidth` | `self$options$laneWidth` | Output item / Table |
| `markerSize` | UI Control `markerSize` | `self$options$markerSize` | Output item / Table |
| `plotTheme` | UI Control `plotTheme` | `self$options$plotTheme` | Output item / Table |
| `colorPalette` | UI Control `colorPalette` | `self$options$colorPalette` | Output item / Table |
| `showLegend` | UI Control `showLegend` | `self$options$showLegend` | Output item / Table |
| `referenceLines` | UI Control `referenceLines` | `self$options$referenceLines` | Output item / Table |
| `customReferenceTime` | UI Control `customReferenceTime` | `self$options$customReferenceTime` | Output item / Table |
| `customReferenceDate` | UI Control `customReferenceDate` | `self$options$customReferenceDate` | Output item / Table |
| `sortVariable` | UI Control `sortVariable` | `self$options$sortVariable` | Output item / Table |
| `sortOrder` | UI Control `sortOrder` | `self$options$sortOrder` | Output item / Table |
| `showInterpretation` | UI Control `showInterpretation` | `self$options$showInterpretation` | Output item / Table |
| `personTimeAnalysis` | UI Control `personTimeAnalysis` | `self$options$personTimeAnalysis` | Output item / Table |
| `responseAnalysis` | UI Control `responseAnalysis` | `self$options$responseAnalysis` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |
| `showCopyReady` | UI Control `showCopyReady` | `self$options$showCopyReady` | Output item / Table |
| `showAbout` | UI Control `showAbout` | `self$options$showAbout` | Output item / Table |
| `exportTimeline` | UI Control `exportTimeline` | `self$options$exportTimeline` | Output item / Table |
| `exportSummary` | UI Control `exportSummary` | `self$options$exportSummary` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/swimmerplot.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

