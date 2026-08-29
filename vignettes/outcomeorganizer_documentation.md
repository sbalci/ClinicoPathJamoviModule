# Outcome Organizer for Survival Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `outcomeorganizer`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomeLevel` | UI Control `outcomeLevel` | `self$options$outcomeLevel` | Output item / Table |
| `recurrence` | UI Control `recurrence` | `self$options$recurrence` | Output item / Table |
| `recurrenceLevel` | UI Control `recurrenceLevel` | `self$options$recurrenceLevel` | Output item / Table |
| `patientID` | UI Control `patientID` | `self$options$patientID` | Output item / Table |
| `followupTime` | UI Control `followupTime` | `self$options$followupTime` | Output item / Table |
| `analysistype` | UI Control `analysistype` | `self$options$analysistype` | Output item / Table |
| `multievent` | UI Control `multievent` | `self$options$multievent` | Output item / Table |
| `dod` | UI Control `dod` | `self$options$dod` | Output item / Table |
| `dooc` | UI Control `dooc` | `self$options$dooc` | Output item / Table |
| `awd` | UI Control `awd` | `self$options$awd` | Output item / Table |
| `awod` | UI Control `awod` | `self$options$awod` | Output item / Table |
| `useHierarchy` | UI Control `useHierarchy` | `self$options$useHierarchy` | Output item / Table |
| `eventPriority` | UI Control `eventPriority` | `self$options$eventPriority` | Output item / Table |
| `intervalCensoring` | UI Control `intervalCensoring` | `self$options$intervalCensoring` | Output item / Table |
| `intervalStart` | UI Control `intervalStart` | `self$options$intervalStart` | Output item / Table |
| `intervalEnd` | UI Control `intervalEnd` | `self$options$intervalEnd` | Output item / Table |
| `adminCensoring` | UI Control `adminCensoring` | `self$options$adminCensoring` | Output item / Table |
| `adminDate` | UI Control `adminDate` | `self$options$adminDate` | Output item / Table |
| `outputTable` | UI Control `outputTable` | `self$options$outputTable` | Output item / Table |
| `diagnostics` | UI Control `diagnostics` | `self$options$diagnostics` | Output item / Table |
| `visualization` | UI Control `visualization` | `self$options$visualization` | Output item / Table |
| `showNaturalSummary` | UI Control `showNaturalSummary` | `self$options$showNaturalSummary` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |
| `addOutcome` | UI Control `addOutcome` | `self$options$addOutcome` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/outcomeorganizer.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

