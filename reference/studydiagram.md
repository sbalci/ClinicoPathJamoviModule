# Study Diagram

Creates professional study flow diagrams including CONSORT diagrams and
flowcharts using multiple rendering engines

## Usage

``` r
studydiagram(
  data,
  data_format = "step_summary",
  diagram_type = "consort_standard",
  participant_id = NULL,
  step_excluded = NULL,
  exclusion_reason_participant = NULL,
  step_name = NULL,
  participant_count = NULL,
  exclusion_reason_summary = NULL,
  participant_id_mapping = NULL,
  exclusion_reason_mapping = NULL,
  exclusions_after_step1,
  exclusions_after_step2,
  exclusions_after_step3,
  exclusions_after_step4,
  exclusions_after_step5,
  step1_label = "Screening",
  step2_label = "Enrollment",
  step3_label = "Treatment",
  step4_label = "Follow-up",
  step5_label = "Analysis",
  show_percentages = FALSE,
  show_exclusion_boxes = FALSE,
  direction = "TB",
  color_scheme = "gray",
  show_interpretation = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- data_format:

  How your data is structured. Step summary: one row per study step with
  participant counts. Exclusion mapping: map exclusion reasons to
  specific study steps.

- diagram_type:

  Rendering engine and diagram style. CONSORT for clinical trials,
  Flowchart for general study flows. Standard uses DiagrammeR, ggplot2
  uses modern styling.

- participant_id:

  Variable identifying individual participants (for participant tracking
  format)

- step_excluded:

  Variable indicating at which step number the participant was excluded
  (for participant tracking format)

- exclusion_reason_participant:

  Variable containing exclusion reason text (for participant tracking
  format)

- step_name:

  Variable containing step/stage names (for step summary format)

- participant_count:

  Variable containing participant counts at each step (for step summary
  format)

- exclusion_reason_summary:

  Variable containing exclusion reason descriptions (for step summary
  format)

- participant_id_mapping:

  Variable identifying individual participants (for exclusion mapping
  format)

- exclusion_reason_mapping:

  Variable containing all exclusion reasons (for exclusion mapping
  format)

- exclusions_after_step1:

  Select exclusion reasons that occur AFTER step 1 (reducing
  participants from step 1 to step 2). These exclusions will be shown on
  the transition from step 1 to step 2.

- exclusions_after_step2:

  Select exclusion reasons that occur AFTER step 2 (reducing
  participants from step 2 to step 3). These exclusions will be shown on
  the transition from step 2 to step 3.

- exclusions_after_step3:

  Select exclusion reasons that occur AFTER step 3 (reducing
  participants from step 3 to step 4). These exclusions will be shown on
  the transition from step 3 to step 4.

- exclusions_after_step4:

  Select exclusion reasons that occur AFTER step 4 (reducing
  participants from step 4 to step 5). These exclusions will be shown on
  the transition from step 4 to step 5.

- exclusions_after_step5:

  Select exclusion reasons that occur AFTER step 5 (if applicable)

- step1_label:

  Label for step 1 in the diagram

- step2_label:

  Label for step 2 in the diagram

- step3_label:

  Label for step 3 in the diagram

- step4_label:

  Label for step 4 in the diagram

- step5_label:

  Label for step 5 in the diagram

- show_percentages:

  Show percentage of initial participants retained at each step

- show_exclusion_boxes:

  Show side boxes with exclusion counts and reasons

- direction:

  Direction of diagram flow

- color_scheme:

  Color scheme for diagram elements

- show_interpretation:

  Show explanatory text about the generated diagram

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$todo` |  |  |  |  | Instructions and data format guidance |
| `results$summary` |  |  |  |  | Summary of participant flow through study steps |
| `results$plot` |  |  |  |  | an image |
| `results$diagram` |  |  |  |  | Interactive study flow diagram |
| `results$interpretation` |  |  |  |  | Explanatory text about the diagram and results |
| `results$warnings` |  |  |  |  | Alerts about potential data quality issues |
| `results$notices` |  |  |  |  | a preformatted |
| `results$clinicalSummary` |  |  |  |  | Plain-language summary for clinical interpretation |
| `results$reportSentence` |  |  |  |  | Copy-ready sentence for manuscripts |
| `results$aboutAnalysis` |  |  |  |  | Explanation of analysis type and usage |
| `results$caveatsAssumptions` |  |  |  |  | Important considerations and limitations |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
