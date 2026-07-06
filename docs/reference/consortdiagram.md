# CONSORT Flow Diagram

Creates CONSORT 2010 compliant flow diagrams for clinical trials using
the consort R package. Automatically calculates participant numbers from
patient-level data with exclusion columns. Follows the approach
described by Riinu Ots (https://www.riinu.me/2024/02/consort/).

## Usage

``` r
consortdiagram(
  data,
  participant_id,
  screening_exclusions,
  enrollment_exclusions,
  randomization_var,
  allocation_exclusions,
  followup_exclusions,
  analysis_exclusions,
  show_exclusion_details = FALSE,
  diagram_width = 800,
  diagram_height = 600,
  text_width = 50,
  study_title = "Study Flow Diagram",
  screening_label = "Assessed for eligibility",
  enrollment_label = "Enrolled",
  allocation_label = "Allocated to intervention",
  followup_label = "Completed follow-up",
  analysis_label = "Analyzed",
  export_format = "png",
  include_interpretation = FALSE,
  consort_validation = FALSE
)
```

## Arguments

- data:

  Patient-level data frame where each row represents one participant.
  Exclusion columns should have non-NA values for excluded participants.

- participant_id:

  Unique participant identifier variable.

- screening_exclusions:

  List of variables representing screening-stage exclusion reasons.

- enrollment_exclusions:

  List of variables representing enrollment-stage exclusion reasons.

- randomization_var:

  Treatment arm or group assignment variable for randomized trials.

- allocation_exclusions:

  List of variables representing post-randomization exclusion reasons.

- followup_exclusions:

  List of variables representing follow-up-stage exclusion reasons.

- analysis_exclusions:

  List of variables representing analysis-stage exclusion reasons.

- show_exclusion_details:

  Whether to display detailed exclusion reasons with counts.

- diagram_width:

  Diagram width in pixels.

- diagram_height:

  Diagram height in pixels.

- text_width:

  Maximum characters before text wraps in diagram boxes.

- study_title:

  Title for the CONSORT diagram.

- screening_label:

  Custom label for screening stage.

- enrollment_label:

  Custom label for enrollment stage.

- allocation_label:

  Custom label for allocation stage.

- followup_label:

  Custom label for follow-up stage.

- analysis_label:

  Custom label for analysis stage.

- export_format:

  Export format for the diagram.

- include_interpretation:

  Whether to include interpretation section.

- consort_validation:

  Whether to perform CONSORT 2010 compliance validation.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$flowImbalanceNotice` |     |     |     |     | a html   |
| `results$duplicateIdNotice`   |     |     |     |     | a html   |
| `results$flowSummary`         |     |     |     |     | a table  |
| `results$armSummary`          |     |     |     |     | a table  |
| `results$exclusionBreakdown`  |     |     |     |     | a table  |
| `results$diagram`             |     |     |     |     | an image |
| `results$interpretation`      |     |     |     |     | a html   |
| `results$consortValidation`   |     |     |     |     | a html   |
| `results$exportInfo`          |     |     |     |     | a html   |
| `results$clinicalSummary`     |     |     |     |     | a html   |
| `results$aboutAnalysis`       |     |     |     |     | a html   |
| `results$caveatsAssumptions`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$flowSummary$asDF`

`as.data.frame(results$flowSummary)`
