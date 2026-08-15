# Adverse Events Butterfly Plot

Back-to-back (butterfly) bar plot of adverse-event frequencies by
preferred term, comparing a test arm against an optional control arm and
splitting each bar into all-grade and high-grade (e.g. grade \>= 3)
severity. Accepts patient-level data (incidence computed internally) or
pre-summarized percentages. Inspired by the Jamovi-TrialPlots module by
highwind.

## Usage

``` r
aeplot(
  data,
  inputMode = "patient",
  subjectID = NULL,
  aeTerm = NULL,
  armVar = NULL,
  gradeVar = NULL,
  gradeThreshold = 3,
  aeTermS = NULL,
  testAll = NULL,
  testHigh = NULL,
  controlAll = NULL,
  controlHigh = NULL,
  barShape = "inside",
  colorScheme = "nejm",
  showValues = FALSE,
  topN = 0
)
```

## Arguments

- data:

  .

- inputMode:

  'patient' computes adverse-event incidence from patient-level rows;
  'summary' reads pre-computed all-grade and high-grade percentages.

- subjectID:

  Subject identifier used as the incidence denominator (distinct
  subjects).

- aeTerm:

  Adverse-event preferred term (one row per subject-event).

- armVar:

  Optional treatment arm. When omitted the plot shows a single arm.

- gradeVar:

  Optional numeric severity grade used to derive the high-grade split.

- gradeThreshold:

  Events with grade greater than or equal to this value count as high
  grade.

- aeTermS:

  Adverse-event term column for pre-summarized input.

- testAll:

  Test-arm all-grade incidence percentage.

- testHigh:

  Test-arm high-grade incidence percentage.

- controlAll:

  Control-arm all-grade incidence percentage (optional).

- controlHigh:

  Control-arm high-grade incidence percentage (optional).

- barShape:

  'inside' overlays high-grade on all-grade; 'outside' stacks the two.

- colorScheme:

  Journal-style color palette (via ggsci) for the test arm.

- showValues:

  Print the all-grade percentage next to each bar.

- topN:

  Keep only the N most frequent terms (by test-arm all-grade); 0 shows
  all.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$instructions`   |     |     |     |     | a html   |
| `results$freqTable`      |     |     |     |     | a table  |
| `results$plot`           |     |     |     |     | an image |
| `results$interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$freqTable$asDF`

`as.data.frame(results$freqTable)`
