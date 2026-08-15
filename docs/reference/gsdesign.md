# Group-Sequential Design & Sample Size

Group-sequential trial design and sample-size / events calculation for
survival (time-to-event), binary, and continuous endpoints using the
gsDesign package. Reports efficacy (and optional futility) boundaries,
per-look sample size, and a boundary plot. Inspired by the
Jamovi-TrialPlots module by highwind.

## Usage

``` r
gsdesign(
  data,
  endpoint = "survival",
  sided = "2",
  alpha = 0.05,
  power = 0.9,
  kMax = 2,
  sfu = "OF",
  sfupar = -4,
  timing = "",
  testType = "efficacy",
  hazards = "proportional",
  delayMonths = 3,
  hrDelayed = 1,
  hr = 0.7,
  medianControl = 12,
  accrualDuration = 12,
  followupDuration = 18,
  ratio = 1,
  dropoutRate = 0.05,
  p1 = 0.4,
  p2 = 0.25,
  deltaMean = 0.5,
  stdDev = 1
)
```

## Arguments

- data:

  .

- endpoint:

  .

- sided:

  .

- alpha:

  .

- power:

  .

- kMax:

  .

- sfu:

  .

- sfupar:

  .

- timing:

  .

- testType:

  .

- hazards:

  Whether the hazard ratio is assumed constant over time. Under
  "proportional" the design uses gsDesign::gsSurv with the single hazard
  ratio below. Under "non-proportional" it uses gsDesign2::gs_design_ahr
  with an average hazard ratio, for the delayed-separation pattern
  typical of immunotherapy: no treatment effect for an initial period,
  then the hazard ratio below. Assuming proportional hazards when the
  curves separate late understates the events required.

- delayMonths:

  Non-proportional hazards only. Months from randomisation during which
  the treatment has no effect (hazard ratio held at the value below),
  before the full hazard ratio applies.

- hrDelayed:

  Non-proportional hazards only. The hazard ratio during the delay
  period. 1 means no treatment effect at all until the delay has
  elapsed.

- hr:

  .

- medianControl:

  .

- accrualDuration:

  .

- followupDuration:

  .

- ratio:

  .

- dropoutRate:

  .

- p1:

  .

- p2:

  .

- deltaMean:

  .

- stdDev:

  .

## Value

A results object containing:

|                         |     |     |     |     |          |
|-------------------------|-----|-----|-----|-----|----------|
| `results$summary`       |     |     |     |     | a html   |
| `results$boundaryTable` |     |     |     |     | a table  |
| `results$boundaryPlot`  |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$boundaryTable$asDF`

`as.data.frame(results$boundaryTable)`
