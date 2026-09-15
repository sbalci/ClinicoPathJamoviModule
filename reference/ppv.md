# Positive Predictive Value

Calculates the Positive Predictive Value (PPV) and False Discovery Rate
(FDR) for research findings based on study characteristics. This tool
helps researchers understand the probability that their significant
findings are actually true, considering factors like prior probability,
statistical power, and research biases.

## Usage

``` r
ppv(percTrue = 50, alpha = 0.05, power = 0.8, percHack = 0)
```

## Arguments

- percTrue:

  The pre-study probability that the tested relationships are true. In
  exploratory research fields, this may be 10 percent or lower. In
  confirmatory research with strong theoretical basis, it may be higher.

- alpha:

  The significance level (Type I error rate) used in the studies.
  Standard value is 0.05, but may be lower for multiple testing
  situations.

- power:

  Statistical power of the studies to detect true effects. Well-designed
  studies typically have 80 percent power or higher. Many published
  studies have much lower actual power.

- percHack:

  Percentage of studies with questionable research practices (p-hacking,
  selective reporting, data dredging). This represents bias in the
  research process that increases false positive rates.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$confusion` |  |  |  |  | a table containing the true/false positives/negatives |
| `results$ppv` |  |  |  |  | a html |
| `results$dotPlot` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$confusion$asDF`

`as.data.frame(results$confusion)`
