# Survival Power Analysis - Distribution Support Module

Enhanced distribution support for survivalPower function Implements
Weibull, log-normal, and piecewise exponential distributions

## Usage

``` r
.get_weibull_parameters(median_survival, shape)
```

## Arguments

- median_survival:

  Median survival time

- shape:

  Weibull shape parameter (gamma)

## Value

List with lambda (scale) and shape parameters

## Details

For Weibull distribution: S(t) = exp(-(lambda\*t)^shape) Median: m =
(log(2)/lambda)^(1/shape) Therefore: lambda = (log(2)/m)^(1/shape)

## References

Lachin, J. M., & Foulkes, M. A. (1986). Evaluation of sample size and
power for analyses of survival with allowance for nonuniform patient
entry, losses to follow-up, noncompliance, and stratification.
Biometrics, 42(3), 507-519. PMID: 3567285 Calculate Weibull distribution
parameters from median survival
