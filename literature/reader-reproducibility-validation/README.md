# Reader reproducibility validation

Run `Rscript --vanilla tools/validate_reader_intervals.R` from the repository root.
The script sources only the numerical helpers, does not modify the RNG state of
an analysis, and writes the three audit files in this directory.

## Coverage experiment

Each scenario uses 300 independent simulated studies, 399 whole-case or
whole-cluster bootstrap draws per study and nominal 95% percentile intervals
(R quantile type 7). The joint binary probabilities are given in the script in
the order ++, +−, −+, −−. Clustered scenarios repeat each cluster's binary pair
three times, giving perfect within-patient dependence. The truth is calculated
analytically from these probabilities. Balanced accuracy treats the second column
as the known reference for this simulation only.

`coverage.csv` reports all estimands, their truth, interval availability, coverage
conditional on an available interval, and an exact binomial 95% Monte Carlo
interval for that conditional coverage. Unavailable intervals are counted and
are not treated as successful coverage. In rare-category scenarios, conditional
coverage based on only one or two available intervals is not informative.

| Scenario | CI availability | Conditional coverage of available CIs |
|---|---:|---:|
| 240 independent cases, unequal margins | 100% for all four estimands | 92.7–94.0% |
| 120 cases, rare positive readings | 0.3–73.0%, depending on estimand | Sparse available intervals; see CSV |
| 60 clusters, 180 specimens | 100% for all four estimands | 91.3–93.0% |
| 8 clusters, 24 specimens | 1.0–69.7%, depending on estimand | Strong selection by interval availability |

The 60-cluster balanced-accuracy coverage was 91.3% (Monte Carlo interval
87.6–94.3%); negative specific agreement coverage was 91.7% (87.9–94.5%).
These results do **not** support presenting percentile intervals as a universally
validated 95% procedure. Both analyses label them exploratory. The established
approximate Wilson default in `agreement` is retained; that approximation also
has no guaranteed coverage. `pathagreement` requires explicit opt-in to
bootstrap, while independent-case Bernoulli proportions use exact binomial
intervals. The independent exact-binomial calculation is checked directly
against `stats::binom.test()` in the numerical and exported-wrapper tests.

The guard against any undefined draw or a constant distribution is intentionally
conservative. It does not solve coverage under sparse or clustered sampling.
No coverage claim is made for the optional experimental composite.

## Performance

`timing.csv` records an actual 50,000-row × 50-reader point-only run (1,225 pairs)
and a 50,000-row selected-pair run with 1,000 bootstrap draws. They took 55.744
and 43.461 seconds respectively on this local R 4.6.0 aarch64 run. Timings cover
the preparation/computation/report helpers, excluding jamovi table population and
client rendering. Report objects
occupied 864,624 and 32,536 bytes. These are report-object sizes, not peak memory
measurements; elapsed times are machine/load dependent. Bootstrap loops checkpoint
every 25 draws, and pair loops checkpoint between pairs. All-pair resampling can
be expensive, so users can select one pair.

`environment.txt` records the platform, confidence level, RNG seeds and Monte
Carlo configuration. Unit tests separately verify deterministic results, restoration
of the caller's RNG state, duplicate sampled clusters, unused cluster levels,
negative kappa and undefined-draw accounting.
