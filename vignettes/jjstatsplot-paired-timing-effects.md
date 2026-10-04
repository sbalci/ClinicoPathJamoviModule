# Paired timing effects for fixed cases

In **Box-Violin Plots to Compare Within Groups**, select the measurement columns
in their intended comparison order, then enable **Paired changes table**. Enter
the shared units, such as seconds. Every row must be an independent case, with
the repeated measurements in separate columns. For a study in which several
readers assess the same cases, run a reader-specific analysis or use a
prespecified case-level summary. Stacking reader–case rows does not create
independent cases.

The table reports each later selected measurement minus each earlier one. A
negative change therefore means a shorter later reading time. All contrasts
use the same cases with finite values in every selected measurement, consistent
with the plots; the analysis summary reports exclusions.

Choose an effect definition that answers the intended question:

| Definition | Meaning |
|---|---|
| Mean paired change | Average of each case's later-minus-earlier difference |
| Median paired change | Median of those differences; generally different from subtracting the two marginal medians |
| Hodges–Lehmann shift | Median of the Walsh averages of paired differences; a location-shift interpretation requires symmetry |
| Geometric mean paired time ratio | `exp(mean(log(later) - log(earlier)))`; generally different from the ratio of arithmetic means |

For time ratios, values below 1 mean shorter later measurements. Both selected
measurements must be strictly positive for every included case. If a contrast
contains a zero or negative value, its ratio remains unavailable; cases are not
silently removed to obtain it.

The paired percentile bootstrap resamples whole case rows, preserving the
measurements together. The same resample is used across contrasts. Its sample
count and random seed are reported. **Estimator-specific interval** uses a
paired t interval for mean changes, a normal-approximation signed-rank interval
for the Hodges–Lehmann shift, and a percentile bootstrap for median changes.
Ratio intervals are calculated on the log scale and transformed back. Constant
resampling distributions do not supply an uncertainty interval; the table
explains unavailable results.

All intervals are pointwise. The p column uses the existing selected test and
its multiplicity setting; it does not test every displayed effect definition.
With two measurements this is the paired t-test or Wilcoxon signed-rank test.
With three or more measurements, enable **Pairwise comparisons** to show the
existing adjusted post-hoc p-values. Nonparametric post-hoc comparisons use
Durbin–Conover. Robust and Bayesian outputs remain in the plots, without a new
p-value in this table. Ratios have no additional p-value.

A fixed microscopy → single-layer → seven-layer sequence confounds modality
with measurement period. Neither these paired contrasts nor a washout period
can separate those effects statistically. Report the observed differences
with their uncertainty and this design limitation.
