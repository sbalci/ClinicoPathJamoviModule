# Validate rare-disease AI at the intended prevalence

Use **Clinical ROC Analysis (`enhancedROC`) → Frozen-model validation report** for predictions
from an already fitted model. This report never trains a model, transforms a score by fitting
the outcome, chooses a favorable direction, or searches for a best cutoff.

## Prepare the reference and predictions

1. Supply a binary reference with explicit positive and negative definitions. Select the
   positive class and enter the exact negative reference level. Recode approved categories
   into those two labels explicitly; an unfamiliar diagnosis is not automatically negative.
2. Supply frozen numeric score columns. Declare whether these are arbitrary scores or
   probabilities of the selected positive class. Choose higher or lower explicitly.
   Probability inputs require higher-is-positive; lower-is-positive scores remain supported.
3. Enter a model version, reference-standard description, sampling design, threshold source,
   and fixed score cutoffs. Choose Development / exploratory when cutoffs were selected
   using the same evaluation data. Metadata are declarations, not proof of external validation.
4. For repeated slides, select a globally unique patient identifier. Optional site IDs define
   fixed-site bootstrap strata. A supplied partition column permits an audit for patient
   overlap across partitions; absence of a manifest means overlap was not assessed.

Patient reference status must be consistent. Missing IDs and extra reference categories
are errors. Missing references and nonfinite/missing scores are counted separately before
all models are evaluated on the same complete rows. Technical failures should be recorded
upstream as missing scores with an explicit reason ledger; the analysis cannot infer why a
score is missing. It does not invent technical-failure categories from numerical data.

## Read the denominators and intervals

The report shows rows, positive and negative rows, and independent units. Multiple slides
from a patient remain together during resampling, and all model columns share the same
resampled patients. Performance remains **weighted by slides**; patient-cluster resampling
does not turn it into patient-level diagnostic performance. Aggregate to one prespecified
prediction per patient first if that is the target estimand.

The optional percentile bootstrap describes a frozen model conditional on the observed sites.
It does not estimate training uncertainty or performance in a random population of hospitals.
Per-site tables and comparisons against the first selected score column use the same fixed
cutoffs. No-event strata and one-class bootstrap draws can make metrics unavailable; the
report counts valid and failed draws for each metric. Degenerate intervals are omitted with
a reason. In particular, empirical bootstrapping cannot establish perfect population sensitivity
after observing zero misses. Percentile intervals are approximate and may under-cover,
especially with few positive patients or unequal numbers of slides per patient. The supplied
coverage simulation exposes these limitations; it is not a guarantee of nominal coverage.

Calibration uses supplied probabilities. Brier score supports exact 0/1 predictions, but
joint logistic calibration coefficients are unavailable when logits are undefined, predictors lack
variation, or fitting fails/separates. Calibration-in-the-large is the intercept from an
offset-logit model, distinct from the intercept estimated jointly with a calibration slope.

Large bootstrap jobs scale with patients, score columns, cutoffs, sites and draws. Start
with a small set of prespecified models/cutoffs and verify the data before requesting
thousands of replicates. Patient/site/partition identifiers are used during computation;
the displayed results contain aggregates, not a saved patient manifest.

## Keep three inputs separate

| Input | Purpose |
|---|---|
| Score cutoff | Determines which observations are flagged |
| Disease prevalence | Defines the target population for projected counts and PPV |
| Utility threshold | Defines the harm/benefit exchange rate for net benefit |

Enable Clinical impact analysis to obtain observed or projected counts at the fixed score
cutoffs. With observed prevalence selected, the primary workload row uses the analyzed sample
size. Otherwise it uses the entered prevalence and target population size. Additional
comma-separated prevalence scenarios always describe projected populations.

For population size `N`, prevalence `p`, sensitivity `Se` and specificity `Sp`:

```text
TP = N × p × Se                 FN = N × p × (1 − Se)
FP = N × (1 − p) × (1 − Sp)     TN = N × (1 − p) × Sp
PPV = TP / (TP + FP)            flags = TP + FP
net benefit = TP/N − (FP/N) × utility_threshold/(1 − utility_threshold)
```

Projected counts may be fractional: they are expectations, not observed records. Transporting
these quantities assumes Se/Sp remain applicable despite changes in case mix and setting.
No uncertainty in a user-supplied prevalence is inferred. A positive-only cohort cannot
establish specificity or transported PPV. A flag is not automatically a new patient review,
an ancillary test, or an increment in time; measure those workflow endpoints separately.

The aggregate `decisioncalculator` also provides observed/projected workload from a supplied
2×2 table. Prevalence inputs support values below 0.1%, and plots in `enhancedROC` include
a denser grid at rare prevalences. `decision` and `decisioncompare` offer a validation-context
table while retaining their explicit category mapping and comparator-aware labels.

## Teaching example and precision planning

The accompanying summary arithmetic uses 7,056 slides, 45 positives and rounded specificity
0.905. It implies approximately 666 false-positive flags and PPV 6.3%; these are not measured
workload counts. At specificity 0.956, the corresponding values are approximately 308 and
12.7%. See [the article review](../literature/yang-2026-gaspath-citation-review.md) and
[reproducible arithmetic](../literature/yang-2026-gaspath-arithmetic.R).

`diagnosticsamplesize` now offers a separate zero-miss plan. For example, 72 independent
positive patients with no misses give a two-sided 95% exact lower sensitivity bound of at
least 95%. This is **conditional on no misses**, not a power/assurance calculation. Dividing
the required positives by prevalence gives expected recruitment, not a guarantee of accruing
that many positives. The ordinary CI-width planner remains available for other assumptions.

## Changes to existing analyses

- `psychopdaROC` and `enhancedROC` now share a tie-aware PR helper. Average precision and
  trapezoidal PR area are separate definitions; the recall-zero endpoint is explicit.
- The draft `precisionrecall` offers trapezoidal area or average precision, percentile
  bootstrap intervals and a paired model-swap permutation test. It no longer advertises
  unimplemented nonlinear interpolation or BCa intervals. The permutation null assumes
  within-row exchangeability of the two models, not merely equal population AUCs. Its
  pairwise p-values are unadjusted and exploratory.
- The impact panel no longer uses prevalence as a cutoff. Saved analyses that previously
  relied on that behavior must explicitly set their intended cutoffs and utility threshold.
  Inverse net benefit and inverse Youden index are labeled as indices, not workload counts.
- Frozen validation is a dedicated output mode. Other ROC panels retain their ordinary
  independent-row methods and are not reinterpreted as patient-cluster analyses.
- Modified analyses remain in testing routes; no production promotion is performed here.

Implementation details, coverage simulation and verification results are recorded in the
[implementation report](../literature/yang-2026-gaspath-implementation.md).
