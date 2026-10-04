# Reference meaning, nondiagnostic readings and paired reader studies

Start with the question: what establishes positivity? Expert cytology consensus
establishes a classification for this comparison. It does not, by itself, establish
histologically proven disease. Select **Expert consensus comparator** to report
positive and negative percent agreement (PPA/NPA); choose **Disease reference standard**
only when the reference supports that disease interpretation. The default **Not specified**
retains diagnostic terminology and explicitly identifies the undeclared interpretation.

In **Compare Medical Decision Tests** (`decisioncompare`), select distinct positive and
negative levels for the reference and every test, even for a two-level variable. Both selectors
are required in every reference mode. An unused third test may keep both selectors unset.
Results outside these pairs stop analysis unless **Exclude other observed levels** is selected.
No observed label, including a nondiagnostic call or unavailable reference, becomes negative
automatically. The mapping table shows the exact labels and counts, including excluded and
missing results. Per-test denominators and common paired denominators remain separate.

Exclusion produces estimates conditional on determinate reference/test pairs. Report the
excluded counts alongside those estimates; use the reader agreement analysis below to assess
nondiagnostic status and agreement among all observed readings. Choosing Bethesda III means
exactly III, not III–VI. For a cutoff or a target-versus-complement question, create an explicit
binary variable first, preserving unavailable results separately.

Saved `decisioncompare` analyses and R calls using implicit negative levels must supply them
before recalculation. Calls with additional observed labels also require explicit exclusion
or prior recoding. For example, use `goldNegative = "Negative"`, `test1Negative = "Negative"`
and `test2Negative = "Negative"` when those are the actual data labels; use `NULL` only for an
unselected variable. The analysis does not silently migrate historical one-versus-rest results.

PPA is positive matches divided by reference-positive determinate readings. Symmetric
positive specific agreement is twice the positive matches divided by the number of
positive calls across both methods. They are different quantities: exchanging the
reference and test can change PPA, while symmetric agreement is unchanged.

## Keep result availability separate from binary calls

The ordinary binary table still uses determinate positive/negative pairs. Enable
**Account for nondiagnostic readings** and enter one exact label to retain a separate
status ledger. Missing data remain missing; an unrecognized label causes an explicit
mapping error in this advanced workflow. The observed-value mapping table shows how
raw labels were interpreted. Reference labels other than the selected positive and
negative states remain excluded and are counted separately.

For an ordered Bethesda scale, declare I through VI in order, identify I as nondiagnostic,
and specify III as the positive cutoff. The analysis removes I from the ordered severity
scale before applying that cutoff. Recode the reference explicitly: II as negative,
III–VI as positive, I as an excluded reference classification. No thyroid-specific recoding
is automatic. A histology question needs its own reference, target and prespecified cutoff.

The microscopy aggregate fixture from Satturwar et al. (DOI: 10.1002/cncy.70149) contains
108 positive matches, 13 positive calls with negative reference, 63 negative calls with
positive reference, and 67 negative matches. There are also 9 nondiagnostic readings
among reference-positive and 25 among reference-negative observations.

| Estimand | Calculation |
|---|---:|
| Determinate PPA | 108/171 = 63.16% |
| Determinate NPA | 67/80 = 83.75% |
| Positive-call agreement, nondiagnostic retained | 108/180 = 60.00% |
| Negative-call agreement, nondiagnostic retained | 67/105 = 63.81% |
| Reference positive among positive calls | 108/121 = 89.26% |

The optional **Agreement including nondiagnostic calls** table uses observed readings
with an eligible reference. Nondiagnostic calls count in its denominators but not its
match numerators. Missing readings are excluded. It is not an all-enrolled failure
analysis, and it does not feed binary likelihood ratios, Fagan plots or disease-risk
calculations. The original determinate binary table remains unchanged.

## Compare modalities on matched cases

Use long format with one row per case × reader × modality and explicit case, reader,
modality and rating columns. Select a prespecified baseline and comparison modality.
Duplicate combinations, empty IDs, or reference/patient IDs that change within a case
are rejected. Other modalities are not included in that selected contrast.

The comparison first selects a complete matched grid across the selected fixed readers
and both modalities. **Determinate in every reading** excludes any nondiagnostic or
missing comparison cell. **Observed in every reading** retains declared nondiagnostic
calls, while excluding missing cells. Both require an eligible reference. The results
report excluded cases, included cases and readings, and the sampling-unit count.

Per-reader available-case summaries remain separate. On the complete grid, equal reader
weights make the pooled rate equal the mean reader rate. The contrast is comparison
minus baseline in percentage points. Resampling draws whole cases, or whole patients when
a patient ID is provided, and retains every reader and modality in each draw. Individual
readings are not independent sampling units. No p-value is inferred from the percentile
interval, and additional contrasts are exploratory rather than simultaneous inference.

These intervals concern the observed fixed readers. They do not generalize to a new panel
of readers. Constant bootstrap distributions and undefined draws leave the affected interval
unavailable with an explanation. Percentile interval coverage is design-dependent; the
analysis remains in testing pending method/release validation for intended designs.

## Describe changes without calling them errors

For ordinal input, **Ordered category transitions** reports unchanged, higher and lower
categories plus negative-to-positive and positive-to-negative cutoff crossings. These
counts use determinate paired readings. Becoming determinate, becoming nondiagnostic,
and remaining nondiagnostic use the observed-pair population. The transition matrix retains
all declared categories, including unused V; nondiagnostic I appears as an adequacy state,
not the lowest disease grade. Moving upward is not necessarily becoming more correct.

For wide data, specify the exact baseline and comparison reader column names. For long
data, each reader's baseline/comparison modalities supply the paired roles automatically.

A complete reproducible example is in
[`inst/examples/pathagreement-reader-study.R`](../inst/examples/pathagreement-reader-study.R).
Its individual records are **synthetic**. The article's aggregate counts are used only for
arithmetic checks; they are never expanded into purported patient data for paired inference.
