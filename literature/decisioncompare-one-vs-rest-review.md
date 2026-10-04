# Focused review: decisioncompare one-versus-rest recoding

Reviewed 4 October 2026 against the working implementation and generated classes.
**Conclusion: mathematically valid for an explicitly defined class-versus-complement
comparison, but unsuitable as an implicit default for diagnostic data containing
nondiagnostic, equivocal, invalid or unavailable classifications.** The previous plan's
decision to preserve it in every reference mode needs revision before release.

**Follow-up implemented:** analysis 1.1.0 removes implicit one-versus-rest recoding.
Every selected variable requires distinct positive and negative levels. Other observed
labels cause an error unless exclusion is explicitly enabled. This is shared across all
reference modes; the new mapping table lists labels and counts. Historical analyses that
need new selections stop before publishing estimates. The analysis remains in `meddecideT`.

The findings and CSVs below document the pre-fix behavior. Regression coverage lives in
[`test-decisioncompare-explicit-levels.R`](../tests/testthat/test-decisioncompare-explicit-levels.R).
The focused audit script now checks the corrected behavior and preserves the historical CSVs.

## Contract at the time of review

`excludeIndeterminate` defaults to false in
[`decisioncompare.a.yaml`](../jamovi/decisioncompare.a.yaml). Both the reference and each
test are coded as follows:

```text
selected positive label                       -> positive
every other nonmissing label                  -> negative
actual NA (including an explicit NA level)    -> missing
```

The selected negative label changes this rule only when `excludeIndeterminate = TRUE`.
If it is missing, the code continues one-versus-rest even with that checkbox enabled.
The rule is applied separately to the reference and each test.

Legacy/disease modes implement this in `R/decisioncompare.b.R`, `.processSingleTest()`
(around lines 1133–1199). Consensus/method modes use
`R/utils-diagnostic-comparator.R`, `.diagComparatorBinary()` (lines 61–71), called by
`.runComparator()` in the backend (around line 439).

## Findings

### 1. High: nondiagnostic calls become apparent negative matches

For an eligible reference-positive case, an ND call is coded as a false negative or
negative-call/positive-reference discordance. For a reference-negative case, the same
ND call becomes a true negative or negative match. This rewards abstention on negative
cases. It is not the optional all-observed agreement estimand implemented in `pathagreement`.

The thyroid microscopy counts reproduce the issue directly:

| Policy | Analyzed readings | PPA | NPA | Overall agreement |
|---|---:|---:|---:|---:|
| Selected positive/negative calls; conditional on determinate results | 251 | 108/171 = 63.16% | 67/80 = 83.75% | 175/251 = 69.72% |
| Retain ND as nonmatches in separate all-observed metrics | 285 | 108/180 = 60.00% | 67/105 = 63.81% | 175/285 = 61.40% |
| Current one-versus-rest, ND coded negative | 285 | 108/180 = 60.00% | **92/105 = 87.62%** | **200/285 = 70.18%** |

The extra 25 negative matches are nondiagnostic readings. Relative to the stated
all-observed negative-match estimand, NPA is higher by 25/105 = **23.81 percentage points**.
Relative to the conditional determinate NPA, the increase is 3.87 points; these populations
must not be conflated. Positive-call agreement remains 108/121 because ND is folded into
the negative calls, while reference-negative proportion among negative calls changes
from 67/130 to 92/164. Hence this is not a mere terminology issue.

These aggregate cells are expanded only for arithmetic verification. They do not support
patient-level paired inference, and the audit does not calculate such inference from them.

### 2. High: incomplete exclusion settings still produce results

With `excludeIndeterminate = TRUE` and `test1Negative = NULL`, all 285 microscopy readings
remain analyzed. Supplying `test1Negative = "N"` reduces the population to 251.

A single analysis can therefore apply different policies to otherwise identical tests:
with a negative label selected only for test 1, its denominator is 251 while test 2's is
285. The paired common-case machinery is correctly aligned, but it cannot detect that
the user intended the same exclusion policy for both tests.

### 3. High: the new comparator branch loses the existing strong warning

Legacy/disease modes issue “Multi-Level Results Combined” or “Negative Level Required
for Exclusion” when more than two observed categories trigger these cases. The new
consensus/method branch bypasses that code and emits neither warning. Its definition table
says “all other observed levels”, but it still computes results when the user's exclusion
choice cannot be applied. Switching to agreement terminology does not repair the mapping.

Even the legacy warning is conditional on more than two observed categories: a column
containing only `Positive` and `Nondiagnostic` is still not semantically a binary
positive/negative diagnostic result. Counting levels cannot establish category meaning.

### 4. High: an unavailable reference becomes a negative reference

The same complement rule acts on the reference. Unknown, unverified or nondiagnostic
reference labels are not evidence of disease absence or a negative consensus classification.

In the separate toy reference fixture, 30 eligible cases contain 10 positive matches,
5 negative discordances, 5 positive discordances and 10 negative matches. Adding 10
unavailable-reference cases with negative test calls changes the default NPA from 10/15
to 20/25 and analyzed N from 30 to 40. The direction depends on those test calls; arbitrary
reference pooling can increase or decrease estimates. It also changes the reference case
mix and, in disease mode, downstream predictive values and likelihood-ratio interpretation.

### 5. High: paired comparisons can reward nondiagnostic output

A wholly synthetic example has 10 reference-positive cases called positive by both tests,
10 reference-negative cases called negative by both, and 10 reference-negative cases where
test 1 is positive but test 2 is ND. Default recoding reports test 2's overall agreement
as 100%, versus 66.67% for test 1; the displayed Holm-adjusted p-value is 0.00293.
Those ten ND calls have all been scored as correct negative calls. With selected-level
exclusion, only 20 common determinate cases remain and the observed difference is zero.
The McNemar calculation itself is correct for its recoded events; their interpretation
is the problem. Neither result alone describes the complete diagnostic workflow.

### 6. Moderate: nominal pooling is not an ordinal threshold

Selecting `III` as positive means exactly III versus all other labels. IV, V and VI become
negative. Selecting II as the negative label with exclusion enabled compares III with II
and excludes IV–VI. Neither setting means Bethesda III+. Use an explicit upstream binary
recode, or the ordinal-cutoff reader workflow. A three-outcome diagnostic matrix is not
needed to resolve this selection issue.

### 7. Tests establish compatibility, not suitability of the default

The previous passing regression suite established the existing numerical contract. It did
not establish that its default target/population matches the user's clinical question.
For example, `test-decisioncompare-arguments.R` around line 243 checks only the result class
for the exclusion toggle while supplying NULL negative levels. The two configurations can
therefore return identical counts and the test still passes. The warning example in
`test-decisioncompare-critical-fixes.R` around line 91 inspects factor levels without
executing the actual warning path. Stronger existing numerical exclusion tests are useful,
but they do not replace mode-by-mode tests of missing mappings and emitted notices.

## Recommendations recorded before the fix

1. Make selected positive/negative levels the normal diagnostic comparison policy. Require
   an explicit negative mapping for ambiguous inputs; review both selected categories even
   when only two labels occur. Unknown reference classifications need separate exclusion
   accounting rather than automatic negative status.
2. When exclusion is requested and a required mapping is missing, stop with a specific
   instruction naming the column. Do not silently or warning-only fall back to another policy.
3. Offer one-versus-rest only as an explicit, separately named choice for a meaningful
   target class versus its complement. Show the actual labels and counts pooled on each side;
   the user must define unavailable/invalid states separately. Do not infer these states
   from English label spellings or arbitrary factor order.
4. Use one shared mapping/validation path for legacy, disease, consensus and method modes.
   Reference interpretation should change statistical interpretation, not input safeguards.
5. Keep ND status and all-observed accounting in `pathagreement`. Selected-level 2×2
   estimates must be labeled conditional on determinate results and accompanied by exclusions
   and yield where applicable. Simply dropping ND is not an unconditional accuracy estimate.
6. Preserve historical one-versus-rest analyses through explicit compatibility metadata or
   an analysis-version migration. Do not silently recalculate saved results under a new
   default. The migration/default decision is part of the next implementation change.

FDA's diagnostic-test guidance cautions that ordinary binary methods do not directly
apply when tests produce equivocal outcomes and that discarding those results can bias
reported performance. It discusses alternative explicit treatments rather than endorsing
automatic negative pooling. This supports disclosing conditional populations and separately
accounting for nondiagnostic results. [FDA guidance, section 6.2](https://www.fda.gov/media/71147/download).

## Reproduction

```sh
Rscript --vanilla development-scripts/audit_decisioncompare_one_vs_rest.R
```

The script exercises actual generated Options/Analysis/Results classes and the corrected
backend. It checks all four reference modes under three settings: unapproved exclusion
(blocked), missing negative mapping (blocked), and explicit pairs with exclusion (251
analyzed readings; NPA 67/80). See the
[post-fix verification](decisioncompare-explicit-levels-verification.csv).

The regression tests additionally cover asymmetric mappings, unavailable references,
actual NA versus string-coded missingness, ordinal equality, paired abstention, optional
test slots, and stale output clearing. Existing numerical tests now supply the required
negative levels explicitly.

Historical pre-fix snapshots remain available:
[mode/policy results](decisioncompare-one-vs-rest-audit.csv),
[reference pooling](decisioncompare-reference-pooling-audit.csv), and
[synthetic paired example](decisioncompare-abstention-pair-audit.csv).

The user's final instruction requires explicit positive and negative selection throughout.
Accordingly, the implementation removes the one-versus-rest option entirely; a meaningful
class-versus-complement comparison requires explicit recoding before analysis. Older
configurations requiring new selections stop before recalculation, rather than being
silently migrated. These choices supersede recommendations 3 and 6 above.

This is a focused mapping correction, not a full module release audit.

## Verification after the correction

The official jamovi compiler regenerated analysis 1.1.0 and its UI successfully using
the installed app's verified version (`--assume-app-version 28.3.0`). No generated header
was hand-edited. The full `decisioncompare` test files and shared comparator tests were
run; four older test cases were updated to use explicit mappings and rerun. The final
combined result is **1,463 passing assertions across 13 files**, with no failures, errors,
warnings or skips in those tests. A final validation check covers NA positive selections.
Six focused linters found no issues in the backend, mapping helper and new regression file.
The surrounding R environment still emits its existing glmmTMB/TMB and XQuartz notices
during namespace loading; these are separate from test results.

The analysis remains in its testing menu. Native saved-document reopening was not
validated in this correction; historical mappings requiring new selections are covered
through generated options/classes and invalid-rerun tests.

Documentation was regenerated with roxygen2 directly from the compiled header after
stopping the slow package-wide `devtools::document()` run. The first targeted output
call invoked roxygen's stale-file cleanup on the repository manuals; all 1,560 removed
tracked files were restored, and the seven previously modified manual entries were
regenerated from their unchanged headers in an isolated output directory using
`roxygen2::roxygenize()` and the package's markdown settings. The final
manual status contains the same seven pre-existing changes plus `decisioncompare.Rd`,
with no deletions. All eight changed manuals parse, and the affected manual contains
the new requirements and corrected examples. No manual was edited by hand.
