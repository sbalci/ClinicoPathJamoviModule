# Code review: survivalPower — 2026-09-13

The repaired implementation passes its existing regression suite, but independent review
found five remaining issues. Whole-study event reporting and a non-inferiority conclusion
are misleading; supplementary multi-arm power uses the wrong rejection region; NI effect
inversion rejects a feasible request; and analytical multi-arm calculations alter the
caller's random-number state. Address these before unrestricted release.

This review follows the [review-function playbook](../.claude/commands/review-function.md)
and repository guidance. It examines the current working-tree implementation after the
[previous repairs](survivalPower-fixes-2026-09-13.md). No analysis implementation or
generated files were changed in this review. New evidence and reproduction scripts are
separate from the historical audit and repair records.

| Dimension | Rating | Basis |
|---|---|---|
| Overall quality | **3/5** | Substantial validation and useful safeguards, with remaining numerical/reporting defects. |
| Maintainability | **MEDIUM** | Shared calculation helpers help consistency; a large backend and repeated design derivations still permit contradictory outputs. |
| Performance | **NEEDS_WORK** | Basic designs are fast; a six-look Cox design with sensitivity takes about eight seconds in the measured run. |
| User experience | **NEEDS_WORK** | Clear controls, explanations and notices; contradictory NI wording, an unnecessary validation barrier and partial localization remain. |
| Mathematical/statistical correctness | **MAJOR_ISSUES** | R1 and R3 affect reported event counts and supplementary family power. Existing two-arm checks still pass. |
| Clinical and release readiness | **NOT_READY** | Resolve the confirmed defects in the offered modes before unrestricted release; this is a software review, not clinical/regulatory certification. |
| CRAN compliance — scoped hygiene only | **MINOR** | One confirmed RNG side effect; no package-wide CRAN assessment performed. |
| Static analysis — prescribed lintr set | **CLEAN** for real-bug linters | Seventeen non-blocking style findings; R6 scope blind spots checked separately. |

## Findings and recommendations

### R1 — High: whole-study multi-arm event totals use two-arm allocation

**Locations:** [resolved design](../R/survivalPower.b.R:270),
[public effect-size table](../R/survivalPower.b.R:1644).

`.resolved_design()` calculates events for total enrollment using a two-arm allocation,
then divides by the design factor for comparison-effective events. For more than two arms,
the whole-study total therefore gives the control arm too much weight when experimental
and control event probabilities differ. The comparison-effective total is correct in the
independently checked cases; correcting the whole-study total must preserve that distinction.

Reproduction: multi-arm effect-size mode, 10 arms, N = 1,000, equal allocation, target power
80%, Bonferroni adjustment, and other defaults. The solved HR is **0.513607**. The public
table displays **589 events** (unrounded 588.6465), while independent integration gives
**498.7075 events**, which rounds to **499**. A three-arm example displays 651 instead of
634. The accompanying note explicitly calls the first number the whole-study event count.

For control:each-experimental allocation ratio `r`, `k` total arms and event probabilities
`qC` and `qE`, derive both totals from the actual arm sizes:

```r
nE <- N / (r + k - 1)
nC <- r * nE
whole_events <- nC * qC + (k - 1) * nE * qE
comparison_events <- nC * qC + nE * qE
```

**Recommendation:** centralize actual arm allocation and calculate these two totals
separately. Do not divide the corrected whole-study total by the old multi-arm factor.
Add public-table checks for three and ten arms, unequal allocation, and HRs below/above one.
Evidence: [displayed effect tables](survivalPower-review-2026-09-13/multiarm-effect-table.json),
[independent event integrals](survivalPower-review-2026-09-13/multiarm-events.csv).

### R2 — High: NI table predicts success without considering power

**Location:** [true-HR interpretation](../R/survivalPower.b.R:1785).

The table says **“Should demonstrate non-inferiority”** whenever the assumed HR is below
the NI margin. This is an alternative-hypothesis condition, not evidence that the design
has adequate power. With HR = 1, margin = 1.25, one-sided alpha = 0.025 and N = 200, calculated
power is only **26.147%**, yet that sentence is displayed. At N = 10, it still appears with
**4.800%** power. The regulatory table and low-power notices correctly identify insufficient
power, leaving directly contradictory outputs in the same analysis.

**Recommendation:** use neutral, design-specific wording, such as “The assumed HR is below
the NI margin; calculated power is 26.1%.” In planning modes, distinguish the target from
calculated power and describe the objective rather than promising the study's conclusion.
Test the NI table together with notices and summary at low and adequate power. Also replace
automatic “conservative/moderate/liberal” margin labels with a requirement for
disease-specific justification, consistent with the existing NI assumptions explanation.
Evidence: [NI output snapshots](survivalPower-review-2026-09-13/ni-low-power.json).

### R3 — Medium: supplementary two-sided family power uses a one-sided rejection region

**Locations:** [integration helper](../R/survivalPower.b.R:3737),
[multi-arm table and explanatory note](../R/survivalPower.b.R:1862).

`.disjunctive_power()` reconstructs a noncentrality using `zcrit + qnorm(pairwise_power)`
and integrates only an upper-tail union. Superiority pairwise power includes both tails.
Reconstructing one-sided marginal power does not recover the joint probability that any
two-sided comparison rejects, although the table describes exactly that probability.

Under the null HR = 1, equal allocation gives correlation 0.5 under the implementation's
normal model. The reference probability is
`1 - P(-z <= Z_i <= z for every comparison)`. A deterministic one-dimensional integral
using the shared normal factor gives:

| Total arms | Family alpha | Displayed total-study power | Two-sided normal reference |
|---|---:|---:|---:|
| 3 | 0.05 | 4.5% | 4.64734% |
| 10 | 0.05 | 3.7% | 3.93129% |
| 10 | 0.10 | 6.8% | 7.37479% |

The reported discrepancy exceeds both table rounding and integration error. The reference
integrals have estimated absolute errors below `8e-11`. These are checks of the stated
asymptotic model, not claims about exact finite-sample survival-test probabilities.

**Recommendation:** for fixed two-sided designs, calculate noncentrality from the actual
model and integrate the full symmetric non-rejection rectangle; retain the appropriate
one-sided region for NI. For sequential multi-arm designs, validate the joint arm-and-look
calculation or withhold this supplementary metric with an explanation. A marginal
sequential rejection probability alone does not define its joint distribution across arms.
Retain null and non-null independent reference cases. The official
[mvtnorm documentation](https://search.r-project.org/CRAN/refmans/mvtnorm/html/pmvnorm.html)
supports arbitrary lower and upper integration bounds.
Evidence: [family-power comparisons](survivalPower-review-2026-09-13/multiarm-two-sided-power.csv).

### R4 — Medium: unused starting HR blocks feasible NI effect-size inversion

**Locations:** [input validation](../R/survivalPower.b.R:555),
[second guard in NI calculation](../R/survivalPower.b.R:1230).

The requirement that input HR be below the margin applies before branching on calculation
mode. In effect-size mode, the HR is the quantity being solved, so this check rejects a
feasible request based on an irrelevant starting assumption. With N = 1,000, margin = 1.25,
target power 80% and one-sided alpha 0.025, starting HR = 1 produces a maximum true HR of
**1.011904**. Changing only the input HR to 1.5 stops calculation with “Effect Not Below
Non-inferiority Margin.”

**Recommendation:** apply the assumed-effect/margin feasibility guard only to modes
that consume the assumed effect; use the margin and allowed search domain to bound the
effect inversion. Disable the unused HR input in NI effect mode. Verify that both starting
values yield the same solved threshold, while invalid assumed effects remain blocked in
sample-size, power and duration modes.
Evidence: [paired NI requests](survivalPower-review-2026-09-13/ni-effect-starting-value.json).

### R5 — Medium: analytical multi-arm calculations alter global RNG state

**Location:** [unseeded multivariate integration](../R/survivalPower.b.R:3759).

With simulation disabled, running a five-arm analysis changes `.Random.seed`. The default
`mvtnorm::pmvnorm()` Genz–Bretz algorithm is randomized; its dependence on the RNG is
documented in the official
[function reference](https://search.r-project.org/CRAN/refmans/mvtnorm/html/pmvnorm.html).
The analysis can therefore affect later random operations and produce slightly different
supplementary power estimates on reruns. The simulation branch correctly preserves RNG
state; that protection does not cover this earlier analytical call.

**Recommendation:** use deterministic quadrature for this equicorrelated problem, which
also offers an efficient solution to R3, or use a controlled integration seed that restores
the caller's state. Inspect the integration error estimate instead of dropping attributes
with `as.numeric()`. Check both existing-seed and absent-seed cases and repeated outputs.
Evidence: `multiarm_rng_preserved: false` in
[runtime measurements](survivalPower-review-2026-09-13/performance.json).

## Strengths and implementation checks

- The earlier repairs have meaningful acceptance tests covering independent event
  integration, event timing during accrual, numerical inversions, effect-scale conversion,
  whole-cluster allocation, upstream Cox probabilities and injected simulation failures.
- Fixed and sequential Cox calculations use a shared numerical path. The installed
  upstream source was checked alongside the primary
  [nSurv documentation](https://keaven.github.io/gsDesign/reference/nSurv.html) and
  [gsSurvPower documentation](https://keaven.github.io/gsDesign/reference/gsSurvPower.html).
  Unsupported methods and missing sequential APIs have explicit guards.
- [Resolved-design assessment](../R/survivalPower.b.R:298) reports low calculated power,
  sparse expected events and the limits of simple cluster inflation. The NI contradiction
  in R2 is a remaining table branch, not absence of these safeguards.
- Results use fixed row keys initialized before population. All five image callbacks
  guard NULL state. Errors use visible notices instead of hiding failed outputs. HTML
  uses inherited text color and a translucent instruction background; no prohibited
  opaque backgrounds or nonstructural named entities were found.
- The UI already has collapsed advanced panels, optional explanations, glossary, guided
  help and report sentences. Narrative options default off. Data-variable selection is
  unnecessary for this numeric planning analysis. Do not add duplicate summary/help outputs.
- Options referenced by the backend exist in the schema, and every declared option is
  read. All nine explicitly namespaced packages are in Imports. All eight referenced
  citation keys resolve with author/year metadata, and the module citation is first.
  The review-required menu group is already `PowerT`.

## Code hygiene and static analysis

The prescribed review linters were run, including non-default `sprintf`, unreachable-code,
duplicate-argument and missing-argument checks, with repository style suppression retained.
They found **no real-bug-linter findings**. The 17 remaining entries are 14 brace-style and
three semicolon findings, mainly compact helper expressions. They are non-blocking;
[the complete lint table](survivalPower-review-2026-09-13/lintr.csv) records their positions.

Because `object_usage_linter` and `vector_logic_linter` cannot analyze the R6 method bodies
reliably, private methods were separately passed to `codetools::checkUsage`, their candidate
messages reviewed, and the parsed syntax tree inspected for single `&`/`|` in scalar
`if`/`while` conditions. No unresolved variable or scalar-vector-logic defect was identified.
Three unused locals remain: `hr` in
[regulatory considerations](../R/survivalPower.b.R:2524), and `analysis_type`/`power` in
[the natural-language summary](../R/survivalPower.b.R:4257). Removing these is a small cleanup.

The real scoped hygiene issue is R5. No package-source `library()` calls, hardcoded
`set.seed()`, unrestored `par()` changes, missing Imports, raw non-ASCII backend characters,
or missing generated-help return section were found. The following are legitimate:

- [`withr::local_seed()`](../R/survivalPower.b.R:3942) preserves simulation RNG state and
  uses the configured seed; it must not be removed.
- Warning-handler [`<<-` assignments](../R/survivalPower.b.R:4022) update counters in the
  enclosing method frame, not `.GlobalEnv`.
- Extracted-method warnings about injected `self`/`private` and tidy-evaluation `.data`
  are framework-context false positives, not undefined application variables.

Package-wide `checktor`, CRAN metadata categories and full `R CMD check` are outside this
single-function review. No package-wide cleanliness claim is made.

## Performance, localization and clinician-facing improvements

Measured warmed R analysis runs on this machine, including construction and calculation
but not GUI interaction or device rendering:

| Scenario | Elapsed seconds |
|---|---:|
| Default log-rank | 0.033 |
| Fixed Cox | 0.086 |
| Cox, five interim analyses, Pocock spending | 2.904 |
| Same Cox design with sensitivity | 8.135 |
| Default design with 2,000 simulation replicates | 3.138 |

These are single-run measurements, not a formal performance profile. Power/sample-size
curves and sensitivity reuse similar grids but repeat inversions; impact formatting also
recalculates baseline scenarios. Cache numerical results by resolved design inputs, reuse
curve data, and add checkpoints in long analytical loops. Simulation already checkpoints
every 50 iterations. Preserve the shared calculation path and numerical acceptance tests
when optimizing.

Localization is **partial**, not a completed TR/EN workflow. The backend has 17 explicitly
marked message literals. Exact matching against parsed Turkish PO message IDs, including
multiline entries, confirms **14 are absent from the catalog**. Three marked severity
prefixes have trailing spaces, contrary to the i18n guide. Most legacy HTML, table notes
and explanatory text remains hardcoded English; the alpha phrase inserted into a translated
sentence is also constructed in English.

Complete translation using whole phrases and named placeholders, update the catalog, then
review Turkish medical terminology in context. Preserve existing optional report sentences
and ensure they use the same resolved quantities as tables. Replace the NI table's “Varies
by analysis type” sample-size placeholder in effect/duration modes with the known input N.
After correctness repairs, verify plot contrast and text sizes in both themes and use a
color-blind-safe palette where groups/curves need differentiation. Clinical presets should
remain explicit worked examples rather than recommendations for a real trial.

Cluster inflation remains an approximation requiring cluster-specific validation. Simulation
is limited to the supported fixed, two-arm exponential designs with uniform accrual.
Unsupported distributions/design combinations remain gated. These documented limits should
stay visible; extending the advertised scope needs independent operating-characteristic
validation, not just UI controls.

## Verification and reproducibility

- **904 assertions across 105 test blocks in 10 survivalPower files passed**, with zero
  failures, errors, skips or test warnings. The **six rendering-contract assertions passed**.
  Fresh logs: [regression](survivalPower-review-2026-09-13/regression.log),
  [rendering contract](survivalPower-review-2026-09-13/rendering-contract.log).
  [Verification totals](survivalPower-review-2026-09-13/verification-summary.json) and
  [per-test results](survivalPower-review-2026-09-13/regression-results.csv) are retained.
- The new review cases exercise gaps in those passing assertions: actual multi-arm
  whole-cohort events, two-sided joint rejection, contradictory NI table text, inversion
  independence from an unused input, and analytical RNG preservation. They record
  discrepancies rather than being added as passing regression tests.
- The prepared isolated package matches the reviewed backend, generated base, distributions
  helper, schemas, reference registry and Rd file byte-for-byte. The earlier successful
  prepare/document and visual plot checks remain applicable to these unchanged files;
  see the [repair evidence](survivalPower-fixes-2026-09-13/). Preparation/documentation and
  live jamovi GUI installation were not rerun during this read-only implementation review.
- Validation environment: R 4.6.0, jmvcore 2.7.38, gsDesign 3.11.0, mvtnorm 1.4.2,
  survival 3.8.11 and testthat 3.3.2. Namespace loading emitted pre-existing glmmTMB/TMB
  version and XQuartz connection warnings; lintr was built under R 4.6.1. These setup
  warnings are separate from the zero warnings in focused test cases.

From the repository root:

```sh
Rscript development-scripts/review_survivalPower_cases.R
Rscript development-scripts/review_survivalPower_static.R
```

The [case script](../development-scripts/review_survivalPower_cases.R) uses fast deterministic
one-dimensional quadrature for the reference family probability. The
[static script](../development-scripts/review_survivalPower_static.R) reproduces the lint,
R6 usage candidates, option/import/reference validation, PO coverage and source fingerprints.
Its final fingerprint assertion intentionally detects changes from the reviewed repair
baseline; revise the baseline explicitly when reviewing a subsequent implementation.

Re-run focused tests after loading the prepared package:

```r
.libPaths(c(.libPaths(), '/Applications/jamovi.app/Contents/Resources/modules/base/R'))
pkgload::load_all(helpers = FALSE)
testthat::test_dir('tests/testthat', filter = '^survivalPower', stop_on_failure = TRUE)
testthat::test_file('tests/testthat/test-zzz-results-rendering-contract.R')
```

The immediate acceptance work is R1/R2 public-output consistency, R3 joint probability,
R4 mode-specific validation, and R5 RNG isolation. Translation, performance and minor
cleanup can follow without changing the supported statistical scope.
