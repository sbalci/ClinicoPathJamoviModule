# survivalPower review repairs — 2026-09-13

Applied the five findings from the [review](survivalPower-review-2026-09-13.md), together
with related reporting, UI, performance, localization and plot recommendations. The
historical review evidence is preserved. This report describes the subsequent repaired
implementation, whose [evidence directory](survivalPower-review-fixes-2026-09-13/) is separate.

## Applied fixes

| Finding | Repaired behavior | Independent acceptance evidence |
|---|---|---|
| R1: incorrect whole-study multi-arm events | A shared arm-allocation helper resolves the control and each experimental arm. Whole-study events sum all actual arms; comparison-effective events include one control/experimental pair and any cluster design effect. | The ten-arm detectable-effect example now displays **499 events**, matching independent integration of 498.7075 instead of the former 589. Three/ten arms, three allocation ratios and three HRs agree with independently integrated event probabilities. |
| R2: NI table predicts success from HR alone | The table reports calculated power in power mode and target power in planning modes. It describes the NI objective without promising success. Margin selection requires disease-specific historical and clinical justification. | N = 200, HR = 1 and margin = 1.25 now reports **26.1% calculated power** in the NI table. N = 10 and N = 1,000 are also covered. |
| R3: one-sided integration for two-sided family power | Fixed-design family power integrates the entire non-rejection region through a deterministic shared-normal-factor calculation. Superiority uses both tails; NI uses one. Cox uses its actual null/alternative event variances. | Independent multivariate-normal integration matches log-rank, Cox and NI cases at HR 0.6, 1 and 1.2 with unequal allocation. The ten-arm null example at family alpha 0.10 now displays **7.4%**, matching 7.374785%. |
| R4: unused HR blocks NI effect inversion | NI effect mode uses a fixed allowed search domain and ignores the unused input HR. Its effect-type/value and related RMST controls are disabled. Other calculation modes retain the effect/margin feasibility guard. | Starting HRs 0.1, 1, 1.5 and 5 all return maximum true HR **1.011904** at N = 1,000. Sample-size, power and duration requests remain rejected when their assumed HR is outside the NI alternative. |
| R5: analytical integration changes RNG state | The joint normal integral uses deterministic one-dimensional quadrature, checks the estimated integration error, and reports an explicit notice if the accuracy requirement is not met. | Repeated analytical multi-arm results are identical. Both an existing `.Random.seed` and its absence are preserved. Simulation retains its separate configured, locally preserved seed. |

The [resolved design and allocation](../R/survivalPower.b.R:266),
[NI table](../R/survivalPower.b.R:1753), and
[fixed normal model and joint integration](../R/survivalPower.b.R:3684) contain the principal
repairs. No noncentrality is reconstructed from a rounded or two-sided marginal power.

For **sequential multi-arm designs**, the supplementary total-study-power cell is unavailable
and a visible notice explains that a validated joint arm-and-look calculation is required.
Per-comparison sequential power remains available. The fixed family calculation is explicitly
described as a normal approximation: Schoenfeld tests use allocation-based correlation;
Cox uses shared-control expected-event variance. Existing conservative Bonferroni treatment
of the Holm/Dunnett selections remains disclosed.

The primary method references were checked against the official
[gsDesign survival documentation](https://keaven.github.io/gsDesign/reference/nSurv.html).
Independent reference integration uses `mvtnorm`, whose
[documentation](https://search.r-project.org/CRAN/refmans/mvtnorm/html/pmvnorm.html) describes
two-sided rectangular bounds and the randomized default algorithm replaced in the backend.

## Recommendations implemented

- **Consistent NI reporting:** effect/duration tables display the known input enrollment
  instead of “Varies by analysis type.” The superiority-event comparison uses the resolved
  HR, so an unused starting value cannot change the table. At HR = 1, it explicitly says
  that a matching superiority requirement is undefined, rather than substituting HR 0.75.
- **Repeated numerical work:** [memoization](../R/survivalPower.b.R:2877) caches numerical
  results by arguments and relevant design options, with a bounded cache. Sample-size and
  sensitivity plots reuse the same calculation path and curve data. Baseline sensitivity
  calculations and sequential information designs are reused. Changed dropout and changed
  explicit inputs have separate cached results, covered by regression tests.
- **Responsiveness:** analytical grids and sequential Cox inversions now checkpoint as
  well as the existing simulation loop. The sensitivity alpha floor now matches the
  schema minimum of 0.001 instead of increasing a smaller alpha to 0.01.
- **Translatable reports:** the [copy-ready report](../R/survivalPower.b.R:2449) uses complete
  sentences with named placeholders. Objective sentences contain the appropriate sidedness
  directly; an English alpha fragment is no longer inserted into a translated objective.
  Formatting delegates to `jmvcore::format`.
- **TR/EN catalogs:** all **54 explicitly marked backend message IDs** are present in the
  catalog and English/Turkish catalogs, with nonempty Turkish translations and matching
  placeholders. This includes the previously missing notices, the repaired NI table,
  family-power explanations and report sentences. Severity prefixes have no trailing spaces.
  Existing nonempty translations were preserved. A runtime translation test checks that
  a Turkish NI report retains the HR, margin and calculated power without untranslated
  significance-level text or unresolved placeholders.
- **Plot readability:** plots use a blue/orange palette; survival curves also differ by
  line type and share one legend. Light/dark rendering was inspected at 800 × 600 pixels
  with a 12-point theme. The timeline's meaningless vertical numeric labels remain hidden
  after the supplied theme is applied. NULL-state and older power-plot-state behavior remains
  supported. The analysis continues to respect the app's supplied theme.
- **Cleanup:** the three unused locals identified in the review were removed. Extracted
  R6-method usage checks found no remaining unused-local candidates.

Localization here covers the repaired notices, NI reporting and complete report templates.
Legacy educational HTML and some other labels still contain English; this is not a claim
of complete localization of the analysis or the entire module. The existing glossary,
guided help and optional narratives remain in place, with narrative defaults off.

## Performance

Three paired warmed R runs compared the timestamped pre-repair backend and repaired backend
in the same process. Sample sizes and event counts matched in every pair.

| Scenario | Previous median | Repaired median |
|---|---:|---:|
| Default log-rank | 0.030 s | 0.038 s |
| Fixed Cox | 0.042 s | 0.045 s |
| Cox with five interim analyses and Pocock spending | 3.021 s | 2.830 s |
| Same Cox design with sensitivity | 8.088 s | 4.299 s |

The complex sensitivity case is approximately **47% faster**. Basic designs acquire a
few milliseconds of cache bookkeeping. These are local analysis-execution measurements,
not GUI latency guarantees. [Individual measurements](survivalPower-review-fixes-2026-09-13/paired-benchmarks.csv)
and the [verified summary](survivalPower-review-fixes-2026-09-13/benchmarks.log) are retained.

## Validation

- **1,201 assertions across 114 test blocks in 11 survivalPower files pass**, with zero
  failures, errors, skips or test warnings. The new
  [review regression file](../tests/testthat/test-survivalPower-review-fixes.R) contributes
  **296 assertions**. Existing tests were corrected where they assumed a one-sided joint
  rejection region, an invented HR for a superiority comparison, or exactly the simulation
  loop's checkpoint count despite additional analytical checkpoints.
- **Six rendering-contract assertions pass.** All 24 declared outputs populate in the
  applicable scenarios. All five renderers pass valid, light/dark and NULL-state checks;
  the older saved power-plot state also renders. Eight reference keys resolve.
- The prescribed scoped review linters find **no real-bug-linter findings**. The remaining
  entries are 13 brace-style and three semicolon-style advisories. R6 method usage and
  scalar `if`/`while` logic were checked separately. No missing option, unread option,
  missing qualified-package Import, raw non-ASCII backend character, trailing whitespace
  in a marked translation, or marked message absent from the Turkish catalog was found.
- `jmvtools::prepare(home = '/Applications/jamovi.app')` completed successfully in the
  isolated package with `ELECTRON_RUN_AS_NODE` unset. Sandbox access to the app was
  insufficient, so the authorized preparation ran outside that sandbox. The successful
  compiler log contains no warning/error diagnostics.
- `devtools::document()` regenerated help in the same isolated package. Only the target
  generated `survivalPower.h.R` and `survivalPower.Rd` were copied back; generated files
  were not hand-edited. The prepared schema/base files and final backend match the
  corresponding workspace files.
- Namespace loading/documentation emitted the pre-existing glmmTMB/TMB version mismatch
  and XQuartz connection warnings. These are separate from the zero warnings in the
  focused test cases. No full-module release check or live jamovi installation was performed.

Key artifacts: [verification totals](survivalPower-review-fixes-2026-09-13/verification-summary.json),
[per-test results](survivalPower-review-fixes-2026-09-13/regression-results.csv),
[full regression log](survivalPower-review-fixes-2026-09-13/regression.log),
[compiler log](survivalPower-review-fixes-2026-09-13/prepare.log),
[documentation log](survivalPower-review-fixes-2026-09-13/document.log),
[plot PDF](survivalPower-review-fixes-2026-09-13/rendered-plots.pdf), and
[source fingerprints](survivalPower-review-fixes-2026-09-13/source-fingerprints.csv).

## Reproduce and restore

After preparation and package loading, run the focused tests:

```r
.libPaths(c(.libPaths(), '/Applications/jamovi.app/Contents/Resources/modules/base/R'))
pkgload::load_all(helpers = FALSE)
testthat::test_dir('tests/testthat', filter = '^survivalPower', stop_on_failure = TRUE)
testthat::test_file('tests/testthat/test-zzz-results-rendering-contract.R')
```

From the repository root, these commands write new evidence without overwriting the
historical review/repair directories:

```sh
Rscript development-scripts/review_survivalPower_cases.R development-ideas/survivalPower-review-fixes-2026-09-13
Rscript development-scripts/validate_survivalPower_fixes.R development-ideas/survivalPower-review-fixes-2026-09-13
Rscript development-scripts/review_survivalPower_static.R development-ideas/survivalPower-review-fixes-2026-09-13
```

The [paired benchmark script](../development-scripts/benchmark_survivalPower_review_fixes.R)
takes the timestamped backup directory as its argument. Source backups preserve the
pre-existing working-tree changes and are located at:

```text
/tmp/survivalPower-review-fixes-backup-20260913-142504/
```

They mirror relative paths with a `.bak` suffix. Restore individual files from those
backups only if intentionally reverting this repair; subsequent edits could otherwise
be overwritten. No commit, push or production installation was made. The menu remains
in `PowerT` for development testing.

The confirmed review defects are repaired within the offered scope. Cluster designs remain
explicit approximations, and simulation remains limited to the supported fixed two-arm
exponential designs. Clinical use of complex designs still needs design-specific validation.
