# LASSO Cox implementation: first batch

Implemented 2026-08-31, analysis version 0.0.4, module version 1.0.8.04.

This implements the safety and reproducibility portion of the
[review roadmap](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/quality-reports/lassocox-review-and-roadmap-2026-08-31.md).
It does not implement shared internal/external validation or RSF modelling.

## Changes

- Reject overlapping time/outcome/predictor roles before modelling.
- Reject infinite-valued and entirely missing predictor columns before constant
  screening. Distinguish ordinary complete-case exclusions from invalid inputs.
- Reject nonpositive follow-up times with an explicit time-origin explanation;
  do not advise automatic addition of an arbitrary constant.
- Use explicit treatment contrasts for nominal and ordered factors. Preserve
  original-term assignments, factor levels, references, and encoded columns.
- Add the optional predictor-encoding table and distinguish original predictor
  counts from encoded-column counts in the model summary.
- Use the full-data fit from the selected CV path consistently for coefficients,
  scores, and paths; preserve valid empty models.
- Add an optional coefficient-path plot with both lambda choices and nonzero
  counts. Limit display to 30 traces with a visible explanation; fitting still
  uses all retained columns.
- Add exact lambda values, seed, software versions, removed constants, and
  per-fold event/censor counts to an optional reproducibility table.
- Add executable upstream R code reproducing the recorded development analysis,
  including explicit factor levels, folds, and row-aligned scores. Quoted strings
  safely preserve unusual variable names; no patient values are embedded.
- Clear new outputs and stale summary/suitability HTML before reruns and on errors.
- Bound pairwise-correlation diagnostics at 500 encoded columns and use stored
  term assignments instead of variable-name-prefix heuristics.
- Add `Matrix` to Imports for sparse path summaries. Generated headers and help
  were regenerated through the compiler/roxygen tooling, not edited manually.

**Behavior change:** ordered factors previously followed default polynomial
contrasts. They now use categorical treatment contrasts. Custom/global contrasts
are intentionally overridden. This can change fitted results and must be stated
when comparing old and new analyses. Reuse of the CV-path fit can also produce
small numerical differences from the old separate single-lambda refit.

## Verification

A temporary package named `ClinicoPath` containing only the current `lassocox`
sources, generated bindings, required existing utility helpers, and bundled test
datasets was compiled, documented, and installed into a temporary library. The
working repository's unrelated analyses were not regenerated.

`jmvtools::prepare()` initially could not probe the desktop application's version
from this shell. The same official compiler's supported `--assume-app-version`
option was then used with version `28.2.0.0` read from the installed application's
metadata. Analysis, results, and UI compilation completed without errors.
`devtools::document()` and temporary-package installation completed successfully.

| Test file | Blocks | Passing assertions | Failures/errors/skips |
|---|---:|---:|---:|
| `test-lassocox-safety-provenance.R` | 6 | 66 | 0 / 0 / 0 |
| `test-lassocox-basic.R` | 5 | 9 | 0 / 0 / 0 |
| `test-lassocox-arguments.R` | 12 | 27 | 0 / 0 / 0 |
| `test-lassocox-release-fixes.R` | 7 | 23 | 0 / 0 / 0 |
| **Total** | **30** | **125** | **0 / 0 / 0** |

The new tests exercise leakage rejection, invalid input, explicit ordinal
encoding under conflicting global contrast settings, overlapping design-column
names, unusual quoted/backticked names in executable exports, missing-row
alignment, RNG preservation, both standardization settings, exact selected-path
coefficients, and a constructed null model that remains empty under the 1-SE rule.
They execute exported R code and compare lambda, coefficients, scores, folds, and
apparent C-index with backend results. New output clearing and null plot state
are also checked.

Older focused tests required repair: the breast-cancer `.rda` stores an object
named `breast_cancer_data`; required censor levels were omitted; some assertions
accessed a nonexistent nested `results` object; `expect_no_error()` does not accept
the supplied `info` argument; note contents are in `Note$note`; and old expectations
still allowed substitution of lambda.min for an empty 1-SE result. The lung option
tests now simulate an independently censored endpoint in memory because the
bundled stress fixture has only two censored observations. No bundled dataset was
modified.

The coefficient-path image was rendered through the generated result object's
renderer and visually inspected. The actual jamovi client options-panel harness
passed: `placeholder present = false`, correct analysis title, `errors = undefined`.
The sandbox initially prevented the harness's background server; the authorized
local test rerun outside the sandbox succeeded.

Scoped checks found no unguarded image-state reads, no theme-safety changes needed,
and no undeclared runtime packages. Lintr found only 18 style advisories and no
enabled AST bug-linter findings; its R6 scope limitations still apply. Handwritten
source passes `git diff --check`. The generated header retains 16 trailing-space
lines produced by the compiler's table template; these were not hand-edited.

Environment: R 4.6.0, jmvcore 2.7.38, glmnet 5.0, survival 3.8.9. This was not a
full repository `devtools::test()`/`devtools::check()` run, release build, clinical
validation, or a performance benchmark at thousands of predictors. Other existing
LASSO test files were not part of the focused passing run.

## Follow-on work

The next substantive step is a shared validation engine that repeats preprocessing,
tuning, selection, and fitting in development resamples, with a separate contract
for frozen-model external evaluation. Then add baseline survival/horizon-specific
predictions, calibration, RSF adapters, fixed risk cutoffs, and paired comparison.
Do not relabel the current apparent C-index as validated performance.

User-facing details and limitations:
[encoding and reproducibility guide](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/vignettes/jsurvival-lassocox-safety-and-reproducibility.md).
