# `lassocox`: full audit

> Historical report for 0.0.4. The subsequent [0.0.5 repair report](lassocox-fixes-2026-08-31.md) records fixes and verification.

Date: 2026-08-31. Analysis version: **0.0.4**. Mode: **report-only**.

**Status: NEEDS WORK. Priority: Medium.** The core penalized fit is functional and
agrees with independent calculations. Four main findings remain: a valid
predictor name breaks optional Cox comparisons, the coefficient-path legend can
erase the plotting area, the suitability report misdescribes removed constants,
and older regression tests do not exercise the current interface correctly.
No critical core-fitting defect was demonstrated in this audit.

This evaluates the current working tree **after** the first implementation batch,
not the earlier implementation described in the initial roadmap. No production
source, schema, generated binding, existing test, or dataset was changed by this
audit. The only repository additions are this report and two evidence images.

## Scope and evidence

- **25 options:** all have an observable computational, display, validation, or
  output-activation effect. `data` and `riskScore` are partly framework-managed;
  their absence from direct `self$options$...` reads is not an unused-option defect.
- **22 results:** all are populated when applicable. No dead result declaration,
  permanently hidden result, or undeclared result reference was found.
- **19 input scenarios**, public-API differential runs, all four plot renderers,
  saved-score activation/alignment, and a 501-column fit were exercised.
- All **seven** LASSO test files ran: **201 passing assertions, 5 failing
  assertions, 8 errored test blocks, 1 skipped block**, across 77 test blocks.
- The module-wide rendering-contract file passed all **6 assertions in 4 blocks**.
  This is not a full module test or package-check pass.

The temporary installed package was named `ClinicoPath` and contained the current
LASSO backend, generated header, required existing helpers, schemas, and bundled
fixtures. Backend/header contents matched the working tree; the three extracted
helper function bodies matched `R/utils.R`. MD5 checks before and after the audit
confirmed no changes to the backend, header, utilities, or three LASSO YAML files.

Environment: R 4.6.0, jmvcore 2.7.38, glmnet 5.0, survival 3.8.9. Some installed
packages report that they were built under R 4.6.1. Scratch scripts, JSON results,
and test result objects are in `/private/tmp/lassocox-full-audit/`; this is a
temporary location, not a permanent release artifact.

## Ranked findings and recommended repairs

### 1. P2 — predictor `y` breaks both optional comparison rows

The comparison method evaluates `coxph(y ~ ., data = selected_X)` and a second
analogous formula on all columns. A legitimate predictor column named `y` masks
the intended `Surv` response in the formula's data environment.

**Reproduction:** on 180 synthetic observations, use explanatory columns
`y`, `z`, and three-level `grade`, select `lambda.min`, and enable
`showModelComparison`. Both comparison rows have missing C-index, AIC, and
log-likelihood, with the note “response must be a survival object.” Rename only
`y` to `marker`, preserving every value: both rows compute C = 0.7322,
AIC = 1127.2843, and log-likelihood = -559.6422. The penalized linear predictors
are identical between runs, with maximum difference **0**.

**Recommendation:** construct collision-safe internal predictor/response names
for these refits, following the protection already used in the PH diagnostic.
Preserve original labels separately. Add a rename-invariance regression check;
do not merely suppress the failure note.

Source: [selected refit](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:2262),
[all-column refit](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:2294).

### 2. P2 — high-dimensional coefficient-path output becomes unreadable

With 501 fitted columns, the intended 30 displayed traces generate a large legend
under the current jamovi plot theme. At the declared **750 × 500** image size,
the legend consumes nearly the entire panel: paths have essentially no vertical
plotting area, and explanatory text is clipped. A larger 900 × 650 rendering
still has a collapsed plotting area. The renderer returns `TRUE`, so a test of
render success alone misses this defect.

**Recommendation:** bound the legend's physical space as well as the number of
traces. Consider a compact legend, a separate column-to-color table, or fewer
visible traces; retain all columns in fitting. Specify a minimum usable panel
height and inspect the image at its declared dimensions. The ordinary coefficient
bar plot also has an unwrapped subtitle clipped at 600 × 400; wrap that text and
replace the raw `coefficient > 0`/TRUE/FALSE legend with a readable description.

Source: [path renderer](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:1190),
[coefficient renderer](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:1218).
Evidence: [501-column path image](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/quality-reports/lassocox-full-audit-2026-08-31/path-501-columns.png),
[clipped coefficient subtitle](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/quality-reports/lassocox-full-audit-2026-08-31/coefficient-subtitle.png).

### 3. P2 — the suitability panel forgets removed constant predictors

Setting candidate `x` to a constant correctly removes it and produces a visible
warning. However, the Data Quality row is green and says “Complete data with no
constant predictors.” The diagnostic checks variance only in the already-cleaned
design matrix, so it cannot discover the constants that cleaning removed.

**Recommendation:** use recorded preprocessing provenance, including
`removed_constants` and any columns removed after complete-case filtering.
Distinguish the submitted candidate set from the retained matrix. Report the
number/names removed and make the advisory row consistent with the top warning.
The retained model itself was fitted correctly in this scenario.

Source: [constant-quality check](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:2061).

### 4. P2 — remaining legacy tests do not provide a reliable release gate

Five assertions in `test-lassocox-edge-cases.R` fail, and all eight blocks in
`test-lassocox-integration.R` error before completing their assertions. These
failures must not be presented as thirteen independent statistical defects.

- Four edge assertions expect thrown errors/warnings although the backend now
  collects them into visible results. The separate scenario harness confirms
  rejection of invalid times/all-censored inputs and a warning for few events.
- The single-level-factor test embeds `paste0(...)` inside a captured predictor
  expression; the wrapper interprets it as a column name instead of testing the
  intended factor case.
- Two integration blocks load the breast-cancer fixture under an object name it
  does not contain and pass `NULL` as data. The stored object is
  `breast_cancer_data`, not the dataset-file name.
- Four integration blocks access `result$results$...`; the public wrapper returns
  the result group itself, so the correct access is `result$modelSummary`, etc.
- The genomic block similarly captures `gene_vars` as a literal variable name.
- The reproducibility block omits the required `censorLevel` argument.
- One older coefficient test is reported as skipped because its conditional
  branch executes no assertions when the selected model is empty.

**Recommendation:** repair fixtures and API usage, then assert actual contents,
notice visibility, numeric results, and legitimate empty-model behavior. Merely
checking that no R error is thrown is insufficient when errors are result HTML.

Sources: [edge tests](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/tests/testthat/test-lassocox-edge-cases.R:89),
[fixture lookup](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/tests/testthat/test-lassocox-integration.R:33),
[nested result access](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/tests/testthat/test-lassocox-integration.R:66),
[genomic argument](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/tests/testthat/test-lassocox-integration.R:178).

## Argument behavior matrix

Baseline: 180 synthetic rows, 140 events, continuous `x`, differently scaled `z`,
ordered three-level `grade`, and `noise`; explicit event `1`/censor `0`.
Comparisons used public analysis runs. Artifact: `differential.json`; Output
activation is recorded separately in `extra.json`.

| Option | Baseline → variation | Observed effect | Effective? |
|---|---|---|:---:|
| `data` | 180 → 160 rows | Model, scores, diagnostics and plots change | Yes |
| `elapsedtime` | `time` → `time^1.3` in `time2` | Survival time axis changes; Cox coefficients unchanged, as expected for preserved risk-set ordering | Yes |
| `outcome` | `status` → different binary `status2` | Event counts, model, scores and plots change | Yes |
| `outcomeLevel` | `1` → `0`, paired with censor reversal | Reverses event coding and changes fit; equal event/censor choices are rejected | Yes |
| `censorLevel` | `0` → `1`, paired with event reversal | Changes coding jointly; conflicting equal level is rejected | Yes |
| `explanatory` | Four predictors → `x`, `z` | Design dimension, selection, scores and plots change | Yes |
| `lambda` | `lambda.1se` → `lambda.min` | Selected coefficients, scores and summaries change | Yes |
| `nfolds` | 10 → 5 | Tuning path summary, selected fit and plots change | Yes |
| `random_seed` | 123456 → 425 | Fold allocation and fitted result change | Yes |
| `standardize` | True → false | Penalty behavior, fit and explanatory notes change | Yes |
| `suitabilityCheck` | True → false | Suitability output hidden and calculation skipped | Yes |
| `cv_plot` | True → false | CV image hidden; state not prepared | Yes |
| `coef_plot` | True → false | Coefficient image hidden; state not prepared | Yes |
| `survival_plot` | True → false | Survival image hidden; state not prepared | Yes |
| `path_plot` | False → true | Path state populated and image visible | Yes; layout defect above |
| `showEncoding` | False → true | Five encoded-column rows appear | Yes |
| `showReproducibility` | False → true | 21 provenance/fold rows appear | Yes |
| `showRCode` | False → true | Executable upstream R code appears | Yes |
| `riskScore` | Disabled → enabled | Framework Output becomes active; 180 values with NAs at excluded rows 2, 8, 41 | Yes |
| `showSummary` | False → true | Data-specific development summary appears | Yes |
| `showExplanations` | False → true | Four educational HTML panels appear | Yes |
| `showMethodologyNotes` | False → true | Methodology HTML appears | Yes |
| `includeClinicalGuidance` | False → true | Clinical caveats HTML appears | Yes |
| `showVariableImportance` | False → true | Three selected-column importance rows appear | Yes |
| `showModelComparison` | False → true | Two unpenalized comparison rows appear | Yes; name collision above |

Event and censor levels form one binary coding contract; changing either alone to
the other's current value is intentionally invalid. An unchanged penalized fit
under a strictly increasing time transformation is not an ineffective time option.
The Output control is a jamovi option, not a public R-wrapper argument.

## Output population matrix

All-enabled results are recorded in `all-outputs.json`. The setter method names
below refer to the current [backend](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R).
All tables match their declared column names/types. Model-dependent results list
the relevant fitting/coding options in `clearWith`; educational panels depend on
their display switches. Data changes are framework-managed.

| Result | Type | Population method / setter | Visibility and observed output |
|---|---|---|---|
| `todo` | Html | `.init`, `.run` → `setContent` | Welcome/errors/warnings at top; two coding notices in baseline |
| `suitabilityReport` | Html | `.generateSuitabilityHtml` → `setContent` | `suitabilityCheck`; populated advisory report |
| `modelSummary` | Table | `.populateModelSummary` → `addRow` | Core output; 13 rows |
| `coefficients` | Table | `.populateCoefficients` → `addRow`, `setNote` | Core output; 3 selected rows |
| `performance` | Table | `.populatePerformance` → `addRow`, `setNote` | Core output; apparent C-index row |
| `cv_plot` | Image | `.savePlotData` → `setState`; `.cvPlot` | `cv_plot`; rendered successfully |
| `coef_plot` | Image | `.savePlotData` → `setState`; `.coefPlot` | `coef_plot`; rendered, subtitle clips |
| `survival_plot` | Image | `.savePlotData` → `setState`; `.survivalPlot` | `survival_plot`; descriptive curves and at-risk table rendered |
| `riskScore` | Output | `.savePlotData` → `setValues` | Output activation controls export; row-aligned scores |
| `path_plot` | Image | `.savePlotData` → `setState`; `.pathPlot` | `path_plot`; rendered, 30-trace layout fails |
| `encoding` | Table | `.populateReproducibility` → `addRow`, `setNote` | `showEncoding`; 5 rows |
| `reproducibility` | Table | `.populateReproducibility` → `addRow`, `setNote` | `showReproducibility`; 21 rows |
| `rCode` | Html | `.generateRCode` → `setContent` | `showRCode`; nonempty export |
| `summaryText` | Html | `.populateSummary` → `setContent` | `showSummary`; fitted quantities and validation caveat |
| `lassoExplanation` | Html | `.populateLassoExplanation` → `setContent` | `showExplanations`; static educational text |
| `methodologyNotes` | Html | `.populateMethodologyNotes` → `setContent` | `showMethodologyNotes`; educational text |
| `clinicalGuidance` | Html | `.populateClinicalGuidance` → `setContent` | `includeClinicalGuidance`; development-use cautions |
| `variableImportance` | Table | `.populateVariableImportance` → `addRow`, `setNote` | `showVariableImportance`; 3 selected rows |
| `modelComparison` | Table | `.populateModelComparison` → `addRow`, `setNote` | `showModelComparison`; 2 rows, with explicit failure note when needed |
| `regularizationPathExplanation` | Html | `.populateRegularizationPathExplanation` → `setContent` | `showExplanations`; nonempty |
| `crossValidationExplanation` | Html | `.populateCrossValidationExplanation` → `setContent` | `showExplanations`; nonempty |
| `riskScoreExplanation` | Html | `.populateRiskScoreExplanation` → `setContent` | `showExplanations`; nonempty |

## Notices coverage matrix

The implementation uses the top `todo` HTML element, advisory HTML, and table
notes rather than native `jmvcore::Notice` objects. This is not automatically a
defect: the repository's current compiler/runtime guidance prohibits declaring
`type: Notice` in result YAML. Do not apply the playbook's older insertion snippets
mechanically. Evaluate whether the information actually reaches the user.

| Trigger | Effective severity / position | Coverage and assessment |
|---|---|---|
| Missing required inputs | Information, top | Welcome explains required fields; appropriate before a user has configured an analysis |
| Time/outcome overlap or use as predictor | Error, top | Tested: no model rows, actionable rejection |
| Zero, negative, infinite time | Error, top | Tested: rejected, no arbitrary time adjustment |
| Infinite predictor | Error, preflight/top | Public R API rejects through jmvcore validation; direct backend guard also covered by focused tests |
| Entirely missing predictor | Error, top | Tested: rejected instead of silently removing the candidate |
| Missing observations | Warning, top plus summary | Tested: 20 exclusions counted; score alignment checked separately |
| Constant predictor | Warning, top | Removal reported correctly; contradictory green advisory row is finding 3 |
| One outcome state / additional outcome state | Error, top | Tested: exact binary outcome contract enforced |
| Fewer than 10 complete rows | Error, top | Tested with 9 rows |
| Fewer than 3 events or censored observations | Error, top | Tested with 2 censored; explicitly described as computational requirement |
| Few events / reduced fold count | Warning, top | Five-event case fits with instability warning and folds reduced 10 → 5 |
| More columns than events | Warning, top | Tested in 501-column case; regularization not described as proof of adequacy |
| Ordered/categorical coding | Warning/information, top and encoding table | Explicit treatment coding and separate column selection stated |
| Empty selected model | Warning, top and coefficient/plot explanations | Preserved; focused regression test verifies zero scores and no forced substitution |
| PH concern or unavailable diagnostic | Advisory, near top | Selected unpenalized refit explicitly labeled; no guarantee of PH compliance |
| Correlation diagnostic too large | Advisory, suitability report | Tested: 501 columns triggers explicit 500-column cap, without dropping fitted columns |
| Optional comparison cannot fit | Contextual note under comparison | Present and specific; the `y` case is a real avoidable cause |
| Development-only interpretation | Table notes, summary and guidance | Apparent performance, post-selection limits, and unvalidated cutoffs stated |
| Missing required/optional packages | Top HTML / fallback | Source reviewed; dependency-removal scenarios not exercised |

Warnings about instability and routine coding share one visual warning class.
Distinguishing their urgency would improve usability. An arbitrary C-index cutoff
or a universal ten-event rule is not a justified reason to reject every model;
the existing separation of computational feasibility from adequacy should remain.

## Numerical and upstream comparison

The comparison used independent `glmnet::cv.glmnet` calls with identical encoded
inputs, explicit folds, `alpha = 1`, standardization choice, and Breslow ties.
Harrell concordance was also computed independently from comparable patient pairs
for the continuous-time synthetic dataset, avoiding reuse of `concordance()`.

| Check | Result |
|---|---|
| Synthetic baseline: 180 rows, 140 events, 5 design columns | Apparent C = 0.724276467524738 |
| Selected lambda versus direct glmnet | Absolute difference 0 |
| Penalized coefficients versus direct glmnet | Maximum absolute difference 0 |
| Linear predictors versus direct matrix multiplication | Maximum absolute difference 0 |
| Concordance versus independent pairwise calculation | Difference approximately 2.0 × 10^-15 |
| Rounded times: 147 duplicated time values | Lambda and coefficients still agree exactly with explicit Breslow reference |
| Upstream `CoxExample`: 1,000 rows, 30 predictors | Lambda and coefficients agree exactly; 13 model-summary rows |
| High-dimensional example: 180 rows, 501 predictors | Fit completed in 42.863 seconds; all 501 encoding rows retained; correlation cap reported |
| Generated R-code regression checks | Export executed and compared with backend for folds, lambda, coefficients, scores and C-index |
| Empty-model and scaling regression checks | Preserved empty 1-SE model; original-scale coefficients and scores verified |

The 43-second observation is one run, not a performance guarantee. The backend
still constructs a dense design matrix; thousands of predictors and full-process
resampling require separate memory, interruption, and runtime testing.

| Aspect | Local behavior | Upstream comparison / action |
|---|---|---|
| Interface | Time/status variables, factor expansion and UI options | Deliberate adapter around matrix `x` and survival response `y`; not a signature mismatch |
| Penalty | Fixed LASSO, `alpha = 1` | Upstream supports elastic net; exposing alpha is an enhancement, not an unused local option |
| Tuning | Deviance CV; default 1-SE selection; stratified folds | Explicit local fold policy; upstream also offers concordance tuning |
| Standardization | Per-fit glmnet scaling; original-unit displayed coefficients | Correct; no separate full-cohort scaling passed into CV training fits |
| Ties | Explicit `cox.ties = "breslow"` | Locks intended behavior across the announced default change |
| Outcome structure | Strictly positive times, exactly two observed outcome states, at least three of each for stratified CV | Narrower local workflow than the general upstream Cox engine; document as such |
| Prediction | Development linear predictor only | Not a baseline-survival estimate or an absolute-risk calculator |

Primary documentation checked: the [glmnet Coxnet vignette](https://glmnet.stanford.edu/articles/Coxnet.html)
and [glmnet reference](https://glmnet.stanford.edu/reference/glmnet.html). These
document the Cox interface, penalty/tuning options and scaling. The Coxnet vignette
also announces the Breslow-to-Efron default change planned for glmnet 5.1; the local
explicit Breslow argument is therefore worth preserving.

## Test results and engineering checks

| Test file | Blocks | Passing assertions | Failures | Errored blocks | Skipped blocks |
|---|---:|---:|---:|---:|---:|
| `test-lassocox-arguments.R` | 12 | 27 | 0 | 0 | 0 |
| `test-lassocox-basic.R` | 5 | 9 | 0 | 0 | 0 |
| `test-lassocox-edge-cases.R` | 12 | 7 | 5 | 0 | 0 |
| `test-lassocox-integration.R` | 8 | 0 | 0 | 8 | 0 |
| `test-lassocox-release-fixes.R` | 7 | 23 | 0 | 0 | 0 |
| `test-lassocox-safety-provenance.R` | 6 | 66 | 0 | 0 | 0 |
| `test-lassocox.R` | 27 | 69 | 0 | 0 | 1 |
| **Total** | **77** | **201** | **5** | **8** | **1** |

The four files repaired/added in the first implementation batch retain their
**125 passing assertions with no failures, errors, or skips**. The broader run
exposes older test maintenance that the first batch explicitly did not cover.

- All option/UI/result definitions align; analysis and panel titles match.
  `menuGroup: SurvivalT` is already routed to the test module.
- All eight citation keys resolve in `jamovi/00refs.yaml` with author/year fields.
- Runtime package namespaces are declared in Imports; `base::` is an automatic
  base dependency and not an omission.
- Scoped state-guard scan: **0 unguarded image-state reads**. Theme scan:
  **0 proposed changes**. No unsupported named HTML entities found.
- Module-wide rendering contract: **6/6 assertions pass**. Passing this limited
  contract does not prove every UI lifecycle or plot layout is correct.
- Generated header and schema remain consistent with the successfully compiled
  first implementation batch. No recompilation, documentation regeneration,
  full package check, or new desktop UI test was performed in this audit.

## Additional quality and lifecycle follow-up

These are lower-priority review concerns or unverified gaps, separate from the
four confirmed findings above:

1. `.init()` imperatively hides core results for missing inputs, while `.run()`
   restores only the tables. Reusing that same object through private methods
   leaves the three original plot elements hidden despite populated states.
   That is a backend lifecycle concern, **not proof that a normal desktop reload
   fails**. A proper serialized reload test was attempted but could not run because
   this R environment has no `RProtoBuf` package. Prefer declarative visibility
   and verify incomplete → complete → invalid → recovered states in the actual
   supported jamovi runtime.
2. Fixed result rows are generally constructed in `.run()`, contrary to the
   repository's preference for option-determined row creation in `.init()`.
   Dynamic selected-coefficient rows are a different case. This is a maintenance
   and lifecycle concern, not a numerical discrepancy demonstrated here.
3. Long educational HTML blocks remain literal English. Complete Turkish/i18n
   coverage and sentence-case, noun-based control labels remain unfinished.
   The unused `.interpretCindex`/`.interpretHazardRatio` helpers also retain
   qualitative threshold labels and should be removed or reconsidered before reuse.
4. Preserve guarded image states and clearing of tables, scores, summary,
   suitability and R-code output. Verify note clearing and final GUI visibility
   through saved-file tests; do not infer those from successful fresh R calls.
5. Test the optional `survminer` fallback and missing-dependency messages in an
   isolated supported runtime, without uninstalling packages from the user's
   working environment.

## Documentation and placeholder assessment

**Classification: FUNCTIONAL, not a scaffold.** Data and options drive actual
survival models, cross-validation, diagnostics and scores. Static educational HTML
is intentional. A zero-selected-variable model is a legitimate statistical
result, and a comparison row with an explicit failure note is not by itself a
placeholder. There is no declared but permanently unpopulated result.

Current schema help and the
[safety/reproducibility guide](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/vignettes/jsurvival-lassocox-safety-and-reproducibility.md)
describe the important recent changes accurately: treatment coding for ordered
factors, separate indicator selection, original predictor versus column counts,
preserved empty models, recorded folds, and development-only performance.
Older duplicate guides carry links to the new guide but still warrant eventual
consolidation. Migration notes should remain visible because changing ordered
contrasts and using the CV path can change results from earlier versions.

Two lower-priority explanation mismatches also need correction: the CV explanation
mentions nonzero counts at the top, but the rendered CV image does not display
them (the new path image does); the coefficient-summary explanation labels the
axes as variables on X and coefficients on Y, while the rendered horizontal bar
plot uses the reverse. These are instructional-text defects, not fitting errors.

## Reproducible compact audit harness

Use an installed package built from the current source. Dynamic predictor vectors
are passed to the generated Options class, avoiding formula-capture ambiguities
in the public convenience wrapper. This reproduces the principal numerical and
name-collision checks without relying on the temporary audit directory.

```r
library(ClinicoPath)
set.seed(818)
d <- data.frame(x = rnorm(180), z = rnorm(180, sd = 10),
  grade = ordered(rep(c("G1", "G2", "G3"), 60)), noise = rnorm(180))
event_time <- rexp(180, exp(1.2 * d$x - .05 * d$z + .7 * (d$grade == "G3")))
censor_time <- rexp(180, .35)
d$time <- pmin(event_time, censor_time)
d$status <- as.integer(event_time <= censor_time)

run_case <- function(data = d, changes = list()) {
  args <- modifyList(list(elapsedtime = "time", outcome = "status",
    outcomeLevel = "1", censorLevel = "0",
    explanatory = c("x", "z", "grade", "noise")), changes)
  opts <- do.call(getFromNamespace("lassocoxOptions", "ClinicoPath")$new, args)
  a <- getFromNamespace("lassocoxClass", "ClinicoPath")$new(options = opts, data = data)
  a$run()
  a
}
a <- run_case()
p <- a$.__enclos_env__$private
clean <- suppressWarnings(p$.cleanData())
fit <- suppressWarnings(p$.fitModel(clean))
ref <- glmnet::cv.glmnet(clean$X, survival::Surv(clean$time, clean$status),
  family = "cox", cox.ties = "breslow", alpha = 1, standardize = TRUE,
  foldid = fit$foldid)
stopifnot(isTRUE(all.equal(fit$lambda_optimal, ref$lambda.1se)),
  isTRUE(all.equal(unname(fit$coef_matrix), unname(as.matrix(coef(ref, s = "lambda.1se"))))))
comparable <- outer(clean$time, clean$time, "<") & (clean$status == 1)
delta <- outer(fit$risk_scores, fit$risk_scores, "-")
pairwise_c <- mean((delta[comparable] > 0) + .5 * (delta[comparable] == 0))
stopifnot(abs(pairwise_c - fit$performance_metrics$cindex) < 1e-12)

d$y <- d$x
bad <- run_case(d, list(explanatory = c("y", "z", "grade"),
  lambda = "lambda.min", showModelComparison = TRUE))
as.data.frame(bad$results$modelComparison) # currently missing all comparison metrics
good <- run_case(d, list(explanatory = c("x", "z", "grade"),
  lambda = "lambda.min", showModelComparison = TRUE))
as.data.frame(good$results$modelComparison) # same values, safe name, finite metrics
```

## Readiness and next implementation priorities

| Dimension | Assessment |
|---|---|
| File integration | Pass for current LASSO schema/backend/UI wiring |
| Statistical core | Agreement demonstrated in the tested configurations |
| User-visible validation | Substantial coverage; contradictory advisory and severity/lifecycle follow-up remain |
| User experience | Needs repair for high-dimensional paths and clipped plot text |
| Research release | **Not ready to sign off:** repair findings and restore a meaningful focused test gate |
| Clinical prediction/deployment | **No:** current outputs remain development-only |

After the local repairs, the highest-value addition remains a shared validation
and frozen-prediction interface. Internal resampling must repeat preprocessing,
penalty selection and fitting; external evaluation must apply a frozen model and
encoding. Then add baseline survival and horizon-specific absolute risk,
censoring-aware calibration/Brier/AUC, fixed external risk groups, and paired
model comparisons. Grouped or elastic-net penalties and a separate RSF adapter
can follow. These are missing capabilities, not alleged failures of the present
penalized-coefficient estimator.

The motivating article reports external validation. The gap is reproducing that
workflow in the module, not a claim that the paper omitted it. The present audit
uses synthetic and upstream example data, not the article's patient-level data.

### Acceptance checklist for the repair pass

- [ ] Predictor renaming, including `y`, cannot change optional comparison availability.
- [ ] Path plots with 5 and 30 traces retain a readable plotting region at declared dimensions.
- [ ] No plot subtitle/caption is clipped at the normal result size.
- [ ] Constant-removal notices, model provenance and suitability rows agree.
- [ ] All seven LASSO test files pass meaningful assertions, including valid empty models.
- [x] Explicit-fold upstream agreement, tied times, independent concordance and CoxExample checked.
- [x] Ordered-factor encoding, executable export, RNG preservation and row alignment checked.
- [x] All options and outputs exercised; input notices and 501-column diagnostic cap checked.
- [ ] Saved `.omv` reload/recovery and missing-package fallbacks verified in supported jamovi runtime.
- [ ] Full release build/check completed before release; no claim of clinical validation inferred from software tests.

No repairs were applied in this audit.
