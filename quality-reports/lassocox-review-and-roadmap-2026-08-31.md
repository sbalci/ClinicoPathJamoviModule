# `lassocox`: evaluation and implementation priorities

Date: 2026-08-31. Scope: the current working-tree implementation, evaluated against the workflow illustrated by Yao et al., DOI 10.1007/s12029-026-01582-z. This is an assessment and proposed roadmap; no analysis implementation, schema, generated file, or existing test was changed.

**Recommendation:** retain `lassocox` as a focused penalized-Cox analysis. Fix its input and interpretation defects, add the missing coefficient-path display, and prioritize a shared model-development/validation layer over adding more separate modelling methods. It is a useful development component, but does not yet provide a complete, externally applicable clinical prediction model.

The article is an example of a workflow, not an implementation specification. It reports external validation; the gap discussed here is the module's ability to perform and reproduce that workflow. A LASSO Cox model, an unpenalized Cox refit after LASSO, and a survival forest using LASSO-selected inputs are three different models. They require separate predictions and validation. An RSF importance score is not a regression coefficient for constructing an additive nomogram.

## Review assessment

| Dimension | Assessment |
|---|---|
| Overall quality | 3/5: useful core with material safety and workflow gaps |
| Maintainability | Medium: fitting is separated into methods, but preprocessing provenance and reusable prediction interfaces are missing |
| Performance | Needs work for high-dimensional use; default correlation diagnostics allocate a dense square matrix |
| User experience | Good basic workflow; factor interpretation, labels, translated explanations, and reproducibility need improvement |
| Mathematical/statistical correctness | Core fitting agrees with a direct reference in the checked example; major input-safety issues and a confirmed contrast-description error remain |
| Clinical and release readiness | NEEDS_VALIDATION for research release; not a validated clinical risk calculator |
| Scoped code hygiene | Clean dependency/reference/state checks, with UI/i18n and lifecycle issues described below |
| Scoped lintr | Minor: 18 style findings, no AST bug-linter findings; R6 blind spots reviewed manually |

## Existing strengths to preserve

The implementation already has safeguards that should not be replaced by the article's less clear practices:

- **Honest penalty selection.** The requested `lambda.1se` rule is respected even when it selects no predictors. An empty model is preserved instead of silently switching to `lambda.min` ([fitting](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:659)).
- **Consistent penalized predictions.** Reported coefficients and risk scores use the same coefficient vector. `glmnet` handles standardization inside fitting; returned coefficients are on the original design-column scale. Breslow tie handling is explicit.
- **Correct discrimination orientation and labeling.** Higher scores mean worse prognosis; concordance uses `reverse = TRUE`. The displayed C-index is labeled apparent, not validated ([performance](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:761)).
- **No misleading routine post-selection inference.** Penalized coefficients are not presented with ordinary refit p-values or confidence intervals. The same-sample median-split survival curves are descriptive and omit a group-comparison p-value.
- **Useful reproducibility and error handling.** Event/censor-stratified folds, a user seed, RNG preservation, explicit missing-row exclusions, and visible captured warnings are already implemented.
- **Importance caveats.** `|beta| × SD(X)` is described as a scale-adjusted coefficient magnitude. Path inclusion is not labeled bootstrap selection stability. Preserve both distinctions.

## Confirmed defects and immediate improvements

Priority P0 means fix before extending the prediction workflow. P1 means the next substantive enhancement. The evidence distinguishes observed behavior from static inspection.

| Priority | Finding and evidence | Recommended change |
|---|---|---|
| P0 | **Outcome/time leakage is accepted by the backend.** `.cleanData()` takes explanatory columns without rejecting overlap with the time/outcome roles. In a synthetic 200-row example, adding follow-up time as a predictor gave apparent C = 0.996911; adding event status gave C = 0.788597. Both fitted without a warning. This establishes a backend/programmatic guard gap, not that the normal variable-picker UI necessarily permits it. [Input roles](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:271) | Reject overlap between predictors and outcome/time before preparing the matrix. Reject using the same column for time and outcome. Explain that predictors must be available at the intended prediction time; role checks cannot detect differently named post-outcome variables. |
| P0 | **An infinite predictor can be discarded as “constant.”** The variance check treats `NA`/`NaN` variance as constant, before the later infinite-matrix guard. One `Inf` in a third predictor caused the whole predictor to be removed, with the inaccurate warning “Removed constant explanatory variables: bad”; the model then fitted the other two. [Constant screening](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:385) | Check non-finite numeric values before variance screening. Distinguish invalid infinity, all-missing columns, constant columns, and ordinary row missingness. Report their handling separately; do not silently redefine the candidate model. |
| P0 | **Ordered-factor encoding contradicts the explanation.** An ordered three-level grade produced `grade.L` and `grade.Q` under default polynomial contrasts, while the function warned that categorical inputs were represented by indicator columns. The coefficient notes also imply reference-category HRs. [Matrix construction](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:470), [notes](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:886) | Define an explicit encoding policy. Use treatment contrasts for nominal categories; for ordinal variables, let users deliberately select categorical versus trend encoding. Store terms, contrasts, levels, reference category, and design-column assignments. Never describe a polynomial-contrast HR as a category-versus-reference comparison. |
| P1 | **Zero-time messaging conflicts with the fitting engine.** A zero follow-up time receives advice to add a small constant, then fitting fails because this Cox engine requires positive times. [Time checks](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:429) | Reject zero times early with an explanation tied to time origin and measurement resolution. Any correction must be explicit and scientifically justified; do not automatically perturb times. |
| P1 | **Candidate counts are ambiguous after factor expansion.** “Total Variables” and “Selected Variables” count design columns, not necessarily original clinical predictors. [Summary table](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:780) | Show original candidate predictors, encoded candidate parameters, selected columns, and selected original terms separately. Include the encoding/reference table in exported reports. |
| P1 | **Default diagnostics can undermine high-dimensional fitting.** `cor(X)` allocates a dense p-by-p matrix, followed by nested loops. At 10,000 columns the matrix alone needs approximately 0.8 GB; copies and sorting add more. This is a complexity estimate, not a measured benchmark. [Diagnostics](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/lassocox.b.R:1700) | Use sparse matrix preparation where suitable and bounded/blockwise correlation diagnostics. Set a transparent diagnostic size limit, add checkpoints, and avoid sorting every pair. Preserve model-matrix term assignments rather than guessing origins from name prefixes. |

Pairwise correlations also cannot rule out all multivariable linear dependencies. Change “No concerning collinearity detected” to a narrower statement about the correlations actually checked.

## Highest-value additions to `lassocox`

### 1. A real coefficient-path plot — small effort, immediate value

The existing `coef_plot` is correctly documented as a **bar plot at the chosen lambda**, not a regularization-path plot ([option](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/lassocox.a.yaml:150)). Add a separate plot showing coefficients across log(lambda), the two CV choices, the number of nonzero columns, and trace labels or an accompanying mapping table. This directly supports the LASSO figure used in the article.

Reuse the fitted CV path rather than fitting it again. A selected-lambda coefficient table and path table can share the same source. In the checked example, the current single-lambda refit differed from the CV-path coefficients by at most 0.00003296; this small numerical difference is not evidence of a substantive error, but using one fit would simplify consistency checks. Official `glmnet` documentation already provides coefficient-path and prediction interfaces, sparse input support, and penalty factors. [glmnet documentation](https://glmnet.stanford.edu/articles/glmnet.html)

### 2. Full-process validation — greatest statistical value

Add internal validation that repeats **preprocessing, lambda tuning, selection, and fitting** within each development resample. If the workflow uses a selected-variable Cox refit or RSF, that stage must also be repeated. The current CV curve tunes lambda; the subsequently reported training C-index is not an estimate of the full procedure's generalization performance.

Prefer one shared engine with explicit modes:

- Development bootstrap: refit the entire pipeline, evaluate each fitted pipeline in its bootstrap sample and original development sample, and estimate optimism for a defined metric.
- Nested CV: tune only within each outer training set and evaluate on untouched outer test rows. Predefine how metrics are aggregated; linear-predictor scales can differ between folds, so blindly pooling all fold scores is not always appropriate.
- External validation: apply a frozen training model; resample validation patients only when estimating validation-sample uncertainty. This does not require retraining and must not be called development optimism correction.

Output attempted/successful/failed resamples, effective folds, metrics with an explicit estimand and uncertainty method, seeds, and warnings for unstable or unsupported horizons. Do not promise that resampling makes a small dataset adequate. This separation of development, model availability, discrimination, calibration, and clinical usefulness is consistent with the reporting and assessment frameworks in [TRIPOD+AI](https://www.bmj.com/content/385/bmj-2023-078378) and [PROBAST+AI](https://www.bmj.com/content/388/bmj-2024-082505).

### 3. Genuine selection-stability output

Report predictor/column selection frequencies, coefficient-sign frequencies, model-size distributions, and lambda variability under a clearly specified resampling protocol. Label selection across repeated CV runs separately from fixed-penalty subsampling. A fraction of the regularization path is not a sampling probability.

Reuse ideas from `highdimcox`, but audit its protocol first. Its backend already supports elastic net/ridge/adaptive LASSO ([fitting](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/highdimcox.b.R:415)) and actual subsampling ([stability](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/highdimcox.b.R:510)). Its stability routine changes ridge to elastic net and does not carry the adaptive fit's weights into that routine. The comment claiming fixed lambda alone maintains theoretical FDR/FWER control should not be copied. These are source findings, not a full audit of `highdimcox`.

### 4. Frozen predictions and absolute risk

The current output is a development-row **linear predictor**, not a 1-, 3-, or 5-year event probability. Add a reusable prediction specification containing coefficients, lambda, column ordering, contrasts/levels, transformations, units, baseline survival, time origin, and version metadata. Provide explicit handling of unseen factor levels and missing predictors.

For a Cox model, horizon-specific survival requires a baseline survival estimate consistent with the fitted coefficients and centering convention. Verify it against an independent reference. Keep baseline estimation within development data, and distinguish untouched external validation from deliberate recalibration. Export only necessary prediction metadata; inspect model exports for retained patient data.

A generated R script, exact lambda/seed/fold provenance, and a complete model-specification table are useful early deliverables while cross-analysis model transfer is designed. Do not rely on serializing an arbitrary R6/model object into jamovi image state.

### 5. Clinically structured predictor handling

Add an option for prespecified required predictors using penalty factors, applied consistently across every resample. Consider nonlinear continuous effects with documented transformations, and optional user-supplied fold groups for center, patient, or temporal validation.

**Do not build duplicate group/elastic-net analyses.** `highdimcox`, `grouplasso`, `sparsegrouplasso`, `adaptivelasso`, and `ncvregcox` already exist. In particular, ordinary LASSO can select only some columns of a multilevel factor; whole-factor selection belongs in the existing group-penalty implementations or a shared backend. Their existence is established; their full clinical readiness is not established by this review.

## Other module gaps worth implementing, in order

| Rank | Gap | Existing code to repair or reuse | Minimum useful deliverable |
|---|---|---|---|
| 1 | **Trustworthy shared survival-model validation** | `survivalvalidation` has explicit bootstrap/external stubs; `survivalmodelvalidation` resamples fixed scores and labels the resulting difference “optimism.” `multisurvival` has a genuine fixed-formula Cox refit helper that is a better starting point. | Separate full-pipeline internal validation from fixed-model external evaluation; correct score orientation, resampling units, and uncertainty. |
| 2 | **Frozen external prediction and horizon-specific calibration** | Integrate `lassocox` with repaired `survivalcalibration` and validation helpers. | Cox and supplied-model adapters; survival/risk at requested times; censoring-aware Brier/AUC; calibration curves and appropriate slope estimates; all requested horizons; sufficient-follow-up checks. |
| 3 | **A dedicated random survival forest analysis** | Genuine `randomForestSRC::rfsrc` exists inside `stagemigration`, but a staging comparison is an awkward sole entry point. Repair the stage-variable omission before extracting reusable code. | Standalone fit/predict with tuning, reproducible folds, survival probabilities, OOB performance clearly labeled, VIMP, and full-process validation. LASSO screening should be optional and assessed as a separate pipeline. |
| 4 | **Fixed risk cutoffs and paired model comparison** | Existing `survival` plots and staging/comparison tools provide components. | Derive cutoffs in development data only, freeze score definition/units/tie rules, apply unchanged externally, and export aligned groups. Compare models on the same validation patients using paired C/AUC/Brier differences and uncertainty. A smaller log-rank p-value is not a model-superiority test. |
| 5 | **Reproducible prediction reporting and planning** | Reuse current summaries and data preparation helpers. | Training/validation flow counts, candidate parameters, event definitions, missingness, complete model specification, software/fold provenance, and prediction-model sample-size planning. Add learned imputation only when it can run inside the resampling pipeline without leakage. |

Concrete source findings supporting the first three priorities:

- [survivalvalidation.b.R:315](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/survivalvalidation.b.R:315): bootstrap and external helpers return implementation-pending notes. These are missing implementations, not merely missing GUI options.
- [survivalmodelvalidation.b.R:383](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/survivalmodelvalidation.b.R:383): bootstrap operates on already-computed risk scores; the concordance formula also lacks an explicit higher-is-worse orientation. Fixed-score resampling can be appropriate for external uncertainty, but is not development-pipeline optimism correction.
- [survivalcalibration.b.R:294](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/survivalcalibration.b.R:294): silently caps requested bootstrap iterations at 100. Its Brier calculation excludes people censored before the horizon instead of implementing an IPCW estimator; the grouped slope is not a standard individual-level survival calibration slope. Repair these before advertising publication-ready calibration.
- [stagemigration.b.R:26514](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/stagemigration.b.R:26514): filters the proposed renamed stage predictor against the original dataset's names before creating it. In ordinary data this can omit stage from the forest, or return no model when no other covariates remain. This was verified by source inspection, not a complete forest runtime test.

Do not prioritize an “RSF points nomogram” obtained by treating VIMP as coefficients, automatic surveillance intervals inferred from retrospective risk groups, automatic switching away from an empty LASSO model, or naive post-selection confidence intervals. A direct forest prediction calculator is more defensible. A Cox/additive surrogate would be a separately named and validated model, with agreement assessed against the forest.

## Implementation sequence and acceptance criteria

1. **Safety and test repair:** role-overlap rejection; finite-value checks; explicit contrasts and term mapping; positive-time contract; update current-source test fixtures. Acceptance: invalid inputs cannot produce a plausible model, ordinal/nominal labels match encoding, and missing rows remain aligned.
2. **Reproducible LASSO output:** coefficient-path plot, encoding table, original-term versus column counts, penalty/fold provenance, generated R code. Acceptance: displayed coefficients, exported scores, and selected lambda agree with a direct `glmnet` reference, including empty-model and tied-time cases.
3. **Shared validation and frozen Cox prediction:** establish the model/prediction interface, full-process resampling, external evaluation, and censoring-aware horizon metrics. Acceptance: synthetic null data reveal the gap between apparent and held-out performance; external predictions do not change when external outcomes or cohort composition change; attempted resamples equal the requested count unless explicitly interrupted.
4. **RSF and fixed risk-group workflow:** extract/repair the existing forest component, attach the shared validator, then add fixed cutoffs and paired comparison. Acceptance: staging is actually included, tuning sees training rows only, predictions/group assignments are stable for an unchanged patient, and comparison resampling preserves patient pairing.

Use ordinary `.a.yaml` options, matching `.u.yaml` controls, `.r.yaml` tables/plots/output columns, and thin `.b.R` orchestration around tested helpers. Regenerate `.h.R` and documentation through repository tooling only when implementation begins. Keep educational output opt-in; data-error notices must remain visible independently of those toggles.

## Code hygiene, UX, and maintainability

- All three YAML schemas and the backend parse. All eight cited keys resolve with nonempty author/year. Runtime package namespaces are declared in Imports; `base::` is an automatic base dependency, not a missing-Imports defect.
- The scoped state-guard tool found zero unguarded image-state reads. The theme checker proposed zero changes. No library-loading calls or unpreserved global RNG changes were found in the backend.
- The `<<-` writes in warning/error closures update their enclosing method frame; they are legitimate and should not be mechanically replaced.
- Lintr reported 16 quote-style, one comma-style, and one infix-spacing finding. No findings came from the enabled high-value AST bug linters. `object_usage_linter` and `vector_logic_linter` do not reliably inspect these R6 methods; manual review found no additional proven undefined-variable or scalar/vector-logic defect. Unused interpretation helpers and locals remain cleanup candidates.
- The `.init()` hide/`.run()` restore pattern conflicts with the repository's declarative visibility guidance. Review it alongside interrupted/failed rerun behavior; plot-state tests alone do not validate the whole GUI lifecycle. The clear-output helper does not explicitly clear every explanatory HTML item, so stale-output behavior deserves a GUI regression check rather than an unverified bug claim.
- Long educational HTML passages are literal English instead of translatable messages. Translate whole sentences, finish Turkish wording, use sentence-case control labels, and remove leading “Show” where library guidance requires it. Change “HR = 2.0 (doubled risk)” to doubled hazard, not doubled event probability.
- Retain the opt-in summary, explanation, and clinical-guidance switches. Prefer an encoding/reference table and explicit “development only” versus “validated” labels over adding more prose panels.

## Verification performed and limits

The source was loaded into an isolated R environment with its generated base and the necessary existing utility helpers. This avoids claiming that an installed package or jamovi application was validated. No article-level patient data were available; the example below is synthetic and does not reproduce the paper's estimates.

Environment: R 4.6.0; jmvcore 2.7.38; survival 3.8.9; glmnet 5.0; lintr 3.4.0. jmvcore/survival emitted notices that they were built under R 4.6.1.

| Check | Observed result |
|---|---|
| Standard current-source fit, 200 synthetic patients / 137 events | Two coefficients selected at lambda.min; apparent C = 0.720637 |
| Direct reference using identical input, folds, penalty, standardization, and tie method | Lambda difference 0; maximum coefficient difference 0; maximum linear-predictor difference 0 |
| Global RNG state before/after fitting | Preserved |
| Ordered three-level predictor | `.L`/`.Q` columns, inconsistent with indicator-column warning |
| One infinite value in an added predictor | Whole predictor removed as “constant”; remaining model fitted |
| Time or status supplied as an explanatory variable | Accepted by backend; artificial apparent discrimination increased |
| Zero follow-up time | Warning followed by lower-level non-positive-time rejection |

Three existing test files were also attempted against the isolated source environment. **This was not a passing package test run.** The basic file had one failed expectation and two test errors, including `readDataset` callback errors in this isolated setup. The arguments file had errors in all 12 test blocks: examples include unsupported `expect_no_error(..., info=...)` arguments under the installed testthat version and an omitted required `censorLevel`. All seven release-fix tests skipped because ClinicoPath is not installed. These counts describe this limited harness, not a verdict that every test fails in the normal package environment.

Static test review additionally found obsolete release-fix expectations for replacing an empty 1-SE model with lambda.min ([test](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/tests/testthat/test-lassocox-release-fixes.R:87)). Update these to protect the current, safer empty-model behavior. Prefer tests that exercise current backend output and an independent reference over tests that merely reconstruct a formula inside the test itself.

No full `devtools::test()`, `devtools::check()`, module build, installed-package run, graphical rendering review, high-dimensional benchmark, or external-cohort prediction test was performed. The other-function findings above are targeted static checks, not full audits of those functions.

Related article review: [Yao 2026 statistical and implementation review](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/literature/Yao-2026-GI-NEN-postoperative-surveillance-citation-review.md).
