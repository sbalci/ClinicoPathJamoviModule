# Interaction-Terms Rollout — tracking checklist

Roll out the Cox/regression **interaction-terms model builder** (built for `multisurvival`)
to all regression-style functions, in tier order. **GUI test gate after each function.**

## Reusable pieces (the proven template)
- **UI:** `jus: '2.0'` + `Supplier` (name `interactionSupplier`) with `events: update` →
  `onUpdate_interactionSupplier`; `explanatory`/`contexpl` boxes get
  `events: change` → `onChange_predictors`; analysis-level `events: update`.
- **Events JS** (`<fn>.events.js`): `this.valuesToItems` / `cloneArray` / `findChanges`
  pattern (works under jus 2.0; NOT jus 3.0).
- **Option:** `interactions` (`type: Terms`, `default:`) in `.a.yaml`.
- **Result tables:** `interactionTest` + `subgroupHR` in `.r.yaml`.
- **Pure helpers** (`R/multisurvival-interactions.R`, function-agnostic):
  `.mapInteractionTerms`, `.interactionTermsForFormula`, `.interactionTermsForFinalfit`,
  `.interactionModeratorInfo`, `.interactionTestTable`, `.computeSubgroupHRs`.
- **Backend:** inject `interaction_terms` into that function's formula builder + populate
  the two tables (guarded run-once). Fix any duplicated-`asSource` bug found.

Per-function `_updateModules_config.yaml`: add `multisurvival-interactions.R` to that
function's home module's `r_files` (and JamoviTest) if not already present.

## Process per function
1. Study the target (predictor options, formula builder, asSource).
2. Add `interactions` option (.a.yaml) + T-suffix menuGroup for testing.
3. Add jus-2.0 model-builder UI (.u.yaml) + events JS.
4. Inject interaction terms into the formula builder (.b.R).
5. Add interactionTest + subgroupHR tables (.r.yaml + .b.R) where applicable.
6. Fix asSource duplication if present.
7. prepare() clean; deploy (`_updateModules.R`).
8. **GATE: user tests in GUI → confirms before next function.**

## Order (CORRECTED — dropped single-predictor functions)

DROPPED (single predictor `type: Variable` — cannot form an interaction):
`survival` (explanatory), `survivalcont` (contexpl), `competingsurvival` (explanatory).

### Gate 0 — TEMPLATE ✅ confirmed working in GUI (2026-07-04); committed as 0f1f30b2.

### Tier A — Cox / competing-risks family (DIRECT reuse of HR helpers — easiest, high value)
- [ ] `finegray`             (covariates)  competing risks, coxph/finegray
- [ ] `causespecifichazards` (covariates)  competing risks, cause-specific Cox
- [ ] `flexcomprisk`         (covs)        flexible competing risks
- [ ] `coxphw`               (covariates)  weighted Cox
- [ ] `coxrobust`            (covariates)  robust Cox
- [ ] `firthregression`      (predictors)  Firth penalized Cox/logistic
- [ ] `flexparametric`       (covariates)
- [ ] `flexrstpm2`           (covariates)
- [ ] `stratifiedparametric` (covariates)
- [ ] `robustaft`            (covariates)
- [ ] `transformationmodels` (covariates)
- [ ] `frailtysurvival`      (covariates)
- [ ] `parametricfrailty`    (covariates)
- [ ] `multistatesurvival`   (covariates)
- [ ] `illnessdeath`         (covariates)
- [ ] `rmstregression`       (explanatory)
- [ ] `recurrentsurvival`    (covariates)
- [ ] `relativesurvival`     (covariates)
- [ ] `intervalsurvival`     (covariates)

### Tier B — logistic / non-Cox (needs OR/coef-adapted interaction + subgroup tables)
- [ ] `oddsratio`            (explanatory) logistic; OR-based, not HR
- [ ] `treatmenteffects`     (covariates)
- [ ] `causalmediation`      (covariates)

### Tier C — penalized / ML (interactions can be pre-specified; penalty selects — subgroup table maybe N/A; confirm per function)
- [ ] `lassocox`  · `adaptivelasso` · `grouplasso` · `ncvregcox` · `plscox` · `pcacox` · `lassologistic`

Total valid targets: **29** (was 33; 3 dropped, 1 done = multisurvival).

## Notes
- Each target's `asSource` should be checked for the same manual-arg + `.asArgs`
  duplication bug found in multisurvival/survivalcont; comment out the custom asSource
  if it duplicates.
- Tier 4 (penalized): the model builder can pre-specify interaction terms but the penalty
  selects among them — lower value; confirm with user whether to include the subgroup-HR
  table there.

## 2026-07-04 UPDATE — exclude D-suffixed functions (user directive)
Do NOT touch any function whose menuGroup ends in `D` (dev-held). This drops the
entire high-value Cox/competing-risks/parametric family (all SurvivalD/ClinicoPathD)
and coxphw (reverted). Remaining NON-D candidates (10, all already ...T):
- High value:  firthregression, relativesurvival, oddsratio(OR-adapted)
- Low value (penalized; interactions pre-specified, penalty selects):
  lassocox, adaptivelasso, grouplasso, ncvregcox, plscox, pcacox, lassologistic
Order: firthregression -> relativesurvival -> oddsratio -> (penalized 7).
Deploy step per NEW-to-JamoviTest function: _updateModules.R (copy) THEN prepare the
JamoviTest module (register .h.R + 0000.yaml). These 10 are already SurvivalT/meddecideT
so already in JamoviTest — lighter deploy.
