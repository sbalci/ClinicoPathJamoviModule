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

## Order

### Gate 0 — TEMPLATE
- [ ] `multisurvival` — confirm jus-2.0 build works in GUI (pool populates, treatment×biomarker builds, tables populate). THEN commit the multisurvival work as the stable base.

### Tier 1 — easiest ports
- [ ] `survival`
- [ ] `survivalcont`  (also fix duplicated asSource)

### Tier 2 — highest clinical value
- [ ] `oddsratio`
- [ ] `treatmenteffects`
- [ ] `causalmediation`
- [ ] `competingsurvival`
- [ ] `finegray`
- [ ] `causespecifichazards`
- [ ] `flexcomprisk`

### Tier 3 — survival-regression family
- [ ] `coxphw`
- [ ] `coxrobust`
- [ ] `firthregression`
- [ ] `flexparametric`
- [ ] `flexrstpm2`
- [ ] `stratifiedparametric`
- [ ] `robustaft`
- [ ] `transformationmodels`
- [ ] `frailtysurvival`
- [ ] `parametricfrailty`
- [ ] `multistatesurvival`
- [ ] `illnessdeath`
- [ ] `rmstregression`
- [ ] `recurrentsurvival`
- [ ] `relativesurvival`
- [ ] `intervalsurvival`

### Tier 4 — penalized / ML (interactions pre-specified, lower priority)
- [ ] `lassocox`
- [ ] `adaptivelasso`
- [ ] `grouplasso`
- [ ] `ncvregcox`
- [ ] `plscox`
- [ ] `pcacox`
- [ ] `lassologistic`

## Notes
- Each target's `asSource` should be checked for the same manual-arg + `.asArgs`
  duplication bug found in multisurvival/survivalcont; comment out the custom asSource
  if it duplicates.
- Tier 4 (penalized): the model builder can pre-specify interaction terms but the penalty
  selects among them — lower value; confirm with user whether to include the subgroup-HR
  table there.
