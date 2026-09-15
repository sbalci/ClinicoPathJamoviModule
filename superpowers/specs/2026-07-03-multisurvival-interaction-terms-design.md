# Design: Interaction Terms for Multivariable Survival Analysis (`multisurvival`)

**Date:** 2026-07-03
**Module:** jSurvival — Multivariable Survival Analysis (`multisurvival`)
**Author:** Serdar Balci (with Claude Code)
**Status:** Approved design, pending implementation plan

## 1. Motivation

A user (breast-cancer researcher) requested Cox proportional-hazards **interaction
terms** — e.g. `Surv(time, event) ~ Treatment * Biomarker` — for predictive-biomarker
analysis in randomized trials. The treatment-by-biomarker **interaction test** is the
standard method for asking "does the biomarker predict who benefits from treatment?"

**Current state (verified in code):** `multisurvival` does NOT support interactions in
any form.

- `explanatory` (factors) and `contexpl` (continuous) are the only predictor inputs
  (`jamovi/multisurvival.a.yaml:337,349`).
- The RHS is built purely additively: `formula_parts <- c(myexplanatory, mycontexpl)`
  (`R/multisurvival.b.R:2313`) and `rhs <- paste(escaped_predictors, collapse = " + ")`
  inside `.buildSurvivalFormula` (`R/multisurvival.b.R:337`). No `*`, no `:`.
- The R wrapper builds main-effects-only formulas too; interactions are not exposed
  anywhere.
- The online docs / README claim "Interaction Terms: Include interactions if needed" —
  **aspirational and inaccurate**; no such control exists. This is a documentation bug to
  fix regardless.

**Why interaction ≠ stratification:** stratification (`strata()`, already supported via
`use_stratify`/`stratvar`) gives each stratum its own baseline hazard but *forces a common
covariate effect across strata* — it cannot answer whether treatment benefit differs by
subgroup. Interaction estimates a coefficient for the crossed term and tests effect
modification. They answer opposite questions; stratification is not a substitute.

## 2. Scope (decided)

- **Builder generality:** full jamovi "Model-Terms" builder — users cross any 2+
  already-selected variables (2-way, 3-way, …), like jmv's own regression/ANOVA.
- **Output richness:** interaction terms appear in the existing HR table + forest plot,
  PLUS a dedicated **Effect-Modification (interaction test)** table AND a
  **Within-Subgroup Hazard Ratios** table.

Out of scope (YAGNI): time-varying / spline / frailty interactions (existing EXPERIMENTAL
blocks stay disabled); interaction terms on the KM (`surv_plot`) grouping.

## 3. Architecture

One new option feeds a single backend helper, which injects interaction terms at every
site that builds a Cox model or a finalfit call. All name mapping (display label → real
column name) and escaping is centralized so there is exactly one source of truth.

### 3.1 UI — Model-Terms builder (`jamovi/multisurvival.u.yaml`)

`explanatory` + `contexpl` already sit inside one `VariableSupplier` (u.yaml lines 7–40).
Add an interaction builder that draws from that same variable pool, following jmv's
ANOVA/regression pattern:

- Add a **Model Terms** target (`type: ModelTerms`, option `interactions`). If the "cross"
  action buttons require a `Supplier`/`format: term` wrapper, add the minimal wrapper.
- **No custom events JS for main effects** — main effects continue to come from
  `explanatory`/`contexpl`; the builder only assembles crossed terms.
- Because only already-selected variables are crossable, **marginality/hierarchy is
  enforced by construction** (cannot form `A:B` without A and B present as main effects).
- Reference: `vignettes/jamovi_u_yaml_guide.md`, `vignettes/jamovi_js_guide.md`.

### 3.2 Options (`jamovi/multisurvival.a.yaml`)

```yaml
- name: interactions
  title: Interaction Terms
  type: Terms
  default: null
  description:
    R: >
      Interaction (crossed) terms to add to the Cox model, built from variables
      already selected as explanatory or continuous explanatory variables. Each term
      tests effect modification (e.g. Treatment x Biomarker for predictive-biomarker
      analysis).
```

Constraint from CLAUDE.md: `type: Terms` must NOT carry unsupported sub-properties; keep
to `name/title/type/default/description`.

### 3.3 Backend — one helper, multiple injection sites (`R/multisurvival.b.R`)

Add a private helper `.buildInteractionTerms()` that:

1. reads `self$options$interactions` (list of labelled-name vectors),
2. maps each component labelled name → real column name (same `all_labels` mechanism used
   for `explanatory`/`contexpl`),
3. returns two parallel representations:
   - **escaped real-name terms** (`` `A`:`B` ``) for the Cox model formula,
   - **finalfit-name terms** (`A:B` in the same namespace `explanatory_formula` uses) for
     finalfit calls.

Injection sites (all traced in current code):

| Site | Location | Change |
|---|---|---|
| Cox model formula | `.buildSurvivalFormula` call, `:2316` | Extend `.buildSurvivalFormula` with `interaction_terms` arg, appended to RHS **without re-escaping** (already escaped by helper). |
| finalfit HR table | `explanatory_formula`, `:6300` | Append finalfit-name interaction terms to the vector. |
| finalfit `hr_plot` | `explanatory = formula2`, `:3848` | Append finalfit-name interaction terms. |
| `survminer::ggforest` | `:3995`/`:3997` | Automatic — reads `cox_model`, which now carries the terms. |
| KM `surv_plot` | `:4268` | Untouched. |

`.buildSurvivalFormula` extension: main-effect `predictors` keep going through the existing
`.escapeVariableNames` path; the new `interaction_terms` are concatenated onto the RHS as-is
(already escaped), before the `strata()` term. Guard: interaction terms only appended when
non-empty.

### 3.4 Within-subgroup HRs — relevel-and-refit (no new dependency)

For each **2-way** term `A:B` where the **moderator (second-listed variable) is
categorical**:

- For each level `b` of the moderator B: relevel `mydata` so B's reference = `b`, refit the
  **same full** Cox model (all covariates + all interactions), and extract the focal-A
  HR / 95% CI / p. This is the fully-adjusted effect of the focal variable within subgroup
  B = b.
- **Convention:** focal = first-listed variable, moderator = second-listed variable
  (documented in the UI hint and the output caption). To get the other direction, the user
  builds `B:A`.
- Works when focal A is categorical (one row per non-reference level) or continuous (HR per
  1 unit).
- **Skip subgroup HRs — report the interaction coefficient only, with an explanatory note —
  when:** the moderator is continuous, OR the term is 3+ way.

Rationale for relevel-refit over `emmeans`/manual contrasts: exact, transparent to
clinicians, uses only `survival` (no new dependency). Releveling changes only the
parameterization, not the fit, so refits are cheap and give identical model fit. The
resulting subgroup effects are the interaction model's *implied conditional effects*
(borrowing strength across subgroups for non-focal covariates) — the statistically
preferred estimand for predictive biomarkers, distinct from independent subgroup-only fits.
This distinction is stated in the output caption.

### 3.5 Output (`jamovi/multisurvival.r.yaml`) — serialization-safe tables

Two new `type: Table` items, `visible` only when `interactions` is non-empty:

- **Interaction (Effect-Modification) Test** — columns: term · HR · CI lower · CI upper ·
  p · interpretation.
- **Within-Subgroup Hazard Ratios** — columns: interaction · moderator level · focal
  effect · HR · CI lower · CI upper · p.

Both are `Table` (NOT `jmvcore::Notice`) to avoid the protobuf "attempt to apply
non-function" serialization trap documented in CLAUDE.md. Any explanatory prose uses the
existing HTML-message helper (`private$.addHtmlMessage`) already present in the file, or
table footnotes (`setNote`, limited HTML allow-list).

## 4. Guardrails & edge cases

- **Marginality** — auto-enforced by the builder (only selected variables crossable).
- **Factor levels** — moderator factor must have >=2 levels; otherwise skip subgroup HRs
  with a note.
- **Low subgroup event counts** — warn when a subgroup has few events (unstable HRs);
  reuse the existing EPV/low-event messaging style.
- **Refit convergence** — every relevel-refit wrapped in `tryCatch`; on failure, note it
  and continue (do not abort the whole analysis).
- **EPV** — the existing EPV check counts `length(coef(cox_model))`, so interaction-inflated
  models are flagged automatically; no change needed.
- **Competing risks (Fine-Gray)** — interaction terms flow through the formula into
  `finegray()`, so the model + HR table + forest plot support them; but **within-subgroup
  refits are disabled with a note** in Fine-Gray mode (weighted subdistribution refits are
  fragile).
- **Backward compatibility** — `interactions` defaults to `null`; existing saved analyses
  and all non-interaction code paths are unchanged. New tables are hidden when unused.

## 5. Documentation fix

Correct the "Interaction Terms: Include interactions if needed" claim to match reality.
Grep `README.Rmd` and `vignettes/` for the phrasing; update in-repo sources (the pkgdown
website regenerates from these). Add a short usage note describing the builder, the
focal/moderator convention, and the interaction test.

## 6. JamoviTest routing

Per CLAUDE.md, while `multisurvival` is under modification append a `T` suffix to the
`menuGroup:` line in `jamovi/multisurvival.a.yaml` (touch only `menuGroup`, leave
`menuSubgroup`). The user moves it back manually after testing.

## 7. Verification story

1. `jmvtools::prepare()` and `devtools::document()` complete with no errors/warnings.
2. `/generate-test-data` produces a dataset with a real Treatment×Biomarker effect.
3. Confirm the interaction row appears in the HR table and forest plot.
4. Confirm the interaction p-value matches `survival::coxph(Surv ~ Trt*Bio, ...)` run
   directly in R (same estimate/SE/p).
5. Confirm within-subgroup HRs match a manual relevel-and-refit in R.
6. Confirm competing-risks mode: interaction in HR table works, subgroup table shows the
   "disabled in Fine-Gray" note.
7. Confirm backward compatibility: with `interactions` empty, output is byte-identical to
   pre-change behavior for a representative analysis.

## 8. Files touched

- `jamovi/multisurvival.a.yaml` — add `interactions` option; `menuGroup` T-suffix.
- `jamovi/multisurvival.u.yaml` — add Model-Terms builder.
- `jamovi/multisurvival.r.yaml` — add two Tables.
- `R/multisurvival.b.R` — `.buildInteractionTerms()` helper; extend `.buildSurvivalFormula`;
  inject at HR-table / hr_plot sites; within-subgroup refit computation; populate new
  tables; guardrail notes.
- `R/multisurvival.h.R` — regenerated (do not hand-edit).
- `man/multisurvival.Rd` — regenerated (do not hand-edit).
- `README.Rmd` and/or vignette(s) — documentation fix.
- Test data + a validation script under `tests/`.

## 9. Non-goals

- No `emmeans`/`marginaleffects` dependency.
- No higher-order (3+ way) subgroup decomposition.
- No changes to time-varying / spline / frailty EXPERIMENTAL blocks.
- No refactor of unrelated `multisurvival` code.
