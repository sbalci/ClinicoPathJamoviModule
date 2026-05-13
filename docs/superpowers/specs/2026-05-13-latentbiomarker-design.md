# `latentbiomarker` — Latent Biomarker Construct + Cox Regression

**Status:** Design approved 2026-05-13, awaiting user spec review
**Owner:** ClinicoPath / meddecide submodule
**Author:** Serdar Balcı (brainstormed with Claude)
**Type:** New jamovi function

---

## 1. Purpose & Scope

Estimate a **single reflective latent biomarker construct** from 3+ indicators (continuous, ordinal, or binary) using confirmatory factor analysis (`lavaan`), then relate the construct to a right-censored time-to-event outcome via Cox proportional-hazards regression (`survival`). The function auto-selects MLR estimation when all indicators are continuous and switches to WLSMV with polychoric/tetrachoric correlations when any indicator is ordinal or binary. Inappropriate inputs (insufficient sample size, under-identified models, formative composites, unconfirmed reflective assumption) are refused with explanations that describe what failed, why it matters statistically, and what the user should do instead. Factor scores can be optionally written back to the dataset for downstream use in `multisurvival` or other Cox workflows.

The function exists because `SEMLj` and `jAMM` do not handle the survival hand-off cleanly, and because clinical SEM workflows benefit from opinionated refusal of bad inputs more than from configuration flexibility.

### Non-goals (out of scope for v1)

- Multiple correlated factors — defer to `SEMLj` or a future `latentbiomarker2`.
- Bootstrap propagation of factor-score measurement uncertainty into Cox HRs (the Murphy–Topel two-stage problem). v1 ships with a prominent notice flagging that HR confidence intervals do not account for this; v2 may add an optional bootstrap pipeline.
- Continuous / binary outcomes — future siblings `latentbiomarker-continuous` and `latentbiomarker-binary`.
- Mediation chains — future `pathologymediation`.
- Latent class analysis for imaging-circularity workflows — future `imagingbiologyLCA`.
- User-typed lavaan syntax — that is `SEMLj`'s niche and we explicitly do not compete.

---

## 2. Architecture

Standard ClinicoPath 4-file jamovi pattern.

| File | Role |
|---|---|
| `jamovi/latentbiomarker.a.yaml` | Options/parameters |
| `jamovi/latentbiomarker.u.yaml` | UI layout |
| `jamovi/latentbiomarker.r.yaml` | Result definitions |
| `R/latentbiomarker.b.R` | Backend (R6 class inheriting from `latentbiomarkerBase`) |
| `R/latentbiomarker.h.R` | Auto-generated header (do not hand-edit) |

**New dependencies (DESCRIPTION):**

- `lavaan` (Imports) — CFA estimation
- `semPlot` (Suggests) — path diagram rendering, used only when `show_plot_path = TRUE`

Existing dependencies reused: `survival`, `ggplot2`, `survminer`, `jmvcore`.

**Menu placement:** `meddecide` → new subgroup **Biomarkers**. Routed via the `T` `menuGroup` suffix during development per CLAUDE.md, removed when promoting to production.

**Internal pipeline (linear, gateable):**

```
inputs
  │
  ▼
[gate stack: G1–G6, in order]
  │  refuse with explanation if any hard gate fails
  ▼
indicator-type auto-detection
  │
  ▼
estimator selection (MLR or WLSMV)
  │
  ▼
lavaan::cfa() — measurement model
  │
  ▼
fit indices, loadings, reliability (ω, AVE)
  │
  ▼
lavPredict() — factor scores (Bartlett or regression)
  │
  ▼
optional: write factor_score_name column to data
  │
  ▼
survival::coxph(Surv(time, event) ~ factor + adjusters)
  │
  ▼
KM plot stratified by factor-score quantile, PH test
  │
  ▼
generated R code (if requested)
```

Each stage emits its own result group; failures convert to HTML notices rather than throwing.

---

## 3. Options (`a.yaml`)

| Option | Type | Default | Notes |
|---|---|---|---|
| `dep_time` | Variable (continuous) | — | Follow-up time |
| `dep_event` | Variable (factor, 2 levels) | — | Event indicator |
| `event_level` | Level | — | Which factor level codes the event |
| `indicators` | Variables (≥ 3) | — | Biomarker indicators |
| `indicator_types` | List | `"auto"` | `"auto"` \| `"continuous"` \| `"ordinal"` |
| `adjusters` | Variables (optional) | — | Cox covariates (age, stage, …) |
| `reflective_confirmed` | Bool | `FALSE` | G6 hard gate; must be `TRUE` to run |
| `standardize_scores` | Bool | `TRUE` | Standardize factor scores before Cox |
| `factor_score_method` | List | `"regression"` | `"regression"` \| `"Bartlett"` |
| `save_factor_scores` | Bool | `FALSE` | Write column back to data |
| `factor_score_name` | String | `"biomarker_factor"` | Column name when saving |
| `km_strata` | List | `"tertile"` | `"median"` \| `"tertile"` \| `"quartile"` |
| `show_plot_km` | Bool | `TRUE` | Kaplan–Meier plot |
| `show_plot_loadings` | Bool | `TRUE` | Loadings forest plot |
| `show_plot_path` | Bool | `TRUE` | Lavaan path diagram |
| `show_diagnostics` | Bool | `TRUE` | Fit indices, PH test, multivariate normality |
| `show_r_code` | Bool | `FALSE` | Generated reproducible R code |

---

## 4. UI (`u.yaml`)

```
[ Variables ]
  Time            : [time var]
  Event           : [event var]    Event level: [dropdown of factor levels]
  Indicators (≥3) : [variables list]
  Adjust for      : [variables list, optional]

[ Measurement model ]
  ☐ I confirm these indicators reflect an underlying construct,
    not a composite score that constitutes one              [ ? help icon ]
  Indicator type:  ( ) Auto  ( ) Continuous  ( ) Ordinal
  Factor score method: ( ) Regression  ( ) Bartlett
  ☑ Standardize factor scores

[ Output ]
  ☑ Kaplan–Meier plot, stratify by: [Tertile ▾]
  ☑ Loadings plot
  ☑ Path diagram
  ☑ Model diagnostics (fit indices, PH test, multivariate normality)
  ☐ Save factor scores to data, column name: [biomarker_factor]
  ☐ Show R code
```

The help icon next to the reflective checkbox opens a brief explainer naming `cSEM` and `seminr` as the right R tools when the answer is "no, my model is formative."

---

## 5. Output Structure (`r.yaml`)

Six output groups, in display order:

### 5.1 Notices (HTML)

Refusals and warnings using the HTML-notice pattern from `R/waterfall.b.R`. Per CLAUDE.md, **do not** use `insert(999, notice)` with `jmvcore::Notice` — serialization will fail. The notice list is built incrementally during `.run()` and rendered to a single HTML output at the end.

### 5.2 Sample & Model Summary (Table)

n, n events, K indicators, K parameters, cases-per-parameter ratio, estimator chosen, missing-data handling (FIML for MLR / pairwise for WLSMV).

### 5.3 Measurement Model

- **5.3a Loadings table** — indicator, λ (standardized + raw), SE, z, p, R² (communality)
- **5.3b Reliability** — McDonald's ω, AVE
- **5.3c Fit indices** — χ², df, p, CFI, TLI, RMSEA + 90% CI, SRMR; traffic-light interpretation (green/yellow/red) per conventional cutoffs (CFI ≥ 0.95, TLI ≥ 0.95, RMSEA ≤ 0.06, SRMR ≤ 0.08)
- **5.3d Loadings plot** (`Image`) — horizontal forest of standardized loadings with 95% CIs
- **5.3e Path diagram** (`Image`) — via `semPlot::semPaths`

### 5.4 Structural / Cox

- **5.4a Cox table** — HR for factor + adjusters, 95% CI, Wald z, p
- **5.4b Global PH test** (`cox.zph`) — table, with prominent warning if any p < 0.05
- **5.4c KM plot** (`Image`) — stratified by factor-score quantile, with log-rank p

### 5.5 Diagnostics (collapsed by default)

- **5.5a Multivariate normality** — Mardia test, under MLR only
- **5.5b Modification indices** — only MIs > 10 shown, with explicit warning *"Do not act on these without theoretical justification. Modification indices are post-hoc and inflate Type I error if used to refit the model."*
- **5.5c Factor-score reliability** — correlation between regression and Bartlett scores as a sanity check

### 5.6 Reproducibility (Preformatted)

Generated R code containing a complete runnable script (`library(lavaan); library(survival); ...`) that reproduces the analysis offline. Shown only if `show_r_code = TRUE`.

---

## 6. Refusal & Warning Policy

### Hard refusals (G1, G2, G3, G6)

Function emits an error notice and stops before fitting.

| Gate | Condition | Refusal message (template) |
|---|---|---|
| **G1** | n < 100 | "SEM is data-hungry. With n < 100, factor loadings and fit indices are unreliable. Consider (1) a simpler z-score composite with conventional Cox regression, or (2) waiting for a larger cohort." |
| **G2** | Cases-per-parameter < 5 (computed over **CFA** parameters: loadings + residual variances + factor variance) | "Your measurement model has K = {k} CFA parameters, requiring n ≥ {5k} (and ideally n ≥ {10k}). You have n = {n}, giving CPP = {cpp}. Options: (1) reduce indicators, (2) use a summary score with conventional Cox, or (3) collect more data. Note: this gate is about the CFA fit; the Cox stage is gated separately by G4." |
| **G3** | Indicators < 3 | "A reflective factor requires at least 3 indicators to be identified. With 2 indicators the model is under-identified and cannot be fit." |
| **G6** | `reflective_confirmed == FALSE` | "CFA assumes indicators *reflect* an underlying latent construct (e.g., CD8/PD-L1/TIL all reflect 'immune activation'). If indicators *constitute* a composite where each adds independent information (e.g., a histologic grade summing nuclear grade + tubules + mitoses), CFA gives misleading results. Use `cSEM` or `seminr` for formative models, or compute the composite directly. Tick the confirmation box if your model is genuinely reflective." |

### Soft warnings

Function runs, but warnings are pinned at the top of the results pane.

| Gate | Condition | Warning message |
|---|---|---|
| **G1**-soft | 100 ≤ n < 200 | "Sample size is modest for SEM. Results may be unstable; interpret loadings and CIs with caution." |
| **G2**-soft | 5 ≤ CPP < 10 | "Cases-per-parameter ratio is {cpp}; recommended ≥ 10. Standard errors may be optimistic." |
| **G3**-soft | Exactly 3 indicators | "With 3 indicators and 1 factor, the model is just-identified (df = 0). CFI, RMSEA, and SRMR are not meaningful." |
| **G4** | Events-per-Cox-variable < 10 | "Cox model has {epv} events per covariate; recommended ≥ 10 (Peduzzi/Concato). Consider reducing adjusters." |
| **G5** | All inter-indicator correlations < 0.3 | "Indicators are weakly intercorrelated (max r = {rmax}). They may not reflect a common construct." |
| Fit-poor | CFI < 0.95 OR RMSEA > 0.10 | "Single-factor model fits poorly (CFI = {cfi}, RMSEA = {rmsea}). Consider splitting into multiple constructs — `SEMLj` supports multi-factor SEM." |
| PH-violated | Global `cox.zph` p < 0.05 | "Proportional-hazards assumption violated (p = {p}). HR is an average effect; consider stratification or time-dependent terms." |
| Uncertainty | Always, when Cox runs | "HR confidence intervals do not account for measurement uncertainty in the latent factor (two-stage Murphy–Topel issue). True CIs are wider than reported. Interpret conservatively." |

All refusal/warning strings live in a single `private$.messages` list at the top of `.b.R` to support future translation via `/prepare-translation`.

---

## 7. Error Handling

- HTML notice infrastructure copied verbatim from `R/waterfall.b.R` (reference implementation per CLAUDE.md).
- `tryCatch()` wraps `lavaan::cfa()`, `lavPredict()`, `coxph()`, and `cox.zph()`.
- Convergence failures, non-positive-definite covariance matrices, and Heywood cases (negative variances, |λ| > 1) each map to specific notices with actionable text.
- Reflective-confirmation gate runs **first** before any computation.
- WLSMV with sparse ordinal cells (any 2×2 cell n < 5) emits a notice recommending collapsing categories.
- Multilevel-data scenarios (clustered observations) are not detected automatically; the function assumes IID observations and the limitation is documented in the vignette.

---

## 8. Testing Strategy

### Unit tests (`tests/testthat/test-latentbiomarker.R`)

- Synthetic continuous data with known 1-factor structure (3, 5, 8 indicators) → recover loadings within tolerance (|estimated − true| < 0.1).
- Synthetic ordinal data → WLSMV path produces consistent estimates.
- Refusal gates fire correctly:
  - n = 50 → G1 refuses
  - n = 200, K params = 50 → G2 refuses (CPP = 4)
  - 2 indicators → G3 refuses
  - `reflective_confirmed = FALSE` → G6 refuses
- Cox HR recovery on simulated survival data with known factor effect (HR within 15% of truth at n = 500).
- Generated R code (when `show_r_code = TRUE`) parses without syntax error.

### Integration test (`tests/testthat/test-latentbiomarker-integration.R`)

- Load `histopathology` example dataset; run end-to-end with realistic options; snapshot result structure.

### Manual checklist

Six scenarios in the vignette:

1. Continuous-only happy path (immune markers → OS)
2. Ordinal-only (IHC 0/1+/2+/3+ scores)
3. Mixed continuous + ordinal + binary
4. Insufficient n triggering G1
5. Exactly 3 indicators triggering just-identified warning
6. Two-factor reality (e.g., proliferation + invasion forced into one factor) triggering the "consider `SEMLj`" fit-poor notice

---

## 9. Build Sequence

Each commit independently verifiable in jamovi via `jmvtools::prepare()`. `menuGroup` retains `T` suffix throughout development.

1. **Commit 1** — Scaffold via `/create-function latentbiomarker`. YAML files + empty backend that only emits the reflective-confirmation gate.
2. **Commit 2** — Indicator-type auto-detection + estimator selection + sample-size/CPP/indicator-count gates. All refusal copy in `.messages` list.
3. **Commit 3** — `lavaan::cfa()` fit + loadings table + fit-indices table. No Cox yet.
4. **Commit 4** — Factor score extraction + internal Cox + KM plot.
5. **Commit 5** — Path diagram + diagnostics + save-to-data + R code export.
6. **Commit 6** — Tests + vignette + NEWS.md + DESCRIPTION update + `T` suffix removal for promotion.

---

## 10. Open Questions for Implementation

These were intentionally deferred to the implementation plan rather than answered here:

- **Path diagram styling** — `semPaths` defaults vs. a custom ClinicoPath theme. Resolve when implementing 5.3e.
- **R code export format** — match existing `add-R-code` skill output or roll a bespoke template. Resolve in commit 5.
- **Translation strings** — invoke `/prepare-translation` before commit 6 or defer to a follow-up.

None of these block the design.

---

## 11. References

- Rosseel Y. *lavaan: An R Package for Structural Equation Modeling.* J Stat Softw 2012; 48(2): 1–36.
- Murphy KM, Topel RH. *Estimation and Inference in Two-Step Econometric Models.* J Bus Econ Stat 1985; 3(4): 370–9.
- Peduzzi P, Concato J, Feinstein AR, Holford TR. *Importance of events per independent variable in proportional hazards regression analysis.* J Clin Epidemiol 1995; 48: 1503–10.
- Kline RB. *Principles and Practice of Structural Equation Modeling*, 4th ed. New York: Guilford; 2016.
- jamovi module patterns guide: `vignettes/jamovi_module_patterns_guide.md`
- Reference implementation for HTML notices: `R/waterfall.b.R`
- Sibling functions whose Cox UI patterns inform the "lite" Cox here: `R/multisurvival.b.R`, `R/coxdiagnostics.b.R`
