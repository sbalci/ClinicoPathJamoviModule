# Two open defects in `multisurvival` — adjusted Cox table & contrast wording

Both surfaced in a real jamovi run. Both verified against that output. Neither is
speculative.

Repo of record: `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule`
(the umbrella). `jsurvival` is a synced copy — **edit the umbrella, copy across**.

Read `TODO-CR3-adjusted-curves.md` in this directory first: its "Traps" and
"Verification recipe" sections apply verbatim and are session-earned.

---

## ISSUE 1 (major) — "Adjusted Cox Model Results" is a DIFFERENT model

Observed in one report, same run:

| | Main multivariable table | Adjusted Cox Model Results |
|---|---|---|
| `performance_status` | **two** rows (level 1 = 0.82, level 2 = 0.68) — a factor | **one** row (0.82, 0.65-1.04) — continuous |
| Likelihood ratio df | **13** | **12** |
| `gradePoor` p | .129 | .127 |
| `stageIII` CI | 1.89-5.76 | 1.89-5.75 |

Two models are being fitted and both are displayed, with nothing saying so. This is
the same defect class as CR-1 (competing risks showed Fine-Gray prose over a
cause-specific table), which was fixed by refusing to display the second model.

`performance_status` was passed in `contexpl` (continuous explanatory), but the
underlying column is a factor. One code path respects the column's type; the other
coerces to numeric. **Find which, and why they differ.**

### Required end state

- ONE model behind the main table and the adjusted Cox table. They must not be able
  to disagree — derive both from the same fitted object, as CR-3 did for the
  adjusted curves and tables.
- If the adjusted Cox table is genuinely meant to be a different specification,
  that is a product decision, not an accident: in that case it must be titled and
  captioned to say so explicitly. Default to making them the same model.
- Decide and document what happens when a factor column is placed in `contexpl`:
  either honour the factor (preferred — never silently coerce a clinician's factor
  to a score) or reject with a clear notice. Do not silently coerce.

### Acceptance criteria

- Same covariates, same df, same HRs/CIs/p-values in both tables, proven by a
  runnable comparison printing both.
- A factor placed in `contexpl` behaves identically in both tables.
- Regression test pinning the two tables' agreement.

---

## ISSUE 2 (major, clinical wording) — "for each unit increase in stageIV"

Verbatim from the report:

> For **stageIV**, the adjusted hazard ratio is 7.53 … there is a **653.4 % increase
> in hazard for each unit increase in stageIV**.

Three defects in one sentence:

1. **"each unit increase"** is meaningless for a factor contrast. Stage IV is not a
   quantity that increases; the HR is **stage IV versus the reference level (I)**.
   Applies to `treatmentTreatment A`, `gradePoor`, `sexMale`, etc.
2. **Raw coefficient names leak** — `treatmentTreatment A`, `stageIV`, `gradePoor`.
   Should read as variable + level (e.g. "treatment: Treatment A (vs Control)").
   The variable/level split already exists elsewhere in this file — reuse it, and
   note that finalfit tables carry the variable name only on the first row of each
   block (forward-fill; this has bitten twice already).
3. **"653.4 % increase in hazard"** — arithmetically right, rhetorically inflated,
   and paired with the wrong comparison it is simply false. For continuous
   predictors "per one-unit increase" is correct; for factor levels it must be
   "compared with <reference level>".

### Required end state

- Continuous predictor → "per 1-unit increase in X".
- Factor level → "for X = <level> compared with <reference level>".
- Names rendered from the variable and its level, not the raw coefficient string.
- Keep percentage phrasing only where it is defensible; prefer stating the hazard
  ratio with its CI. Hazard ratios are **not** cumulative risk — this file already
  carries that rule in other summaries; apply it here.
- Consistent with the house style already used in this codebase: association
  language, no treatment advice, no prognosis verdicts.

### Acceptance criteria

- No output line contains "unit increase" for a factor contrast. Assert this in a
  test over a model containing a multi-level factor.
- Reference level is named in every factor-contrast sentence.
- No raw coefficient name (e.g. `stageIV`) appears in user-facing prose.

---

## Verification (both issues)

Follow the recipe in `TODO-CR3-adjusted-curves.md` exactly. Summary:

1. `parse()` clean.
2. `jmvtools::prepare(".")` from the umbrella — 0 errors.
3. Sync `.b.R`, `.h.R`, `.r.yaml`, tests to `jsurvival`.
4. `cd /Users/serdarbalci/Documents/GitHub/jsurvival && R CMD INSTALL --no-docs --no-byte-compile .`
   — **always cd first**; running from the umbrella builds the wrong package and the
   tests then measure a stale build.
5. Suite must be **>= 418 passed / 0 failed / 0 errors** (current baseline).
6. `jmvtools::install()` with explicit `setwd()`, then confirm the installed
   `.rdb` mtime actually advanced — it can silently no-op while printing success.

Exercise the analysis with `do.call(jsurvival::multisurvival, list(...))`; the
wrapper does NSE and will capture a bare symbol instead of its value.

Reproduce ISSUE 1 with a factor column placed in `contexpl` (that is what triggered
it: `performance_status`).
