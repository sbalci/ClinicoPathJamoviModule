# The recorded-formula defect class — `survminer::ggsurvplot()` and friends

Status: **survminer sinks closed** (15 files, rounds 0-3). Regression guard:
`tests/testthat/test-survminer-callformula-regression.R`. Sibling `rms` instances
closed in round 3. **Open:** latent factory sites that have no re-evaluating
consumer *today* — see "Still open".

Repo of record: `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule`
(the umbrella). `jsurvival` / `OncoPath` are synced copies — **edit the umbrella,
then copy across**.

The **test file is the specification**, not this document. It encodes the invariant
executably and was checked against real `ggsurvplot()` behaviour on all six
construction shapes. If the two ever disagree, the test wins and this file is stale
again.

---

## The defect

A library records the model call with `match.call()` and later `eval()`s a piece of
it **in its own frame**. `survminer::ggsurvplot()` -> `surv_pvalue()` -> `.pvalue()`
does exactly this:

```r
sdiff <- survival::survdiff(eval(fit$call$formula), data = data)
```

and the `surv_summary()` / `.extract.survfit()` path behind it runs even with
`pval = FALSE`. The lookup chain from survminer's frame is
`namespace:survminer -> imports -> base -> globalenv`. **An R6 method frame is never
on that chain.** So anything the recorded formula names that lived only in
`.plotSomething()` is unreachable by the time the picture is drawn — which, for a
jamovi Image, is a *different call stack entirely* (`.run()` fills state, the
renderer draws, and it redraws again on every resize and on `.omv` reopen).

The failure is almost always **swallowed**: a `tryCatch` paints a placeholder, a
degraded base-R plot, or an instructions panel. The analysis "succeeds", tables are
correct, and only the picture is wrong. That is why three rounds were needed to find
them all — reading code found the first six, *driving survminer* found the rest.

---

## The pattern matrix

Two distinct failure modes. Naming them separately matters — round 1 conflated them
and missed six defects by treating row 3 as safe.

| # | Construction | `fit$call$formula` holds | Verdict |
|---|---|---|---|
| 1 | `survfit(Surv(t, e) ~ g, data = d)` | a **call**: `Surv(t, e) ~ g` | **SAFE** — self-contained, resolves against `data` |
| 2 | `f <- ...; survfit(f, data = d)` | a **name**: `f` | **BROKEN** — `object of type 'symbol' is not subsettable` |
| 3 | `s <- Surv(t, e); survfit(s ~ g, data = d)` | a call naming a **method local** | **BROKEN** — `object 's' not found` |
| 4 | `d$s <- Surv(t, e); survfit(s ~ g, data = d)` | a call naming a **column of `d`** | **SAFE** — `s` resolves in `data` |
| 5 | `survfit(s ~ 1)` with no `data =` | a call naming a method local | **BROKEN** if it ever reaches a sink; inert otherwise |
| 6 | `fit$call$formula <- f` after the fit | a **formula object** + its environment | SAFE, but see the cost below |

**Row 3 is the trap.** `survfit(surv_obj ~ g, data = d)` looks like row 1 and fits
perfectly at construction time — `survfit` evaluates in the formula's own
environment, which *is* the method frame. It only dies later, in someone else's
frame. Row 4 is the same source text and is genuinely safe; the difference is
invisible at the call site and visible only in `data`.

Live example of row 4 in this repo: `R/permutationsurvival.b.R:~537-541` attaches
`plotData$surv` and then fits `surv ~ group` over that frame. Also
`R/stagemigration_helpers.R:236-241`, which builds `trend_data` with a `surv_obj`
column first. Neither is a bug. Do not "fix" them.

Reproducer (the shapes, and why a top-level version of this test passes for the
wrong reason — at top level the "method local" is in globalenv, which **is** on
survminer's chain):

```r
d <- data.frame(time = rexp(60, .05), event = rbinom(60, 1, .6),
                g = factor(rep(c("a","b"), 30)))
B <- R6::R6Class("B", public = list(make = function(kind, data) {
  s <- survival::Surv(data$time, data$event)
  f <- survival::Surv(time, event) ~ g
  switch(kind,
    inline   = survival::survfit(survival::Surv(time, event) ~ g, data = data),
    varform  = survival::survfit(f, data = data),
    localobj = survival::survfit(s ~ g, data = data))
}))$new()
survminer::ggsurvplot(B$make("localobj", d), data = d)   # object 's' not found
```

---

## Preferred fix vs fallback

**Preferred — inline the formula over columns of the frame you hand to
`ggsurvplot(data = )`.** Same frame, no captured environment, nothing to go stale:

```r
fit <- survival::survfit(survival::Surv(time, event) ~ group, data = plot_data)
survminer::ggsurvplot(fit, data = plot_data, ...)
```

Reference implementation: `R/progressionsurvival.b.R:295-305`.

**Fallback — repair the recorded call** where the formula genuinely has to be built
as a string (escaped user column names, optional strata, `.asSurvivalFormula()`):

```r
fit <- survival::survfit(surv_formula, data = plot_data)
fit$call$formula <- surv_formula
```

### The cost, and the rule

A recorded **name** or **call** carries no environment. A recorded **formula object**
carries `environment(f)` — here, the R6 method frame, and through it `self`,
`private`, and every local. Serialized size of the `survfit` object, minimal R6
builder, re-measured 2026-08-30:

| shape | serialized |
|---|---|
| unfixed (bare symbol) | 5 476 B |
| `fit$call$formula <- f` | **26 968 B** |
| inlined | 5 570 B |

(Round 2 measured 6 861 / 14 793 / 6 903 B on a different builder. **The constant is
not the point** — the multiplier is whatever the method frame happens to hold. Add
one 20 000-element numeric to the R6 object and the repaired fit goes to 187 142 B
while the other two do not move at all. It is unbounded by construction.)

> **Rule.** Does this fit reach `image$setState()`?
> **Yes** -> inline, always. The payload rides along on every run and every `.omv` save.
> **No** (fit built inside the renderer, consumed and discarded) -> either is fine;
> use the fallback when a string-built formula is unavoidable.

`R/patientsimilarity.b.R:~907-910` uses the fallback legitimately: the fit is
constructed inside `.plotSurvival()` and never stored.

---

## What is DONE

Survminer sinks, from `git diff` on `fix/stagemigration-wireup`:

| File | Sites | Form |
|---|---|---|
| `R/patientsimilarity.b.R` | ~907-910 | fallback (renderer-local; also gained `data = surv_df`) |
| `R/jvisr.b.R` | 491-494 | fallback; the dead `visR::estimate_KM` branch above it was deleted (it always `stop()`ed on this module's frame — CDISC ADaM column contract — so it only ever cost a thrown-and-caught error per run) |
| `R/survivalfeaturerank.b.R` | 639-642 | fallback |
| `R/survivalcont.b.R` | 4033-4036, 4539-4542 | fallback ×2 |
| `R/jiwillsurvive.b.R` | 182-206, 240-254 | both: `Surv()` term composed over `data`'s columns **and** the call repaired; `ggsurvplot` also gained the `data =` it never passed (`:348`) |
| `R/alluvialSurvival.b.R` | 394, 414 | inline |
| `R/clinicalheatmap.b.R` | 1479-1485, 1507 | inline (+ a plain numeric `surv_time` column — a `Surv` matrix column is deliberately *not* stored, plot state is serialized); `survdiff` too |
| `R/concordanceindex.b.R` | 909-916 | inline; `plot_data` hoisted so the fit and the plot share one frame |
| `R/epidemiosurvival.b.R` | 296-302, 315-317 | inline, both the grouped and the `~ 1` branch |
| `R/ihccluster.b.R` | 3290-3295 | inline; `ggsurvplot` gained a `data =` argument it never had, and the survival columns now come from the prepared `df` rather than `self$data` (they were misaligned with `clusters` whenever rows were dropped) |
| `R/mediansurvival.b.R` | 104-107, 122-126 | inline, single-group and grouped |
| `R/progressionsurvival.b.R` | 295-305 | inline — **reference implementation** |
| `R/survivalendpoints.b.R` | 1004-1006 | inline |
| `R/rmst.b.R` | 373-430 | restructured: state carries plain columns, the fit and plot move into a new `.plotRMST()` renderer, formula inlined. Was storing a whole `ggsurvplot` object (~17 MB, its `plot_env` captured the method frame). `.plotTauAnalysis()` split out for the same reason |
| `R/permutationsurvival.b.R` | 456-560 | restructured the same way; the renderer attaches `plotData$surv` and fits `surv ~ group` — matrix row 4, safe |

Same class, non-survminer sink:

- `R/survival_utils.R:600-604` — `.buildSurvivalFormula()` now forwards
  `env = parent.frame()` to `.asSurvivalFormula()`. Without it the formula's
  environment defaulted to the *builder's* frame, which holds `time_var` and the
  escaped names but no data, so `cox.zph()` and anything else re-evaluating the
  model call died with `object 'mydata' not found`.

### Verified clean — do not re-audit

| Site | Why |
|---|---|
| `R/comparingSurvival.b.R`, `R/singlearm.b.R`, `R/stagemigration.b.R` | `@importFrom survminer ggsurvplot` with **zero call sites**. Dead imports. `singlearm` still has `inherits(x, "ggsurvplot")` guards; they are leftovers |
| `R/multisurvival.b.R:5093` | already inline over `plotData` |
| `R/survival.b.R:6489` | already inline over `plot_data` |
| `lassocox`, `latentbiomarker`, `plscox`, `simonmakuch`, `treatmentswitching` | all inline over the frame they pass as `data =` |
| `groomecompare.b.R:435-436`, `rpasurvival.b.R:566` | inline; the fit *is* stored in state, but a recorded **call** carries no environment, so the payload is the fit and the data frame only |

---

## Still open

None of these is currently reachable by a re-evaluating consumer. They are listed
because they become live defects the moment someone adds a plot — which is precisely
how rounds 1-3 happened.

**1. Factory helpers that hand back a fit built from a formula variable.**
`R/stagemigration_helpers.R` — 13 sites, `survival::coxph(old_formula, data = data)` /
`new_formula` at `:29, :31, :117, :174, :175, :187, :192, :346, :347, :448, :449,
:539, :540`. The returned models today feed `concordance()`, `AIC()`, `predict()` and
bootstrap loops, none of which re-parse `$call$formula`. `R/survival_utils.R`'s
`.buildSurvivalFormula()` produces the same shape for every caller. Cheapest
durable fix if one of these ever gets plotted: `fit$call$formula <- old_formula`
right at the factory, *if* the fit is not stored in state.

**2. Bare-symbol `Surv` locals, no `data =`.** Matrix row 5. Currently inert; each
would need a data frame built alongside it before it could be plotted.

```
R/stagemigration.b.R:17758,17762,17771,17775,17913,17931,18422,18427,18437,
                     18481,18486,18496,21300,26151,28892,28990,29052
R/ihcsurvival.b.R:231,268,632,1035
R/pseudosurvival.b.R:349,665
R/rpasurvival.b.R:392,421      (subscripted vectors, not even columns)
R/weightedlogrank.b.R:193      R/generalpseudo.b.R:805
R/sparsegrouplasso.b.R:915     R/survivalmodelvalidation.b.R:998
R/progressionsurvival.b.R:742  R/lassocox.b.R:1999,2031  (`y ~ .` — see below)
```

`lassocox:1999,2031` are the highest-risk of these: `coxph(y ~ ., data = selected_X)`
with a method-local `y` is the *exact* shape that broke `rms::cph` in round 3, and
`.` expansion has the same frame problem as the `Surv` local.

**3. `R/rpasurvival.b.R:392,421`** are worse than the rest — the terms are
`timeVar[groupIdx]`, subscripted method locals that are not columns of anything.
Nothing short of rebuilding a per-group data frame would make them plottable.

Regenerate this list after any refactor:

```sh
grep -rnE '(survfit|survdiff|coxph|cph)\(\s*[A-Za-z_.][A-Za-z0-9_.]*\s*~' R/ \
  | grep -viE '\((Surv|survival::Surv)'
```

Every hit is either matrix row 3/4/5 or a formula variable. Decide which by asking
one question: **is that symbol a column of the frame passed as `data =`?**

---

## Sibling instances outside survminer (round 3)

The same shape — a library resolving a **name** in its own frame — appears in `rms`.
`rms::Design()` does `eval(as.name(getOption("datadist")))`, so passing the *name* of
a datadist rather than the object fails identically:

- `R/clinicalnomograms.b.R:472, :500` — `options(datadist = "private$dd")` never
  resolved (`"private$dd"` is not even a name), so **every** `cph()`/`lrm()`/`ols()`
  fit below it failed. Now `options(datadist = private$dd)`.
- `R/clinicalscore.b.R:1128-1132` — `options(datadist = "dd")` naming a method local:
  `dataset dd not found for options(datadist=)`. Now the object.
- `R/clinicalscore.b.R:1133-1137` — same method, the survival branch also did
  `rms::cph(surv_obj ~ . - y - time, data = df_rms)`. `cph` can expand neither `.`
  nor a method-local `surv_obj` from its own frame, so the branch always errored.
  Now an explicit `Surv(time, y) ~ <named terms>` built with `.asSurvivalFormula()`.

**Recognise the shape:** any API whose contract is "give me a name / a string and
I'll look it up" is an environment bug waiting for an R6 method. Give it the object.

---

## The regression suite

`tests/testthat/test-survminer-callformula-regression.R`.

**The invariant.** `survminer_formula_ok(fit, data)`: `fit$call$formula` is not NULL
and not a bare name, and `eval()`ing it in a frame that holds none of the builder's
locals then resolving it with `model.frame(f, data = ...)` succeeds.

**It is checked against reality, not asserted.** The first test builds all six shapes
*inside an R6 method*, `gc()`s the builder frame away, and asserts the invariant's
verdict equals what a real `print(ggsurvplot(fit, data, pval = TRUE))` does. If
survminer changes how it recovers the formula, that test fails first and tells you
the invariant is now wrong.

**How the per-analysis guards work.** `with_survminer_probe()` mocks
`survminer::ggsurvplot` to record every `(fit, data)` pair the module hands over —
and still calls the real function, so rendering failures surface too. `render_ok()`
exists because `jmvcore`'s `Analysis$.render()` returns TRUE whenever the render
function returned *anything* non-NULL, so a renderer that bailed with `FALSE` still
reports success; the renderer's own return value survives on `image$plot`, which is
checked as well.

`expect_survminer_fits(..., min = 0L)` for `progressionsurvival` and `mediansurvival`:
both call a **bare, `@importFrom`-ed** `ggsurvplot()`. R copies an imported binding
into the package's imports env at load time, so `local_mocked_bindings` cannot reach
them in an installed package. Their survminer failures are *not* swallowed, so
`render_ok()` alone is a real assertion there.

### The two honest skips

1. **`jvisr`** — `skip_if_not_installed("visR")`. The survminer path in `jvisr` is a
   *fallback*; with visR healthy it is dead code and the test would be vacuous. The
   test therefore forces `visR::estimate_KM` and `visR::visr` to `stop()` in order to
   put `.jvisr_plot_fallback` under test — which needs visR present to mock.
2. **`alluvialSurvival`** — skipped outright. It needs one row per patient per
   timepoint and `histopathology` is one row per patient. It also dies before
   survminer on `max(Outcome)` over a factor, because `survivalVar` is
   `permitted: [factor]`. **Its fix at `:394, :414` is therefore unproven by test.**
   Re-enable with a longitudinal fixture.

Several other guards carry `# unrelated defect` comments naming a *second* bug that
had to be steered around to reach survminer at all (`mediansurvival`'s grouped path
dies on a matrix `quantile()`; `jiwillsurvive`'s `show_statistics` dies in
`pchisq(chisq, NULL)`; `jvisr`'s `fun_type` default is rejected by `ggsurvplot`;
`survivalendpoints` cannot parse `histopathology`'s date strings). Those are real and
unfixed — read the comments before widening any of these tests.

---

## Checklist for a new survival plot

- [ ] Formula written inline over columns of the frame passed as `ggsurvplot(data =)`.
- [ ] `data =` actually passed. (`ihccluster` and `jiwillsurvive` both omitted it.)
- [ ] No `Surv` matrix column stored in `image$setState()` — plot state is serialized.
- [ ] No `ggsurvplot` / `ggplot` object stored in state — build it in the renderer.
- [ ] Renderer guards `image$state` for NULL; it runs on resize and on `.omv` reopen.
- [ ] A case added to `test-survminer-callformula-regression.R`.
