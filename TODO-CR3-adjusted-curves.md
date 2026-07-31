# CR-3 — Adjusted survival: one estimator behind the plot and the tables

Status: **closed** (verified). Shared estimator is `.adjustedCurveData()` in
`R/multisurvival.b.R`; the plot and both adjusted tables read it. Regression tests:
`tests/testthat/test-multisurvival-adjusted-curves.R` (both repos). jsurvival suite
418 passed / 0 failed / 0 errors (358 pre-existing + 60 new).

Everything else from the external evaluation was already fixed (CR-1, CR-2, CR-4,
CR-5, OTHER-1).

Repo of record: `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule`
(the umbrella). `jsurvival` is a synced copy — **edit the umbrella, then copy across**.

---

## The defect (verified TRUE, adversarially confirmed)

`self$options$ac_method` is read in **exactly one place** in `R/multisurvival.b.R`:
inside `.plot_adj()`, where it is passed to `survminer::ggadjustedcurves(method = ...)`.

Every other consumer ignores it:

- `.adjustedSurvTable()` — the adjusted survival probability table
- the adjusted **median** survival table
- the accompanying natural-language narrative

Those build their own prediction instead: a **single mean/mode covariate profile**
per level of the adjustment variable, via `survfit(cox_model, newdata = level_data)`.

Consequences:

1. `ac_method = "average"` and `ac_method = "conditional"` produce **byte-identical
   tables**. The user changes the estimand and the numbers do not move. The plot
   changes; the tables do not; nothing says so.
2. `n.risk` / `n.event` in those tables are taken from the model's **common risk
   set**, so they are identical for every group — yet the narrative describes them
   as group-specific counts. They are whole-cohort numbers wearing a group label.
3. Plot and table can therefore disagree in the same report, which is the same
   class of defect as CR-1 (two estimands in one panel).

`survminer` documents `average`, `conditional`, `single` and `marginal` as
**distinct estimands**, not display variants.

---

## Required end state

**One estimator, called by both the plot and the tables.**

1. Add a private helper, e.g. `.adjustedCurves(cox_model, mydata, adjvar, method)`,
   returning a tidy data frame: `time`, `surv`, `group`, and (where the method
   supports it) `lower` / `upper`.
   - `average` / `marginal`: g-computation — set **every observed patient** to each
     level in turn, predict, average across patients. (There is already a worked
     example of exactly this in the Fine-Gray branch of `.plot_adj()` added for
     CR-5 — reuse its shape.)
   - `conditional`: the mean/mode reference profile currently used by the tables.
   - `single`: as `survminer` defines it.
   - If a method cannot be supported, **refuse it explicitly** with a notice; do not
     silently substitute another. Silent substitution is what caused this bug.
2. `.plot_adj()` plots that data frame (keep the existing Fine-Gray CIF branch
   untouched — that is CR-5 and is already correct).
3. The survival table and the median table are **derived from the same returned
   object**, so they cannot disagree with the plot by construction.
4. `n.risk` / `n.event`: either drop them from adjusted tables (they are not
   defined for a hypothetical standardised curve), or keep them and relabel
   explicitly as observed whole-cohort counts. **Do not** present whole-cohort
   numbers under a group heading.
5. State the estimand in the table caption/note — e.g. "standardised over the
   observed patients (average)" vs "at the mean/mode covariate profile
   (conditional)" — so the reader knows which question was answered.

---

## Acceptance criteria

- `ac_method = "average"` and `ac_method = "conditional"` give **materially
  different** numeric tables on data where the covariate distribution is skewed.
  Prove it with a runnable comparison, not by inspection.
- The plot's curve values and the table's values **agree** for the same method
  (spot-check a few time points).
- No table presents whole-cohort `n.risk`/`n.event` as group-specific.
- `Rscript -e 'parse("R/multisurvival.b.R")'` clean.
- `jmvtools::prepare(".")` from the umbrella — **0 errors**.
- jsurvival suite: **358 passed / 0 failed / 0 errors** minimum, plus new tests.
- Add regression tests pinning: (a) the two methods differ; (b) plot and table
  agree; (c) the Fine-Gray CIF branch still works (CR-5 must not regress).

---

## Verification recipe (follow exactly — these are session-earned)

```bash
# 1. parse
cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule
Rscript -e 'parse("R/multisurvival.b.R"); cat("PARSE OK\n")'

# 2. yaml, if .r.yaml touched
Rscript -e 'yaml::read_yaml("jamovi/multisurvival.r.yaml")'

# 3. prepare (regenerates .h.R module-wide)
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare(".")'

# 4. sync to jsurvival
cp R/multisurvival.b.R R/multisurvival.h.R /Users/serdarbalci/Documents/GitHub/jsurvival/R/
cp jamovi/multisurvival.r.yaml /Users/serdarbalci/Documents/GitHub/jsurvival/jamovi/

# 5. install jsurvival — ALWAYS cd first, see trap below
cd /Users/serdarbalci/Documents/GitHub/jsurvival && R CMD INSTALL --no-docs --no-byte-compile .

# 6. tests
Rscript -e 'library(testthat); library(jsurvival)
  d <- as.data.frame(test_dir("tests/testthat", reporter="silent", stop_on_failure=FALSE))
  cat(sum(d$passed), "passed", sum(d$failed), "failed", sum(d$error), "errors\n")'

# 7. jamovi module
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE");
  setwd("/Users/serdarbalci/Documents/GitHub/jsurvival"); jmvtools::install()'
```

---

## Traps that cost time in this session — do not rediscover them

- **cwd trap.** `R CMD INSTALL .` and `jmvtools::install()` act on the *current
  directory*. The shell cwd resets between calls. Running either from the umbrella
  builds `ClinicoPath`, not `jsurvival`, and the test then measures a stale build.
  Always `cd` explicitly or pass `setwd()`.
- **`jmvtools::install()` can silently no-op** while printing "Module installed
  successfully". Verify with the installed `.rdb` mtime:
  `find "$HOME/Library/Application Support/jamovi/modules/jsurvival" -name jsurvival.rdb`
- **Early returns leave stale output on screen.** jamovi does not clear a result
  item just because the code path that fills it was skipped. Any guard that returns
  early must `setContent("")` / `setVisible(FALSE)` on everything it would have
  written. This produced two separate false bug reports this session.
- **`clearWith` must list every option the item depends on**, or the panel keeps
  last run's content. Newly-consumed options must be added.
- **Never index a model summary by position.** `summary(coxph)$coefficients` has 5
  columns normally and **6 with clustering** (`robust se` inserted). Positional
  indexing reported the z statistic as a p-value. Index by name, prefer
  `robust se` when present. Three separate instances of this class have now been
  found in this codebase.
- **`type: Level` options cannot have `default:`** — the jamovi compiler rejects it.
  They are therefore required wrapper arguments. `.h.R` is auto-generated; never
  edit it.
- **Output-type options are not wrapper arguments** and are not populated outside
  jamovi, so a `type: Output` column cannot be read back in a plain R harness.
- Test the analysis with `do.call(jsurvival::multisurvival, list(...))` — the
  wrapper does NSE and will capture a bare symbol name instead of its value.
- `sink()` without `on.exit(sink(), add = TRUE)` swallows all later output when the
  wrapped call errors.

---

## Reference points in the code

- `.plot_adj()` — the only current reader of `ac_method`; its Fine-Gray branch is
  the CR-5 g-computation implementation to model the `average` method on.
- `.adjustedSurvTable()` — the survival probability table.
- The adjusted median table and its narrative — same file, nearby.
- `survfit(cox_model, newdata = level_data)` — the mean/mode profile prediction the
  tables currently use; this becomes the `conditional` branch.
