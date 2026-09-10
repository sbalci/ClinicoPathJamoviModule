# Engineering Lessons Log

Failure modes found during development, how they were caught, and the rule that
prevents them. Newest first. Release notes for users live in `NEWS.md`.

---

## 2026-09-10 — stagemigration deep audit and repair

### Declared options "dead" by searching only the backend

- **Failure mode:** The dead-option scan read `.b.R` and helper files but not `.r.yaml`
  `visible:` expressions. `cureModelComparison`, `generateCureSummary` and
  `generateForestSummary` gate their tables purely through `visible:` and were reported dead
  (29 genuinely dead became "32"). An earlier session had likewise reported those three tables
  "never populated".
- **Detection signal:** Tracing each table to its populate function, then that function to a
  caller (lines 28468, 28556, 27469).
- **Prevention rule:** An option is wired if it appears in `.b.R`, any helper R file, or any
  `.r.yaml` `visible:` / `clearWith:` expression. An output is populated only once you have found
  both the populate function and a call to it.

### ugrep `$` anchor trap recurred despite being logged

- **Failure mode:** Four more false conclusions this session from `grep` patterns containing
  `$` (`self$results$x`, `self$options$x`), although the rule was already logged today in the
  survivalPower section.
- **Detection signal:** Python scans of the same code disagreed; a probe showed ugrep 7.8.4
  returns 0 for `grep 'self$results$foo'` and 1 with `-F`.
- **Prevention rule:** A logged rule is not a habit. Default to `grep -F` or Python for any
  pattern containing `$`, and treat a 0 from such a grep as unverified.

### Attributed each UI control the previous control's label

- **Failure mode:** Label extraction scanned a fixed line window around `name:` and took the
  first `label:`, which usually belonged to the control above. Reported `frailtyAdvancedInference`
  as labelled "[Experimental] Cure Models" and missed that the real mislabel was
  `optimizeForLargeDatasets` ("n>1000" for code gated at 10,000).
- **Detection signal:** The extracted label for `enableAccessibilityFeatures` matched a
  different option's `.a.yaml` description.
- **Prevention rule:** Parse `.u.yaml` controls by `- type:` list-item boundaries, never by a
  fixed line window.

### Audit recommended the Notice API that CLAUDE.md says breaks serialization

- **Failure mode:** The report's actionable fix used `jmvcore::Notice` + `results$insert()`,
  which `CLAUDE.md` documents as failing protobuf serialization.
- **Detection signal:** Re-reading `CLAUDE.md` before implementing.
- **Prevention rule:** Check `CLAUDE.md`'s known-issue notes before recommending an API
  pattern in a report, not only before writing code. Here: HTML notices per `R/waterfall.b.R`.

### Nearly deleted a results item that still had ten live writers

- **Failure mode:** Planned to delete the hidden `mydataview2` debug item after a
  `grep ... | head -6` showed only its obvious writer. Ten more writers (multi-state, competing
  risks, random forest, cure) were sending real user warnings into it; deletion would have
  crashed each with `NULL$setContent()` ("attempt to apply non-function").
- **Detection signal:** An untruncated reference listing before editing.
- **Prevention rule:** Never infer the full reference set from truncated output. Before deleting
  a schema item, enumerate every reference. A contract test now asserts every
  `self$results$<item>` in `stagemigration.b.R` exists in `.r.yaml`.

### Made an option functional without adding it to clearWith

- **Failure mode:** A previous session wired `confidenceLevel` into ~85 interval computations
  but left it out of `clearWith` on all 40 CI-bearing tables; 34 are `rows: 0` + `addRow` with no
  `deleteRows()`, so changing the level would accumulate stale rows.
- **Detection signal:** The `check-function-full` clearWith audit.
- **Prevention rule:** When an option starts driving an output, add it to that output's
  `clearWith` in the same change.

### Differential probes that could not show an effect

- **Failure mode:** Probed `showMigrationMatrix` at `TRUE` (its default) and `confidenceLevel`
  with no CI-bearing table enabled; both came back "NON-EFFECTIVE" although both work.
- **Detection signal:** The results contradicted earlier verified evidence.
- **Prevention rule:** Probe every option at a non-default value, with the output it drives
  enabled.

### Test asserted two p-values differ when both underflowed to zero

- **Failure mode:** A test meant to show Gray's test differs from the cause-specific log-rank
  used effects so strong that both p-values were 0, and `all.equal(0, 0)` is `TRUE`.
- **Detection signal:** The test failed against correct code.
- **Prevention rule:** Build the scenario so the two quantities are designed to diverge (equal
  cause-1 hazard, different competing hazard) and confirm it across seeds (40/40 and 36/40 here)
  before pinning one.

### Promoted internal messages to user-facing notices without auditing them

- **Failure mode:** Rerouting validation output from `warning()` to notices, I (a) merged the
  validator's informational bucket ("Event level used: 1", "Removed N incomplete cases") into the
  same WARNING notice as real data-quality warnings, putting a yellow banner on every clean run; and
  (b) surfaced `stagemigration_checkSampleSize()` text verbatim, including the MARGINAL message
  "25 events - adequate for comprehensive analysis", which states the opposite of the truth (25 is
  below the 100 recommended), and "NOTICE:"/"WARNING:" prefixes that contradicted each notice's
  own severity.
- **Detection signal:** A runtime check that printed every notice's type *and content*, not just
  titles.
- **Prevention rule:** Before making hidden text visible to users, read every message that can
  reach the new sink: check its wording against the condition that emits it, and keep separate
  buckets separate. Verify with rendered content, not titles or counts.

### A sibling-pattern test that only matched one syntactic form

- **Failure mode:** After fixing "p = <1e-04" wording, the regression test matched only
  `sprintf("... p = %s", format.pval(...))` on a single line, so it passed while
  `paste("p =", format.pval(x))` in the clinical-interpretation table still rendered
  "p = < 2.22e-16".
- **Detection signal:** A deliberate scan of every remaining `format.pval(` call.
- **Prevention rule:** When pinning "this defect class is gone", enumerate the call sites of the
  underlying function and test every construction form (sprintf, paste, multi-line), not only the
  one just fixed.


### A rename left one reference to the old name behind

- **Failure mode:** Fixing clinical NRI to apply ONE risk threshold to both systems, I replaced
  `old_threshold`/`new_threshold` with `risk_threshold` but left `threshold = old_threshold` in the
  return `list()`. The name is undefined there, the `tryCatch` handler returns NULL, and the
  "Clinical NRI (high-risk threshold)" row silently disappeared. On the bundled cohort the lost row
  is NRI 0.164 (95% CI 0.090-0.237, p = 1.3e-5).
- **Detection signal:** `/review-function` lifted every R6 method to a top-level function and ran
  `object_usage_linter` (blind inside `R6Class`). A runtime call caught
  "object 'old_threshold' not found" and returned a result once the name was defined.
- **Prevention rule:** After renaming a local, search the whole method for the old name. Any
  function whose error handler returns NULL needs a test that asserts a non-NULL result.

### Fixed a field's meaning on code paths that never run

- **Failure mode:** I removed `sqrt()` from two readers of timeROC's `inference$vect_sd_1` (already
  an SE) and missed the third, in `.populateROCAnalysis`. None of the three runs: with one requested
  time, timeROC returns slots for t = 0 and t, and the module reads `AUC[1]`, the NA t = 0 slot. Every
  time-dependent ROC therefore uses the pROC fallback, which drops patients censored before t.
- **Detection signal:** Printing a single-time timeROC object: `times: 0 26.25`, `AUC: NA 0.712`.
- **Prevention rule:** When correcting how a field is read, enumerate every reader (`grep -F`), and
  prove the path is reachable by running it and checking the value. A fix on dead code is not a fix.


### Deleted "unused" locals that formulas read by name

- **Failure mode:** Removing the 113 locals that `object_usage_linter` reported as assigned but unused,
  I deleted 8 that were read only through formula strings (`as.formula(paste("surv_obj ~", stage))`
  resolves `surv_obj` in the method frame). Every affected method (cross-validation, pseudo-R2, SHAP
  models, interaction detection, stage-specific C-index) would have failed inside a `tryCatch` that
  returns NULL, with no visible error.
- **Detection signal:** A follow-up scan of each method body for the removed names inside string
  literals, run before any further edit.
- **Prevention rule:** A linter's "unused" is a candidate list, not a verdict. Before deleting a local,
  search its method for the name inside quotes and NSE calls, and keep RHS calls with side effects,
  RNG use, or a meaningful error path.

- **Addendum (release review):** The string-literal guard added after the first catch missed locals used in
  UNQUOTED formulas (`survfit(migrated_surv ~ 1)`): codetools skips formula contents whether quoted or not.
  29 more assignments were deleted from Will Rogers, survival-comparison, frailty, cut-point and homogeneity
  methods, and were restored with a textual check (keep any local named anywhere else in its method). The
  runtime battery passed because it never ran those methods; the release review's numeric check of
  `.calculateWillRogersEffect` exposed it. Prevention: a cleanup that deletes code needs a runtime pass over
  every method it touched, not only the methods a battery happens to cover.

### Reported Fine-Gray as missing from a stale summary

- **Failure mode:** The `/review-function` report stated "no Fine-Gray model is fitted" and listed adding
  one as an action item. The claim came from an open-items list carried over a context compaction;
  `.performFineGrayAnalysis` already fits `cmprsk::crr`. Its real defects (crude CIF columns mapped by
  position, `model.matrix` dropping NA rows, positional coefficients) went unreported.
- **Detection signal:** Reading the competing-risks code before implementing the action item.
- **Prevention rule:** Re-verify every inherited "missing feature" claim against the current code before
  it goes into a report; a summary is a lead, not evidence.
### A label case sweep broke indented, hyphenated and proper-noun labels

- **Failure mode:** The sentence-case pass over `.u.yaml` treated the empty string before a label's
  leading spaces as its first word, so "  Clinical Threshold" became "  clinical threshold" and
  "  Show ROC Comparison Plot" kept its verb; hyphenated words kept a capital ("Copy-Ready"); the
  correction pass then produced "Kaplan-meier" and mixed-case `.a.yaml` titles ("Optimal cut-point
  Determination").
- **Detection signal:** Printing every old -> new label before running `prepare()`.
- **Prevention rule:** For bulk text rewrites, print the full mapping and read it before regenerating;
  handle leading whitespace, hyphenated words and a proper-noun list explicitly, and apply case rules
  only to text that is otherwise in the target style.

## 2026-09-10 — pathagreement audit and repair

### Hiding a failure concealed that a feature had never worked

- **Failure mode:** The clinical summary read results back with `kappaTable$getCell(...)`,
  which returns a jmvcore `Cell` R6 object, not the value. `as.numeric(<Cell>)` failed inside
  a `tryCatch`, the value became `NA`, and the panel was hidden with `setVisible(FALSE)`. The
  summary and its copy-ready report sentence had never been generated for any dataset.
- **Detection signal:** Replacing the silent hide with a visible "could not be generated"
  message, then a test asserting the summary text exists.
- **Prevention rule:** Use `table$getCell(...)$value` (or better, pass the computed values
  instead of reading results back). Never hide an output on failure: a visible message turns
  a permanent silent failure into a test failure.

### A swallowed error silently dropped every categorical rater characteristic

- **Failure mode:** The association test read `fisher.test()$statistic`, which does not exist
  for an r x c table. The effect size became `numeric(0)`, the interpretation threw
  "argument is of length zero", `tryCatch` returned `NULL`, and the characteristic row
  vanished. On the shipped META_ data only "Experience" was ever reported.
- **Detection signal:** A characteristic present in the data was missing from the table;
  calling the helper directly returned `NULL`.
- **Prevention rule:** A `tryCatch(..., error = function(e) NULL)` that feeds a table must be
  exercised by a test that asserts the row exists. Read the return value of a test function
  (`names(fisher.test(...))`) before indexing a field.

### A tidy-looking index labelled precise estimates "Unstable"

- **Failure mode:** Bootstrap "stability" was `|mean / SE|` with thresholds, so a kappa of
  -0.002 with SE 0.04 was called "Unstable (low variability)". The kappa it bootstrapped was
  also raters 1-2 only, under the label "kappa".
- **Detection signal:** Running the analysis on low-agreement data with many cases.
- **Prevention rule:** Report uncertainty as an interval, not a ratio that mixes size and
  precision. A statistic's label must state which raters it covers.

### Changed user-facing text without grepping the tests that assert it

- **Failure mode:** Rewording messages for translation broke two assertions in
  `test-pathagreement-refinements.R`, which had never run because it sourced the backend by an
  absolute `/Users/...` path.
- **Detection signal:** Running every `test-<fn>*.R` file, not only the ones known to work.
- **Prevention rule:** Before changing message text, `grep -rn` the old wording across
  `tests/`. Never source package files by absolute path in a test; bind internals with
  `getFromNamespace()`.

### Interpretation text claimed clinical suitability from a point estimate

- **Failure mode:** Kappa >= 0.8 was "suitable for all clinical applications including
  critical diagnoses", ignoring the confidence interval, prevalence and the consequence of
  disagreement. Three different kappa band schemes appeared in one analysis.
- **Detection signal:** Reading every interpretation string against the statistic it describes.
- **Prevention rule:** Generated interpretation describes the estimate with a cited convention
  and states what it cannot establish; it never certifies clinical use.

### An upstream package renamed its result rows and ICC went silently empty

- **Failure mode:** `.performICCAnalysis` indexed `psych::ICC(...)$results["ICC2", "ICC"]`.
  psych 2.x names the rows `Single_random_raters` and keeps `ICC2` in a `type` column, so
  every value was `NA` and the table read "Unable to calculate" with no error. Behind it,
  `as.factor()` re-sorted the categories alphabetically (ICC 0.44 instead of 0.70).
- **Detection signal:** Running the analysis on bundled ordinal data and reading the row.
- **Prevention rule:** Index library results by a documented field (`type`), never by a
  row name. A test must assert a finite value equal to the library's own output, so an
  upstream rename fails loudly.

### An invented sample-size formula

- **Failure mode:** Sample-size planning used `(z_a + z_b)^2 * k(1 - k) / precision^2`,
  divided by 3 "for 3 raters" and multiplied by 1.2 "for multiple testing". None of it has a
  derivation; it understated the required cases 1.6-8.5-fold against Rotondi & Donner.
- **Detection signal:** Comparing the default output with `kappaSize`, which the module
  already imported for three other analyses.
- **Prevention rule:** Planning and inferential formulas come from a cited method or an
  established package, with a test against that package. Search the module for an existing
  implementation before writing one.

### Tightening a threshold silently disabled a dependent option

- **Failure mode:** "Majority" used `ceiling(n/2)`, so 2 of 4 raters was a majority.
  Making it strict (`floor(n/2) + 1`) is correct, but tie-breaking lived inside the
  threshold branch, where a tie can no longer occur - the `tie_breaking` option would have
  stopped doing anything with no error.
- **Detection signal:** The existing `global_mode` regression test encoded the old
  semantics; tracing which branches can still be reached after the change.
- **Prevention rule:** When a threshold changes, list every option whose code runs inside
  that branch and re-establish where it applies. "Consensus achieved" must count only the
  outcome the label names, not placeholders such as `ARBITRATION_NEEDED`.

### Weighted agreement coefficients computed on an alphabetical category order

- **Failure mode:** Switched Gwet's AC2 and Krippendorff's alpha to `irrCAC` without passing
  `categ.labels`. irrCAC then sorts categories alphabetically, so Benign < Atypical <
  Malignant was weighted as Atypical < Benign < Malignant: ordinal alpha 0.595 instead of
  0.042, AC2 0.562 instead of 0.123. Shipped for one pass as a "correctness fix".
- **Detection signal:** A constructed data set with known one-step vs two-step
  disagreements, run with and without `categ.labels = levels`.
- **Prevention rule:** When adopting a library for a weighted/ordinal statistic, confirm
  how it orders categories before trusting it. Test on a scale whose alphabetical order
  differs from its clinical order.

### A library's "Krippendorff's alpha" was not Krippendorff's alpha

- **Failure mode:** Replaced a nonexistent `krippendorff.alpha()` call with
  `irrCAC::krippen.alpha.raw()` and shipped it as a fix. Its `"ordinal"` and `"linear"`
  weights are Gwet's schemes, not Krippendorff's frequency-based metric: ordinal .834 and
  interval .800 on Krippendorff's own worked example, whose published values are .815 and
  .849. Passing `categ.labels` (the previous lesson) fixed the ordering but not this.
- **Detection signal:** Three implementations disagreed in the third decimal on random
  data; Krippendorff's (2011) published example separated them (`irr::kripp.alpha` matched
  all four levels).
- **Prevention rule:** Validate an agreement coefficient against the method author's
  published worked example before shipping it, not against another library or the
  module's own arithmetic. Keep that example as a regression test.

### A differential harness produced false verdicts

- **Failure mode:** The option-by-option harness reported crashes and NO-EFFECTs that were
  its own bugs: `vars = V` captured the symbol name `"V"` (twice), options were toggled
  without their enabling option, and `sub("_.*", "", name)` turned `consensus_method` into
  `consensus`.
- **Detection signal:** Verdicts contradicted the code (`clusteringMethod` is read and
  passed to `hclust`; the reported error was "invalid 'row.names' length").
- **Prevention rule:** Call wrappers via `do.call(fn, list(...))` with values, test each
  option inside its enabling context, and never derive option names by string surgery.
  Confirm a NO-EFFECT by reading the code path before reporting it.

### A test suite that never ran looked green

- **Failure mode:** All 36 `test-pathagreement.R` tests skipped because
  `data(..., package = "ClinicoPath")` needs an installed build. Fabricated plots, a
  default-configuration crash and a phantom method all passed.
- **Detection signal:** Reading the skip reasons, not the summary line.
- **Prevention rule:** Treat an all-skip suite as a failure. Load fixtures from the source
  tree (`helper-pathagreement.R`).

### A placeholder that impersonated results

- **Failure mode:** Trend, bias and difficulty plots drew hardcoded vectors and `runif()`
  under authoritative titles, next to tables computed from the real data. A 7-rater
  "Minimal bias" table sat beside a 4-rater plot flagging "Moderate bias".
- **Detection signal:** Rendering the plot on data with a deliberately different shape.
- **Prevention rule:** Grep renderers for `runif(`, literal numeric vectors and
  "Placeholder". A plot's test must assert that the plotted data equals its table.

### A double-quoted grep for `$` matched nothing and produced a false claim

- **Failure mode:** `grep -n "options\$sft"` and `grep "private\$\.messages"` treated `$`
  as an end-of-line anchor, returned zero hits, and led to reporting live code as unused
  ("messages are never rendered").
- **Detection signal:** A later single-quoted search found the calls.
- **Prevention rule:** Single-quote grep patterns and write `$` as `[$]`. A zero-hit search
  is a claim that needs a second, differently-built search before it is reported.

## 2026-09-10 — survivalPower audit and repair

### A design adjustment was applied to one analysis type only

- **Failure mode:** Multi-arm shared-control, cluster design-effect and group-sequential
  adjustments were added to the *sample-size* path. Power, detectable effect and duration
  kept treating n as a plain two-arm fixed trial, and log-rank and non-inferiority ignored
  interim looks entirely: a 3-arm trial sized at 1059 for 80 percent reported 93.8 percent
  power, and a cluster trial sized at 2012 reported 99.9 percent. The detail tables
  recomputed their own numbers and disagreed with the headline.
- **Detection signal:** Round trip. Run the power analysis at the n the sample-size analysis
  returned; anything other than the target power is a bug.
- **Prevention rule:** When adding an adjustment, grep every analysis-type branch (sample
  size, power, effect, duration) and every test type; add a round-trip test per design.
  Detail tables must read the headline's numbers, never recompute them.

### An unexamined library default changed the design

- **Failure mode:** `gsDesign::gsSurv()` defaults to `test.type = 4`, a non-binding futility
  bound. The options exposed only efficacy spending and the table only efficacy bounds, yet
  the 3-look O'Brien-Fleming N was 618 instead of 588. Its own regression test used the same
  default as "ground truth", so it locked the error in.
- **Detection signal:** Comparing against `test.type = 1` while reviewing what the UI offers.
- **Prevention rule:** Pass every design-defining argument explicitly (test type, sidedness,
  spending functions). A test's reference call must not share the implementation's defaults.

### Diff tool reported only the first difference per table

- **Failure mode:** The golden-output diff printed the first differing row of each table, so a
  sensitivity table where all four rows changed looked like a one-row change. Separately, a diff
  that found only row-name changes printed nothing for 61 items, which looked like a silent failure.
- **Detection signal:** "Identical" rows 2-4 whose inputs had provably changed; "61 differing
  items" with no detail lines.
- **Prevention rule:** A verification diff must enumerate every difference (or print an explicit
  count per item) and state when differences are attribute-only (row names from rowKeys).

### Unanchored substring search selected the wrong YAML block

- **Failure mode:** `s.index("    - name: clinical_interpretation")` first matched a table
  *column* `          - name: clinical_interpretation`, because the 10-space line contains the
  4-space pattern.
- **Detection signal:** The script's own count assertion on the selected block failed before
  writing.
- **Prevention rule:** Anchor YAML item searches on a line start and exact indent
  (`\n    - name: X\n`). Assert on the selected segment before editing it.

### Sensitivity table compared two different formulas

- **Failure mode:** Scenarios used a 0.67 average-follow-up approximation without dropout; the
  base case came from the exact headline. The "change" mixed models, and for a longer control
  median the crude path gave a *smaller* N (554 vs 583) where the exact answer is larger (644).
- **Detection signal:** A zero-perturbation scenario should report no change; recomputing each
  scenario through the headline machinery disagreed.
- **Prevention rule:** Base case and scenarios must go through one function. Test invariance
  (unchanged input gives 0 percent) and that a scenario equals a full run at that value.

### A plot layer was silently dropped

- **Failure mode:** `ggplot(...) + geom() + if (cond) { plot <- plot + layer }` never adds the
  layer; the renderer still returned TRUE, so state and render tests passed.
- **Detection signal:** Counting `$layers` on the printed plot (2, expected 3).
- **Prevention rule:** Add conditional layers in a separate statement; build plots in a testable
  builder and assert the layer count.

### Test asserted a validation at the wrong entry point

- **Failure mode:** `expect_error(<fn>Options$new(number_of_arms = 2.5))` failed although the
  `type: Integer` fix works: `OptionInteger` validates in `check()`, which `run()` calls.
- **Detection signal:** Tracing construction, `check()`, and `run()` separately.
- **Prevention rule:** Test option validation through `run()` or `check()`, not construction.

### Overwrote an existing file after inferring it did not exist

- **Failure mode:** Concluded `jamovi/js/survivalPower.events.js` was missing from
  `ls jamovi/js | head`, which truncated the listing before `s`. Wrote a "new" file over
  a working handler, and reported the presets as dead when they were not.
- **Detection signal:** `git status --porcelain` showed ` M` (modified), not `??` (new).
- **Prevention rule:** Before creating a file, test the exact path
  (`test -e <path>` / `git ls-files <path>`). Never infer absence from a truncated
  listing (`head`, paged output). Before overwriting, look at the target.

### New statistical formula validated against the wrong reference

- **Failure mode:** Conditional power used the fixed-design drift `z_{a/2} + z_b`; a
  group-sequential design carries more information, so it understated CP by ~2.5 points.
  A first comparison against `gsDesign::gsCP` was invalid (it returns a design object,
  not a probability) and so proved nothing.
- **Detection signal:** Direct simulation of the B-value process disagreed (83.9% vs 86.4%).
- **Prevention rule:** Validate any new statistical quantity against a from-first-principles
  simulation, not against a library call whose return shape you have not confirmed, and
  never against the module's own arithmetic.

### Fixed one instance of a formatting bug, missed its siblings

- **Failure mode:** Corrected `round(alpha * 100)` ("2%" for 2.5%) in the report sentence
  but left the same expression in five other narrative sites.
- **Detection signal:** A regression test asserted on the whole interpretation panel,
  not only the sentence being fixed, and still found "2% significance".
- **Prevention rule:** After fixing a string/formatting defect, `grep` the pattern
  file-wide before declaring it fixed. Write the test against the rendered output, not
  the one call site.

### Same root cause patched at one caller while siblings stayed broken

- **Failure mode:** Six sites read `self$options$effect_size` raw instead of
  `.get_effect_hr()`; three copies of the effect-size solver had different brackets.
  Fixing one caller at a time left the plain-language summary inverting the conclusion.
- **Detection signal:** Comparing each narrative output against the value the analysis used.
- **Prevention rule:** When a derived value exists (`.get_effect_hr()`), grep for every
  raw read of its input. Consolidate duplicated solvers instead of patching each copy.

### `.u.yaml` `enable:` written with `==` inside an `||` chain

- **Failure mode:** Wrote `(test_type=="rmst_test" || effect_size_type=="rmst_difference")`.
  The client-side binding parser folds strictly left to right and does not list `==` as
  an operand form.
- **Detection signal:** Existing memory note on the `enable:` grammar.
- **Prevention rule:** Test List levels with the documented `name:level` form:
  `(test_type:rmst_test || effect_size_type:rmst_difference)`.

### Test inputs chosen outside the schema

- **Failure mode:** A test used `simulation_runs = 200` (schema `min: 1000`) and a
  pairwise reference computed at a different `allocation_ratio` than the case under test.
- **Detection signal:** Run error "simulation_runs must be between 1000 and 1e+05";
  a 942-vs-1032 mismatch.
- **Prevention rule:** Read the option's `min`/`max` in `.a.yaml` before choosing test
  inputs, and hold every parameter but the one under test fixed in a reference case.

### Wrongly concluded `private$.checkpoint()` did not exist

- **Failure mode:** Refused to add `private$.checkpoint()` to a 10,000-iteration Monte Carlo
  loop, reporting that it was absent from `jmvcore::Analysis` and unused in the module. Both
  claims were false: it is a `jmvcore::Analysis` private method called from 83 `.b.R` files,
  and `CLAUDE.md` says so explicitly. The evidence came from two searches that silently return
  nothing: `ls(Analysis$private_methods)` hides names starting with `.`, and
  `grep "private\$\.checkpoint()"` hands grep a `$` it reads as an end-of-line anchor.
- **Detection signal:** The user pointed to the project guidance; `grep -F` found 480+ calls.
- **Prevention rule:** When a search contradicts written project guidance, distrust the
  search. Search R accessors with `grep -F` (fixed string), and list R6 members with
  `names()`, never `ls()`. An empty result from either is not evidence of absence.
- **Placement rule it surfaced:** on restart, `.checkpoint()` raises an *error*-class
  condition, so any enclosing `tryCatch(error = ...)` swallows the restart. Re-raise it.
