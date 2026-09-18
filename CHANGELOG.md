# Engineering Lessons Log

Failure modes found during development, how they were caught, and the rule that
prevents them. Newest first. Release notes for users live in `NEWS.md`.

---

## 2026-09-18 — OncoPath `/check-module`: two checks that could not fail

### A plot-warning harness that was blind to the warning

- **Failure mode:** to verify the `waterfall` spider-plot fix (`geom_line(size = 1)` →
  `linewidth`), the harness rendered with `print(r$spiderplot)` and captured warnings. The
  pre-fix build printed "no warnings" too: jmvcore's `print()` path swallows renderer
  conditions, so the check passed on the broken code. The first report had also stated the
  warning "lands in Analysis Notes", which was never observed.
- **Detection signal:** running the same harness on a build of the *pre-fix* file
  (`R CMD INSTALL` into a scratch library). Identical output before and after meant the check
  measured nothing.
- **Prevention rule:** before trusting a harness, run it on the unfixed code and see it fail.
  To observe a renderer, call it directly:
  `img$analysis$.__enclos_env__$private$.<renderFun>(img, ggtheme = ggplot2::theme_grey(), theme = list())`
  inside `withCallingHandlers()`, with `options(lifecycle_verbosity = "warning")` so
  once-per-session deprecations always fire.

### A `size` → `linewidth` sweep that only knew ggplot2's geom names

- **Failure mode:** the first sweep listed ggplot2 line geoms by name, fixed 5 sites, and
  missed `ggswim::geom_swim_arrow(size = 1.5)`. That layer draws on every swimmerplot with a
  censor variable and kept emitting the deprecation warning.
- **Detection signal:** the release-profile review (`S46`/`S47`); reproduced with the
  direct-renderer harness above.
- **Prevention rule:** decide per layer from the geom itself. Build the layer, then read
  `layer$geom$default_aes`: flag `size =` only where `linewidth` is an aesthetic and `size` is not.
  This covers extension packages (ggswim, ggrepel …) that no name list keeps up with.

### A release-review regression test that reported as a skip

- **Failure mode:** `test-ihcheterogeneity-release-review.R` "Levene's test … actually reports a
  result" filtered `tt$test` (the column is `test_type`) for "Levene" (the row became
  "Brown-Forsythe"), and put every expectation behind `if (nrow(lev) > 0)`. It could never fail;
  testthat counted it as an empty-test SKIP.
- **Detection signal:** a SKIP in a run where the skip's stated reason (psych missing) was false.
- **Prevention rule:** assert the row exists (`expect_equal(nrow(lev), 1)`) rather than guarding
  the assertions with `if`; treat every unexplained skip as a dead test.

## 2026-09-18 — OncoPath `devtools::document()` stopped on shared `utils.R`

- **Failure mode:** `✖ utils.R:82: @details has mismatched braces or quotes.` The `.fmt()` docs
  say "contains a `` `{` ``". The umbrella and the other four submodules run roxygen in markdown
  mode and render it as `\verb{\{}`. OncoPath's hand-maintained DESCRIPTION had no
  `Roxygen: list(markdown = TRUE)`, so the raw `{` reached the Rd, and every other backtick in
  OncoPath's docs rendered as literal text.
- **Detection signal:** `devtools::document()` in OncoPath only; the umbrella documented cleanly.
- **Prevention rule:** `apply_distribution_plan()` sets the `Roxygen` field on every submodule
  (asserted in `test-update-modules-plan.R`). Don't put a lone brace in roxygen prose, even in
  backticks; the `.fmt()` text now says "an opening brace".

## 2026-09-17 — library audit round 4 (OncoPath): our own remediation caused five of eight findings

### `waterfall` could not run at all in the shipped module

- **Failure mode:** round 2's cleanup put `magrittr` in OncoPath's `prune_imports`
  (`55a4e7186`); 114 `%>%` uses in `waterfall`/`swimmerplot` lost their import.
- **Detection signal:** the library reviewer (`2026-09-16 OncoPath` [CRITICAL]). Every local check
  passed: `~/.Rprofile` attaches magrittr, `load_all()` sees everything, `R CMD check` only NOTEs it.
- **Prevention rule:** names must resolve from the submodule's installed namespace; enforced by the
  bare-symbol guard and `Rscript --vanilla tools/submodule_smoke.R <sibling>`; guide §19.

### Every OncoPath install carried 7.3 MB of another module's translations

- **Failure mode:** round 2 answered "no catalogs" by copying the umbrella catalog through
  `i18n_files` (`99779adfe`); 31,237 of 32,761 msgids were unused (same in CPD and meddecide).
- **Detection signal:** the library reviewer (`2026-09-16 OncoPath` [MEDIUM]); no check existed.
- **Prevention rule:** `build_module()` runs `jmvtools::i18nUpdate()` before `prepare()`
  (keeps every used translation, drops the rest); `release_gate.py` `check_i18n_catalog_scope`
  (WARN until CPD/meddecide/jsurvival are regenerated); guide §9.

### Turkish output printed "Inf", doubled words, cut sentences short, or stopped with a format error

- **Failure mode:** the August i18n pass (`0551a2d57`) spliced band words into sentences
  (`ile ile`), compared `.("not estimable")` with English (always TRUE once translated: `Inf`
  printed, `NaN` crashed), kept `[[APPROX]]` after a space (jmvcore cuts ` [..]` when a string has
  no catalog entry), and wrote braced `\u{2265}` inside `.()` (never extracted, so never
  translated: 110 shipped sites). Separately `tr.po` rendered `100%%` as `%%%100` and `50%%` as
  `%%%50`, which `sprintf()` rejects — `diagnosticmeta` and jsurvival `singlearm`, Turkish only.
- **Detection signal:** the library reviewer (`2026-09-16 OncoPath` [LOW] padding/fragments and
  [INFO] TODOs); the rest by running the builders with the Turkish catalog and a `«…»`
  pseudo-catalog. English output and every existing test were clean.
- **Prevention rule:** whole `.()` sentences per band; branch on untranslated keys; no ` [` and no
  `\u{}` inside `.()`; translations keep every conversion. Enforced by `release_gate.py`
  `check_i18n_bracket` (FAIL), `check_i18n_braced_escape`, `check_i18n_po_formats`,
  `check_i18n_padding`, and the pseudo-translation test in `test-oncopath-library-audit.R`;
  guide §7, §9; `jamovi_i18n_guide.md` §5.6, §11.7. Guide §7 and five command checklists had
  taught the braced escape — corrected.

### Notice titles fell below 3:1 contrast in the dark theme

- **Failure mode:** severity-coloured titles on a translucent tint, under a round-2 comment
  claiming they were "saturated enough to read on both" themes: `#dc2626` 2.93:1 and `#2563eb`
  2.74:1 on a dark pane. The pattern was copied from `waterfall`, the documented reference
  implementation (19 renderers in the umbrella).
- **Detection signal:** the library reviewer (`2026-09-16 OncoPath` [INFO], suggesting native
  `type: Notice`, which still does not compile); contrast measured before replying.
- **Prevention rule:** notice titles `color: inherit`; `check_notice_title_colour` (WARN); guide §4.
  A comment asserting a measurable property must have been measured.

### A reviewer-suggested flag removal would have silently dropped a plot's annotation tracks

- **Failure mode:** the report listed `waterfallplot` among six state-only renderers; its helper
  `.annotationTrack()` reads `self$data`. `image$.render()` in tests nulls the data before the
  renderer runs, so it cannot show the difference.
- **Detection signal:** the helper trace in `release_gate.py`, confirmed on the engine path
  (`.createImage()` with a counting dataset source).
- **Prevention rule:** trace helpers before removing `requiresData`; test flagged images through
  `.createImage()`; guide §15, `jamovi_plots_guide.md`.

### `NEWS.md` one release behind in all five modules

- **Failure mode:** `_updateModules.R` rewrites `Version:` on every regeneration and never writes
  `NEWS.md`.
- **Detection signal:** the library reviewer (`2026-09-16 OncoPath` [LOW], meddecide [INFO]).
- **Prevention rule:** `release_gate.py` `check_news` (WARN); guide §1.

## 2026-09-16 — `_updateModules`: three hand-kept ways to ship a helper, and a build that could not fail

### A shared helper was claimed by one analysis's file name

- **Failure mode:** helpers reached submodules three ways that disagreed. A file-name regex
  `^<analysis>[-_].*\.R` treated any file starting with an analysis name as that analysis's
  companion. Beside it sat a hand-kept `r_files` list and a symbol generator. `survival` claimed
  `survival_utils.R`, which 15 analyses in three modules use, so re-routing `survival` would have
  deleted it. Four data-doc files were copied as "companions" into modules without their datasets.
  A pruned file came back on the next run because the copy step ran after the prune step.
- **Detection signal:** a read-only review of the updater before restructuring it, plus a routing
  and helper census of all 390 analyses.
- **Prevention rule:** the file name decides nothing. `_updateModules_plan.R` resolves which
  helper files an analysis needs from the symbols its code uses: calls, values, infix operators and
  `exists("f")`/`get0("f")` strings, followed transitively. Anything the updater manages but no
  longer plans is deleted, except an explicit hand-maintained allowlist. Names are for people:
  `<analysis>-<topic>.R` for one analysis, `utils-<topic>.R` for shared helpers.
  `test-zzz-analysis-file-naming.R` fails when a name disagrees with the actual callers.

### Build errors printed "All jamovi modules built" and exited 0

- **Failure mode:** each of the six copy-pasted build blocks wrapped `prepare()`, `document()`
  and the Imports sync in `tryCatch(..., error = warning)`. "completed successfully!" was printed
  before any build started. `jmvtools::prepare()` itself exits 0 on a YAML compile error. OncoPath
  shipped a `Collate:` naming 12 deleted files; roxygen never removes a stale Collate, and
  nothing else touched it.
- **Detection signal:** injecting failures into sandbox clones (APFS `cp -Rc`) of the sibling repos.
- **Prevention rule:** one pipeline for every module (plan -> apply -> build -> verify -> install).
  A plan error writes nothing. Build steps run in `Rscript --vanilla` children whose logs are
  scanned. A module that fails verification (Collate, `pkg::` declarations, `prune_imports`,
  bare-symbol resolution) is never installed. The summary is printed last and the exit status is 1
  on any failure. Tested by three injected failures: unresolved `str_detect`, a load-time
  `stop()`, and a broken `.u.yaml`.

### An empty test module stopped the whole run

- **Failure mode:** once all 59 T analyses moved to P, `TEST: true` failed planning with
  `R/.b.R does not exist`. The helper seeds were `paste0(analyses, ".b.R")`, and `paste0()` turns a
  zero-length vector into `".b.R"`. The zero-analysis unit test checked the deletions but never
  `plan$errors`. Two smaller problems in the same case: an empty module would still have gone to
  `prepare()`/install, and the allowlist pattern `-data\.R$` kept the stale legacy file
  `pcaloadingtest_data-data.R`.
- **Detection signal:** the user ran it; reproduced with `Rscript _updateModules.R --dry-run JamoviTest`.
- **Prevention rule:** use `paste0(..., recycle0 = TRUE)` over any vector that can be empty. A test
  for an edge case asserts `plan$errors` as well as the outcome. A module with no analyses is
  pruned (files and `0000.yaml` entries), reported as `EMPTY`, and never built. Allowlist patterns
  are anchored to the whole name (`^[A-Za-z0-9.]+-(package|data)\.R$`).

### "Test" meant both "under test" and "not ready"

- **Failure mode:** 59 analyses had sat on `...T` menu groups for weeks, so JamoviTest mixed work
  under active test with work that was simply unfinished. Four more used T groups that no rule
  matched, so they were silently dropped.
- **Detection signal:** the routing census; the user asked for four categories.
- **Prevention rule:** production `<Group>`, tests `<Group>T` (JamoviTest), pending `<Group>P`,
  drafts `<Group>D`. Routing is exact, and any other group is a plan error. The 59 moved to P.

---

## 2026-09-16 — library-audit tooling: checks that looked fine and were not

### A plain `Rscript` passed the OncoPath build that could not run `waterfall`

- **Failure mode:** `~/.Rprofile` runs `library(magrittr)` in every directory without its own
  `.Rprofile` — every sibling submodule repo. The umbrella's commented-out `.Rprofile` shadows it, so
  behaviour depended on the working directory: an installed-namespace scan of pre-fix OncoPath
  1.0.81 reported `UNRESOLVED %>%` from the umbrella and `PASS` from `/tmp`. Nothing checked the
  *installed* submodule at all, so the 2026-09-16 CRITICAL reached the reviewer.
- **Detection signal:** building the `library-audit` skill; positive control
  `git -C OncoPath archive 49e259a` run with and without `--vanilla`.
- **Prevention rule:** submodule checks run `Rscript --vanilla`. `tools/submodule_smoke.R` installs a
  sibling into a temp library, resolves every function called in every R6 method from the installed
  namespace, and refuses to run in a session with anything extra attached (exit 3). Its first run on the
  live OncoPath working tree failed to install — uncommitted state only, the committed build installs:
  a stale `Collate:` naming 12 missing `stagemigration*` files and 8 undeclared `NAMESPACE` imports.
  Nothing in `_updateModules.R` cleans `Collate:`.

### The release gate's citation check skipped any `refs:` list containing a comment

- **Failure mode:** `check_refs()` matched `refs:` items with a regex that stopped at the first
  non-item line, so a comment under `refs:` hid every key after it from the dangling-key FAIL.
  `decisioncurve.r.yaml` hid all 6 of its keys that way.
- **Detection signal:** fact-checking the library-audit skill's breadcrumb placement rules.
- **Prevention rule:** comment lines are allowed inside the list and keys are read only from item
  lines (umbrella: 413 → 417 cited, still 0 dangling; all five siblings 0 dangling). Breadcrumbs go
  above `refs:`, never inside it.

### `tools/annotate_audit.py` deleted every response in any report it touched

- **Failure mode:** `strip_old()` removed all `<!-- response:start -->` blocks in a file before writing
  the JSON's entries, so a hand-written response (the 2026-09-16 OncoPath CRITICAL) or a trailing
  block (four "UMBRELLA NAMESPACE REVERIFIED") vanished on the next run. `--check` was parsed and
  ignored, the date silently defaulted to 2026-08-20, CRITICAL was not a known severity, and the
  docstring and STATUS.md claimed it regenerated STATUS.md, which it never did.
- **Detection signal:** reading the script before reusing it for the new reports.
- **Prevention rule:** the script replaces only the findings named in the JSON; `--selftest` asserts a
  hand-written block survives, a no-op run is byte-identical and re-runs are idempotent; all 16 report
  files verified byte-identical on a no-op run.

### A "no-op" mirror into a sibling changed it

- **Failure mode:** verifying `tools/mirror_to_submodule.R` by mirroring files believed identical, one
  was `R/utils.R` — an `r_symbol_files` source, so the whole spec regenerated and carried a same-day
  umbrella change (`survival_utils.R` narrowed to `.medianFollowUp`) plus an umbrella test copy into
  the dirty OncoPath tree. No snapshot had been taken.
- **Detection signal:** a before/after fingerprint of `git diff` + `git status`; restored by trying
  HEAD/current combinations until the fingerprint matched.
- **Prevention rule:** the mirror script takes `git stash create` itself, prints `SNAPSHOT <sha>` first
  and `CHANGED since <sha>` last; restore unintended drift with
  `git restore --source=<sha> --worktree -- <path>` (not `checkout`, which also stages). `--regen`
  restores the `inst/i18n/*.json` catalogs that `jmvtools::prepare()` deletes.

---

## 2026-09-16 — survival_utils.R distribution: a code generator that changed the code it copied

### NA_real_ reached OncoPath as a logical NA

- **Failure mode:** `distribute_selected_r_symbols()` rebuilds selected helpers with
  `deparse(control = "keepInteger")`. Without `keepNA`, `NA_real_` and `NA_character_` come out as a
  plain `NA`, so OncoPath's `.medianFollowUp` returned logical CI bounds while the umbrella's were
  double. swimmerplot happened to read them only through `is.na()`, so nothing visible changed.
  Every generator error was also downgraded to `warning()`, which kept the old file and installed anyway.
- **Detection signal:** comparing each shipped definition with the umbrella one via
  `identical(deparse(x), deparse(y))` using the *default* control. It flagged a file regenerated
  minutes earlier, so the difference could not be staleness.
- **Prevention rule:** a generator that re-renders code must check that the parse tree round-trips
  and stop if it does not. Both are now in place, along with an `NA_real_`/`NA_character_` fixture in
  `test-update-modules-dependency-guard.R`, and R-file copy errors now stop the run.

### Same helper, different output per module

- **Failure mode:** `.fmtTimeLabel` called bare `format()`. Under `import(jmvcore)` (umbrella,
  OncoPath) that is `jmvcore::format`, which returns the number and ignores `nsmall`, giving "25".
  jsurvival imports only `.` from jmvcore and printed "25.0".
- **Detection signal:** reviewing the shipped copies module by module against each NAMESPACE.
- **Prevention rule:** shared helpers call `base::format` explicitly. Guarded in `test-median-followup.R`.

---

## 2026-09-16 — lassocox: an output column addressed by position instead of by row number

### Saved risk scores landed on the wrong patients whenever a jamovi filter was active

- **Failure mode:** `.savePlotData()` wrote the risk-score Output with `setValues(<vector>)` and no
  `setRowNums()`. jmvcore fills row numbers only for a data.frame, so none were sent, and jamovi's
  server (`analyses/analysis.py`) falls back to `row_nums = range(n_rows)` — value *i* goes to row
  *i*. jamovi hands an analysis only the rows that pass its filters, keeping their original row
  names, so every score shifted. With an `er_status` filter, 68 of 68 scores sat on the wrong row.
- **Detection signal:** `/check-function-full lassocox`, reading the Output write. The release pass
  an hour earlier had cleared the analysis, because testthat never filters rows: row names are
  always 1..n there, which is the one case where position and row number agree.
- **Prevention rule:** `setRowNums(rownames(self$data))` immediately before every Output
  `setValues()`. Guarded by `tests/testthat/test-lassocox-output-rows.R`, which passes
  `data[-(1:20), ]` and asserts the element's row numbers. Still unaudited elsewhere: cotest,
  dendrogram, lassologistic, retracted, sequentialtests, survivalfeaturerank.

### Four copies of a fallback panel, each with newlines inside a translatable string

- **Failure mode:** `.survivalPlot()` repeated the same 18-line grid-text block four times, two of
  them in branches that could not run, and each `.()` string carried `\n\n` and bullet characters
  that a translator cannot see are load-bearing.
- **Detection signal:** the i18n checklist item "no `\n` inside a `.()` string"; the release gate's
  padding check does not look for newlines.
- **Prevention rule:** one `private$.drawNotice(paragraphs)` helper joins the paragraphs, so each
  `.()` wraps a single complete sentence. A test asserts no `.()` in the file contains `\n`.

---

## 2026-09-15 — lassocox release check: a policy fixed in the shared coder, not in a sibling copy

### lassocox inferred the event from level order and numeric size

- **Failure mode:** With no Event level, lassocox took `sort(levels)[2]` for a factor
  ("Died"/"Survived" made *Survived* the event) and `max()` for numeric codings such as 1/2.
  With no Censored level it took `min()`, so a 1/2 outcome with event 1 stopped with "Event level
  and censored level must be different". The same day's multisurvival fix had already made
  `.defineEventIndicator()` refuse to guess, but lassocox keeps its own two-level coder.
- **Detection signal:** `/check-function lassocox --profile release`, while explaining why
  `censorLevel = NULL` changed no output; the `.a.yaml` description documented the guess as intended.
- **Prevention rule:** When an outcome-coding policy changes, grep every coder, not only the shared
  helper (`observed_levels[2]`, `max(observed`, `levels(x)[2]`). Only numeric 0/1 has a default; the
  censored level may default to the other observed value. Guarded by
  `tests/testthat/test-lassocox-event-coding.R`.

---

## 2026-09-15 — jamovi library audit round 3 (jsurvival): why written rules did not hold

### Guides taught `requiresData: true` as boilerplate

- **Failure mode:** `multisurvival` `plot_adj` / `survMetricsPlot` refit the Cox model from
  `self$data` in the renderer without `requiresData: true` → "Data contains no (complete) rows"
  on resize, `.omv` reopen and export (HIGH); 29 other images carried the flag for nothing.
  Every guide example showed the flag, and the plots guide table gave its default as `true`
  (jmvcore's is `FALSE`).
- **Detection signal:** Library reviewer; confirmed in jmvcore 2.7.38 source (`run()` nulls
  `private$.data`; `.createImage()` re-reads only for `requiresData`). Invisible to testthat
  because the R wrapper passes `data =` (`.dataProvided` defaults `TRUE`).
- **Prevention rule:** `tools/release_gate.py` `check_requires_data` traces each renderer through
  its `private$` helpers (FAIL for shipped analyses). Guide §15. When a rule says "renderers run
  without `.run()`", apply it to every renderer input, not only `image$state`.

### Templates taught the catch-all `tryCatch` around `reject()`

- **Failure mode:** `lassocox` `.run()` wrapped all validation in `tryCatch(error=)`, swallowing
  33 `jmvcore::reject()` messages and deleting `.init()` rows (MEDIUM). The same shape was in the
  `create-function` `.b.R` template and notices guide §8.
- **Detection signal:** Library reviewer.
- **Prevention rule:** Template and guide corrected; guide §16. Wrap only the third-party call;
  an uncoded `reject()` cannot be told apart from a library error by class.

### A noisy check and instance-level fixes let a known class recur

- **Failure mode:** 14 new `.()` strings with a leading space/punctuation after August's sites
  were fixed. The checklist grep `'\.\(" |\ "\)'` returned 200 hits on jsurvival (every
  `collapse = ", "`), so nobody could read it. `CollapseBox` Title Case (§12) had no check at all.
- **Detection signal:** Reviewer; the tightened regex finds all 14 (the report names 13 locations; the 14th is `survivalcont.b.R:1554`).
- **Prevention rule:** A rule ships with a machine check validated against the reviewer's list
  (`check_i18n_padding`, `check_collapsebox_titlecase`). A check with hundreds of hits is not a check.

### Generator counted commented-out refs as citations

- **Failure mode:** `_updateModules.R` `collect_used_refs()` matched `#   refs: ggstatsplot`, so the
  per-submodule trim kept a dead `00refs.yaml` entry.
- **Detection signal:** Reviewer flagged the key; `grep` found only a commented-out reference.
- **Prevention rule:** Skip comment lines in the extractor (fixed). Audit refs in the generated
  submodule, not only the umbrella.

### Reviewer's fix would have reintroduced a crash

- **Failure mode:** The report proposed `.()` around two `reject()`s in `.eventIndicator()`, a
  file-level helper — issue #122 ("object 'self' not found").
- **Detection signal:** File header comment + jmvcore source (`.()` = `eval.parent(self)`).
- **Prevention rule:** Verify the suggested fix, not only the problem. Guide §9 and the i18n guide
  now say so explicitly.

### Fix pass: a failed `prepare()` exited 0, and my YAML check could not see why

- **Failure mode:** Adding `url:` to the `dichotomizing` citation created a duplicate key — the
  umbrella entry already had a DOI url on the line just past a truncated `grep -A8`. PyYAML's
  `safe_load` accepted the file silently; jamovi's compiler rejected it, and
  `jmvtools::prepare()` printed the parse error but **exited 0**, so no `.h.R` was regenerated.
  The reviewer had seen an older submodule copy, not a missing url in the source.
- **Detection signal:** The prepare log tail showed a YAML error at `00refs.yaml:1597`, and
  `git diff --stat R/*.h.R` was empty although 29 `requiresData` lines had been removed.
- **Prevention rule:** Never trust `prepare()`'s exit status: after every run, check the log for
  errors **and** confirm the expected generated diff exists. Don't validate jamovi YAML with PyYAML
  alone (duplicate keys pass). Read a YAML block to its end, not through a fixed `-A` window, and
  compare the umbrella source with the submodule before "fixing" what a reviewer saw there.

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
