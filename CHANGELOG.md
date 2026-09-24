# Engineering Lessons Log

Failure modes found during development, how they were caught, and the rule that
prevents them. Newest first. Release notes for users live in `NEWS.md`.

---

## 2026-09-24 — explanatory text drifted from the limits it explains (`agreement` grading section)

### The guide and the refusal sentences quoted figures the gates never used

- **Failure mode:** the simulation grid was rerun and `AGREEMENT_LATENT_GATES` updated, but the
  guide still said "model-based kappa from 30 cases", "intervals only with 4 pathologists" and
  "a median of 6 ratings per case", against gates of 50 cases, 3 pathologists and never shown.
  The refusal for 3-pathologist intervals quoted 85-91% coverage, but that was measured at 30-50
  cases. At 100 or more cases, 3 pathologists covered 0.922-0.979, so the limit itself was set
  on evidence from the wrong region. The skew sentence blamed a 70% result on every share from
  50% up, when κ_m passed at 58%.
- **Detection signal:** an adversarial review that read every sentence against the constants and
  against `summary_latent.csv`, not against the code.
- **Prevention rule:** a number that explains a gate is built from the gate's constant
  (`sprintf(.(…), G$…)`), never typed. A simulation figure quoted to a user names the cell region
  it came from. When a limit moves, re-derive every quoted figure from the summary file. The
  pinned-constants test says which texts assume which constants.

### The all-ratings kappa fix reached the headline and missed three siblings

- **Failure mode:** the headline Fleiss/Conger moved to every observed rating
  (`.fleissAllRatings`). The bootstrap row (same label), the hierarchical per-institution kappa
  and the bootstrap percent agreement still dropped cases, or counted a single rating as
  agreement. On incomplete data each printed a different number under the headline's name.
- **Detection signal:** the review; tests now compare each sibling with the headline on data
  with missing ratings (`test-agreement-fixes-2026-09-24.R`).
- **Prevention rule:** same as the entry below. After changing how a statistic is computed,
  `grep` for every other site that prints it under the same label, and test them against the
  headline on the input that exposed the difference (here, missing ratings).

### The grid checked validity at study sizes up to 200 x 10; nobody timed 1000 x 20

- **Failure mode:** the grading-tendency fit (`clmm`, one fixed effect per pathologist) scales with
  ratings x pathologists: 14 s at 300 x 10, 439 s at 1000 x 20, with no guard and no warning. The
  simulation grid had never gone above 200 cases or 10 raters, so the tests only ever saw fast fits.
- **Detection signal:** a runtime reviewer that stress-tested sizes beyond the grid.
- **Prevention rule:** for any model fitted inside an analysis, time it at the largest study a
  user plausibly has, not just at the validation sizes, and add a pre-fit ceiling with a note
  when the fit cannot be made fast. The same reviewer found that single-read cases counted toward
  the case minimums: a gate must count what the simulation counted.

### A section's tests reported 0 pass, 3 skip, and looked green

- **Failure mode:** `test-agreement-grading-section.R` uses `skip_on_cran()`. When run through
  `testthat::test_file()` from `Rscript`, `NOT_CRAN` is unset, so every block was skipped and the
  file printed no failures.
- **Detection signal:** a per-file skip count in the suite runner.
- **Prevention rule:** set `Sys.setenv(NOT_CRAN = "true")` in any runner outside
  `devtools::test()`, and treat a skip count above zero as something to explain.

---

## 2026-09-24 — a fix applied to one member of a family is the dominant failure in `agreement`

### Eleven defects were the SAME shape: correct at one site, missed at its siblings

- **Failure mode:** `/check-function-full agreement` found that most of its defects were fixes
  applied to one member of a family and not to the others. `.checkpoint()` was in 3 of 4
  `nBoot`/`pairedBootN` loops, so the fourth froze the pane uncancellably at up to 10,000
  replicates. The `dropped_replicates` disclosure was in 1 of 2 bootstrap tables.
  `.mergeDeclaredLevels()` was used by 2 of 4 scale-building sites; Gwet AC2 and Krippendorff
  built their ordinal scale from the column-order union, so G1 < G3 < G2. The
  `vapply(as.character)` factor-code fix was at 3 of 6 row conversions. The `.init()` retitle of
  mislabelled ICC columns covered 2 of 3. Finn's scale-size guard protected the factor path and
  not coded-numeric ratings, where `finnLevels = 10` turned 0.000 "Poor" into 0.919
  "Outstanding" in silence. The 0/0-to-`NA` rule was fixed in one per-class F1 table and not the
  other.
- **Detection signal:** reading each sibling after finding the first. A parallel sweep per defect
  class found siblings the audit had missed: Robinson's A's bootstrap and three per-case loops had
  no checkpoint, a fourth naive union sat in `.calculateBootstrapCI`, and Robinson's A and case
  clustering had error handlers that would swallow a restart.
- **Prevention rule:** after fixing any defect, state its CLASS and sweep every member before
  closing, preferably with a whole-file mechanical check. Two now exist for this file: every
  `.checkpoint()` inside a `tryCatch` must sit under a handler that re-raises
  `code == "restart"`, and every `self$options$X` read must name an option in `.a.yaml`. The
  second caught a read of a non-existent `caseClusterHeatmap` option minutes after it was
  written; jmvcore raises on it.

### A formula oracle is not a truth oracle

- **Failure mode:** the new binary route's latent-scale ICC matched a direct `lme4::glmer` refit
  to 1e-5, so the implementation was right, but the NUMBER was wrong. glmer's Laplace
  approximation shrinks the case variance with few ratings per case. In simulation the latent ICC
  ran -0.19 low at 2 raters, -0.08 at 3, -0.05 at 4 and -0.02 at 5
  (`development-scripts/validate_agreement_latent_icc_bias.R`), and 2 raters is the commonest
  pathology design.
- **Detection signal:** one simulated dataset came back 0.507 against a true 0.556. It was
  quantified before being dismissed as noise.
- **Prevention rule:** a new estimator gets two checks, never one. Check the formula against an
  independent call, and check recovery against simulated truth across the design range users will
  hit. The route is now withheld below 5 raters, with the measured figures in its note.

### Tests that pinned the defect

- **Failure mode:** three existing tests asserted the old wrong behaviour and failed only when it
  was fixed. `test-agreement-itemmodal.R` asserted the zero-width `[1, 1]` interval, a fabricated
  claim of perfect precision. `test-agreement-release-review.R` asserted the min-single-cell
  sparse note, which fires on nearly every 3x3 table. VAL-agreement-06 asserted that categorical
  ratings fill no hierarchical table. Separately, 136 of 269 agreement `test_that` blocks were
  smoke-only (`expect_s3_class`), including the block that loads `agreement_perfect` and asserts
  nothing about its kappa.
- **Detection signal:** the full-suite run after the fix, compared against a baseline taken before
  any edit (744 pass / 0 fail).
- **Prevention rule:** take the baseline first. Read every newly failing test before touching it,
  because a test that pins a defect is updated to the corrected contract with the reason in a
  comment, not deleted. Run every new regression test against the UNFIXED code too: all 18 new ones
  failed there, which is the only evidence they guard anything.

---

## 2026-09-23 — a new option can change a table's numbers without being one of its dependencies

### `kappaCIMethod` reached five tables and was in none of their `clearWith` lists

- **Failure mode:** the *Kappa interval method* option (added the same day) changes the limits in
  five tables through one shared helper, `.pairKappaWithCI()`. No `.r.yaml` item named it under
  `clearWith`, so in jamovi a user switching Wald to Fisher z kept the old interval on screen until
  the re-run overwrote it. (Row duplication could not follow here: the reset block at the top of
  `.run()` calls `deleteRows()` on every `rows: 0` table. An earlier draft of this entry said
  otherwise; an adversarial review of the pass caught it.)
  Every test passed: the R wrapper builds a fresh results object per call, so a missing
  `clearWith` is invisible to testthat by construction.

- **Detection signal:** `/check-function agreement --profile release`. The option inventory
  records, per option, whether the `.r.yaml` mentions it anywhere (`visible:` or `clearWith`);
  `kappaCIMethod` was the only computational option with no `.r.yaml` reference at all. That
  column, not a test, found it.

- **Prevention rule:** an option is either cosmetic or a dependency. When a new option is read by
  a method that writes into a table, add it to that table's `clearWith` in the same edit, and
  trace the helper to every *consumer of its output*, not every caller. This pass traced the five
  callers of `.pairKappaWithCI()` and still got two things wrong: the intra-rater caller took the
  helper's SE and rebuilt its own Wald interval, so the option never reached that table although
  the description now said it did; and the subgroup forest plot and the summary panel draw the
  same interval without being callers at all, so they kept stale limits. A release gate check for "option read by a
  table-writing method but absent from that table's `clearWith`" is filed in TODO.md.

- **Same pass, same shape:** two tables declared CI columns that `.init()` hid unconditionally
  and `.run()` never filled — a structural fact expressed as a runtime toggle. The declarative
  form (no column) removed eleven lines and a `setVisible(FALSE)` that the review checklist
  reads as a failure signal.

- **Same pass, `check_external`:** `irrCAC::gwet.ac1.raw()` in irrCAC 1.4 (CRAN 2026-04-27)
  returns a ONE-SIDED p, `1 - pt(AC/SE, n-1)`; irrCAC 1.0 (CRAN 2019-09-23, the only earlier
  release) returned the two-sided `2*(1-pt(|AC/SE|, n-1))`. The analysis displayed the 1.4 value
  as *p-value* beside two-sided kappa tests. The validation suite pinned Gwet's *coefficient*
  against irrCAC and never its p. Found by deparsing the installed 1.4 function; the package ships
  no NEWS. Rule: when a table cell comes straight from an upstream field, pin every displayed
  cell (estimate, SE, interval, p), and read the upstream formula for the p once.

- **The first fix was wrong too, and its test could not tell.** The pass folded irrCAC's value,
  `2 * min(p, 1 - p)`. That is right on 1.4 and doubles the p on 1.0, and `DESCRIPTION` does not
  pin irrCAC. The fix now recomputes the two-sided p from the coefficient and SE, which both
  versions return. The pass's regression test compared `2 * min(p1, 1 - p1)` with the module on a
  fixture where AC/SE was 13: `1 - pt(13, 66)` is exactly 0 in double precision, so the test
  compared 0 with 0 and passed against the unfixed code as well. **Prevention rule: before a
  p-value regression test is credited, compute the old and the new value on its fixture and
  assert they differ** (`expect_gt(abs(new - old), tol)`). A strong-signal fixture underflows
  every p to 0, and every p-value transform agrees at 0. Pick a fixture with the statistic near
  1 to 3, and add one on the other side of zero when the transform has a sign.

---

## 2026-09-23 — a bad argument in a vignette was hiding a bug in the analysis

### Correcting a test input revealed a defect that errored a whole feature

- **Failure mode:** `agreement`'s Mixed-Effects Comparison passed the
  `multipleTestCorrection` OPTION LEVEL straight into `stats::p.adjust()`. Those are two different
  vocabularies: the level for Benjamini-Hochberg is `bh`, while `p.adjust()`'s methods are
  `holm`/`hochberg`/`hommel`/`bonferroni`/`BH`/`BY`/`fdr`/`none`. So selecting
  "Benjamini-Hochberg (FDR)" reached `match.arg()` and **errored the whole analysis** — the user got
  no result, not a wrong one. The sibling call site in the same file mapped the level through a
  `switch()` correctly; only one of the two did.

- **Detection signal:** nothing in the module. The shipped vignette had been calling the analysis
  with `multipleTestCorrection = "BH"` — an *invalid option level*, which jamovi's own option layer
  rejected first. That rejection masked the backend bug completely. The defect only appeared when
  the vignette was corrected to pass the real level `bh`, and the error moved one layer down from
  the option validator to `p.adjust()`. Two wrongs had been cancelling for as long as both existed.

- **Prevention rule:** **an invalid input in a test or a vignette is not a cosmetic problem — it is
  a blindfold.** When a fixture supplies a value the schema rejects, every layer behind that
  validator goes untested, and the document that was supposed to demonstrate the feature is
  demonstrating the error message instead. Fix invalid fixture arguments when you find them, and
  re-run: the interesting failures are the ones that appear afterwards. Related: `error = TRUE` in a
  knitr chunk turns every failure into rendered output, so "render succeeded" says nothing about
  whether the document is correct. Check the rendered artefact for `Error`, not the exit status —
  this vignette re-knitted "successfully" at one point into 135 KB of cascading errors, down from
  2.5 MB, because a data path did not resolve.

- **Second rule:** **an option level is not an API argument.** Wherever a `type: List` level is
  handed to a third-party function, the mapping belongs in one named helper that both ends share.
  Two call sites, one mapped and one not, is the shape this defect always takes.

---

## 2026-09-23 — a green suite can be green because it stopped looking

### Three vacuous guards, found only because something forced them to be re-read

- **Failure mode:** across three rounds of work on `agreement`, three separate checks reported
  success while testing nothing. (1) `IR2-07b` in the acceptance harness was written with the
  literal string `"PASS"` in its result column; it reported PASS while its observed value was `NA`,
  and had never compared anything. (2) A neighbouring assertion compared six cells that were all
  `NA` on both sides — `all.equal(NA, NA)` is `TRUE`, so it passed vacuously; the fixture had
  drifted onto the variance boundary where the fix correctly blanks those cells. (3)
  `test-benford-release-review.R` walks benford's AST for `jmvcore::format` calls — the moment the
  module-wide sweep converted all 47 of them, that walk would find zero, `problems` would be
  `character(0)`, and the test would have gone green **because its subject no longer existed**.
  Nobody would have noticed: the file's name and its passing status would both look right.

- **Detection signal:** none of the three was caught by running the suite — running the suite is
  what produced the false green. (1) was caught by reading the matrix CSV and noticing an
  `observed` column that said `NA` next to a `PASS`. (2) was caught by asking why (1)'s fixture had
  no numbers in it. (3) was caught by an agent scoping the sweep, which asked what each existing
  test would still be testing *after* the change it was about to make.

- **Prevention rule:** **a check that can only be read as passing is not a check.** Three concrete
  habits, all of which paid here. Never write a literal verdict into a result table — compute it
  from the comparison, so the row can go red. Before crediting any numeric agreement, require the
  compared values to be finite; `all.equal` treats `NA == NA` as agreement, and so does
  `all(is.na(x))` on a zero-length vector. And when a change removes the thing a test searches for,
  the test is part of the change: retarget it in the same pass, or it becomes a permanent
  false green.

- **Second rule, same pass:** **when a mechanical sweep is proposed, the deliverable is the trap
  list, not the sweep.** A 430-site rewrite across 34 files was safe as a one-line `sed` for 429 of
  them — but only because a scoping pass first enumerated the call shapes and found the three that
  were not: a `do.call` form that would have *errored* at runtime under a token swap, a call
  passing `context =`, which is a real formal of `jmvcore::format` and was silently swallowing the
  value instead of substituting it (a live bug the sweep would have preserved byte-for-byte), and
  `R/utils.R` itself, where the swap would have turned the wrapper's own delegation into infinite
  recursion. The same pass measured that the guard test's glob `R/*.b.R` had been blind to 13 live
  calls in `R/stagemigration-part*.R` the whole time.

- **Third rule:** **prove provenance before claiming it.** The sweep's verification run showed 9
  failures across 231 files. Two were genuinely exposed by the work; the rest were pre-existing —
  but that was established by swapping the pre-sweep sources back in and re-running the identical
  harness, not by reasoning about byte-identity. One of the nine looked like a regression
  (`pathagreement-wiring`, 0 failures before, 1 after) and was not: it passes in isolation and in a
  six-file harness both before and after, and only fails inside a 231-file batch. A large batch is
  evidence about the batch.

---

## 2026-09-23 — a fix pass needs its own adversarial review, because it writes new defects

### The `agreement` remediation closed 16 findings and introduced a BLOCKING one

- **Failure mode:** closing the audit's FIX-07 meant letting the rater variance decomposition
  accept ordered factors. It did that by merging both rater columns' declared levels with
  `lv_list[order(-lengths(lv_list))]` then `Reduce(union, lv_list)`. When neither column's level
  set contains the other — an `Absent < Focal < Diffuse` scale where rater A never used Focal and
  rater B never used Diffuse, which is exactly what jamovi produces from a CSV — the merged order
  is **not determined by the data**. It depends on which variable the user dropped into the box
  first. Measured on one dataset: `vars = c("RA","RB")` reported *"Most of the disagreement is a
  systematic offset between raters"* (systematic share 0.70) and `vars = c("RB","RA")` reported
  *"No systematic rater offset is identifiable in these data"*. A second fixture moved the rater
  component 20-fold. The `ordinal_codes` note truthfully printed the fabricated order, so the
  disclosure was present and the number was still wrong. The pre-fix code refused non-numeric
  ratings outright, so this failure mode did not exist before the fix.

- **Detection signal:** not the acceptance harness. `development-scripts/audit_agreement_2026-09-23.R`
  went 16 CONFIRMED-DEFECT → 29/29 PASS and the 11 `test-agreement*` suites went 517 → 616 passing,
  with the defect live the whole time. It was found by an adversarial review run **after** the
  fixes, by a reviewer told to assume the fix was wrong, which built the degenerate level sets on
  purpose. Three of the four reviewers also caught majors: a quantitative iota graded on the kappa
  scale beside an ICC panel grading the same number differently; a note asserting Pearson publishes
  no interpretive bands when a widely cited medical rule of thumb does; and three tables rendering
  `90% CI` as a span over columns still titled `95% CI Lower`.

- **Prevention rule:** **a fix pass is a change, and changes get reviewed — by someone who did not
  design them.** A green acceptance harness proves the reported defects are closed; it says nothing
  about what the fix wrote. Budget an adversarial pass over the fix diff, with the reviewers told to
  assume it is wrong, and give at least one of them the pre-fix source so it can A/B replay every
  result table. That reviewer diffed 45 tables at 1e-10 against the pre-fix build and separated
  "this pass broke it" from "this was already broken" — a distinction no amount of after-the-fact
  reading recovers.

- **Second rule, from the same pass:** **a merged ordinal scale must be unique, not merely
  consistent.** The old check asked whether each column's order *contradicted* the merge; the
  failing cases were *underdetermined*, not contradictory, and passed it. The condition to test is
  nestedness by inclusion — every level set a subsequence of the largest — which makes the merged
  order unique. The same helper backed weighted kappa in six other places, so one fabricated scale
  could reach the headline coefficient; fixing it at the helper also surfaced a latent bug where
  All-Pairs Kappa let every rater pair re-derive its own scale.

- **Third rule:** **a hard-coded verdict in a test harness is not a test.** `IR2-07b` in the audit
  script was written with the literal string `"PASS"` in the result column. It reported PASS while
  its observed value was `NA` — it had never compared anything. `all.equal(NA, NA)` is `TRUE`, so a
  neighbouring assertion was passing vacuously too. Every verdict in a harness is computed from the
  comparison, and any comparison that can see `NA` needs a finiteness guard before its result is
  credited.

---

## 2026-09-23 — a measurement taken while an analysis is parked is not a measurement of the module

### A clean module read as six unused dependencies and eight test failures

- **Failure mode:** `agreement` was moved to `menuGroup: meddecideT` so it could be worked on in
  isolation. `_updateModules.R` then stopped shipping it and deleted its files from
  `../meddecide`, and every instrument that reads the module reported the consequences as
  defects. `release_gate.py --root ../meddecide` said `Imports: 31 checked, 6 unused` — so the six
  packages only `agreement` calls (`DescTools`, `irrCAC`, `lme4`, `lmerTest`, `psych`, `vcd`) were
  added to `prune_imports` and their `@importFrom` tags deleted from the sibling's
  hand-maintained `R/zzz_imports.R`. The sibling test suite said eight failures — seven of them
  `cannot open the connection` from `readLines(file.path(root, "R", "agreement.b.R"))`, because
  the audit test's analysis list is a hard-coded snapshot of membership. Nothing in either output
  named the cause. `prune_imports` is a standing DELETE order, the only way to remove an Import,
  so a reading taken during a temporary routing state was written into permanent config.

- **Detection signal:** `prune_conflicts()` — the plan-time guard added the previous day
  (2026-09-22, below) — raised `prune_imports would remove DescTools, irrCAC, lme4, lmerTest, vcd,
  which shipped R code uses` the moment `menuGroup: meddecide` was restored, and `--dry-run`
  reported it before anything was written. The guard worked exactly as designed. What it cannot
  do is fire at *authoring* time: while the analysis is parked the entry is genuinely correct for
  what the module ships, so the collision is invisible until someone promotes. It also does not
  read `R/zzz_imports.R`, so the deleted tags were nobody's finding. The 2026-09-22 note recorded
  "all 17 current entries across four modules are clean"; that was true, and was measured against
  *shipped* files only — the question nobody had asked was what the entries do to the *parked*ate
  ones.

- **Prevention rule:** nothing may be derived from which analyses a module holds today —
  membership is one `menuGroup:` line per analysis and changes without notice. Derive it, never
  record it; where it must be recorded, record the condition under which the record expires.
  A test names its analyses by listing `jamovi/*.a.yaml`, or guards each read with
  `skip_if_not(file.exists(f), "<name> does not ship in this module")` — a skip naming the
  analysis is diagnostic, `cannot open the connection` is not. Before adding a `prune_imports`
  entry, check `grep -l 'menuGroup: <module>[TPD]' jamovi/*.a.yaml`: if anything is parked, the
  Imports you are reading are a snapshot. Enforced by `tools/promotion_screen.py`, which now
  counts a pruned package a parked analysis calls as a promotion blocker and prints the list;
  guide §29. Measured 2026-09-23: **0 shipped, 54 parked** — meddecide 29, OncoPath 17,
  jsurvival 4, ClinicoPathDescriptives 3. Every one is a plan error waiting for whoever removes
  that analysis's suffix.

- **Two things the sweep itself taught.** (1) A hand-written sweep matching `<Group>[TPD]$` found
  30 collisions; the detector matching `(?:Extra)?[DPT]+$` found 54. The 24 it missed were all
  `...ExtraD` groups. Reconcile a detector against a hand list *in both directions* before
  trusting either. (2) Deciding "unused" needs the parser or a whole-file masker, never a
  line-at-a-time one. On `R/agreement.b.R`, `psych::` appears 7 times raw; a line-at-a-time
  string/comment masker still reports 1, because `:7616` (`<code>psych::cohen.kappa()</code>`)
  sits inside a multi-line HTML string whose own line carries no quote character. A whole-file
  masker and R's parser both report 0. That single false positive is the difference between
  "psych is prunable" (true, and it stayed pruned) and "psych is used" (false).

---

## 2026-09-23 — library audit round 4 (meddecide): a grep with 620 hits is not a check

### A table appeared empty and then jumped — the third audit in a row to say so

- **Failure mode:** a table whose whole row set is known before the data is seen
  (`epiR::epi.tests()` always returns LR+, LR-, DOR, Youden, NNDx) was declared `rows: 0` and
  built with `addRow()` inside `.run()`. jamovi draws the empty header first and restructures
  when the computation returns, so the user watches the table jump on every run. The rule
  against it — review guide §6 — has been written since round 1 (2026-07-13), and meddecide was
  told about the same class in **all three** of its audits: MEDIUM, MEDIUM, then LOW.

- **Detection signal:** the library reviewer (`2026-09-16 meddecide` [LOW]), three times.
  Locally, pre-submission checklist item 5 was `grep -n "addRow(rowKey *= *[\"']" R/*.b.R` —
  **620 hits across the umbrella**, because almost every hit is one extra *conditional* row
  added to a table that already has an `.init()` skeleton, which is correct and does not blink.
  An unreadable check is an absent check; this is the same failure the `.()` padding grep had in
  round 3 (200 hits, 14 real).

- **Prevention rule:** change the **unit**, not the pattern. `check_init_row_structure` in
  `tools/release_gate.py` asks the question of the *table*: no `rows:` in the `.r.yaml`, nothing
  reachable from `.init()` (following `private$.helper()` calls transitively) names it, and
  **every** `addRow()` it receives uses a literal string `rowKey` — the proof that the row set
  was known when the code was written. That is **1 shipped hit** across all five modules
  (`OncoPath waterfall::responseDurationTable`) and 57 promotion-debt hits, against 8 shipped
  from a call-level probe of which 6 were the benign conditional-row shape. Guide §6 "Enforce
  it"; the checklist item and the `create-function` / `release-review-function` templates now
  call the gate instead of the grep. **Known blind spot, documented at every copy:** a loop index
  over a fixed-length result (`for (i in 1:nrow(res)) addRow(rowKey = i)`) is not a literal key,
  and that is precisely the shape this report named — a 0 here is not proof.

### A seed set for one bootstrap stayed set for the whole session

- **Failure mode:** every analysis in a jamovi session runs in one R process. `agreement` and
  `lassologistic` called `set.seed(seed_val)` and never restored the RNG state, so the next
  analysis the user added — resampling without a seed of its own, because it had no reason to —
  drew from a deterministic stream and returned the same "random" bootstrap CI on every run.
  Nothing errors; the numbers just stop being random. One module was using three idioms at once:
  `withr::local_seed()`, a hand-rolled `.Random.seed` save/restore, and the bare call.

- **Detection signal:** the library reviewer (`2026-09-16 meddecide` [LOW]). No local check, and
  no rule anywhere — the response filed at the time cited the review guide for a section that
  did not exist.

- **Prevention rule:** seed scoped to the function — `withr::local_seed()` — never globally;
  and show the seed beside any result that depends on it. Guide §28; enforced by
  `check_bare_set_seed` (WARN, 0 shipped across all five modules), which blanks strings and
  comments first so generated R code in an `.asSource()` template is not a false hit.

---

## 2026-09-22 — `prune_imports` was an unchecked standing order

### A config list deleted dependencies the code still needed, and only users found out

- **Failure mode:** `prune_configured_module_imports()` deletes each configured package from a
  generated submodule's DESCRIPTION **unconditionally** — the list says "remove P" and P is
  removed, whether or not the module still needs it. It exists because
  `sync_namespace_with_description()` is add-only: nothing else can ever remove a stale Import
  from a sibling repo's persistent DESCRIPTION. That makes every entry a claim about the code
  that silently expires the moment a newly shipped analysis starts using the package.
  It expired once already: `magrittr` was pruned from OncoPath (2026-09-16), `%>%` vanished
  from the generated DESCRIPTION, and `waterfall` could not run in jamovi at all.

- **Detection signal:** none at the time. Since then three checks in `verify_module()` block
  the module before install (undeclared `pkg::`, "prune_imports still imported", bare-symbol
  resolution) — but all three run *after* apply, prepare and document, i.e. after the sibling
  working tree has been rewritten, and all three are skipped entirely under
  `modes$extended: false`, which still lets the prune itself run. The user's question — "if I
  add another function they may need, doesn't that cause a problem?" — was the real signal.

- **Prevention rule:** a standing instruction is re-derived from reality on every run, as early
  as the data allows. `prune_conflicts()` (`_updateModules_utils.R`) now re-checks every entry
  at **plan** time against the umbrella sources the module is about to ship, so a stale entry is
  a plan error that `--dry-run` reports before anything is written. A `requireNamespace()`-gated
  use counts as a use: jamovi installs Imports and never Suggests, so pruning a guarded package
  does not make the capability optional, it makes it dead. Measured against the real config, all
  17 current entries across four modules are clean — the guard costs nothing today.
  Its limits are stated in its own comment: it cannot see the `magrittr` shape itself (a bare
  symbol with no tag names no package, so no source scan can find it); that class stays with the
  bare-symbol check in `verify_module()`, which has a real NAMESPACE to resolve against.
  Secondary fix: the "prune_imports still imported" message used to say *"remove its
  @import/@importFrom tag"* — advice that is exactly backwards when the config is the stale side.
  It now names both remedies and says not to assume the config is right.
  Tests: eight cases in `tests/testthat/test-update-modules-plan.R`, plus the OncoPath audit
  test, whose pinned `c("cluster", "tidyr")` had already gone stale against the working tree and
  is now the invariant (`prune_conflicts()` returns nothing) plus the one literal worth keeping:
  `magrittr` must never enter that list.

- **What the adversarial review of the new guard found (65 agents, 25 confirmed findings).**
  Two mattered here. (1) A `pkg::` call in a **formals default** — `function(x, engine =
  DescTools::CCC)` — was invisible: formals are a pairlist, not a call, so the walker stepped
  over them. That was a hole in `scan_r_package_usage` itself, so it blinded the new plan guard
  AND the long-standing `pkg::`-declaration check in `verify_module()`. Fixed in the walker, so
  both gain at once; all five submodules stay clean and all 17 prune entries still pass. Trap
  worth remembering: a parameter with no default is the empty symbol, and `for (d in
  as.list(formals))` raises *"argument 'd' is missing, with no default"* — index straight into
  `is.call()`, never bind it. (2) A grep implementation of this guard would have been wrong:
  over all umbrella `R/`, 15 of 17 entries have a `P::` hit (the umbrella carries every draft),
  and even scoped to shipped files, `robustbase` and `psych` hit **comment lines only**. Parsing
  with `keep.source = FALSE` is what makes the guard usable; there is a regression test for it.

---

## 2026-09-22 — library audit round 5 (OncoPath): an inferred cause, believed for ten months

### Users saw `attempt to apply non-function` and we blamed the wrong object

- **Failure mode:** `jmvcore::Group$insert(index, item)` has no bounds check. For any index above
  `length(items)` it evaluates `.items[index:length(.items)]`; R's `:` counts DOWN, so both slices
  run off the end and the results tree fills with `NULL`. The next traversal calls
  `NULL$asProtoBuf()` — which is exactly `attempt to apply non-function`, arriving through the
  serialization path. Measured on jmvcore 2.7.38 against a real `waterfall` run: 21 items,
  `insert(999,)` → 1978 items / 1955 `NULL`s; `insert(22,)` → 1 `NULL`. **A `jmvcore::Html` at the
  same index fails identically** — the element type is irrelevant. `insert(1, ...)` into an *empty*
  group is the same bug (`1:0` is `c(1, 0)`).

  What we concluded instead, in 2025-12: *"ALL dynamically inserted `jmvcore::Notice` objects cause
  serialization errors."* `R/survival.b.R` had ten notices — nine at index 1 or 2, one at 999.
  Removing the `insert(1, ...)` did not help (the 999 was still live), removing all ten did, and
  the wrong lesson was drawn from the difference. The claim had in fact been written into
  `CLAUDE.md` as mechanism on 2025-11-16, **six weeks before the incident it purports to explain**,
  and six later write-ups in `tests/*.md` each cite `CLAUDE.md` as their authority. None tested it.
  Cost: `jmvcore::Notice` was abandoned for hand-rolled HTML — **80 `.addNotice()` helpers,
  1,408 call sites, 0 uses of the native element** — and the false mechanism propagated into
  `CLAUDE.md`, three guides, five command files, a template library and a code breadcrumb the
  jamovi library reviewer reads.

- **Detection signal:** the library reviewer (`2026-09-22 OncoPath` [LOW]) — correcting *their own*
  earlier advice, and reading our breadcrumb to do it. Every local check missed it because nothing
  tested the claim: it was documentation agreeing with itself. `.claude/commands/fix-notices.md`
  had taught `self$results$insert(999, n)` since 2025-11-14, and the pattern spread to 321 call
  sites at index 999 before the first crash.

- **Prevention rule:** an inferred cause is labelled as inferred until it is measured, and a fix
  that removes N things at once has not identified which one mattered. Enforced by
  `tools/release_gate.py` `check_sentinel_insert` (**FAIL**; 0 hits in the umbrella and all five
  siblings the day it was written) plus the corrected teaching sites; guide §13.

### A WARN that is red in every module is a broken check

- **Failure mode:** `check_news` warned that `NEWS.md` had no heading for the `DESCRIPTION`
  version, in all five siblings, forever. But `.github/workflows/release.yaml` publishes nothing at
  all for a four-component version, which is what the updater writes on every regeneration. Four of
  the five warnings were about a file the release pipeline never opens, and they buried the fifth —
  OncoPath at 1.0.83, genuinely about to publish a release whose body would read `Release 1.0.83.`
  It had already happened 14 times in 16 workflow-era releases.
- **Detection signal:** the reviewer supplied the consequence we had not traced (the extractor's
  `(^|[^0-9.])<version>([^0-9.]|$)` regex and its placeholder fallback). The gate itself had been
  reporting the symptom since 2026-09-18 and was skimmed, exactly as a permanently red line is.
- **Prevention rule:** a detector must ask the question that matches the thing that breaks. Rewritten
  to mirror the workflow's own cascade — silent on dev builds, and it names the placeholder body it
  would publish. 5/5 red → 1/5. Related: the heading must be written **before** the version bump,
  because the workflow skips a version whose tag exists, so a release cannot be re-cut.

### Fixing the third instance is not sweeping the class

- **Failure mode:** `diagnosticmeta` and `waterfall` greet a user with a red
  `ERROR: Variables required` the moment the analysis is opened, before anything has been chosen.
  Round 1 raised the fatal half of this class, round 2 the empty-state half; both were marked DONE.
  Round 2's fix moved the banners into a new `notices` channel and re-created them there
  (`0dbd3d348`), and `48b8edb13` — **one day before this audit** — fixed `swimmerplot` alone and
  wrote the reasoning the reviewer then quoted back at us.
- **Detection signal:** the reviewer, at three named sites. A class sweep found **7 empty-state and
  9 fatal-via-banner shipped hits** across the five modules — including `swimmerplot`'s own second
  instance, which the reviewer missed while holding swimmerplot up as the model.
- **Prevention rule:** guide §25, with the empty-state / fatal split and the two detector
  refinements (measured: 53% false positives without them, 0/15 with). Not gated yet, and the guide
  says why rather than shipping a noisy gate.

### Tabular data rendered as raw HTML ignores results theme and loses user interactions

- **Failure mode:** reference tables in `diagnosticmeta` were rendered inside HTML blocks using raw
  `<table style="...">` markup with fixed `#ddd` borders and light headers. They ignore jamovi's
  dark results theme, cannot be copied as LaTeX or tabular data (Copy outputs raw markup rather than
  a grid), and cannot be translated via `.()`.
- **Detection signal:** the reviewer (`2026-09-22 OncoPath` [MEDIUM]). The sweep found 97 shipped
  `<table>` blocks across five modules (33 computed, 25 static grids, 27 prose, 12 exempt help).
- **Prevention rule:** tabular data belongs in a `type: Table` result in `.r.yaml`, with static rows
  in `.init()` and computed values in `.run()`. Reserve `type: Html` solely for complex packages with
  mature HTML rendering engines (gt/gtsummary) or empty-state layouts. Guide §26.

### Module self-description contradictions propagate through separate authorship channels

- **Failure mode:** capability claims (RECIST v1.1 compliance vs adaptation) differed between
  `DESCRIPTION`, `README.md` and `jamovi/0000.yaml`; bug-report URLs pointed to different repositories;
  `diagnosticmeta.a.yaml` used a `|` literal block scalar so description lines were rendered with
  awkward paragraph breaks; example datasets shared identical descriptions.
- **Detection signal:** the reviewer (`2026-09-22 OncoPath` [LOW]).
- **Prevention rule:** single source of truth for repository URLs, folded `>` block scalars for
  analysis descriptions, distinct dataset descriptions, and aligned clinical standard qualifications.
  Enforced by `check_description_newlines`, `check_issue_urls`, and `check_dataset_descriptions` in
  `tools/release_gate.py`; guide §27.

### Declared-but-unused dependencies left behind after refactoring

- **Failure mode:** `stringr` remained declared in `DESCRIPTION` `Imports:` and `R/zzz_imports.R`
  after its sole call (`str_to_title()`) was removed in a refactor. The `@importFrom` tag hid the
  staleness from `R CMD check`, forcing every user install to resolve and load an unused package.
- **Detection signal:** the reviewer (`2026-09-22 OncoPath` [LOW]); `check_unused_imports` in
  `tools/release_gate.py` (which also found `psych`).
- **Prevention rule:** remove unused dependencies from `DESCRIPTION` and `R/zzz_imports.R`; guide §10.

---

## 2026-09-20 — routing an analysis out of a module took its imports with it

`Rscript _updateModules.R` stopped OncoPath at verify: `called but not resolvable from the
namespace: . (swimmerplot-html.R)`.

- **Failure mode:** `#' @importFrom jmvcore .` was never on `swimmerplot.b.R`. OncoPath got the
  import from `waterfall.b.R`, which carried the tag for the whole module. Suffixing waterfall's
  and ihcheterogeneity's `menuGroup` with `T` (the documented JamoviTest dev routing) left
  swimmerplot as OncoPath's only analysis, and the module namespace lost `.` — so every `.()`
  call in `swimmerplot-html.R` and `swimmerplot.b.R` would have died at run time with
  `could not find function "."`. Same class as the 2026-09-16 `%>%` audit: a bare symbol
  resolves only from the module's own namespace or its imports, and `Imports:` alone puts
  nothing in scope.
- **Detection signal:** the dependency guard in `verify_module()` — the only check that sees it.
  `devtools::load_all()`, the umbrella test suite and `R CMD check` all pass, because the
  umbrella namespace has `.` from twenty other `.b.R` files.
- **Prevention rule:** the `@importFrom` tag belongs on the file whose analysis uses the symbol,
  not on whichever sibling happens to ship alongside it today. Module membership is temporary —
  the `T`/`P`/`D` menuGroup suffixes move analyses between modules on a whim, and each move
  silently re-computes what the target namespace imports. Before trusting a module's namespace,
  run `Rscript --vanilla tools/submodule_smoke.R <sibling repo>`: it checks the *installed*
  namespace, which is the only place this shows up.

---

## 2026-09-21 — `swimmerplot`: what adversarial verification actually caught

Six agents were asked to REFUTE the twelve fixes rather than confirm them. Five refuted, and the
regression sweep came back major. Every one of the following was found by that pass, not by me.

### A fix can ship the very bug it is fixing, in a new spelling

- **Failure mode:** the fix for "a stale response row survives and the rates sum to 125%" introduced
  "a response category called `missing` collides with the no-response row and the rates sum to 62.5%".
  Both are the same defect - a row key that is not unique - and I wrote the second while fixing the
  first, because I reused `paste0("response_", x)` without asking what `x` could be.
- **Prevention rule:** when a key is built by concatenating a user-supplied string onto a prefix, ask
  what user value reproduces a reserved key. The fix is a sentinel the concatenation cannot generate,
  not a longer prefix.

### Two mechanisms for one invariant means one of them is dead

- **Failure mode:** I added a reset at the top of `.run()` AND a prune inside `.updateSummaryTable()`,
  for the same guarantee. The reset runs first and always, so the prune's "has anything gone stale?"
  test was always true, and it rebuilt the five rows it had just been given on every single run. My
  regression test passed only because it called the method directly and skipped the reset.
- **Prevention rule:** after adding a guarantee in one place, re-derive whether the other place can
  still observe the state it was written for. And when a test has to bypass the normal entry point to
  exercise the code it covers, that is evidence the code is unreachable, not evidence of good isolation.

### The obvious idempotent operation is not always safe

- **Failure mode:** `deleteRows()` before a rebuild loop looks perfectly idempotent. jmvcore's
  implementation clears `.rowKeys` and `.rowCount` but leaves `.rowNames` untouched, so rebuilding to
  an EMPTY set leaves phantom row names with no keys, and `fromProtoBuf` indexes out of bounds.
- **Prevention rule:** read the library method you are relying on for idempotence. Here the answer was
  to not call it at all on the common path - compare the current key set to the wanted one and rebuild
  only on a difference, which also removes the cost from every repeat.

### A house rule beats a general principle, and a test is where the house rule lives

- **Failure mode:** I split a translated string to get markup out of the message catalog - a good
  general principle - and thereby reverted a fix that a standing test exists to protect. This module's
  rule is that `.()` wraps a COMPLETE sentence; a whole-sentence template with markup and a `{message}`
  placeholder satisfies it, and a two-word fragment plus loose tags does not. Worse, I had written my
  own test asserting the opposite, so the repo contained two tests that could not both pass.
- **Prevention rule:** before applying a general principle to a repo, grep the test suite for the thing
  you are about to change. If a test names the exact string, that test is the specification. And when
  two of your own tests disagree, the older one guarding a documented reviewer finding wins.

### Auditing names is not auditing effects

- **Failure mode:** I validated the example script's argument NAMES against the schema, declared it
  fixed, and shipped a file where eight calls still rendered nothing because they supplied an event
  variable without the boolean that switches event markers on - under headings promising an adverse-event
  timeline. The names were all real. A separate example titled "Date/Time Format Handling" passed
  `timeType = "raw"` and numeric columns, never touching the dataset's actual dates.
- **Prevention rule:** for examples, "it parses" and "the arguments exist" are the weakest checks
  available. Execute the file and assert on the OUTPUT - that events exist, that the dates path ran -
  because an example's job is to demonstrate an effect, not to survive argument matching.

## 2026-09-21 — `swimmerplot`: what a five-lens review found that three fix passes did not

23 findings, 23 held under adversarial verification. The three previous passes had closed 60 findings
in the same file. What made these different is worth recording.

### A helper that returns "the natural unit" is a trap the moment two callers differ

- **Failure mode:** `.asNumericTime()` returned each class's own epoch unit - days for `Date`, seconds
  for `POSIXct`. Every one of its six callers compares a start against an end, so a `Date` start with a
  `POSIXct` end compared 18262 against 1583020800. Person-time came out 104,106,728 months beside a
  correct mean duration of 2.5, silently, because the two neighbouring estimators build a
  `lubridate::interval` on the original objects and are immune.
- **Prevention rule:** a conversion helper feeding comparisons must return ONE scale, not each input's
  preferred one. And when two functions compute related quantities by different routes, feed them the
  same normalised input - the fact that only one of them was wrong is what made this invisible.

### Two code paths doing the same job, only one of them defensive

- **Failure mode:** `.processEventMarkers` filters markers to the patient's window and raises a notice
  naming what it dropped. `.processMilestones`, written for the same kind of data, never compared a
  milestone to the timeline at all - and the module's own test dataset has 13 of 49 outside it.
- **Prevention rule:** when two sibling paths handle the same shape of input, diff their guards, not
  just their outputs. The asymmetry is the finding; neither path looks wrong on its own.

### A fix can be correct and still ship the wrong rows

- **Failure mode:** I capped an export at 500 rows and wrote a note saying "in the order the plot uses".
  Both true in isolation - but `.applySorting` reverses the factor levels because ggplot draws level 1
  at the bottom, so `[1:500]` took the plot's BOTTOM 500. With the default longest-first sort, the 100
  longest-followed patients were exactly the ones dropped.
- **Prevention rule:** when you truncate an ordered collection, assert which END you kept against the
  thing the user sees, not against the data structure. One `identical(export, head(rev(levels), 500))`
  would have caught it the day it was written.

### Reconciliation is a property of the page, not of a function

- **Failure mode:** six separate findings were all the same shape - two numbers on one screen that
  cannot both be right under one reading. Mean Duration times n against Total Person-Time. Person-time
  rows summing to less than the total. "Most common response CR (37.5%)" above "No recorded response
  62.5%". A group note printing 0% beside a warning saying rates were withheld. Every individual
  function was correct.
- **Prevention rule:** review the rendered OUTPUT as a reader, not the functions as an author. Add the
  arithmetic check a sceptical clinician would do - do the rates sum, do the rows add to the total, does
  the prose agree with the table - because no unit test on a single method can fail for this.

### The expensive line is rarely where you would look

- **Failure mode:** a data.table fast path, added for speed, split its own finished one-row-per-patient
  aggregate into n one-row tables so that a shared `bind_rows()` could put them back together. That was
  70% of every large run; 10,000 patients took 92 seconds.
- **Prevention rule:** profile before optimising and after adding a "fast path" - and be suspicious of
  any code that converts a result into the shape a later shared step expects. The fix was deleting one
  line and guarding another.

## 2026-09-21 — `swimmerplot`: auditing the audit

### A crosswalk that checks one level of a document proves nothing about the others

- **Failure mode:** I reported "zero citations" for an analysis that had six. My check walked
  `items[*].refs` and the `.a.yaml` root, and never looked at the `.r.yaml` document root, where the
  analysis-level `refs:` block actually lives. The finding survived because the output said `refs=0`
  for all 18 items, which is true and irrelevant. An independent reader found it in minutes.
- **Prevention rule:** when a schema allows a key at more than one level, enumerate the levels before
  concluding it is absent anywhere. And state the shape of the check in the finding itself — "no item
  carries refs" is a claim I could have falsified; "zero citations" is the one I could not, because it
  hid the assumption.

### The guard that fits the neighbouring block is not automatically the right guard

- **Failure mode:** `advancedMetrics` accumulated duplicate rows because it lacked the `rowCount == 0`
  guard that `summary` has four lines above it. The obvious fix — copy the guard — would have been
  worse than the bug: `summary` has five fixed rows, while `advancedMetrics` has 0, 2, 4 or 6 depending
  on two options, and `.run()` fills them with `setRow()`, which *rejects* a missing key. A frozen row
  set would abort the analysis instead of merely duplicating rows.
- **Prevention rule:** before copying a guard from adjacent code, ask what invariant it protects and
  whether this data has the same one. "Fixed set" and "option-dependent set" need different mechanisms
  — `rowCount == 0` for one, `deleteRows()` for the other.

### Retiring a channel is cheaper than keeping two in agreement

- **Failure mode:** severity messages lived in three places — the notices panel, an `Html` item toggled
  with `setVisible`, and the instructions panel. Each had its own reset path, and the notices panel
  never mentioned the other two. The narrow fix is to emit the missing warnings in both places; that
  leaves two reset paths and two chances to drift.
- **Prevention rule:** when the same category of content has two homes, delete a home rather than
  synchronising them. Here it also removed every `setVisible(FALSE)` from the file, so the house rule
  that it must never signal failure is now satisfied by construction instead of by review.

### An example nobody runs is not documentation

- **Failure mode:** the shipped worked example called the analysis 19 times, and 17 of those calls
  passed argument names that have never existed — plus six impossible enum values. It had drifted
  through at least one option rename with nothing to catch it, because no test executes it.
- **Prevention rule:** validate example scripts against the generated wrapper signature, not by eye —
  option names and enum LEVELS both. The cheapest version is a parse of every `fn(...)` call in
  `inst/examples/` against the `.a.yaml`; the honest version executes the file.

## 2026-09-20 — `swimmerplot`: the last mile is still the product

### A definition that is 80% right is a definition that is wrong

- **Failure mode:** the glossary said PD is a ">=20% increase in sum of target lesion diameters". Every
  word is in RECIST 1.1 - and the definition is still wrong, because it omits the nadir as reference,
  the 5 mm absolute minimum and new lesions. A reader who knows RECIST sees nothing amiss; a reader who
  does not now has a rule that misclassifies patients. SD had the same shape, and PR and SD/PD use
  DIFFERENT references, which the panel never mentioned.
- **Prevention rule:** for any clinical definition shown to users, quote the source section and check
  each clause against it, including the reference point. Then cite it in the panel, so the next reader
  can do the same check in ten seconds instead of trusting the paraphrase.

### "Missing" and "not reached" are different facts

- **Failure mode:** the follow-up CI was formatted only when BOTH bounds were non-NA. For a reverse
  Kaplan-Meier the upper bound usually is not reached, so the common case printed nothing and the lower
  bound - the informative half - was destroyed by a guard written for "no interval at all".
- **Prevention rule:** an NA in a statistical result is a value with a meaning, and the meaning differs
  by position. Format each bound separately and give the unreached one its conventional name ("NR"),
  rather than testing the pair and discarding both.

### A number that is a function of its neighbour is noise, and a stratification can be a bias

- **Failure mode:** "Follow-up Density" is 100/Mean Time in the same row - a second column presenting
  one column's information as if it were a finding. Worse, the table splits follow-up by BEST overall
  response, which a patient can only earn by surviving to be assessed: the textbook guarantee-time bias,
  presented without a word of caution in a clinical tool.
- **Prevention rule:** before adding a derived column, check whether it is invertible from one already
  present; if it is, it needs a reason to exist beyond convenience. And any table that stratifies
  outcome by a post-baseline achievement needs the bias named on the table, not in documentation.

### The option key is not the word

- **Failure mode:** `self$options$timeUnit` - "months" - was interpolated into sixteen translated
  sentences. Each sentence was translated and each unit stayed English, which is invisible in
  development and to every English-speaking reviewer.
- **Prevention rule:** an enum key that reaches a user-visible string needs a `switch()` of translated
  words. The generic form: anything crossing from configuration into prose gets translated at that
  boundary, and the raw key continues on to the library that needs it.

## 2026-09-20 — `swimmerplot`: measure the library before designing around it

### The error the user sees is the one thrown last, not the one that matters

- **Failure mode:** text that is not a number became `NA` in `as.numeric()`, the NA rows were removed by
  a validity filter three steps later, and the filter reported what IT knew: "end times are >= start
  times". Every layer behaved correctly and the user was told to check an ordering that was never the
  problem. The same shape appeared twice more - unparseable dates reported as "missing", and a
  stop-the-analysis branch whose only notice was "Time units".
- **Prevention rule:** diagnose at the conversion, where the original value is still in hand, and quote
  it. A coercion that can produce NA is a place where information is destroyed; count what it destroyed
  before moving on. Downstream a missing value has no history, and any message built from it will be
  about the wrong thing.

### Benchmark the API before you redesign around it

- **Failure mode:** a 2000-patient export took 135 s. The tempting conclusion is that the loop or the
  values are expensive. Timing a BARE jmvcore Table showed 250/500/1000 `addRow()` calls costing
  2.1/8.3/32.9 s with no values at all - quadratic, in the library, and `setRow` on pre-added rows is
  identical. That ruled out three "optimisations" that would have changed nothing.
- **Prevention rule:** before optimising code that calls a framework in a loop, time the framework call
  on its own with the work removed. Here it turned an open-ended performance project into a five-line
  cap, and it is the same measurement that justifies the cap to the user.

## 2026-09-20 — `swimmerplot`: the figure is an output too

### Normalising in the consumers instead of at the source

- **Failure mode:** five different places called `.normalizeResponse()` on the way into a table, and the
  plot - which never called it - was left colouring by the raw factor. The result was a figure whose
  legend disagreed with every table beside it, in a way no table-level test could catch.
- **Prevention rule:** normalise once, where the column is created, and give the normalised form its own
  name. If several consumers each normalise for themselves, the one that forgets is invisible, and the
  cost of finding it is a visual diff.

### A catch-all around a renderer hides the error it was written for

- **Failure mode:** `scale_color_manual()` with 8 values throws at build time on 9+ levels. The renderer
  wrapped plot construction in a fallback, so the user got the simplified plot with no message - the
  option they had just chosen destroyed the figure and reported success. The audit found it only by
  calling `ggplot_build()` directly.
- **Prevention rule:** a fallback that swallows an exception must record what it swallowed, and a fixed
  palette must be checked against the data's cardinality before it is applied, not after. When testing a
  renderer, build the plot - returning TRUE proves nothing, since that is exactly what the fallback does.

### Sorting is not the same as displaying

- **Failure mode:** `.sortPatients()` computed a correct order and set it as the factor levels. ggplot
  puts level 1 at the BOTTOM of a discrete y axis, so all four sort orders were displayed upside down
  for as long as the feature has existed. Every unit test of the ordering passed, because the ordering
  was right.
- **Prevention rule:** when an ordering exists to be looked at, assert it on the rendered axis labels,
  not on the data structure. `rev(panel_params$y$get_labels())` gives top-to-bottom, which is what the
  reader sees and what the option promises.

## 2026-09-20 — `swimmerplot`: one option, one thing

### A shared gate couples features that have nothing to do with each other

- **Failure mode:** `.advancedMetricLabels()` opened with `if (!personTimeAnalysis) return(character(0))`
  and then appended ORR and DCR inside it. Written that way the coupling is invisible - the response
  rates are right there under a `responseAnalysis` test - but the outer guard had already decided. Two
  unrelated features shared one switch, and the interpretation text was gated separately again, so the
  page could describe a table it had just suppressed.
- **Prevention rule:** an early `return()` at the top of a function that serves more than one feature is
  a coupling, not a guard. Give each family its own predicate (`want_pt`, `want_resp`), and check that
  every consumer of a feature - table rows, table population, interpretation paragraph, syntax - is
  gated on the same one. The test is a truth table over the options, not a single happy path.

### Ask the option, not the data structure

- **Failure mode:** the group test guarded on `"response" %in% names(patient_summary)`. But
  `.summarizeByPatient()` always emits that column, filled with NA when no response variable was chosen,
  so the guard never fired and the table published "A: 0 of 4 responded (0.0%)" for data that records no
  responses whatsoever. The structure said yes; the user had said no.
- **Prevention rule:** to find out what the user asked for, read `self$options`. A column's presence
  answers a different question - whether a code path upstream created it - and helper functions that
  build a fixed schema make the two answers diverge silently.

### An empty result needs a reason, not just an absence

- **Failure mode:** five `return()` statements ended `.updateGroupComparisonTests`,
  `.updateMilestoneTable` and `.updateEventMarkerTable` early, leaving a visible table with zero rows.
  In the DEFAULT configuration two of them were empty on every run. A reader cannot distinguish
  "nothing to report" from "this is broken".
- **Prevention rule:** pair `visible:` with the reason the item can be empty. Option-driven emptiness
  belongs in the `visible:` expression, where jamovi hides the item without a run; data-driven emptiness
  belongs in a table note written at the point of the `return()`, where the reason is still in scope.

## 2026-09-20 — `swimmerplot`: a filter without a count is a silent data loss

### Every filter needs a counter on the other side

- **Failure mode:** three separate places removed data and said nothing - events outside a patient's
  window, milestone slots above "Maximum milestones", and milestone slots with a blank name. Each read
  as correct defensive code in isolation. What made them bugs is that the surviving rows were then used
  as the denominator, so an event table with three Deaths deleted reported "Scan 4 (80%)".
- **Prevention rule:** when you write a `keep <- ...` filter, write the count of `!keep` in the same
  commit, with the reasons broken out. If the filtered result feeds a percentage, the disclosure also
  has to say which denominator the percentage uses. A filter is a claim about the data; an uncounted
  filter is an unfalsifiable one.

### "Patients" in a message is a unit, and units have to be checked

- **Failure mode:** `.validateClinicalData` ran `sum(is.na(response))`, `length(duplicated(id))` and
  `which(durations > limit)` over a frame with one row per EPISODE, then printed every count as
  "patients". Beside it the summary table counted patients properly, so the page contradicted itself in
  the default configuration for any longitudinal dataset - which is what a swimmer plot is for.
- **Prevention rule:** in multi-row-per-subject data, treat every `nrow()`, `sum(is.na(...))` and
  `duplicated()` as a unit error until proven otherwise. Making each message name its denominator
  ("for 1 of 3 patients") is the cheap fix that makes the next one self-evident, and it disposes of the
  "1 patients" plural at the same time.

### Resolve identity once, before the loop

- **Failure mode:** the milestone loop took the slot's label from `self$options[[name_opt]]` at three
  different points inside the body. That made a blank name a skip condition rather than a fallback, and
  made two slots sharing a name indistinguishable by construction - the table grouped on the label.
- **Prevention rule:** compute the identity of each item (its key, its label) once before iterating, in
  code that can see all the items at the same time. Collisions and blanks are only visible from there;
  inside the loop each iteration looks perfectly reasonable.

## 2026-09-20 — `swimmerplot` display modes: a coordinate is not a statistic

### The same number means different things in different coordinate systems

- **Failure mode:** milestone and event tables called `as.numeric()` on the value the plot draws at. In
  relative mode that value is a duration and the table was right; in absolute mode it is a position on
  the study-time axis and the table published it as "Median Time". Nothing in the code was obviously
  wrong - each half was correct about its own coordinate system, and only the pair was a bug.
- **Prevention rule:** when a quantity is reused for drawing and for reporting, name the coordinate
  system in the conversion function and convert once, there. The invariance test is the cheap oracle:
  the same data in every display mode must give the same statistic, and a hand-computed reference says
  which one. Here it also exposed that dates and raw numbers had silently disagreed for years.

### A guard written for one special case was really about a general property

- **Failure mode:** median/protocol reference lines were suppressed `if (is_date_scale)`. The author was
  right that date axes cannot carry a duration line, but the actual property is "does this axis measure
  duration from each patient's own start" - which raw absolute times also fail, and which equal start
  times satisfy even in absolute mode. Two thirds of the truth table were wrong.
- **Prevention rule:** when a guard names a data TYPE, ask what property of that type it is standing in
  for, then test the property. `is_date_scale` became `.isDurationAxis()`, and the case it had been
  over-rejecting (everyone starts at 0) came back for free.

### Converting to a unit and then subtracting is not the same as subtracting and then converting

- **Failure mode:** relative display rewrites each time as `time_length(interval(anchor, t), "months")`.
  Person-time then subtracted two of those. But a calendar month measured from the anchor is not the
  same length as one measured from the episode's own start, so the identical dataset reported 3.98
  months relative and 4.02 absolute - and an earlier fix in the same function had already patched one
  symptom of this without finding the cause.
- **Prevention rule:** calendar arithmetic does not distribute over subtraction. Keep the original
  Date/POSIXct values and measure the interval you actually want; convert to a unit last, once. When a
  display option can change a reported number, that is the whole bug - test the invariance directly
  rather than the number.

## 2026-09-20 — `swimmerplot` follow-up: the answer depended on how the file was sorted

### Two sites deciding the same thing, one of them by row order

- **Failure mode:** a patient's censoring status was read with `tail(...)` / `[length(...)]` - the last row
  in storage order - while the arrow marking that same patient's ongoing treatment was positioned with
  `which.max(end_time)`. For anyone with more than one episode the two could disagree, and re-sorting the
  identical rows changed the median follow-up. Three sites made this decision (arrow, data.table summary,
  base summary) and each had its own spelling of it.
- **Prevention rule:** when the same fact is derived in more than one place, the duplication is the bug
  before any individual line is. Extract one helper and route every site through it, even when only one of
  them is visibly wrong - the others are the next report. The test that proves it is a permutation test:
  same rows, different order, same answer.

### A vacuous test passes for the same reason the bug hides

- **Failure mode:** my first regression test gave its two patients identical follow-up times, so swapping
  their statuses was a symmetry and the test passed against the *unfixed* file. It looked like a green
  guard and guarded nothing.
- **Prevention rule:** run every new regression test against the pre-fix file before believing it. When it
  passes there, the fixture is degenerate, not the fix redundant - here the two ends had to differ (12 and
  20) so the correct and incorrect rules give different numbers (16 vs 15).

### `NA` fails the positive test, so it silently joins the other arm

- **Failure mode:** `status %in% "censored"` is `FALSE` for a missing value, and `as.numeric()` of that
  turned every patient with no censoring value into a completed event in the reverse Kaplan-Meier, biasing
  the median down. Two separate validity checks ran over the same column and neither counted them.
- **Prevention rule:** a two-arm classification needs three arms in the code. Test for each arm explicitly
  and route what matches neither to an exclusion you report, rather than letting a `FALSE` decide it.

### A computed explanation with no consumer is a bug that has already been diagnosed

- **Failure mode:** the follow-up estimator returned a `reason` field explaining which of two things had
  gone wrong ("nobody censored" vs "the curve never reaches 50%"). Nothing anywhere read it, and the label
  said "no censoring information" - which was not true, since the user had supplied a censoring variable
  and every value in it was understood.
- **Prevention rule:** grep every field a helper returns for a consumer. A field with none is either dead
  weight to delete or, as here, an answer the user needed and never saw.

## 2026-09-20 — `swimmerplot` group comparison: an effect size with no direction is not a result

### Determine an orientation empirically; do not reason about it

- **Failure mode:** the reported odds ratio named neither group. Fixing that meant knowing which way R's
  `fisher.test` points on a table of rows = groups, columns = (non-responder, responder). Rather than reason
  it out, I built a table with a known 8/10-vs-2/10 asymmetry and compared the returned estimate against both
  candidate hand-computed ratios. It is row 2 relative to row 1.
- **Prevention rule:** for any library function whose output has an orientation - odds ratios, differences,
  contrasts, reference levels - determine it with a deliberately asymmetric fixture before writing the label.
  A label that names the direction is worse than no label if the direction is wrong.

### A fixed orientation creates cases that the loose version hid

- **Failure mode:** forcing the outcome column to `factor(levels = c(FALSE, TRUE))` - necessary so the table
  always has a known shape - meant a cohort where every patient responded now produced a 2-column table with
  one empty column, passing the old `ncol >= 2` guard and running a meaningless test (p = 1, OR 0 or Inf).
  Previously `table()` returned one column and the guard rejected it by accident.
- **Prevention rule:** when you replace an implicit guard with an explicit shape, re-derive what the guard
  was actually excluding. Here the real condition is "both outcomes and both groups are represented", which
  now has its own predicate rather than being a side effect of how `table()` drops empty levels.

### Test assertions about strings break on improvements to those strings

- **Failure mode:** two existing tests matched `"OR = "` literally and failed the moment the label gained its
  direction, although the behaviour had strictly improved. A third failure was mine: I asserted a 1200-patient
  run and a 120-patient run produce identical labels, which is false and should be - the larger sample gives a
  tighter confidence interval.
- **Prevention rule:** assert the guarantee, not the rendering. Match the direction and the presence of an
  interval; compare point estimates across code paths, never whole formatted strings whose content legitimately
  depends on n. And when a reciprocal is checked against a 2-dp label, size the tolerance to the rounding
  (1/13.25 = 0.0755 prints as 0.08 - a 6% gap that means nothing).

## 2026-09-20 — `swimmerplot` response rates: count the denominators before fixing any of them

### Reconcile ALL the denominators, or you have only moved the inconsistency

- **Failure mode:** one results page reported response with five different denominators — per-category rates
  over the non-missing patients, ORR/DCR over the CR/PR/SD/PD subset, the Fisher test over a third cohort,
  "Study included N patients" over everyone, and a validation percentage over episode ROWS labelled
  "patients". The audit named three; an exhaustive site map found five.
- **Prevention rule:** before changing a denominator, enumerate every site that prints a count, a percentage
  or an N derived from the same concept — including the narrative sentences and the export. Fixing the three
  that print a *rate* would have left "Study included 12 patients ... ORR 33.3% (4/12)" beside a validation
  warning quoting a percentage of episodes. The cheap way to make a page auditable afterwards is to print
  `n/N` in each row label; then a reader reconciles it by addition instead of by trusting the software.

### Read the standard, do not recall it

- **Failure mode:** the previous pass had deliberately chosen the "evaluable" denominator and written a test
  to pin it. RECIST 1.1 section 4.9.1 says the opposite in as many words: conclusions "should not be based
  on a selected 'evaluable' subset", and NE is one of the five assigned outcomes rather than an exclusion.
- **Detection signal:** a reviewer fetched the guideline PDF and quoted the paragraph, rather than relying on
  recollection of common practice.
- **Prevention rule:** when a statistical default rests on a published standard, quote the standard verbatim
  in the code comment and in the test. A test that pins a deliberate-but-wrong choice is harder to overturn
  than no test, because the next engineer reads it as settled.

### Printed percentages do not have to add up, and should not be asked to

- **Failure mode:** after re-basing the rates, CR 16.7% + PR 16.7% = 33.4% sat beside an ORR of 33.3%. Each
  percentage is rounded independently; nothing is wrong, but a reader cannot tell that from the page.
- **Prevention rule:** never promise additivity of rounded percentages. Print the counts, and say in the note
  that the percentages round independently while the counts reconcile.

### A rank function cannot honour a time rule it was never given the times for

- **Failure mode:** best-overall-response took a bare vector of labels and picked the best rank, so an
  assessment recorded after progression won. Adding "stop at the first PD" required passing the episode
  times as well — without them the truncation would have depended on row order, which is the same class of
  defect in a new place.
- **Prevention rule:** when a rule is temporal ("up to progression", "the latest", "first after baseline"),
  the function implementing it must receive the ordering key as an argument. If it cannot, it is not
  implementing the rule, it is implementing row order.

## 2026-09-20 — `swimmerplot`: an untyped NA, and a convention read backwards

### `NA` is logical, and data.table will not mix it with your numbers

- **Failure mode:** a `by=` aggregation seeded a per-group value with bare `NA`. Every group whose source
  values were all missing returned logical; every other group returned double. data.table rejects that with
  "Column 6 of result for group 2 is type 'double' but expecting type 'logical'", the outer handler turned it
  into "Error in Swimmer Plot Analysis", and the user got an empty results pane. One blank cell in a
  1001-row dataset did it; at 999 rows the slow path ran and nothing happened.
- **Detection signal:** the audit's own reproduction, re-run this session. Note the end-to-end call did NOT
  throw — the error was caught and rendered as a notice — so a test that only asserts "no R error" passes
  while the analysis produces nothing. The test written here asserts the plot state EXISTS.
- **Prevention rule:** inside a data.table `j` expression, every sentinel must carry the column's type:
  `x[NA_integer_]`, `NA_character_`, `NA_real_` — never bare `NA`. The giveaway in this file was that the
  response sentinel was already `NA_character_` and never crashed, while its two neighbours were not.

### Never infer which value means "event" without saying so

- **Failure mode:** the censoring classifier sent every non-zero number to "event". A column coded 1/2 —
  `survival::Surv`'s own convention — therefore contained no censored patients at all, so the reverse
  Kaplan-Meier was abandoned, the median follow-up was computed from observed durations instead, and it came
  out about half the true value. Nothing was printed. Yes/No and TRUE/FALSE "ongoing" flags have the same
  hazard with the opposite polarity.
- **Detection signal:** comparing the module's median follow-up against `survival::survfit` on the same ten
  patients under both codings: 30 vs 15.
- **Prevention rule:** 0/1 is the only numeric coding that may be assumed silently. Recognise {1,2} as the
  survival convention explicitly, and DISCLOSE whichever reading was used — a follow-up figure that halves
  when the coding is misread is not something to leave to inference. Where the polarity genuinely cannot be
  known (an "Ongoing" flag), ask the user rather than guessing better.

## 2026-09-20 — `diagnosticmeta` deferred findings: a zero-length value is not an error

### `sprintf()` with a zero-length argument returns `character(0)` and takes the whole panel with it

- **Failure mode:** a new sentence in the Analysis Summary read `private$.pooled_spec_pi`, a private field
  that an earlier pass **in the same session** had deleted as write-only. R6 returns `NULL` for a field that
  is not there, `sprintf()` given a zero-length argument returns `character(0)` without warning, that value
  flowed into the panel's final `sprintf`, and `setContent(character(0))` wrote nothing. The Analysis Summary
  went from 3,783 characters to 0 with no error, no warning and no notice - the analysis looked like it had
  simply chosen not to render that panel.
- **Detection signal:** a verification script that measured the panel's character count, then a bisect of the
  working file against the previous saved copy. Nothing in the test suite caught it, because no test asserted
  the summary was non-empty.
- **Prevention rule:** deleting a write-only field is safe only until someone writes the reader. When a later
  change needs a value that was removed, restore the field rather than reaching for whatever is in scope —
  and guard every element a `sprintf` interpolates, not just the one the branch tests. The generalisable
  check: a panel whose content can vanish deserves a test that asserts its length, not only its wording.

### A source-scraping test pins syntax, so a legitimate refactor breaks it

- **Failure mode:** a test asserted the meta-regression error handler by regex-matching the exact
  `sprintf(.("%s meta-regression failed"), measure)` spelling. Splitting that title into two whole sentences
  for translation — a required i18n fix — broke the test although the behaviour was unchanged and improved.
- **Prevention rule:** when the only way to test a defensive path is to read the source, assert the CONTENT
  it must contain (the framing sentence, the newline strip), never the call shape. State in the comment that
  it is a source assertion and why the runtime path is unreachable.

### `format(x, big.mark = ",")` silently does nothing in this package

- **Failure mode:** a new note printed "1789 participants" instead of "1,789". The package imports jmvcore,
  whose `format()` masks `base::format()` and ignores `big.mark` — no error, just an unformatted number.
- **Detection signal:** the regression test written alongside the fix asserted the formatted string and failed.
- **Prevention rule:** always write `base::format()` for number formatting in this codebase. This is the
  second time the masking has bitten; the test that caught it existed only because the fix was written with
  its assertion at the same time.

### Fixing a finding can reopen one the same session closed

- **Observation, not a failure:** removing the write-only `.pooled_spec_pi` was correct when nothing read it,
  and restoring it was correct once the copy-ready summary needed both margins. The lesson is not "do not
  clean up" — it is that a cleanup and a feature landing in the same session interact, so re-run the full
  suite after each batch rather than at the end.

## 2026-09-20 — reviewing the fix pass: most of the new defects were in the fixes

Six review lenses over the audit-fix diff, each finding put to a skeptic. Nine survived; seven were
introduced by the fix pass itself. The lesson is not "review your work" - it is what kind of defect a
fix pass produces.

### A guard applied at one of two identical sites

- **Failure mode:** the aliased-slope crash (`cf[2, 3]` on a one-row coefficient matrix) was found, fixed
  and commented on the publication-bias test path. The funnel renderer refits the same model on the same
  data and was left indexing row 2 - and unlike the test path it has no `tryCatch`, so the whole plot died.
- **Prevention rule:** when a fix is "guard this indexing", grep for the *expression being guarded*
  (`summary(...)$coefficients`, the same `lm` call) across the file before declaring it fixed. A defect
  found on one path is a defect on every path that recomputes the same thing.

### Text written for the common case, applied by a guard built for the general one

- **Failure mode:** `estimable <- all(diag(Psi)[1:2] > 1e-4)` is correct for deciding whether a
  *correlation* is identifiable - it needs both components. The `else` text was written as if `all()` meant
  "both are zero", so a mixed fit (tau-squared 1.21 and 3.9e-18) printed both numbers and then said the
  studies were consistent with a single common pair.
- **Prevention rule:** when a boolean guard is an `all()` or an `any()`, read the else-branch prose against
  the *other* way the guard can be false. `all(x > k)` being FALSE does not mean `all(x <= k)`.

### A fix that changes which code paths are reachable changes which messages are reachable

- **Failure mode:** the HSROC convergence retry made the model converge where it previously stopped early.
  The converged fit legitimately returns theta > 1 on some data, which routes into an ERROR notice reading
  "the pooled test performs worse than chance. Check the TP/FP/FN/TN column assignment." That advice was
  always wrong, but it was nearly unreachable before and became routine after.
- **Separately:** moving the meta-regression df guard per-margin turned two `return()`s into `return(NULL)`s,
  so a note saying two models "were fitted" became reachable with zero rows in the table.
- **Prevention rule:** after a fix that makes a previously-failing path succeed (or a previously-aborting
  guard continue), enumerate the messages downstream of it and re-read each one. A message that was
  effectively dead is unreviewed text.

### Do not write documentation from the mental model that produced the code

- **Failure mode:** the audit pass added option help saying DerSimonian-Laird "is not offered" for the
  bivariate model. `mm` is passed verbatim to `mada::reitsma`, and mvmeta's `mm` **is** multivariate DL. The
  sentence came from knowing that `.metaforMethod()` maps `mm` to DL for the univariate tables, and stopped
  there. A second sentence in the same pass claimed the publication-bias path ignores the zero-cell setting,
  which a different verifier disproved by running it.
- **Prevention rule:** every sentence of option help is a claim about a call chain. Follow the option value
  to the function that consumes it, and read that function's documentation, before writing what it does.

### Citation drift: a reference attached to the sentence it sits near, not the claim it supports

- **Failure mode:** "a univariate I-squared does not describe the bivariate model (Zwinderman & Bossuyt
  2008)" - that paper is titled "We should not pool diagnostic likelihood ratios in systematic reviews" and
  is about likelihood ratios throughout.
- **Prevention rule:** when adding a bibliography entry for an in-text citation, read the abstract against
  the sentence it is attached to. The reference being real, correctly formatted and about the right field is
  not the same as it supporting the claim.

### A test that passes before and after the fix must say so in its own comment

- **Failure mode:** of the five tests written for these fixes, four fail on the pre-fix code and one does
  not - the panel-clearing defect is only reachable across runs inside jamovi, because each `run()` from R
  starts with a fresh results object. Left unlabelled, that test reads as a regression guard it is not.
- **Prevention rule:** run every new test against the pre-fix file, and write the outcome into the test's
  own comment - "NO-REGRESSION GUARD, not a fail-before test", with the reason.

## 2026-09-20 — `diagnosticmeta` deep audit: the defects were all in what the analysis said, not what it computed

### A note set under a condition, with its option absent from `clearWith`, goes stale

- **Failure mode:** `setNote("disabled", "Bivariate analysis disabled by user option")` was written to a hidden
  table when the analysis was switched off. jamovi keeps results between runs and `setNote` has no "unset", so
  reticking the box left that footnote under real pooled estimates - and saved it into the `.omv`. The enabled
  branch never touched that key, and `bivariate_analysis` was not in the item's `clearWith`.
- **Detection signal:** a delegated inventory of all 65 `setNote` calls, each classified conditional or
  unconditional and cross-referenced against its item's `clearWith` list. One of 65 was wrong.
- **Prevention rule:** for every `setNote` whose gate is an option, that option must be in the item's
  `clearWith`, or the else-branch must rewrite the same key. The same file already got this right for its
  `notices` item, which lists all six analysis toggles - so "the other list does it" is not evidence.

### Every result item gated by an option means "all options off" is a reachable state with no output

- **Failure mode:** unticking the one option that is on by default hid every result item; the instructions
  panel had already been hidden imperatively when the variables were assigned. A valid dataset showed a
  completely empty results pane with nothing to explain it.
- **Detection signal:** a coverage question asked of the message inventory - "what happens when nothing is
  switched on?" - rather than any test.
- **Prevention rule:** when every result item has a `visible:` gate, enumerate the all-off state and make sure
  something still speaks. An imperative `setVisible(FALSE)` on a welcome panel makes this worse, because the
  panel that would have explained the empty pane is exactly the one that was hidden.

### Verify a documentation sentence by running it, not by reading the code that inspired it

- **Failure mode:** the fix pass added a sentence to the `zero_cell_correction` help saying the setting "does
  not govern the publication-bias path: Deeks' test and the funnel plot apply their own correction of 0.5 to
  every study whenever any study has a zero cell". False. Those routines receive the data *after* the user's
  correction has been applied, so under `constant`/`zero_cells`/`reciprocal_n` no zero cell survives, the
  uniform step never fires and a verdict is always given. The sentence was true only of the default.
- **Detection signal:** an adversarial verifier re-derived the claim from the call chain and refused the
  edit; a four-way run over the four correction settings then showed verdicts differing exactly as it said.
- **Prevention rule:** a sentence about behaviour is a claim to be executed. Before writing "X ignores Y",
  run X under every value of Y and read the output. This applies hardest to text written from a code comment -
  the comment described the intent of one branch, not the reachable behaviour of the feature.

### Hidden output still costs: `visible:` hides a panel, it does not skip the work

- **Failure mode:** four `show_*` options appeared nowhere in the backend - they existed only as `.r.yaml`
  `visible:` expressions. The 31 KB of HTML behind them was built on every run and serialized into every saved
  `.omv`, ticked or not.
- **Detection signal:** a differential run over every option showed those four changing nothing in the results
  object, which looked like "non-effective option" until the panels turned out to be populated either way.
- **Prevention rule:** declarative `visible:` is the right way to control the pane, but the generator still
  needs its own guard. And the audit corollary: an option that changes nothing in a differential run is either
  dead or visibility-only - distinguish the two before reporting either.

### Deleting a helper is safe only once you have checked what its side effects were

- **Failure mode:** `.generateSummary()` looked like a pure HTML builder and was a natural thing to gate on
  `show_analysis_summary`. It also set a zero-cell disclosure note on the bivariate table, which is shown
  regardless - so gating it would have silently dropped a disclosure.
- **Detection signal:** grepping the function body for `self$results$` before gating the call, which found one
  `setNote` 58 lines above the `setContent`.
- **Prevention rule:** before gating or deleting a function, grep its body for every write to shared state -
  `self$results$`, `private$.` - not just its return value. Hoist the side effect first, then gate.

### `private$.checkpoint()` belongs outside every `tryCatch(error = )`

- **Failure mode:** the analysis had no checkpoints at all; 200 studies froze the UI for about four seconds.
  The obvious placement - inside each `if (option) { tryCatch({ fit }) }` - would have been worse than none,
  because the checkpoint restart is error-class and the handler would have swallowed it.
- **Prevention rule:** put the checkpoint before the guarded block, never inside it, and assert the placement
  in a test that reads the source (the line after each checkpoint must be blank or a comment).

## 2026-09-20 — `diagnosticmeta` release review: a correction that created the effect it tested for

### A continuity correction applied only where it is needed is a covariate

- **Failure mode:** Deeks' test regresses the log diagnostic odds ratio on 1/sqrt(effective sample size). Zero cells
  make that outcome infinite, so the code added 0.5 to the studies that had one. Those are the small, near-perfect
  studies - one end of the regression - so the correction shrank the outcome exactly where the predictor is largest.
  On null data, with no study ever discarded, the module claimed funnel asymmetry in 66.7% of meta-analyses against
  a nominal 5%.
- **Detection signal:** 400-run null simulations across a grid of true sensitivities, comparing three correction
  strategies. Correcting every study halved the error, but no strategy fixed it: at 80% zero-cell studies the
  uniform correction still rejected 42% of the time, dropping the affected studies rejected 15% at 39%.
- **Prevention rule:** a correction that touches only some rows is a covariate; check whether it is correlated with
  the analysis's own predictor. When simulation shows that no repair restores the nominal rate, withhold the verdict
  in that regime instead of shipping the number with a caveat. Report the threshold you measured, not one you
  assumed - here, at or under a quarter of studies corrected the test behaves, past 40% it does not.

### A wide interval is not evidence of heterogeneity

- **Failure mode:** the "substantial between-study heterogeneity" warning and the summary sentence fired whenever the
  prediction interval spanned more than 30 points. That interval is built from the between-study variance AND the
  uncertainty of the pooled mean, inflated by t on k-2 df; at k = 4, t = 4.30. Three identical studies, with Q = 0,
  I-squared = 0 and tau-squared = 0, were reported as "studies differ more than sampling error explains".
- **Detection signal:** an audit lens ran identical and homogeneous study sets through the real class and compared
  the module's claim with metafor's Q and I-squared on the same data.
- **Prevention rule:** trigger a heterogeneity claim on the estimated between-study variance, not on the width of
  anything. A useful gate is the share Psi/(Sig+Psi): it is near 0 when the width is small-k noise. And report a
  correlation between two variance components only when both are estimable - below about 1e-4 on the logit scale it
  is the ratio of two rounding residues, and it will happily read -0.54 where an independent fit gives +0.21.

### A convergence warning that is caught and then suppressed is a silent wrong answer

- **Failure mode:** `mada::phm()` only WARNS ("Reached maximum number of iterations!") when it exhausts its
  100-iteration budget. The handler caught that warning and refitted the same model with `suppressWarnings()` at the
  same limit, so the last iterate was presented as a fit: theta 0.056 / AUC 0.947 where the converged fit gives
  1.574 / 0.389.
- **Detection signal:** refitting the audit's example with l = 100, 1000, 5000 and 20000 and comparing.
- **Prevention rule:** never answer a warning by re-running the same call with warnings off. Either change what the
  warning is about (here, the iteration budget) or surface it. `withCallingHandlers` with a muffle restart lets you
  detect a specific warning without aborting the call - `tryCatch(warning=)` throws away the result you already had.

### A prior review's decision lives in a test; do not overturn it in passing

- **Failure mode:** fixing "the prediction region is tighter than the prediction interval" by switching the region's
  radius from chi-squared to F broke `test-diagnosticmeta-release-review.R`, which pins the region to mada's own
  `plot.reitsma(predict = TRUE)` construction - a parity decision an earlier review had made deliberately.
- **Detection signal:** the existing suite, immediately.
- **Prevention rule:** when a failing test encodes a deliberate choice rather than an accident, treat it as a
  constraint on the fix, not as a test to update. The contradiction was removable by disclosure (say the region is
  the joint version at the large-sample radius and the interval the conservative marginal one), which keeps parity.

## 2026-09-19 — `ihcheterogeneity` release review: the "error-free" level was not error-free

### A simulation that only tests the design's own assumption cannot refute it

- **Failure mode:** the proportional-bias check compared each region along the mean of the OTHER regions and let
  that slope make a row MATERIAL, on the assumption that the other regions share no error with either reading. The
  first design review simulated only nulls that satisfy that assumption. Regions from the same needle pass, block or
  staining run share a case-level deviation u that the whole section lacks; then cov(d, level) = var(u) and unbiased
  regions were called MATERIAL (with "calibrate with a slope" advice) in 13% of studies at n = 300 and 32% at n = 600.
  A sparse second region also shrank the check to 7 of 40 cases and turned a withheld verdict green.
- **Detection signal:** a red-team of the release-review plan added nulls that break the assumption (shared site
  effect, clipping at the scale limits) and asked for n beyond the usual sizes; the simulation lens and two skeptics
  per finding confirmed it through the real class.
- **Prevention rule:** for any statistic that is only valid under an untestable independence assumption, simulate at
  least one null that violates it and run to large n (an artefact grows in significance, not in size). With one
  reading per method, let such a slope only withhold a green verdict, never assert a defect.

### A skill's generic advice conflicted with this repo's version invariant

- **Failure mode:** following the release-review skill ("bump the analysis `version:` when it changes materially"),
  I set `jamovi/ihcheterogeneity.a.yaml` to 1.0.82. `test-oncopath-library-audit.R` failed: `_updateModules.R`
  writes the first three components of the package version into every analysis `version:`, and the test enforces it.
- **Detection signal:** running the OncoPath library-audit test with the analysis suites.
- **Prevention rule:** never hand-edit an analysis `version:` here; the package version (updater `new_version`)
  versions every shipped copy.

---

## 2026-09-19 — `ihcheterogeneity` proportional bias: my first design would have invented bias

### Regressing a difference on a level that shares an error with it builds in a slope

- **Failure mode:** the review found a region that compresses the scale (mean difference ~0) passing as
  "AGREEMENT THRESHOLDS MET". My fix regressed region minus reference on the REFERENCE, with OLS standard errors.
  A base-R simulation by an independent reviewer showed it would call unbiased regions MATERIAL in up to 99.9% of
  studies with a hotspot-like reference (artefact slope -s_ref^2 / var(ref)), and that OLS errors flagged a slope in
  33-48% of studies when the error grows with the level, as IHC error does.
- **Detection signal:** a design-review workflow with a simulation lens (null scenarios with realistic reference
  error and multiplicative error), BEFORE the code was written; a second adversarial review then found that a
  block-only slope still drove "calibrate with a slope" advice (a regression-to-the-mean artefact in 57-83% of runs).
- **Prevention rule:** for any Bland-Altman / method-comparison slope, the level must share no measurement error with
  either reading (here: the mean of the OTHER regions); where no such level exists, a slope may only block "ruled
  out", never support a claim. Use HC3 errors. Simulate the null with unequal and level-proportional error before
  shipping a new test statistic.

### A TODO comment broke the library-audit rule

- **Failure mode:** the review filed three inline `# TODO` comments in `R/ihcheterogeneity.b.R`, following the
  standing "file out-of-scope items as TODOs" habit. `test-oncopath-library-audit.R` requires that OncoPath analyses
  carry no TODO comments.
- **Detection signal:** running the OncoPath library-audit test beside the analysis's own suites.
- **Prevention rule:** for analyses shipped to OncoPath (and any module audited by the jamovi library reviewer),
  file follow-ups in `TODO.md`, not as inline TODOs; run `test-oncopath-library-audit.R` with the analysis suites.

---

## 2026-09-19 — function check of my own `ihcheterogeneity` fix: a new option left an old constant behind

### The margin I added did not reach the column that grades the same difference

- **Failure mode:** round 3 added `bias_margin` and made the verdict, table note and report sentences use it, but the
  per-row "Clinical Impact" column kept fixed 5% / 15% bands on the point estimate beside a 95% CI. At a margin of 10
  the column read "Moderate (5-15%)" for rows the rule had ruled out; at 2 it read "Minimal (<5%)" for an inconclusive
  row. The same round left the limits-of-agreement sentence saying "differ from the reference" in the inter-regional
  design (copy-ready text included), and the column title "Region - Reference" with no reference.
- **Detection signal:** `/function-checker` plus a runtime-contract checker that ran every option at NON-default
  values and read each row's text against the rule's own flags; adversarial verifiers rejected 3 of the checkers'
  patches (e.g. a 95% CI printed beside a zone the 90% CI decides) and supplied the corrected versions.
- **Prevention rule:** when an option replaces a constant, grep the file for the constant's literals ("5%", "15%",
  "reference") and route every consumer through the option in the same change; add one test at a non-default value
  that reads each consumer. Text that names the comparison ("reference", "other regions") must branch on the design
  the row actually used, like the label beside it does.

### A plot fix that only worked in the tests

- **Failure mode:** `theme(axis.text.x = element_text(angle = 45))` and `scale_fill_manual(...)` sat BEFORE `+ ggtheme`.
  jamovi's ggtheme is a list: a complete theme (resets earlier `theme()` and drops `vjust`) plus discrete fill/colour
  palette scales (replace the manual scale). In jamovi the labels overlapped horizontally and the colour-blind-safe
  fill, legend title and unused band vanished; tests passed because they rendered with `ggplot2::theme_grey()`.
- **Detection signal:** a verifier that rendered with `jmvcore:::getGlobalTheme("default", "jmv")$ggtheme` and read
  the resolved `axis.text.x` angle and the fill scale name.
- **Prevention rule:** theme tweaks and manual scales go AFTER `ggtheme` (rotated labels need `vjust = 1`); renderer
  tests use the jamovi global theme, not `theme_grey()`. 19 more sites in 11 other analyses have the old order.

## 2026-09-19 — `stagemigration` verdict; a tool reported "not installed" that was only off PATH

### A worse staging system was recommended for adoption

- **Failure mode:** `.assessSignificance()` called the C-index change "clinically significant" by `abs(delta)`, and
  took "statistically significant" from the nested LR test "new staging adds to the original", which is significant
  for a worse system that carries any extra factor. Simulated n=3000, C 0.728 (old) vs 0.696 (new): "RECOMMEND
  ADOPTION", Confidence: High. The null branch printed "DO NOT ADOPT", Confidence: High for n=120, p=0.849, and also
  whenever both tests failed (NA). The 2026-09-10 release review fixed the LR test itself and passed the verdict.
- **Detection signal:** two synthetic scenarios run end to end that checked the VERDICT row, not the statistics
  feeding it (one with a known worse new system, one small null cohort).
- **Prevention rule:** same lesson as the `ihcheterogeneity` entry below. A verdict is signed: never `abs()` a
  difference that decides "better". A non-significant result is "inconclusive", never a high-confidence "no", and
  a test that did not run must not fall into any verdict branch. Every verdict branch gets a scenario test, including
  one where the new method is truly worse. Filed as TODOs in `stagemigration.b.R`; not fixed yet.

### "codex is not installed" - it was, just not on Claude Code's PATH

- **Failure mode:** the `codex` MCP server failed with ENOENT and I told the user codex needed installing. It was in
  `~/.local/bin`, which (like `/opt/homebrew/bin` and `/usr/local/bin`) the VS Code-launched Claude Code process never
  had: its PATH was `/usr/bin:/bin:/usr/sbin:/sbin` plus plugin dirs, because `~/.zshrc`/`~/.zprofile` never ran.
  Separately, codex 0.155.1 removed `codex mcp-server` (the word is now read as a TUI prompt: "stdin is not a
  terminal"), so the configured server could not start even with the right path.
- **Detection signal:** the user's `where codex` in their own terminal.
- **Prevention rule:** ENOENT from an MCP server or plugin means "not on THIS process's PATH", not "not installed":
  check `echo $PATH` and `zsh -lic 'command -v X'` before saying a tool is missing. PATH is now set in `env.PATH` of
  `~/.claude/settings.json` (and this project's `.claude/settings.local.json`, which replaces it and keeps
  `.venv/bin` first). After a CLI upgrade, probe the configured subcommand, not only `--version`.

## 2026-09-18 — reviewing my own `ihcheterogeneity` rules: significance is not materiality

- **Failure mode:** my first fix judged a systematic difference "material" when its point estimate exceeded 5% and
  its p-value against ZERO was < 0.05, and "ruled out" only when the whole 95% CI fitted the margin. Simulated: a
  truly 3% (non-material) offset was vetoed in up to 35% of studies - more often at larger n - while an unbiased method
  almost never reached the green verdict at usual study sizes. Two further own choices failed the same way: grading
  the LOWEST of k per-region correlations (the minimum of k equally good regions falls as k grows, so sampling more
  cores worsened the verdict), and grading a single-region RMS error against the per-case CV threshold (different
  scales: "above your threshold" beside "no case above your threshold"). Each passed my own tests.
- **Detection signal:** a second independent review that SIMULATED verdict frequencies over a grid of n, k, noise and
  true bias instead of checking single examples.
- **Prevention rule:** a clinical decision rule must be judged by its operating characteristics, not by examples.
  Materiality and equivalence are questions about a MARGIN: equivalence = 90% CI inside it (TOST, Schuirmann 1987),
  materiality = an adjusted CI entirely beyond it, anything else is inconclusive and says so with the estimate. Never
  grade an order statistic (minimum/maximum) against a fixed threshold; grade the pooled estimate and flag a unit only
  when its CI excludes the threshold. Compare a quantity only with a threshold defined on the same scale.

## 2026-09-18 — JamoviTest could not be built: the updater emptied its manifest into a crash

- **Failure mode:** while no analysis was routed to JamoviTest, `prune_orphan_analyses()` removed every entry from
  its `jamovi/0000.yaml` and left a bare `analyses:` (YAML null). When waterfall and ihcheterogeneity were routed
  there, the jamovi compiler died with `TypeError: packageInfo.analyses is not iterable` after writing one `.h.R`;
  prepare() cannot repair a file it fails to read, so every later run failed the same way. `run_child()` treated
  the step as successful (its regex matched `^Error`, not `TypeError`), so the pipeline went on to a module with no
  NAMESPACE and the install failed. Separately, neither shipped analysis declared `@import jmvcore`, so the module
  had no jmvcore import and every `.()` would have been unresolved at run time.
- **Detection signal:** "cannot install test module"; reproduced by running prepare() on a copy of JamoviTest; the
  installed-namespace smoke check (`tools/submodule_smoke.R`) reported `UNRESOLVED . in 56 function(s)`.
- **Prevention rule:** never leave an empty YAML key where a consumer expects a list - write `[]`. A child-process
  step is failed by any `^\w*Error` line, not only `^Error`. Every analysis file declares the imports it needs
  (`@import jmvcore` for `.()`), instead of relying on another analysis in the same module to bring them in.

## 2026-09-18 — `ihcheterogeneity`: a bias veto that averaged the bias away, and a gate that sorted away C5

- **Failure mode:** (1) the "no material systematic bias" verdict tested only the MEAN of the regions against the
  reference, so an invasive front over-reading by 11% and a centre under-reading by 11% cancelled, and every panel said
  "no systematic bias"; an exact constant offset (p undefined) escaped the veto entirely. (2) The minimum-case gate
  counted rows, so a reference scored in 3 of 30 cases produced a full verdict with no small-sample notice. (3) The
  Turkish catalogue reordered `%s ... %s ... %.2f` without `%n$` markers, so the analysis failed on every Turkish run,
  and `tools/release_gate.py` could not see it: it compared SORTED conversion lists, which are equal for a reordering.
  (4) While fixing, a backslash-u escape for the greater-or-equal sign typed into an R string through the Write/Edit
  tools reached the file as the literal non-ASCII character, which R CMD check rejects.
- **Detection signal:** the OncoPath release check (I00/I01, I06-I08, C5) with independent recomputation per region;
  for the gate, running the corrected check on the pre-fix catalogue (old check: 0 of 5 flagged; new: 5 of 5). For
  (4), `grep -nP "[^\x00-\x7F]" R/<file>.b.R` after writing.
- **Prevention rule:** a veto must be evaluated on the unit a clinician substitutes (each region), not on an average
  that lets errors cancel; decide materiality on size (share of the reference mean) AND evidence (Holm-adjusted p, or an
  exact constant offset). Gate on the unit the statistic uses (cases with a reference and a region), and report that n
  everywhere. A format check must compare conversions IN ORDER (or by `%n$` argument), never as a multiset. After
  writing an R file with a tool, grep it for non-ASCII before parsing.

## 2026-09-18 — reviewing my own `waterfall` fix: three majors the fix introduced or missed

- **Failure mode:** the first fix passed 431/431 tests, yet an independent review found (1) exact-boundary progression
  missed ~40% of the time with raw input: `(60 - 50) / 50 * 100` via percent space gives 19.99999999999999, below the
  inclusive `>= 20`; (2) best response still counted scans after a documented progression; (3) a denominator of mine
  that included Unknown patients. A scripted block move also deleted an adjacent line (`n_supplied`).
- **Detection signal:** a six-reviewer workflow with adversarial verification of each issue, then a differential against
  an independently written base-R RECIST reference on random data (pre-fix 435/600, fixed 600/600).
- **Prevention rule:** compare every inclusive clinical threshold with a tolerance when the value is derived; own tests
  written by the implementer share the implementer's blind spots - check a fix against an independent reference on
  random data, and run that check on the unfixed code first. After a scripted block move, grep for every line that sat
  at the block's edges.

## 2026-09-18 — `waterfall`: a PD that could never happen, and an equivalence test that proved nothing

- **Failure mode:** with a time variable, best response was `slice_min(response)` over every row, including the
  time-0 baseline row at 0%. No patient's best response could exceed 0%, so a tumour that only grew was scored SD,
  PD never occurred, and the panel printed "DCR 100% - Excellent disease control". A separate >100-row processing
  path had its own copy of that logic plus group handling that duplicated a patient whose group changed between rows.
- **Detection signal:** the OncoPath release check (C1, C3): independent recomputation of best response by hand.
  Existing tests passed because (1) the baseline test covered the patient who should NOT be scored (baseline only ->
  Unknown), never the one who SHOULD be PD; (2) "large-data path agrees with the standard path" compared two copies
  that shared the bug, on data without a group variable.
- **Prevention rule:** an equivalence test between two implementations proves they agree, not that either is right;
  pin one to a hand computation, or delete the duplicate (done: one path, 18,000 rows in 0.22 s). For every category
  a classifier can emit, keep a test where that category is the expected answer. `df$col` on a tibble warns when
  `col` is absent ("Unknown or uninitialised column"); test `"col" %in% names(df)`.

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
