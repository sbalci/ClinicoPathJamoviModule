# Reader reproducibility implementation record

Date: 2026-10-03. Implements the [approved plan](./Stoieva-2026-IHC-reader-reproducibility-implementation-plan.md).

## Delivered behavior

- `pathagreement` has an off-by-default independent-reader workflow for binary
  calls, ordered scores and numeric cutoffs. The existing categorical workflow
  and generated R wrapper remain compatible with existing calls.
- Accuracy is explicit disease-positive Se/Sp/BA with reader/reference four-cell
  counts, exclusions and available/shared-case populations. The original table
  is now labelled macro-averaged across categories.
- Pairwise cutoff results retain usable pairs despite other readers' missing
  values. They show the four reader-pair counts, observed agreement, discordance,
  Cohen kappa and positive/negative specific agreement, with separate near-cutoff
  populations. Original ordinal weighting respects declared category order.
- The capped disagreement table retains original scores, clinical calls, IDs and
  original dataset row numbers. Changing its cap never changes the aggregates.
  Row structure is recreated during initialization so saved table cells restore.
- Whole-case/whole-patient resampling preserves paired readings, the selected
  reference and specimen weighting. It preserves the caller's RNG state and
  reports attempted, valid, undefined, boundary and constant-distribution diagnostics.
  Independent Bernoulli proportions instead use exact binomial intervals.
- `agreement` offers the shared case/cluster bootstrap for specific agreement,
  retaining its existing complete-case population and explicitly approximate
  Wilson default. Other agreement outputs do not acquire cluster correction.
- The bounded `cohenskappa` repair supplies numeric linear/quadratic weights to
  `psych`, extracts weighted kappa and its matching variance, and requires a
  common ordered scale. This is not a release audit of the whole draft analysis.
- The optional module-specific composite explicitly identifies its accuracy
  reader and selected pair, uses a common pair/reference mask, and reports its
  components. Negative/undefined kappa makes the composite unavailable. It has
  no clinical acceptance bands and is not presented as the paper's verified
  aggregation rule or a reporting standard.

## Evidence-driven refinement to the plan

The seeded coverage experiment did not justify calling percentile bootstrap
intervals a validated nominal-95% procedure. With 60 clusters, balanced-accuracy
coverage was 91.3% (Monte Carlo 95% interval 87.6–94.3%). The new bootstrap is
therefore explicitly exploratory, and `agreement` keeps its existing approximate
Wilson default. Sparse and constant distributions suppress intervals instead of
silently discarding failed draws. See the [coverage and performance audit](./reader-reproducibility-validation/README.md).

The planned cutoff summary is implemented as `readerSettings`, a unified summary
of the reference, labels, cutoff, scale, population, sampling unit and reproducibility
options. New category selectors are default-empty Strings to preserve the optional
R API, as specified in the plan. Prespecified ordinal levels remain in the declared
scale even when no retained specimen uses a boundary category. Because jamovi TextBox controls are single-line, the interface also accepts semicolon-separated boundary labels; newline-separated R input remains supported.

## Examples and documentation

- [Worked guide](../vignettes/oncopath-reader-reproducibility.Rmd) and
  [rendered article](../docs/articles/oncopath-reader-reproducibility.html).
- [Synthetic-data generator](../data-raw/generate_ihc_reader_reproducibility.R),
  [R dataset](../data/ihc_reader_reproducibility.rda),
  [CSV](../data-raw/non-rda/ihc_reader_reproducibility.csv), and
  [jamovi data file](../data-raw/non-rda/ihc_reader_reproducibility.omv).
- The `.omv` example contains synthetic **data**, including declared ordinal
  levels; saved analysis output is covered separately by the real-protobuf
  lifecycle test. The guide and optional R-code output reproduce analysis settings.

## Validation

Baseline was a clean working tree at `ebf58ce10`. Existing pathagreement regression,
wiring and lifecycle tests passed; the initial save/reopen test skipped because
RProtoBuf was absent from the development library. Subsequent runs used the
installed jamovi application's RProtoBuf and locally bound its real transport
functions where standalone development jmvcore leaves engine aliases NULL.
This exercises actual serialization; no numerical outputs are mocked.

Completed checks:

- Hand-calculated accuracy/agreement fixtures; `stats::binom.test()`, `irr` and
  `psych` reference comparisons; undefined estimates, negative kappa, exact-cutoff
  equality, ordinal ordering, metadata mapping, missingness and repeated IDs.
- Exported-wrapper and R-code rerun checks; no-jointly-complete-case regression;
  available/shared denominators; reference-complete composite masks; display caps.
- Real protobuf save/restore of new tables and R code, and legacy plot restoration
  without recomputation. Data-only `.omv` round-trip retains ordinal classes.
- Fixed-seed resampling, RNG restoration, complete sampled clusters including
  duplicate cluster draws, confidence levels and invalid-draw accounting.
- Real jamovi client option-panel loading for all three analyses, including the
  numeric reader workflow with reference, cutoff, bootstrap and composite enabled.
  The initial source-only harness could not resolve `agreement.events`; rerunning
  with jamovi compiler's normal Browserify bundle resolved that harness limitation.
- `jmvtools::prepare(home="/Applications/jamovi.app")` and `devtools::document()`;
  `pkgdown::build_article("oncopath-reader-reproducibility")` rendered the worked
  guide with computed outputs. A whole-site rebuild was not needed for this article.
- Scoped image-state guards: zero unguarded reads. Handwritten-file whitespace
  checks pass; compiler-owned headers retain the compiler's existing whitespace.
- Updater dry run discovers all four helper files from actual callers and the new
  dataset from the JamoviTest data manifest. `withr` is already declared in that
  destination. No runtime dependency was added.

Final focused command (fresh R process) passed with no failures or skips:

```r
.libPaths(c(.libPaths(), "/Applications/jamovi.app/Contents/Resources/modules/base/R"))
devtools::test(
  filter = "^(pathagreement-|reader-resampling|agreement-reader-intervals|cohenskappa$|zzz-analysis-file-naming$)",
  reporter = "summary")
```

This includes the metadata/unused-ordinal-boundary regression, semicolon GUI
input, both cancellation tests, saved-result restoration, wrapper compatibility,
all existing pathagreement regressions, and helper ownership/naming checks.
A separate broad `^agreement-` run had no failures in the pre-existing tests and
retained the existing `VAL-agreement-13` coverage skip. Its newly added cancellation
test encountered the earlier source already loaded by that running process; the
fresh final run above confirms the corrected cancellation path passes.

`python3 tools/release_gate.py` completed with **0 blocking issues and 10 advisory
items**, concerning existing repository-wide issues. No advisories were hidden or
waived for this feature. The broader package check runs with examples, tests,
manuals and vignette rebuilding disabled; focused tests and the worked article
were run separately as described above. It completed with **0 errors, 4 warnings and 2 notes**. Installation, syntax,
namespace loading, R-code analysis and Rd syntax passed. Warnings concern existing
non-portable filenames, duplicate dataset objects/TMB and display-environment
warnings during installation, undocumented pre-existing classes, and unrelated
dataset/documentation mismatches. Notes concern existing package subdirectories
and declared Imports. No finding names the new reader helpers or IHC dataset.
This package check used the implementation snapshot taken when its build started;
final input, resampling, cancellation and UI refinements were covered by the fresh
focused tests and UI checks above, without repeating the expensive whole-umbrella byte compilation.

Package-check command:

```r
rcmdcheck::rcmdcheck(
  args = c("--no-tests", "--no-examples", "--no-manual", "--ignore-vignettes"),
  build_args = "--no-build-vignettes",
  check_dir = "/tmp/reader-package-check", error_on = "never")
```

## Routing and remaining desktop acceptance

`pathagreement` remains `OncoPathT`, `agreement` remains `meddecideT`, and the bounded
draft repair is routed as `meddecideExtraDT`. The updater was only run with
`--dry-run`; no sibling module was synchronized or production analysis promoted.
Its plan also includes pre-existing `nogoldstandard` and `decisioncompare` changes,
which were not modified by this implementation.

The attempted native jamovi inspection stalled during application startup. Manual
clipboard copying, light/dark theme inspection, and opening an analysis-bearing
saved `.omv` in the desktop remain unchecked. Automated real-client rendering,
partial selections/toggles, protobuf restoration and data-file round-trips are
complete; these are not represented as a completed manual desktop acceptance test.
