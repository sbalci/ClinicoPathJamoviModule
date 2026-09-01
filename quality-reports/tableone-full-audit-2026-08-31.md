# SYSTEMATIC CHECK: `tableone`

Historical pre-repair findings. See [repairs and verification](tableone-fixes-2026-08-31.md)
for the subsequent implementation and current validation limits.

Date: 2026-08-31. Status: **NEEDS WORK**. Priority: **High**.

Report-only audit using `check-function-full`. No analysis, schema, generated file,
test, or documentation source was repaired. `menuGroup` was already `ExplorationT`.
Unrelated worktree changes were preserved.

## Quick summary

- Arguments: **8 defined; 8/8 integrated** — seven read from `self$options`, with
  the framework-provided `data` correctly consumed through `self$data`.
- Outputs: **9 defined; 9/9 have reachable setters**. Four mutually exclusive
  table styles, three optional supplementary panels, instructions, and assumptions.
- Functional, not a placeholder. Tested means, SDs, quartiles, percentages, and
  listwise deletion agree with independent calculations on the audit fixtures.
- Release blocker: a style-only transition can retain a previous manuscript
  sentence and summary as **visible and not stale**, although no table is produced.
- Further concerns: silent omission of unsupported R column classes, inaccurate
  published documentation, an outdated test, and raw HTML entities in text output.
- Notices: useful HTML guidance exists, but severity and positioning are only
  partially compliant. The documented local Notice serialization limitation makes
  wholesale replacement with dynamically inserted Notice objects inappropriate.

## Scope and execution evidence

Inspected all four core files, the generated header, six `test-tableone*.R` files,
four documentation files, references, and runtime dependency declarations. Read
`CLAUDE.md`, the canonical audit playbook, and its required development guides.

Runtime: R 4.6.0; jmvcore 2.7.38; jmvtools 28.3; tableone 0.13.2;
gtsummary 2.5.1; arsenal 3.7.1; janitor 2.2.1; kableExtra 1.4.1;
htmltools 0.5.9; testthat 3.3.2. The startup warning that jmvcore was built under
R 4.6.1 is an environment warning, not an analysis-generated warning.

ClinicoPath is **not installed** in the active R library. Tests used the unchanged
`R/tableone.h.R` and `R/tableone.b.R` sourced together with jmvcore and magrittr.
A test-environment adapter loaded `data/tableone_test.rda` when tests requested that
fixture from the uninstalled package; it did not change analysis code or assertions.

| Existing test file | Passed assertions | Failed assertions | Skipped tests |
| --- | ---: | ---: | ---: |
| `test-tableone-arguments.R` | 66 | 0 | 0 |
| `test-tableone-basic.R` | 31 | 0 | 0 |
| `test-tableone-edge-cases.R` | 49 | 1 | 0 |
| `test-tableone-integration.R` | 0 | 0 | 12 |
| `test-tableone-release-review.R` | 47 | 0 | 0 |
| `test-tableone.R` | 12 | 0 | 0 |
| **Total** | **205** | **1** | **12** |

The 12 integration tests skipped explicitly because ClinicoPath was not installed.
The one failure is the outdated janitor expectation described below, not evidence
that the current janitor exclusion policy is wrong. No test warnings or test errors
were recorded in these six files.

Additional verification:

- **16 audit-specific assertions passed**: independent numeric checks, upstream
  gtsummary output agreement, arsenal statistics, janitor denominators,
  `nonnormal` scope, and safe generated syntax.
- **60 edge/style runs**: 15 fixtures across four styles. Covered logical/text
  columns, single/unused factor levels, all-missing variables, complete-case
  exhaustion, empty and one-row data, infinity, constant values, Date columns,
  backticks/braces, and HTML-like names/levels. Infinity was rejected by jmvcore
  before the backend in all four styles, as expected.
- Source analysis/results schemas compiled with the installed jamovi compiler to
  a temporary header. Its parsed R definitions are identical to the existing
  header; only generated documentation differs.
- UI compiled using the repository's copy-first UI compiler harness.
- `test-zzz-results-rendering-contract.R`: **6 assertions passed**.
- Targeted state-guard and theme-safety scans: **0 findings**; no plot exists here.
- All **6 reference keys resolve**, with populated author/year/title/URL fields.
  All 11 explicitly referenced/imported runtime packages are declared in Imports.
- Actual jmvcore result trees converted to protobuf and serialized successfully
  for t1–t4: 6,743 / 7,678 / 7,759 / 9,317 bytes on the audit fixture.
  RProtoBuf 0.4.27 was loaded from the existing jamovi installation after appending
  its base R-library path; nothing was installed.

The lifecycle checks used `results$asProtoBuf()` and
`results$fromProtoBuf(..., oChanges, character())` on newly constructed analysis
instances, with the explicit changed option names. This exercises actual result
restoration and invalidation, **not** a mock results object. It is not a full
`.omv` save/reopen or GUI test. Full `AnalysisResponse` serialization needs the
installed module namespace and was not certified. Whole-module `prepare()`,
`devtools::document()`, installation, and `R CMD check` were not run; they would
broaden or mutate a report-only single-function audit.

Audit scripts and detailed logs are retained in
`/tmp/tableone-audit-kRUrvE/` (`audit.R`, `audit.log`, `existing-tests.R`,
`existing-tests.log`, `verification.R`, `verification.log`, `results.rds`). These
are temporary diagnostic artifacts, not permanent regression coverage.

## Argument behavior matrix

Unless specified otherwise, the baseline selects three variables from 20 rows:
`x = c(1:19, 100)`, a two-level factor with two missing values, and an ordered
three-level factor. Non-input options retain their schema defaults. Required
data/variable selections are populated to make defaults executable.

| Argument | Default/baseline → change | Observed result | Effective? | Evidence |
| --- | --- | --- | --- | --- |
| `data` | Original `x` → doubled `x` | Numeric summary changes | Yes | `R/tableone.b.R:194`, `:259`; `DIFF data` |
| `vars` | Three selected → two | Ordered-factor rows removed | Yes | `R/tableone.b.R:107`, `:194`; `DIFF vars` |
| `sty` | `t1` → `t2`, `t3`, `t4` | Correct alternate table populated and visible; t4 omits numeric `x` with explanation | Yes, lifecycle defect below | `R/tableone.b.R:253`, `:324`, `:347`, `:376` |
| `excl` | `FALSE` → `TRUE` | N 20 → 18; x mean 14.50 → 9.50; missing column becomes 0 | Yes | `R/tableone.b.R:204`; `DIFF excl` |
| `showSummary` | `FALSE` → `TRUE` | Summary panel populated and visible | Yes | `R/tableone.b.R:238`, `:716` |
| `showAbout` | `FALSE` → `TRUE` | Educational panel populated and visible | Yes | `R/tableone.b.R:242`, `:639` |
| `showReportSentence` | `FALSE` → `TRUE` | Copy-ready text populated and visible | Yes | `R/tableone.b.R:246`, `:891` |
| `nonnormal` | `FALSE` → `TRUE` | x mean (SD) 14.50 (20.86) → median 10.50 [5.75, 15.25] | Yes, t1 only | `R/tableone.b.R:284`, `:289`, `:311` |

`nonnormal` was also toggled under t2, t3, and t4: content was identical, as the
option explicitly promises. This is a documented conditional option, not dead
code. The UI could disable it outside t1 to make that limitation clearer.

Default `vars = NULL` and explicit `character()` on a nonempty R data frame failed
in framework initialization with `invalid 'row.names' length`; see the R-entry
limitation below. This was not treated as a working onboarding run.

## Output population matrix

| Output | Type | Setter in `R/tableone.b.R` | Visibility | Populated? / qualification |
| --- | --- | --- | --- | --- |
| `todo` | Html | `:84`, `:98`, `:103`, `:143`, `:147`, `:215` | Always | Yes; onboarding, omissions, and empty-cohort instructions |
| `tablestyle1` | Preformatted | `:322` | `sty:t1` | Yes; rendered string and denominator legend |
| `tablestyle2` | Html | `:345` | `sty:t2` | Yes; gtsummary → kableExtra |
| `tablestyle3` | Html | `:374` | `sty:t3` | Yes; generated entities survive into text conversion |
| `tablestyle4` | Html | `:183`, `:524` | `sty:t4` | Yes; frequencies or explicit omission explanation |
| `reportSentence` | Html | `:891` | `showReportSentence` | Yes; stale-content and omitted-column findings apply |
| `summary` | Html | `:716` | `showSummary` | Yes; stale-content and omitted-column findings apply |
| `about` | Html | `:639` | `showAbout` | Yes; may be computed while hidden on early-return paths |
| `assumptions` | Html | `:792`, `:798` | `vars` | Yes; stale-content and severity-placement findings apply |

There are no native result Tables, Images, or renderers to validate. Every declared
`clearWith` name resolves. `excl` is included on the tables and data-dependent
supplementary panels, but **`sty` is missing from supplementary invalidation even
though t4 changes the included-variable set**. No analysis output is permanently
hidden. No executable `setVisible(FALSE)`, fixed-row `addRow()`, or disabled
schema-referencing code was found.

## Notices coverage matrix

The table distinguishes semantic guidance from native `jmvcore::Notice` objects.
The latter are not used; they are not required merely to satisfy an API checkbox
when the repository documents a serialization limitation.

| Trigger | Intended severity | Position / implementation | Present? | Assessment |
| --- | --- | --- | --- | --- |
| Empty dataset | Error/instruction | Top `todo` HTML; early return | Yes | Actionable; runtime verified |
| No selected variables | Instruction | Top `todo` HTML intended | Partial | R wrapper fails before reaching this branch on nonempty data |
| All-missing variable | Warning | Top `todo` HTML | Yes | Names escaped and omitted columns removed before exclusion |
| No usable variables / no cases after exclusion | Error | Top amber HTML; early return | Yes | Explains remedy; no error status or native ERROR severity |
| N < 10 | Strong warning | Last `assumptions` HTML panel | Yes, partial | Numeric and descriptive; not top-positioned or distinctly severe |
| N 10–29 | Warning | Last `assumptions` panel | Yes | Counts/percentage interpretation guidance |
| Original incomplete cases > 50% / > 20% | Strong warning / warning | Last panel | Yes | Source missingness retained; thresholds compare rounded percentages |
| Case loss > 30% / > 10% | Strong warning / warning | Last panel | Yes | Excluded and retained counts plus next steps |
| Janitor omits numeric/high-cardinality input | Warning | In frequency output | Yes | Explicit reasons and alternatives |
| Upstream drops unsupported Date column in t1 | Warning | R console only | **No visible notice** | Table/report/exclusion inconsistency |
| Methodology and denominator explanation | Info | t1/t4 legends; optional About | Yes | Useful and largely accurate within documented types |
| No threshold crossed | Info | Last panel | Yes | Carefully scoped, not a claim of universal data validity |

AUC, EPV, survival-event and diagnostic-prevalence thresholds are **not applicable**
to an overall-cohort descriptive table. No inferential test is performed, so a
normality-test gate is not necessary. Small-n and missingness thresholds are
implementation heuristics, not externally validated clinical cutoffs.

The official [jamovi Notice API](https://dev.jamovi.org/api_notices.html) describes
native severities and discourages HTML inside native notices. The repository's
toolchain-specific guidance instead supports theme-safe Html and `reject()` where
appropriate. HTML paragraphs here are not a violation of the native single-line
notice constraint; their weak severity/placement remains an independent UX issue.

## Critical / high-priority issues

### 1. [P1] A style-only change leaves an obsolete manuscript report marked current

Evidence: `jamovi/tableone.r.yaml:52` (report), `:63` (summary), `:80`
(assumptions); `R/tableone.b.R:182`–`:186`.

Reproduced using actual protobuf result restoration:

1. Select numeric `x` only under t1 with summary/report enabled; 20 cases are
   described and a table is generated.
2. Restore the results into a new instance with **only `sty` changed to t4**.
3. t4 correctly states that `x` is not tabulated and returns early.
4. The old summary, report sentence, and assumptions remain visible with
   `stale = FALSE`. The report still says the table summarizes 20 patients and
   includes `x`, although no variable was tabulated.

The same source mechanisms apply when all categorical variables exceed the t4
category limit. A mixed-input t1 → t4 transition that retains usable variables
does recompute its report correctly; the defect is the early-return case.

Fix recommendation: include `sty` in invalidation for data-dependent supplementary
outputs and explicitly clear or replace those outputs at run start / no-table
returns. Invalidation alone can leave obsolete text present but stale, so test
the displayed content as well as stale flags. Keep option-bound visibility; do
not hide failed output as a substitute for explaining it.

## Integration and robustness issues

### 2. [P2] Unsupported R columns can alter the cohort while disappearing from the table

Evidence: `R/tableone.b.R:194`, `:204`, `:238`–`:259`, `:859`–`:875`.

The t1 path delegates class handling to `CreateTableOne()` after listwise deletion
and after constructing the report. With `Age = 1:40` and a Date-class `VisitDate`
missing in rows 1–10, `excl = TRUE` yields N=30 and Age mean 25.50. The upstream
engine then drops VisitDate with only an R warning. The report says both Age and
VisitDate were included. Age alone would have N=40 and mean 20.50.

This violates the schema's promise that omitted variables do not enter exclusion
or supplementary reporting. This is an observed **R-interface** edge case; native
jamovi Date handling was not exercised and should not be assumed identical.

Fix recommendation: validate/normalize supported classes before determining the
analysis cohort, or reject unsupported selected classes with an actionable
message. If omitting them, report omissions inline and use the same retained
variable set for exclusion, statistics, summary, and manuscript text. Capture
user-relevant upstream warnings rather than letting them remain console-only.

### 3. [P2] Public documentation describes analyses and examples that do not exist

Evidence: `vignettes/explorationt-tableone-comprehensive.Rmd:27`, `:32`, `:35`,
`:45`; `vignettes/tableone-documentation.md:101`;
`vignettes/testing_tableone.md:24`–`:26`.

The comprehensive vignette claims multi-variable stratification and high-resolution
plots, loads the wrong package name (`ClinicoPathJamoviModule`), and uses a
nonexistent `tableone_test_data` dataset. Its chunks are globally non-evaluating.
The developer guide also claims interactive plots. The testing checklist signs
off zero failures/warnings and completed internationalization despite an existing
failing assertion and many untranslated backend sentences.

Fix recommendation: replace template claims with overall-only descriptive scope,
use `ClinicoPath` and a real fixture, make examples runnable, and replace unchecked
sign-offs with measured results. Regenerate `.h.R`/`.Rd` through the normal tools
afterward: the current header's generated prose is older than `.a.yaml`, though
its executable definitions match fresh compilation.

### 4. [P2] Arsenal's generated HTML bypasses the named-entity release gate

Evidence: `R/tableone.b.R:362`–`:374`.

The raw t3 HTML contains `&nbsp;&nbsp;&nbsp;` before statistic/category labels.
`results$tablestyle3$asString()` retains these literal entities and some table
markup on this runtime. Source-only entity scans pass because the entities are
emitted by arsenal rather than written literally in this backend.

Fix recommendation: normalize nonstructural entities at the HTML-output boundary
and add tests of generated output and text conversion. Verify copy/export in
jamovi; this audit did not claim a Word/PDF export test. Do not globally unescape
user labels or undo the existing HTML-injection protections.

### 5. [P2] The advertised no-variable R entry path fails before onboarding

Evidence: `jamovi/tableone.a.yaml:40`–`:45`; `R/tableone.b.R:97`.

`tableone(data.frame(x = 1:40))`, explicit `vars = NULL`, and `vars = character()`
raise `invalid 'row.names' length` during jmvcore initialization on the audited
runtime. The null-selection message is therefore not reached through that R
entry point. Empty data **with a selected column** does reach the expected
No Data Available panel.

Fix recommendation: trace the empty-selection initialization contract in the
supported jmvcore runtime and ensure the advertised wrapper behavior works before
claiming it is fixed. Add a positive assertion for the intended onboarding
message; do not merely permit either output or an error. GUI onboarding remains
unverified, and this should not be described as a proven GUI failure.

### 6. [P2] One edge test contradicts the intentional janitor exclusion policy

Evidence: `tests/testthat/test-tableone-edge-cases.R:512`–`:520`.

The fixture makes Age missing in rows 1–5 and Sex missing in rows 6–10, then
requires “No cases left” for every style. That is correct for t1–t3. For t4, Age
is omitted before exclusion, leaving the five recorded Sex cases to tabulate.
The failing assertion therefore tests an obsolete contract.

Fix recommendation: assert N=5 and the Age omission explanation for t4, preserving
the no-complete-cases expectations for t1–t3. Keep the current backend policy.

## Code quality and notice improvements

- High-severity sample/missingness warnings are all placed in the final panel
  using the same amber styling. Put serious conditions before copy-ready text and
  the corresponding table; preserve readable, theme-safe HTML.
- Most report/summary/error prose remains English-only. Some newer `.()` calls
  wrap fragments or use `sprintf()` instead of the house placeholder convention.
  The TODO at `R/tableone.b.R:76` incorrectly says there is no `.()` wrapping at all.
- `.run()` spans approximately 460 lines. Separate eligibility, cohort selection,
  engine execution, and output population to keep the same data contract across
  all branches.
- The low-cardinality numeric recommendation at `R/tableone.b.R:777`–`:779`
  always says t1 reports mean (SD), even when `nonnormal = TRUE`; condition the
  wording on the active summary method.
- The janitor percentage-formatting fallback at `:424`–`:429` silently returns
  unformatted fractions if formatting fails. No failure was induced, so this is
  a defensive-code concern, not a demonstrated wrong result.
- All-missing/no-table early returns can compute hidden About content. This is
  minor wasted work, not a permanently invisible result.
- Manuscript prose assumes rows represent patients. The schema/About panel warn
  about repeated records, but no unique-patient validation occurs; that limitation
  should remain explicit, especially with weighted/repeated-row datasets.

## Strengths and placeholder assessment

- Data used: **yes**. Options affect actual computations/visibility: **yes**.
  Results are constant across different data: **no**. Classification: **FUNCTIONAL**.
- No statistical-placeholder methods, simulated production outputs, or fabricated
  p-values were found. There are deliberately no p-values, CIs, or SMDs.
- t1 discloses missing percentages and labels their unit. t4 distinguishes all-case
  Percent from recorded-case Valid Percent. Listwise exclusion has a consistent
  denominator on supported, included variables.
- All-missing columns are removed before exclusion across styles with an explicit
  message. Janitor's class/category checks also occur before exclusion.
- Direct fixture checks found no arithmetic discrepancy: mean 14.5, SD 20.85665,
  quartiles 5.75/10.5/15.25, binary frequency 8/18 = 44.4%, and complete-case N=18.
- HTML-like names and levels did not appear as unescaped injected markup in the
  sampled HTML outputs. Arsenal intentionally escapes labels before rendering;
  gtsummary avoids double escaping; janitor uses `escape = TRUE`.
- Source generation preserves braces, backticks, and quotes in variable names.
  Parse/extraction checks passed; execution of the literal namespace-qualified
  call requires the installed ClinicoPath package and was not claimed.
- Warm timing on 1,000 rows / two variables: t1 0.011 s, t2 0.270 s, t3 0.012 s,
  t4 0.015 s. These are scoped smoke timings, not large-data benchmarks.

## External documentation comparison

No `cran_pkg`, `github_repo`, or `check_external=true` was supplied. A targeted
primary-source spot-check was nevertheless used to verify the statistical and
API statements below. This is not a complete upstream signature/NEWS audit, and
this multi-engine wrapper is not expected to expose every upstream argument.

| Aspect | Local behavior | Upstream comparison | Status / action |
| --- | --- | --- | --- |
| Scope/signature | Eight wrapper arguments; no grouping | Upstream tableone also offers strata, tests, and SMDs | Intentional restriction; correct in schema/About, contradicted by vignette |
| t1 continuous/categorical summaries | Mean/SD or optional median/quartiles; binary second-level display | Agrees with tested tableone behavior | Pass on supported fixtures |
| t2 defaults | `tbl_summary(data)` → `as_kable_extra()` | Median/Q1/Q3 and categorical N/% defaults documented | Pass; low-cardinality auto-classification documented locally |
| t3 defaults | Mean/SD and range, no grouping | Arsenal control defaults include `Nmiss`, `meansd`, `range` | Pass; generated text conversion issue remains |
| t4 missingness | NA row, Percent and Valid Percent | Janitor documents separate missing/all-case versus valid denominators | Pass on checked fixtures |
| Upstream versions | Runtime gtsummary 2.5.1 | Official site documents 2.6.0, released 2026-08-25 | Revalidate with intended release dependency; no compatibility failure inferred |
| Examples | Existing comprehensive vignette is non-running and misnamed | Primary engines provide executable examples | Correct local examples; upstream examples requiring stratification are outside scope |

Primary sources: [tableone project](https://kaz-yos.github.io/tableone/),
[gtsummary tbl_summary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html),
[arsenal controls](https://mayoverse.github.io/arsenal/reference/tableby.control.html),
[janitor tabyl documentation](https://sfirke.github.io/janitor/articles/tabyls.html).
The [gtsummary changelog](https://www.danieldsjoberg.com/gtsummary/news/index.html)
describes the 2.6.0 release as improving performance without changing returned
tables; it was not installed or runtime-tested in this audit.

## Actionable fixes — proposed only

Immediate schema direction for the three data-dependent supplementary panels:

```yaml
# reportSentence, summary, assumptions
clearWith:
  - vars
  - excl
  - sty
```

Backend direction, coordinated with those bindings:

```r
# At the start of a new run, or explicitly on every no-table return:
self$results$summary$setContent("")
self$results$reportSentence$setContent("")
self$results$assumptions$setContent("")
# Then validate engine-supported variables BEFORE listwise deletion;
# populate reports only for the cohort and variables actually tabulated.
```

Use an actionable theme-safe Html message or translated `jmvcore::reject()` for
unsupported inputs. Do **not** add `type: Notice` / `type: Notification` to the
result schema or blindly insert native Notice objects, given the repository's
documented compiler/runtime constraints.

Update the vignette and stale test as separate concerns; regenerate generated
help through normal build tools; rerun the installed-package tests and GUI
copy/export checks before release. No repair was applied by this audit.

## Differential harness and remaining checklist

The diagnostic harness uses real generated classes:

```r
source("R/tableone.h.R")
source("R/tableone.b.R")
run_options <- function(data, options) {
  opt <- do.call(tableoneOptions$new, options)
  analysis <- tableoneClass$new(options = opt, data = data)
  analysis$run()
  analysis
}
# Snapshot each declared result's visible/content fields, compare defaults
# with one changed option, then repeat using real protobuf restoration.
```

- [x] Every option exercised; all four table styles populated.
- [x] Means, SDs, quartiles, percentages, and exclusion checked numerically.
- [x] All-missing, empty, one-row, constant, logical, unusual-name and HTML fixtures.
- [x] Result-tree protobuf restoration and serialization, not just fresh runs.
- [x] Scoped compiler/UI and rendering-contract checks.
- [x] Primary-source statistical/API spot-checks; current dependency version noted.
- [ ] Fix stale supplementary output on no-table style transitions and add a regression.
- [ ] Define supported Date/other R class behavior before exclusion/reporting.
- [ ] Resolve NULL-variable R onboarding on the supported runtime.
- [ ] Correct documentation, generated help, and the obsolete janitor assertion.
- [ ] Test generated HTML/text and actual jamovi copy/export, including dark theme.
- [ ] Run installed-package integration tests, full build/check, and `.omv` reopen.
- [ ] Validate against the dependency versions intended for release.

## Readiness assessment

| Area | Assessment |
| --- | --- |
| Four-file integration | Mostly sound; supplementary invalidation incomplete |
| Statistical calculations | Correct on audited supported-input fixtures |
| Error/notice handling | Partial; unsupported-column warnings and positioning need work |
| User experience/documentation | Needs work; stale copy-ready text is the main risk |
| Placeholder status | Functional implementation |
| Production/release ready | **NO — address the findings and complete installed/GUI verification** |

The numerical checks support use as an overall descriptive table on validated,
supported inputs. They do not justify an unconditional clinical-release sign-off
while stale manuscript text and inconsistent omission/cohort handling remain.
