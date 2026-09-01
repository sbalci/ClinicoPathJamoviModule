# CODE REVIEW: tableone

Follow-up: the three findings below were repaired and verified in the
[review-fixes report](tableone-review-fixes-2026-08-31.md). This document preserves
the original review evidence, not the current open-defect status.

Date: 2026-08-31. Review of the current working-tree implementation, including
the preceding audit repairs. This is a review-only pass: no analysis, schema,
generated binding, help, or regression-test files were changed in this pass.
The earlier [repair report](tableone-fixes-2026-08-31.md) records completed work;
the new findings below are additional, reproducible edge cases.

| Dimension | Assessment |
| --- | --- |
| Overall quality | 3/5 |
| Maintainability | MEDIUM — orchestration is separated, but HTML and engine-specific policy remain substantial |
| Performance | GOOD for the measured two-variable datasets; unused factor levels escape the frequency-table size guard |
| User experience | NEEDS_WORK — strong onboarding, but one contradictory report and incomplete localization |
| Mathematical/statistical correctness | MINOR_ISSUES — ordinary-input regressions pass; edge-case input semantics can change denominators or truncate data |
| Clinical and release readiness | NOT_READY — resolve the denominator mismatch and silent truncation before release |
| CRAN compliance, scoped hygiene only | CLEAN — no real findings in the specified checks; not a package-wide CRAN certification |
| Static analysis | CLEAN for the selected correctness linters; 11 nonblocking style advisories |

## Critical issues

### 1. [P1] Explicit NA factor levels disagree with the complete-case report

Locations: `R/tableone.b.R:31`, `:233`, `:238`, `:401`, `:772`.

A factor made with `exclude = NULL` can contain an actual NA level whose entries
are not considered missing by `is.na()` or `complete.cases()`. Cohort selection
therefore retains these entries. In the arsenal path, assigning escaped levels
with `levels(value) <- ...` removes that NA level and changes its entries into
ordinary missing values, after exclusion has already occurred. Independently,
arsenal's handling of the unmodified NA level also excludes it from the displayed
category percentages, so merely removing the escaping assignment is not a
sufficient fix.

Reproduction:

```r
d <- data.frame(
  x = 1:40,
  g = factor(c(rep("A", 20), rep("B", 10), rep(NA, 10)), exclude = NULL)
)
d$x[1] <- NA
ClinicoPath::tableone(
  d, c("x", "g"), sty = "t3", excl = TRUE,
  showSummary = TRUE, showReportSentence = TRUE
)
```

Observed output:

- Table header: N = 39; factor row: N-Miss = 10.
- Category A: 19 (65.5%), using 29 recorded factor values.
- Summary claims every displayed variable uses the same complete-case denominator.
- Copy-ready sentence describes 39 cases with complete data for every listed variable.

This is a material reporting contradiction, not a rounding difference. Under a
policy treating the NA level as missing, normalizing it before exclusion yields
29 complete cases. Alternatively, an explicit, visible category could be retained
consistently, or the input could be rejected with actionable guidance. The policy
must be settled before cohort calculation and shared by all engines and reports.
Literal text categories such as `"Unknown"` or `"NA"` must not be silently recoded.

Cross-engine probes show t1/t4 retain this NA level as a category, while t2 rejects
it. Add tests for retained/excluded NA-level factors, every style, quality text,
copy-ready text, and serialized output. Preserve HTML-escaping coverage.

### 2. [P2] Matrix-valued R columns are silently reduced to one subcolumn

Locations: `R/tableone.b.R:157` and `:989`.

The dimensionality guard is applied to `self$data` after `super$init()` invokes
framework selection. For a data-frame matrix column, `jmvcore::select()` expands
the matrix, and the original column name ends up referring to its first component.
The later guard sees an ordinary vector and cannot detect the original shape.

```r
d <- data.frame(x = 1:40)
d$m <- matrix(c(1:40, 1001:1040), nrow = 40)
ClinicoPath::tableone(d, c("x", "m"), showReportSentence = TRUE)
```

Observed output summarizes `m` as 20.50 (11.69), entirely from its first component,
without an unsupported-type notice. The second component is not summarized, and
the report still lists the original `m` variable. The input is unsupported, so it
should not produce a plausible but partial summary. This reproduction concerns
the exported R interface; ordinary jamovi scalar columns were not shown to fail.

Validate the original selected R columns before framework coercion, then either
reject matrix/array/list inputs or omit them with an explicit notice. Cover
matrix-only and mixed selections, single-column matrices, arrays, list columns,
and preservation of the existing empty-selection workaround.

### 3. [P2] Unused factor levels bypass the frequency-table output-size guard

Locations: `R/tableone.b.R:195` and `:465`.

Eligibility counts observed categories, but the later `janitor::tabyl()` call
retains unused factor levels by default. Thus the 20-recorded-category check does
not bound the size of the rendered table.

```r
d <- data.frame(g = factor(
  rep(c("L1", "L2"), 20), levels = paste0("L", 1:1000)
))
ClinicoPath::tableone(d, "g", sty = "t4")
```

This 40-row input has two recorded categories but produces 1,000 category rows,
plus a header and total: 1,002 HTML rows and 148,680 output characters. It is not
omitted or warned about. No GUI crash was measured; the demonstrated issue is
unbounded output relative to the intended small frequency-table guard.

Choose and document whether zero-count levels should appear. For observed-only
tables, pass `show_missing_levels = FALSE`. If retaining them is intentional,
validate the number of rendered levels and provide a visible limit or warning.
This is an upstream default, confirmed in the
[janitor tabyl documentation](https://sfirke.github.io/janitor/reference/tabyl.html),
not an error in its frequency calculations.

## Strengths

1. The run pipeline clears stale results, resolves variable eligibility before
   listwise deletion, and publishes supplementary output only after successful
   rendering (`R/tableone.b.R:68`). Prior unsupported-Date and style-transition
   repairs remain covered by passing regressions.
2. Missingness/exclusion guidance uses unrounded counts; heuristic thresholds are
   distinguished from clinical cutoffs. Small samples and case loss are explained
   without claiming a statistical assumptions test.
3. Summary, About, and report text are opt-in. Copy-ready text explicitly says
   counts are not verified unique patients. The menu remains `ExplorationT`.
4. User labels are escaped, exported syntax is executable, and output restoration
   has actual protobuf coverage. The runtime-compatible HTML notice approach is
   appropriate; native Notice objects should not be introduced merely for style.
5. The documented scope is an overall descriptive table, not stratification,
   hypothesis testing, imputation, confidence intervals, or patient-level deduplication.

## Statistical and clinical assessment

For ordinary supported scalar columns, the existing regression suite passed.
Differences between engines are intentional: t1 ordinarily uses mean/SD and can
switch numeric variables to median/IQR; gtsummary applies its own variable-type
and summary defaults; arsenal supplies its own descriptive layout; janitor
tabulates categorical frequencies. These differences should not be described as
interchangeable cosmetic themes. See the primary
[tableone documentation](https://kaz-yos.github.io/tableone/) and
[gtsummary documentation](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).

Additional observations:

- An all-FALSE logical variable in t2 displays `flag 0 (0%)`, meaning the TRUE
  count. This is an upstream dichotomous default, not incorrect arithmetic, but
  the target level should be explicit in the label or educational text.
- Integer frequency weights were exercised: two source rows with weights 1 and
  9 produce the expected expanded N = 10, mean 9.1, and sample SD about 2.85 in
  the applicable engines. This is not evidence of survey-design support. Explain
  frequency-weighted counts where relevant rather than suggesting unique cases.
- Framework validation rejects an infinite numeric measurement even in t4, where
  the analysis would otherwise omit numeric measurements. The visible error is
  fail-safe, but eligibility and validation order deserve a regression case when
  the pre-initialization validation boundary is repaired.
- Do not add Fisher tests, effect sizes, clinical decision thresholds, or a
  mandatory guided wizard to repair an overall descriptive-only analysis.

## Code-hygiene findings

Real findings in the requested scoped checks: **0**.

- No hard-coded random seed, package-source `library()`/`require()`, persistent
  `par()`/`options()` change, or raw non-ASCII source was found.
- Namespaced non-base dependencies are declared. Generated public/class help
  includes return-value documentation.
- All six analysis references resolve to shared metadata with author/year;
  no orphan or option-name reference was found. This was validation only, not
  a reference update.
- Four `<<-` matches at lines 339, 458, 494, and 498 are intentional writes from
  nested error handlers/closures into the enclosing rendering frame. They are
  not writes into `.GlobalEnv`; changing them mechanically would break behavior.
- `import(jmvcore)` supplies `.()`; absence of a separate `importFrom` entry for
  that symbol is not a missing-import defect.

Package-wide checktor, package size, NEWS, DESCRIPTION-wide policy, site links,
and other module-wide CRAN categories were outside this single-function review.

## Static analysis and manual R6 checks

No correctness findings from the selected sequence, NA-comparison, sprintf,
unreachable-code, duplicate/missing-argument, or T/F-symbol linters.

There were 11 nonblocking style advisories: five brace-placement, four quote,
one pipe-consistency, and one semicolon advisory. They do not establish runtime
defects and do not justify a broad formatting rewrite.

The R6 bodies were also checked manually for unresolved local names and scalar
`if()`/`while()` conditions using vector operators. No additional defect was
identified. `object_usage_linter` and `vector_logic_linter` cannot establish this
inside the `R6Class()` call; their lack of output was not counted as proof.

The lint environment emitted dependency-loading/display noise; the installed
analysis regression suite itself reported no test warnings.

## Improvement opportunities and specific recommendations

- Architecture: centralize pre-engine input semantics and enforce a contract
  that presentation escaping cannot change missingness, row count, factor
  membership, or denominator. Check original storage shape before framework
  selection, not only after it.
- Statistical reporting: derive complete-case statements from the same canonical
  data used for each table. A contradictory success sentence must be withheld if
  an engine cannot honor the selected missingness policy.
- Performance: bound actual frequency-table output and add a high-unused-level
  regression; no broad optimization is justified by the ordinary-input timings.
- Localization: finish the English-only welcome/About/summary/error prose and
  plural handling; use complete translatable messages with named placeholders.
  Translation hooks already added are useful but are not complete TR/EN support.
- Clinician-facing guidance: retain optional explanation panels and add a short
  example explaining category-specific denominators and which dichotomous level
  is counted. A green data-quality panel should never imply clinical validity.

## Verification and limitations

The real backend and generated header matched the previously installed isolated
ClinicoPath fixture byte-for-byte. Tests used that package, not mocks of the
engine calculations and not a replacement of the user's installed module.

| Verification rerun in this review | Result |
| --- | --- |
| Seven tableone test files | 353 assertions passed; 0 failures/errors/warnings/skips |
| Scoped rendering contracts | 6 assertions passed |
| New diagnostic assertions | 15 passed, confirming the three defects above; these intentionally assert current defective behavior |
| Reference/import/hygiene checks | No real scoped findings |
| Correctness linters and manual R6 scan | No additional findings |

Single-run, warm elapsed seconds with one numeric and one four-level factor:

| Rows | t1 | t2 | t3 | t4 |
| ---: | ---: | ---: | ---: | ---: |
| 1,000 | 0.025 | 0.386 | 0.015 | 0.022 |
| 10,000 | 0.021 | 0.329 | 0.024 | 0.031 |
| 100,000 | 0.124 | 0.406 | 0.085 | 0.047 |

t4 omits the numeric variable by design. These are smoke measurements, not
benchmarks of many-variable data, worst-case category counts, memory, or GUI
rendering. No resampling is performed, so a seed option is unnecessary.

Runtime: R 4.6.0; jmvcore 2.7.38; tableone 0.13.2; gtsummary 2.5.1;
arsenal 3.7.1; janitor 2.2.1; testthat 3.3.2.

The preceding repair pass compiled isolated schemas/UI, regenerated bindings and
help, and rendered the vignette; those results are recorded in its report, not
claimed as new executions here. A full-module check, actual jamovi light/dark and
clipboard inspection, `.omv` save/reopen, Turkish rendering, and validation with
the exact release dependencies remain outstanding.

Review scripts and detailed output are in
`/tmp/tableone-review-20260831-yeuufa`. That temporary directory is diagnostic
evidence, not a durable test-suite addition. Reproductions are preserved above.

## Action items and release decision

- [ ] Resolve explicit NA-factor-level semantics before cohort selection; test
  all four engines and every supplementary report with/without exclusion.
- [ ] Guard original R column shapes before initialization can flatten them.
- [ ] Align the janitor category policy with the number of rendered levels.
- [ ] Convert these reproductions into regressions asserting corrected behavior,
  then rerun the existing suite and serialization tests.
- [ ] Clarify dichotomous target levels and finish focused localization.
- [ ] Complete the actual jamovi and full-module release checks listed above.

**NOT_READY for release:** ordinary-data results and the preceding repairs are
well covered, but a supported factor representation can generate misleading
complete-case manuscript text, and an unsupported R column can silently yield
a partial summary. Correct those defects before relying on the affected paths
for clinical research reporting. No clinical decision-making validity is claimed.
