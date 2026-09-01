# Table One — audit repairs and verification

Date: 2026-08-31. Functional audit repairs implemented; release-environment
validation remains outstanding. This supersedes the open defect status in the
[pre-repair audit](tableone-full-audit-2026-08-31.md), without rewriting its evidence.

## Repairs

| Audit finding | Implementation and regression |
| --- | --- |
| Stale summary/report on a style-only no-table transition | Added style invalidation; clear all result bodies before early returns; publish supplementary output only after rendering succeeds. Tests restore actual protobuf results across styles and no-data/no-case paths. |
| Unsupported Date columns changing exclusion and report counts | Resolve supported classes before exclusion. Numeric/integer, factor/ordered, character and logical are supported. Date, date-time, duration and custom classes are named and omitted. Tests retain N=40 and Age mean=20.50 despite missing Date/duration values. |
| R onboarding fails when no variables are selected | Narrow public init guard works around jmvcore's empty-column selection bug, restoring the source frame afterwards. Omitted, NULL and empty selections now show instructions; empty data remain a no-data state. |
| Arsenal renderer entities/markup leak into text | Normalize input-only nonbreaking-space entities and cell tags at the output boundary. Preserve escaped user labels. Regression exercises generated HTML, text export and hostile-looking labels. |
| Misleading documentation | Replaced stratification/model/plot/imputation claims with actual scope, types, four engines, denominators and limitations. Vignette uses the real package and dataset and executes its examples. Regenerated public and class help. |
| Obsolete tests | t4 now expects five Sex cases after Age omission; t1–t3 still expect no complete cases. Old NULL-selection error expectations now assert onboarding. |

## Additional recommendations implemented

- Data-quality guidance precedes the table and report. Strong warnings and
  recommendations have distinct translucent, inherited-text styling.
- Missingness/exclusion decisions compare raw counts, not rounded percentages;
  boundary and just-over-boundary tests cover all four percentage thresholds.
  Notices explicitly call these descriptive heuristics, not validated clinical cutoffs.
- Split orchestration into variable eligibility, cohort selection, engine
  rendering and supplementary-report helpers.
- Low-cardinality numeric guidance reflects whether the t1 median option is
  enabled. The median control is enabled only for t1 in the compiled UI.
- Removed the silent janitor raw-fraction fallback. An induced formatter failure
  produces visible details and withholds supplementary reports for incomplete output.
- About content is populated once per run, only when requested.
- Copy-ready text counts cases and explicitly warns that rows are not verified
  unique patients or deduplicated records.
- Added translation hooks to omission, quality and manuscript messages; removed
  the inaccurate claim that the backend had no translation hooks. Full prose
  localization and completed locale catalogs are not claimed.

## Verification actually executed

An isolated temporary package named ClinicoPath contained the real tableone
backend, generated bindings, reference metadata and unchanged source datasets.
It was installed into a temporary library; no user's installed package was replaced.
This is an installed analysis/package integration check, not a full-module build.

| Test file | Assertions passed |
| --- | ---: |
| test-tableone-arguments.R | 66 |
| test-tableone-audit-fixes.R | 129 |
| test-tableone-basic.R | 31 |
| test-tableone-edge-cases.R | 52 |
| test-tableone-integration.R | 16 |
| test-tableone-release-review.R | 47 |
| test-tableone.R | 12 |
| **Total** | **353** |

Installed-suite result: **0 failures, 0 errors, 0 warnings, 0 skips**. New tests
include full analysis-response serialization with six references and execution
of exported R syntax for all four styles, as well as result-tree restoration.
The earlier source-only run passed 324 assertions and skipped the 12 installed
integration cases; the installed run above removes that coverage limitation.

Additional checks:

- Official jamovi analysis/results and UI compilation passed in an isolated
  module. The normal `jmvtools::prepare()` version probe could not access the
  desktop app. Compiler preparation succeeded using its documented
  `--assume-app-version 28.2.0` flag, matching the installed app's Info.plist.
  This does not establish that the desktop application itself ran successfully.
- Official compiler generated `R/tableone.h.R`; roxygen2 generated
  `man/tableone.Rd` and `man/tableoneClass.Rd`. Both Rd checks passed.
- Six tableone-scoped repository rendering-contract assertions passed.
- Sixteen independent numerical/engine/syntax assertions passed. Warm 1,000-row,
  two-variable timings were approximately t1 0.011 s, t2 0.282 s, t3 0.011 s,
  t4 0.014 s; these are smoke timings, not performance guarantees.
- The revised vignette rendered successfully with executable examples.
- Working and installed-fixture backend/header files matched byte-for-byte.
- Whitespace checks passed for hand-authored changes and regenerated help.
  The untouched compiler output contains four trailing spaces in generated
  roxygen comment lines; no manual edits were made to generated bindings.

Runtime: R 4.6.0; jmvcore 2.7.38; jmvtools 28.3; tableone 0.13.2;
gtsummary 2.5.1; arsenal 3.7.1; janitor 2.2.1; testthat 3.3.2; roxygen2 8.1.0.
RProtoBuf was loaded from the installed jamovi base library. The source harness
emitted the existing notice that jmvcore was built under R 4.6.1; the installed
test suite emitted no test warnings.

## Remaining release checks

- Full-module build/check with its complete dependency set.
- Actual jamovi UI light/dark inspection, clipboard/export, and `.omv` save/reopen.
  Protobuf restoration and text-export tests do not substitute for these checks.
- Validation against the exact dependency versions selected for release; no
  gtsummary upgrade was performed.
- Complete localization of remaining educational/summary/error prose and locale
  catalogs, with rendered translation checks.

## Preservation and recovery

The fix-function workflow kept timestamped pre-edit backups and isolated build
artifacts in `/tmp/tableone-fix-20260831-CPhFRe`. The temporary directory also
contains the installed-suite runner/results and rendered vignette. Unrelated
stagemigration edits, staged deletions and `.gitignore` changes were preserved.
No commits, dependency upgrades, application installations or external writes
were performed.
