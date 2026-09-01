# Table One — review fixes and recommendations implemented

Date: 2026-08-31. This records repairs to the additional findings in
[the review](tableone-review-2026-08-31.md), building on the
[preceding audit repairs](tableone-fixes-2026-08-31.md).

## Repaired findings

| Finding | Fix and evidence |
| --- | --- |
| P1: NA factor levels change the table denominator after exclusion | Canonicalize actual NA factor levels before eligibility, missingness calculation and exclusion. All four engines and supplementary outputs now agree with independently constructed canonical factors. The reviewed mixed-data example reports 29 complete cases for t1–t3; t4 correctly retains 30 because the numeric variable is omitted. Literal NA/Unknown categories remain unchanged. |
| P2: matrix-valued R columns silently truncate | Public initialization inspects original selected columns before framework selection. Matrix, array and list columns now produce an actionable error. Mixed and single selections, one-column matrices, arrays, nested/all-missing lists, noThrow initialization and untouched caller data are tested. Unselected unsupported columns do not block valid inputs or onboarding. |
| P2: unused factor levels bypass the frequency-table size guard | Janitor explicitly uses `show_missing_levels = FALSE`. A factor with 1,000 declared levels and two observed categories renders only the observed categories, an NA row when needed, and the total. More than 20 recorded categories still triggers the existing omission policy. |

Presentation escaping now preserves factor codes and missingness by construction;
the arsenal boundary also verifies that the missing-value mask is unchanged.
The existing HTML/syntax security regressions continue to pass.

## Recommendations implemented

- Dichotomous gtsummary rows name the counted level: `flag = TRUE`, `yes = yes`,
  or `code = 1`. The all-FALSE example now clearly means zero TRUE observations.
- About includes denominator examples, interpretation of zero dichotomous counts,
  the NA-level policy, supported input shapes, and frequency-weight semantics.
- Educational, summary, warning, error and copy-ready prose is marked for
  translation. Named placeholders replace positional formatting in dynamic
  messages. Substitution never rescans braces/backslashes in user values.
- Copy-ready text uses whole singular/plural sentence alternatives. The obsolete
  test expecting "1 selected variables" now expects the corrected wording.
- The official compiler extracted 144 scoped messages. Turkish translations were
  supplied, five existing translations preserved, and matching English/template
  entries merged without changing unrelated catalog entries semantically.
- Developer documentation, feature map, testing guide and executable vignette
  explain the revised behavior. Generated header/help were regenerated using
  the official compiler and roxygen2, not hand-edited.

The fix-function workflow supplied backups and staged validation. The
prepare-translation workflow guided complete-message extraction, Turkish
terminology, placeholder checks and a scoped catalog merge. See the
[translation plan](../i18n-plans/tableone-tr-translation-plan.md) and
[English/Turkish inventory](../i18n-plans/tableone-tr-messages.tsv).

## Verification actually executed

An isolated package named ClinicoPath contained the real current backend,
generated bindings, references, source datasets and compiler-generated locale
JSON. It was installed in a temporary library; the user's installed module was
not replaced. Working backend/header files match this fixture byte-for-byte.

| Test file | Assertions passed |
| --- | ---: |
| test-tableone-arguments.R | 66 |
| test-tableone-audit-fixes.R | 129 |
| test-tableone-basic.R | 31 |
| test-tableone-edge-cases.R | 52 |
| test-tableone-integration.R | 16 |
| test-tableone-release-review.R | 47 |
| test-tableone-review-fixes.R | 449 |
| test-tableone.R | 12 |
| **Total** | **802** |

Final installed-suite result: **0 failures, 0 errors, 0 warnings, 0 skips**.
The new tests include canonical NA-factor comparisons, hard-coded expected
cohort counts, hostile-looking labels, real protobuf style restoration, all
108 distinct backend translation messages and compiled Turkish output.
Initial localization-test harness errors were repaired before this final run.

Additional checks:

- Six scoped repository rendering-contract assertions passed.
- Official analysis/results and UI compilation passed in isolation with
  `--assume-app-version 28.2.0`, matching the installed app. The normal
  `jmvtools::prepare()` desktop version probe could not access jamovi.
- English/Turkish runtime catalogs were produced by the official compiler's
  locale builder. Catalog parsing and named-placeholder agreement passed.
- Both generated Rd checks passed; the updated vignette rendered with executing
  examples, including an explicit-NA-factor example.
- Scoped reference/import/ASCII checks passed. Six references resolve. No new
  correctness-linter finding; 11 nonblocking style advisories remain.
- Whitespace checks passed for hand-authored changes, catalogs and generated help.
  Existing compiler-generated trailing whitespace is not manually rewritten.
- Warm 100,000-row smoke timings with one numeric and one four-level factor were
  t1 0.128 s, t2 0.498 s, t3 0.094 s and t4 0.058 s. These are single-run local
  timings, not benchmark guarantees; t4 omits the numeric variable by design.

Runtime: R 4.6.0; jmvcore 2.7.38; jmvtools 28.3; tableone 0.13.2;
gtsummary 2.5.1; arsenal 3.7.1; janitor 2.2.1; testthat 3.3.2.

## Broader release validation

The repository declares no missing installed Imports/Depends in this environment.
A source tarball was built with vignettes/manual/resaving disabled. The build
passed metadata checks; it reported long-path portability warnings for unrelated
meddecide vignette assets. A subsequent tarball check was requested with tests,
manual and vignettes disabled. The completed broader check used
`R CMD check --no-codoc --no-examples --no-tests --no-manual --no-vignettes --no-build-vignettes`
and finished with **0 errors, 6 warnings and 3 notes**. Installation and namespace
loading passed. Warnings concern duplicate data objects, non-ASCII code in
`decisioncalculator.b.R`, undocumented objects (including `tableone_test`), an
undocumented `computeNRI()` argument, and unbuilt vignette outputs. Notes concern
long asset paths, unused imports, and unqualified simulation functions elsewhere
in the module. These do not constitute a clean full-module release check.
One preceding byte-compilation run was intentionally interrupted; its installation
error was caused by that interruption, not a diagnosed source defect. The later
completed check supersedes it. The attempted environment setting to disable byte
compilation was ignored by R; the completed check did perform byte compilation.
The direct unbuilt-directory check had stopped at a required Author field; the
proper tarball build generated metadata and passed that check, so this was not
treated as a Table One defect.

Still required before release:

- Actual jamovi light/dark, clipboard/export and `.omv` save/reopen checks.
  Protobuf and HTML tests do not replace desktop validation.
- Full module tests and vignettes against the chosen release dependencies.
- Native-speaker editorial review and desktop Turkish rendering. Upstream engine
  labels and raw engine error details may remain English.

Infinite numeric measurements still produce jmvcore's explicit validation error,
including when selected alongside categorical variables in t4. They are never
silently converted into finite measurements. Changing that framework-level policy
was not required to repair the three confirmed defects and remains a possible
usability refinement.

## Preservation and recovery

Backups and diagnostics are in `/tmp/tableone-review-fixes-20260831-ldeJp9`,
including the pre-edit reviewed backend, schemas, generated files and locale
catalogs, installed test results, build/check logs and rendered vignette.
The backend briefly changed externally during preparation and then returned
byte-for-byte to the reviewed baseline before patches were applied. No competing
version was overwritten. Unrelated workspace edits were preserved.

No commits, dependency upgrades, external localization publication or desktop
installation were performed. The confirmed Table One findings are repaired;
this is not a blanket full-module or clinical release sign-off.
