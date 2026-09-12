# pathagreement — standard check and limitations follow-up

Completed 2026-09-11 using `.claude/commands/check-function.md` with the default
standard profile. Existing edits were preserved. The analysis was already routed
to `OncoPathT`; checkbox defaults and UI grouping were retained as specified by
this profile.

## Summary

- **Arguments:** 55/55 wired (the data argument and 54 backend options).
- **UI:** every non-data option has a control; no unknown controls.
- **Outputs:** all 50 top-level items have an implementation: 26 tables,
  13 images, 10 HTML items, and one group containing the frequency table.
  Outputs requiring a reference standard, metadata, or discordant cases remain
  conditional on those inputs. No missing setters remain in the checked paths.
- **Variable safety:** exact column indexing preserves spaces, punctuation,
  backticks and Unicode without renaming or constructing formulas. Tests verify
  original rater names, category labels, factor order and variable label attributes.
  The name-cleaning step used by `oddsratio` is unnecessary here.

## Fixes and option effects

| Input or option | Previous behavior | Verified behavior |
|---|---|---|
| `showStatisticalGlossary` | Empty with selected raters | Glossary is populated during analysis |
| `caseID`, incomplete rows | Some tables used renumbered positions or unfiltered IDs | Case outputs use retained original IDs, with positional fallbacks |
| Arbitrary data-frame row names | Reference lookup coerced row names to integers | Retained row positions are tracked explicitly |
| `referenceStandard`, `categoryAnalysis` | Missing reference values aborted category analysis | Reference metrics use observed reference ratings; interrater results retain other complete cases |
| `referenceStandard`, clustering | Excluded rating rows caused reference comparisons to disappear | Group comparisons use the aligned, observed reference subset |
| `styleDistanceMetric`, `autoSelectGroups` | Two clustering paths produced different groups and distances | Tables and plots share one fitted clustering |
| Correlation distance, constant rater | Undefined distances could abort clustering | A visible message explains the fallback to percentage disagreement |
| Correlation and Euclidean distance | Absolute correlation treated reversed raters as identical, and unordered factors were assigned arbitrary numbers | Correlation uses one minus signed Spearman correlation; numeric distances require a shared ordered scale and otherwise fall back with an explanatory warning |
| Ward clustering | The requested `ward` method was passed to the legacy Ward implementation | Ward now uses `ward.D2`, with disagreement and correlation dissimilarities transformed to their Euclidean form |
| `heatmapTheme = cividis` | Used the red/yellow/green fallback | Uses the requested cividis palette |
| Clustering heatmaps | Obsolete state field and invalid `pheatmap` arguments caused errors; labels were hardcoded | Correct fields/API arguments and actual dataset categories are used |
| Named raters in dendrograms | Names were coerced to numbers; label colors failed | Labels are matched by name and group annotations follow leaf order |
| Primary method and dependent options | Summary labels and some invalidation lists omitted dependencies | Overview reflects the selected method; affected `clearWith` lists are updated |
| Option-determined table rows | Rows were created during `.run()` | Fixed rows are initialized in `.init()` and populated with `setRow()` |
| Plot persistence | Renderers depended only on transient private fields | Enabled plots store their data and clustering state; all 13 render after a protobuf save/load cycle with source data removed |
| Reference-comparison CIs | Between-rater dispersion was labeled as a confidence interval despite the raters sharing cases | Optional percentile intervals resample complete cases, keeping each case's raters and reference rating together |
| Long-operation progress option | `showProgressIndicators` emitted R console messages and did not create visible jamovi progress | The ineffective option and console messages were removed; actual data or method warnings remain in the HTML warnings panel |
| Plot lifecycle warnings | Deprecated `geom_errorbarh`, line `size`, and test `context()` calls emitted warnings | Current ggplot2 `orientation`/`linewidth` APIs are used and the obsolete test context call was removed |
| Reference serialization | A numeric article number prevented complete analysis protobuf serialization | Reference pages are stored as text, allowing saved results to serialize and reload |

Derived private data, including clustering results, are cleared before validation.
Variable label attributes are preserved during complete-case filtering. Option
documentation now describes retained case IDs, missing reference handling, and
the kappa values actually displayed in the heatmap.

## Validation

| Check | Result |
|---|---|
| Existing analysis tests | 36 tests, 45 assertions passed |
| Existing refinement tests | 4 tests, 7 assertions passed |
| Existing statistical regressions | 25 tests, 49 assertions passed |
| New wiring/rendering regressions | 8 tests, 37 assertions passed |
| New lifecycle/statistical regressions | 5 tests, 94 assertions passed |
| Total focused tests | **78 tests, 232 assertions; zero failures, errors or skips** |
| All declared image renderers | 13/13 rendered successfully; the category heatmap was visually inspected |
| Saved-results lifecycle | All 13 plots rendered after a protobuf save/load cycle with source data removed |
| Module results-rendering contract | Passed, 6 assertions |
| Image-state guard scan | No unguarded reads found |
| HTML background scan | No findings |
| Runtime namespace dependencies | All referenced packages are declared in Imports |
| `jmvtools::prepare()` | Passed on an isolated full-module copy using installed jamovi |
| `devtools::document()` | Passed on the isolated analysis and the temporary full-module copy; focused tests reran against the generated header and loaded namespace |
| `git diff --check` | Passed |

The initial compiler failure came from jamovi startup: `ELECTRON_RUN_AS_NODE`
made its executable report the Node version, and the sandbox prevented normal
startup. Unsetting that variable and running the installed app outside the
sandbox allowed compilation to complete. Only target generated files were copied
back; `.h.R` and `.Rd` files were not edited manually.

## Remaining scope and statistical caveats

This was the standard check, not an external statistical audit or release signoff.
No external package comparison, full package check, or interactive desktop
click-through was performed.

- Reference-comparison intervals are ordinary percentile bootstrap intervals,
  conditional on the observed raters and the style groups selected from the
  original data. They do not quantify uncertainty from selecting groups or
  sampling a new rater population; the result note states this limitation.
- Correlation and Euclidean clustering require ordered factors with identical
  category order. The analysis falls back to percentage disagreement and tells
  the user when that requirement is not met.
- Save/reopen behavior was exercised through jmvcore's real protobuf state path
  and direct render calls. A manual jamovi desktop `.omv` click-through remains
  useful as release smoke testing, but is no longer the only evidence for state
  restoration.
