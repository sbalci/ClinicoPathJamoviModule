# Agreement Method Guide: validation record

Validated on 12 October 2026. `agreementintro` version **0.0.1** remains under
**meddecideT → Agreement → Method Guide**. This is a tested development candidate;
it does not certify the release or clinical validity of the analyses it recommends.

See the [method guide and decision diagrams](meddecide-agreement-method-guide.md).

## Executed checks

| Check | Result |
|---|---|
| Decision rules, data inspection, schema catalogue, native results and lifecycle | 16 tests, 593 assertions; no failures, errors or skips |
| Recipes executed in existing target backends | 9 tests, 72 assertions; no failures, errors or skips |
| Isolated guide package, `R CMD check --no-manual --no-build-vignettes` | 0 errors, 0 warnings, 0 notes |
| Isolated release gate | 0 blocking findings; one advisory about 13 existing shared references outside this guide lacking URLs |
| Repository results-rendering contract | Passed |
| Compiler and real jamovi options-panel harness | Passed; no placeholder or panel errors |
| Generated R help and isolated pkgdown reference site | Built successfully |
| Five Mermaid diagrams | Rendered successfully; stable decision identifiers checked against the rules |
| English/Turkish catalogue | 305 guide strings; complete translations and matching placeholders; the `meddecideT` brand is preserved |
| Turkish installed R engine | Translated explanations, recommendation labels and newly added specialist messages verified |
| Light/dark HTML rendering | Inherited foreground colors checked; Turkish text visually inspected |
| Native desktop | Menu placement, empty dataset, changed/unknown answers and optional inspection checked |
| Native `.omv` and PDF | Workbook saved, closed and reopened; settings and recommendation retained; two-page PDF inspected |

The target fixtures verify recipe meaning: Cohen, weighted Cohen, Conger, Fleiss,
Krippendorff alpha, Gwet AC2, all six supported ICC settings in both target interfaces,
CCC, paired kappa and exact agreement, Kendall W, marginal comparison, planning inputs,
reader comparator/cutoff/cluster preparation, fixed-reader modality matching and descriptive
multifocal concordance. This is a configuration contract check, not an exhaustive statistical
audit of those existing analyses.

Data fixtures cover wide/long equivalence, incomplete ratings, exchangeable reader identities,
patient-specific focus labels, declared and conflicting ordinal levels, numeric category codes,
duplicate keys, representable large numeric IDs, patient clusters, and consensus versus disease
references. The guide never changes the input frame or the random-number state.

Reused answer/data/selection/visibility states are compared with fresh analyses. Native testing
also caught two issues beyond ordinary result-content checks: dynamic table row-name caches
survived deletion, and empty HTML strings left previous iframe explanations visible. Both are
corrected and covered by regression checks; the latter fix was verified again in the desktop.
The single-page erratum reference uses a string page number for native protobuf serialization.

## Environment and limits

Checks used R 4.6.0, standalone jmvcore 2.7.38, the jamovi 28.3 compiler and jamovi 28.3.0
on macOS. The module's declared `minApp` stays at **2.7.27**; that older desktop version was
not separately exercised. The complete multi-analysis module was not subjected to a full
`R CMD check`; the new component was checked in isolation and the target fixtures ran against
the working source module. Loading that module emitted existing TMB-version and X11-display
warnings, without failing these tests.

English desktop rendering and Turkish installed-engine output were verified separately.
Switching the whole desktop to Turkish requires restarting jamovi; the original language
preference was restored without restarting the user's other open workbook. Dark/light checks
rendered the guide's HTML explanations, not a complete operating-system dark-mode session.

The native candidate used the isolated package namespace `AgreementGuideCheck`, with the same
guide sources and menu settings. The root generated header retains the `ClinicoPath` namespace.
No target statistical backend or production routing was changed for this feature. No commit,
push or production promotion was performed.

## Reproducing the focused source checks

From the repository root, with the module's development dependencies installed:

```r
pkgload::load_all(".")
testthat::test_file("tests/testthat/test-agreementintro.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-agreementintro-targets.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-zzz-results-rendering-contract.R",
                    stop_on_failure = TRUE)
```

The serialization test requires `RProtoBuf`. On this machine the jamovi base-module R library
was appended to `.libPaths()` to supply it. Without it, that test explicitly skips.
Source catalogue/documentation checks also explicitly skip when source files are absent from
an installed R package.

The compiler-generated header and help file were compared with the isolated component output.
The validation artifact directory is `/private/tmp/agreementintro-ejl_h0ys`; it contains the
rollback backup, source hashes, build/check logs, isolated `.jmo`, rendered diagrams,
`guide-native.omv`, `guide-native-final.pdf`, and light/dark HTML previews. These local artifacts
are not part of a published module.
