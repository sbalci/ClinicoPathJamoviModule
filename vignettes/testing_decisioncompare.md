# Testing decisioncompare

## Automated coverage

The focused tests cover paired correctness, class-specific comparisons, available-case
alignment, explicit indeterminate exclusions, sparse/degenerate data, reference-level
coding, unusual variable names, prevalence adjustment, OPA intervals, method-dependent notes,
option reruns, plot state, narrative claims, and the bundled examples.

`test-decisioncompare-original-data.R` covers unused factor levels, the 10,000-cell
original-table limit, the optional third test, warning removal on reruns, and unchanged
diagnostic estimates, comparisons, plots and reports when original tables are omitted.

The sparse-Q suite independently enumerates conditional permutations and checks the exact
p-value, ties, conditional rejection probabilities, invariances, and the exact/asymptotic
boundary. Independent references in the broader suite include epiR, DescTools, binomial
formulas and Newcombe's paired-proportion interval.

```r
devtools::load_all()
source("tests/testthat/helper-decisioncompare.R")
files <- list.files("tests/testthat", pattern = "^test-decisioncompare.*[.]R$", full.names = TRUE)
files <- c(files, "tests/testthat/test-diagnostic-comparator.R",
           "tests/testthat/test-gaspath-live.R")
lapply(files, testthat::test_file)
```

`test-diagnostic-comparator.R` covers the shared consensus/method comparator helpers.
`test-gaspath-live.R` is shared too: besides decisioncompare it also exercises decision,
decisioncalculator, psychopdaROC and precisionrecall, so a failure there may belong to another analysis.

Latest focused release verification: 130 test blocks / 962 passing expectations, with the
corrected examples test rerun separately. No final focused test failures, errors, warnings or
skips. This is not a claim that package-wide R CMD check was run. Startup glmmTMB/TMB and
XQuartz environment warnings are separate from the test results.

Security-repair verification (2026-10-03): all 11 focused test files passed 1,005
expectations, including 43 new original-data display checks. Forty additional
adversarial runs and compiled Turkish warning lookup also passed.

## Release checklist

- [ ] Source options and UI controls mapped; no unused options found. Needs re-verification
  for the explicit-mapping release (1.1.0: reference type, negative levels, provenance options).
- [x] Headless options panel rendered in jamovi's real client with no panel errors.
- [x] Scoped state guards, theme checks and release checks passed.
- [x] Real bundled-jamovi protobuf roundtrip and restored plot rendering verified in the
  preceding repair pass; option replay matched a fresh computation.
- [x] Compiled Turkish lookup tested for the repaired glossary and method text.
- [ ] Regenerated the target header in isolation; verified source/runtime version 1.0.84,
  the Statistic heading, and original-data notice invalidation. Needs re-verification for
  the explicit-mapping release: the 1.1.0 wrapper API and documentation changed.
- [ ] Exercise actual app .omv save/reopen/export and GUI locale switching.
- [ ] Verify the intended submodule after the maintainer promotes the analysis from meddecideT.

The analysis assumes independent cases and an appropriate reference standard. It is not a
cluster-adjusted analysis or a validated treatment recommendation. Empty/invalid reruns should
retain the result structure and clear stale values; they must not hide the pane to signal failure.

Integration-repair verification (2026-10-03): 134 focused test blocks / 1,005 expectations
passed, with no test failures, errors, warnings or skips. Eighteen scoped checks passed.
Real bundled-jamovi protobuf replay confirmed that toggling Original data invalidates
its cached warning. Normal `jmvtools::prepare()` could not query the local app; the isolated
compiler used the installed app version 28.3.0, verified from its bundle metadata. This is
target compilation evidence, not a claim that a package-wide build or GUI test passed.
