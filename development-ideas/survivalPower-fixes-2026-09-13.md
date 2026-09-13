# survivalPower repair report — 2026-09-13

The ten findings in the [full audit](survivalPower-check-full-2026-09-12.md) have been
addressed in the backend, option/UI/result schemas, generated help and regression tests.
The historical audit remains a record of the original defects. Repair evidence is stored
separately in [survivalPower-fixes-2026-09-13](survivalPower-fixes-2026-09-13/).

## Repairs

| Finding | Resulting behavior | Acceptance evidence |
|---|---|---|
| F1: requested power substituted for calculated power | Assessment reads calculated power in power mode and labels target power in planning modes. Low-power and sparse-event notices use the resolved design. Clinical relevance requires disease-specific justification. | The N=10 sparse design reports 5.003% power and “Insufficient” for requested powers 0.5, 0.8 and 0.99. |
| F2: inconsistent duration and event milestones | Tables, summaries and plots use solved follow-up and duration. Calendar event integration starts at recruitment time zero. The unexplained analysis phase is removed. | N=500 gives 45.5756 months throughout; N=1000 reaches half the required events at 15.3373 months, during accrual. |
| F3: assumed HR substituted for detectable HR | Effect-mode summaries, event totals and survival curves use the solved HR. Whole-study and comparison-effective event counts are distinguished. NI effect mode reports a maximum true-HR threshold. | Starting HRs 0.2 and 0.75 both yield detectable HR 0.603459 and 123.0646 expected events for N=200. |
| F4: fractional or infeasible cluster allocation | Enrollment rounds up to whole allocation blocks, with clusters in both arms at the requested ratio. Supplied sample sizes with incomplete clusters are rejected. Outputs identify the approximation and warn about few independent clusters. | The former 30-subject example now requires 200 subjects: one 100-subject cluster per arm. Ratios 0.5, 1, 1.5 and 2 produce integer allocations. |
| F5: simulation warnings, failures and boundary intervals | Event-free trials are non-rejections. Failed tests retain their actual events but have unknown rejection outcomes, excluded with an explicit conditional-power warning. Numerical warnings are summarized. Exact binomial intervals replace Wald intervals; inadequate validation cannot claim convergence. | Zero rejections in 1000 trials gives a 95% interval of [0, 0.003682]. A mixed fault-injection case separately records 250 event-free trials, 250 failed tests and 250 warnings, retaining all event counts. |
| F6: incorrect NI objectives and hazard/risk language | Narratives describe ruling out the NI margin, with assumed HR and one-sided alpha. NI effect-mode wording no longer describes a minimum detectable effect. The glossary defines instantaneous hazards separately from risk. | All four NI calculation modes check the margin-based objective; HR 0.75 is described as 25% lower hazard. |
| F7: mixed Cox calculations | Fixed Cox calculations explicitly use Lachin–Foulkes variances. Cox curves, sensitivity, inversions and simulation comparisons use the same selected power calculation. Interim sample sizes invert `gsSurvPower`; interim boundaries and conditional power read that resolved sequential design. | Unequal allocation and both HR directions agree at plotted design points. Two- and six-look O’Brien–Fleming/Pocock designs attain the requested 80% power after integer rounding. |
| F8: inconsistent effect limits | Backend validation applies HR bounds consistently to direct and converted effects. Signed survival and RMST differences are accepted when feasible. Survival differences use the analytical exponential conversion; RMST inversion handles floating-point boundary values. | Equivalent HR, median ratio, survival difference and RMST inputs yield matching sample sizes at HR 0.1, 0.15, 0.75, 1.5 and 5. A five-percentage-point survival difference is accepted. |
| F9: placeholder sensitivity modes | Sensitivity controls, rows and computation are limited to sample-size and power modes. Other modes receive an explanatory notice. Failed scenarios are explicitly described as not estimable. | Effect and duration modes produce no placeholder rows, sensitivity state or hidden sample-size plot. |
| F10: misleading R preset promise | The preset is documented as a jamovi UI worked example; R callers provide explicit numeric assumptions. Generated R help matches the schema. | Setting an R preset alone leaves numeric arguments unchanged; existing JavaScript preset tests continue to cover GUI event behavior. |

## Presentation and compatibility

Notices are deduplicated and sorted by severity, with a distinct strong-warning prefix.
Key new planning notices and objective sentences use translatable templates with named
values. Control labels use sentence case while preserving abbreviations. Broader translation
of legacy HTML text remains outside this repair; no complete localization is claimed.

The analysis now includes Schoenfeld (1983) and Rothmann et al. (2003) reference keys.
Rothmann is identified as background for margin selection, not as a claim to implement that
paper's full procedure. Stale package-version and narrative-year wording was removed.
The [Rothmann publication record](https://pubmed.ncbi.nlm.nih.gov/12520560/) supports the
added bibliographic metadata.

Cox interim calculations require an installed `gsDesign` exporting `gsSurvPower`.
Older installations receive an explicit error and instructions to update the dependency or
select a fixed design; there is no silent switch to another variance approximation.
The implementation includes both rejection tails for a two-sided test. The upstream
[fixed-survival documentation](https://keaven.github.io/gsDesign/reference/nSurv.html) and
[sequential power documentation](https://keaven.github.io/gsDesign/reference/gsSurvPower.html)
were checked against installed source. Because upstream sizing and power paths calibrate
their canonical references differently, interim enrollment is solved by inverting the
displayed power function instead of assuming the sizing call attains the same power.

The five plot renderers retain NULL-state guards. Power plots also accept older saved
states without the new target-power field. Plot-data errors reach the existing visible
notices. The plain-text notice collector remains compatible with jamovi serialization.

## Verification

All **904 assertions across 105 test blocks in 10 survivalPower test files passed**, with
no failures, errors, skipped assertions or test warnings. The six module rendering-contract
assertions also passed. All 24 outputs populated, eight references resolved, and all five
plot renderers passed valid and NULL-state checks. Older power-plot states rendered as well.
The five plot types and the changed duration/effect displays were visually inspected.

[Verification totals](survivalPower-fixes-2026-09-13/verification-summary.json),
[per-test results](survivalPower-fixes-2026-09-13/regression-results.csv),
[full regression log](survivalPower-fixes-2026-09-13/regression.log) and
[plot PDF](survivalPower-fixes-2026-09-13/rendered-plots.pdf) accompany the source fingerprints.
The expanded acceptance file covers independent event integration, upstream fixed-design
and sequential probabilities, numerical inversions, effect conversions and injected failures.

Preparation used `jmvtools::prepare(home = '/Applications/jamovi.app')` in an isolated full
package copy, with `ELECTRON_RUN_AS_NODE` unset for the local app launcher.
`devtools::document()` regenerated help there. Only the generated `survivalPower.h.R` and
`survivalPower.Rd` were copied back; generated files were not hand-edited.

The validation environment was R 4.6.0, jmvcore 2.7.38, gsDesign 3.11.0 and survival 3.8.11.
Full namespace loading emitted pre-existing glmmTMB/TMB version-mismatch and XQuartz
connection warnings. These were setup warnings, not warnings from the focused test cases.

Reproduce the focused checks after preparation and package loading:

```r
.libPaths(c(.libPaths(), '/Applications/jamovi.app/Contents/Resources/modules/base/R'))
devtools::load_all(helpers = FALSE)
testthat::test_dir("tests/testthat", filter = "^survivalPower", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-zzz-results-rendering-contract.R")
```

From the repository root, `Rscript development-scripts/validate_survivalPower_fixes.R`
regenerates the resolved-design snapshots, output-population matrix, citation checks and
plot artifacts. The [acceptance tests](../tests/testthat/test-survivalPower-full-audit-fixes.R)
preserve the repaired cases.

## Limits of this validation

Cluster power remains an explicitly labeled design-effect approximation for equal-size
clusters; rounding does not validate its operating characteristics with few clusters.
Simulation remains limited to fixed, two-arm exponential log-rank/Cox designs with uniform
accrual. Unsupported distributions, tests and complex simulation designs remain gated.
Sensitivity for effect-size/duration modes and R-side preset application are intentionally
unavailable as documented.

Verification covers R execution, JavaScript event tests, state persistence, schema compilation,
headless plot rendering and visual inspection of exported plots. It does not include installing
the repaired package into the live jamovi app, interactive acceptance of that installation,
a full module release check, or regulatory validation.
