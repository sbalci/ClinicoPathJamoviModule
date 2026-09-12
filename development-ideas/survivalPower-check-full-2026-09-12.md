# SYSTEMATIC CHECK: survivalPower

**Status: NEEDS WORK. Priority: High. Production ready: NO.**

The core planning calculations and jamovi file integration are functional, but several supported modes produce contradictory or misleading secondary results. The most consequential findings concern calculated-power assessment, solved durations and effects, cluster enrollment, simulation diagnostics, and non-inferiority wording. This is a report-only audit; no analysis implementation, schema, generated wrapper, or help file was repaired during this audit.

Audit date: 2026-09-12. Scope: `survivalPower.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`, generated `.h.R`/help, distribution helpers, preset events, references, and focused tests. The requested check-function-full skill and its canonical playbook were followed in audit-only mode. The current notices compatibility guidance takes precedence over the playbook's older dynamic-Notice example.

## Quick summary

- **Arguments:** 37 defined; all 37 referenced by the backend, with no reads of undefined options. Every option received a default-versus-changed run; 14 also received contextual comparisons. The preset is effective through UI events only; Weibull shape is unavailable because Weibull analyses are explicitly blocked.
- **Outputs:** 24 defined; all 24 have setters and populate in an applicable successful scenario. All calculation options are included in result invalidation rules. Population does not establish statistical or interpretive correctness.
- **Tests:** 716 assertions in 90 test blocks across nine focused files passed, with no test failures, errors, warnings, or skips. Six module-wide rendering-contract assertions also passed. All five plot renderers returned `TRUE` with valid state and `FALSE` with NULL state when called with the required theme arguments.
- **Build:** `jmvtools::prepare(home = '/Applications/jamovi.app')` succeeded on the temporary full package copy outside the sandbox, detecting jamovi 28.2.0. `ELECTRON_RUN_AS_NODE` was unset for the invocation. The sandboxed attempt reported that jamovi could not be accessed; its zero shell exit code was not treated as a successful build.
- **Notices:** Fatal unsupported configurations are generally stopped with useful messages. Important gaps remain for sparse expected events, actual rather than requested power, simulation warnings, and distinct strong-warning presentation.
- **Runtime:** R 4.6.0, jmvcore 2.7.38, gsDesign 3.11.0, survival 3.8.11, mvtnorm 1.4.2, testthat 3.3.2. Package-load diagnostics included an existing glmmTMB/TMB version mismatch and unavailable XQuartz display; these were separate from the passing focused test results.

The evidence directory is [survivalPower-check-full-2026-09-12](survivalPower-check-full-2026-09-12/). Source fingerprints identify the audited implementation. Headless R execution, compiler validation, JavaScript preset tests, and PDF rendering were used; this does not claim a live interactive jamovi UI acceptance test or a module-wide statistical review.

## Findings and proposed repairs

### F1 — High: calculated power is assessed using the requested power option

At [R/survivalPower.b.R:2635](../R/survivalPower.b.R#L2635), the regulatory table reads `self$options$power_level`; its classification at line 2687 therefore ignores calculated power. The low-power notice at line 289 uses the same input. With `analysis_type='power'`, N=10, control median=240 months, accrual=1 month, and follow-up=0, calculated power is **5.003%**, yet the regulatory row says **Adequate** and that the usual 80% expectation is met. Only a short-duration warning appears. In the six-look Cox/Pocock case, calculated power stays **29.629%** while changing the otherwise irrelevant requested power from 0.5 to 0.8 changes the assessment from Insufficient to Adequate.

**Proposed repair:** use `primary_numbers$power` in power mode, identify target power explicitly in other modes, and assess low power after a successful calculation. Replace categorical claims of clinical realism based solely on HR 0.5–2.0 with a request to justify assumptions in the disease setting. This audit does not establish regulatory compliance.

Evidence: `reproductions.json`, cases `low_events` and `cox_requested_power_*`.

### F2 — High: duration displays disagree, and the half-event time cannot occur during accrual

For `analysis_type='duration', sample_size_input=500`, the solver and duration table correctly return **45.5756 months**, comprising 24 months accrual and 21.5756 months additional follow-up. The natural-language summary instead reports **24 + 12 = 36 months**. The timeline ends follow-up at month 36 and adds an unexplained three-month analysis phase ending at month 39. The survival-curve horizon also follows the entered timeline. These displays use raw options at [line 2975](../R/survivalPower.b.R#L2975) and [line 4356](../R/survivalPower.b.R#L4356), rather than the solved duration.

The half-event calculation at [line 1735](../R/survivalPower.b.R#L1735) reuses a solver restricted to follow-up after enrollment finishes ([line 3679](../R/survivalPower.b.R#L3679)). For N=1000 and the default 24-month accrual, it reports **24 months** to reach 190 of the 380 required events. Independent integration of uniform recruitment, exponential events, and the same dropout hazard gives **15.3373 months**. The existing solver cannot represent this time during recruitment.

**Proposed repair:** construct a resolved design containing solved duration/follow-up and use it for all displays. Solve event milestones over calendar time from zero, integrating only participants recruited by that time. Remove the arbitrary three-month analysis phase or label it as an explicit user-specified operational assumption. Keep the end-of-accrual constraint for completion of enrollment distinct from event-milestone timing.

Evidence: `reproductions.json`, cases `duration_500` and `half_events`; independent integral in the cases harness.

### F3 — High: detectable-effect event counts and curves use the assumed HR instead of the solved HR

For `analysis_type='effect_size', sample_size_input=200`, both an input HR of 0.2 and an input HR of 0.75 produce the same detectable HR, **0.603459**. Nevertheless, Expected Events changes from **93** to **130**. The expected event count at the solved HR is **123.0646** under the stated assumptions. At [line 1683](../R/survivalPower.b.R#L1683), `expected_events_fun(private$.get_effect_hr())` selects the input HR. Expected survival curves also use that input ([line 2950](../R/survivalPower.b.R#L2950)). Effect-mode summaries and clinical-realism assessments inherit the same ambiguity.

**Proposed repair:** use the solved detectable HR consistently for a reported solved design, and distinguish any separately plotted input scenario. Label whole-study versus comparison-effective event counts, especially for multi-arm and cluster designs. Use the NI-specific threshold description rather than the current generic “smallest hazard ratio” text when non-inferiority is selected.

Evidence: `reproductions.json`, `effect_0.2` and `effect_0.75`.

### F4 — High: cluster sizing returns an individual-level inflation without a feasible cluster allocation

The cluster adjustment at [line 3541](../R/survivalPower.b.R#L3541) only multiplies the individual-level requirement by `1 + (m - 1) * ICC`. With cluster size=100, ICC=0.01, and HR=0.1, the result is **30 subjects**, fewer than one declared cluster. With ordinary default cluster parameters, the result is **2012 subjects** for clusters of 50, without reporting or rounding the number of clusters. An extreme-HR warning in the first case does not explain the infeasible randomization units.

**Proposed repair:** explicitly label this as an approximate design-effect calculation unless a validated cluster survival method is implemented. Compute and report whole clusters per arm, respect allocation, and assess whether enough independent clusters support the approximation. Do not present the individual-level ceiling as a complete cluster-randomized design.

Evidence: `reproductions.json`, `cluster`; contextual `cluster_size` and `icc` comparisons in `argument-behavior.csv`.

### F5 — High: sparse-event simulation produces hidden warnings and falsely exact confidence intervals

The low-event design in F1 has only **0.01261 expected events**. Of 1000 simulations, **984** have zero recorded events or failed tests, and **984 “NaNs produced” warnings** are emitted by the numerical test path. The user-visible notices do not report this. The simulation reports power=0, a **[0, 0]** 95% interval, MC SE=0, and “Simulation precision is adequate.” Its Wald interval ([lines 4093–4100](../R/survivalPower.b.R#L4093)) degenerates at zero or one observed rejection proportion. For comparison, an exact binomial interval for 0/1000 rejections is **[0, 0.003682]**.

The failure handler at [line 4086](../R/survivalPower.b.R#L4086) records failed tests as nonsignificant, with zero events, without a separate failure counter. This can conflate test failures with truly event-free replicates and can distort the reported event average for other failure types.

**Proposed repair:** capture and summarize numerical warnings, retain actual event counts independently of successful fitting, and distinguish zero-information replicates from calculation failures. Use a binomial interval with meaningful boundary behavior and qualify precision diagnostics at boundary estimates. Add expected-event adequacy guidance before presenting asymptotic power as reliable. Planning-specific event guidance is appropriate; fitted-model EPV rules do not directly apply because this analysis fits no covariates.

Evidence: `low-event-diagnostics.json` and `reproductions.json` (`low_events`).

### F6 — High: non-inferiority narratives describe the wrong objective; the glossary confuses hazard with risk

With non-inferiority, true HR=1, margin HR=1.25, and one-sided alpha=0.025, the primary result is **901 subjects and 631 events**. The plain-language summary and the early interpretation paragraphs describe a study “to detect a hazard ratio of 1.” The final report sentence correctly describes ruling out HR 1.25 or worse. Both descriptions appear in the same output, so the correct final sentence does not resolve the contradiction. See [line 2361](../R/survivalPower.b.R#L2361), [line 2412](../R/survivalPower.b.R#L2412), and [line 4288](../R/survivalPower.b.R#L4288).

Separately, the glossary at [line 4429](../R/survivalPower.b.R#L4429) calls HR 0.75 “25% lower risk,” while the educational Cox explanation correctly distinguishes hazards from relative risks. Under this module's own exponential assumptions, at the control median the control risk is 0.5 and treatment risk for HR 0.75 is approximately 0.4054: the risk reduction is about 18.9%, not 25%.

**Proposed repair:** route every narrative through one method-aware objective formatter, including margin, assumed HR and sidedness for NI. Define HR as a ratio of instantaneous event rates. Express absolute or relative risks only at an explicitly stated time using the survival model.

Evidence: `reproductions.json`, `ni_narrative`, and the cited glossary source.

### F7 — Medium: Cox headlines and supporting plots use different calculation methods

The Cox headline uses `gsSurv()` with its default Lachin–Foulkes method ([line 1048](../R/survivalPower.b.R#L1048)); the power, sample-size and sensitivity curves use local Schoenfeld helpers ([line 2868](../R/survivalPower.b.R#L2868)). At HR=0.5 and a 5:1 control:experimental ratio, the headline requires **193 subjects** while the sample-size curve at the same HR says **179**. At the headline N, the power curve says **83.154%**; Cox power mode says **80.035%**. This is more than integer-rounding noise. Neither method is inherently invalid merely because the methods differ; the problem is presenting them as one coherent design.

**Proposed repair:** make the chosen variance method explicit and reuse it for the headline, inverse calculations, sensitivity table, and plots. In the 12 audited fixed-design combinations, current Cox power inversion agrees with upstream `gsSurvPower()` to within **0.00003 absolute power** near the target of 80%; retain that agreement while unifying displays.

Evidence: `cox-consistency.csv`. The upstream [survival-design documentation](https://keaven.github.io/gsDesign/reference/nSurv.html) identifies Lachin–Foulkes as the default and documents the alternate methods.

### F8 — Medium: effect-input limits reject otherwise valid equivalent assumptions

The shared `effect_size` control has bounds 0.1–5 ([jamovi/survivalPower.a.yaml:141](../jamovi/survivalPower.a.yaml#L141)). In survival-probability-difference mode this excludes a common five-percentage-point difference (0.05), while allowing values greater than the feasible probability range. Both numerical conversions use HR brackets limited to **0.2–3**, even though direct HR accepts **0.1–5** ([line 3162](../R/survivalPower.b.R#L3162), [line 3193](../R/survivalPower.b.R#L3193)). Direct HR=0.15 produces N=21, but its equivalent survival difference at the control median, approximately 0.40125, produces Invalid Effect Size.

**Proposed repair:** use effect-type-specific controls/ranges, validate feasible probabilities or RMST differences, and use consistent HR bounds or an analytical exponential conversion. Explain the direction of benefit and permit supported signed differences where appropriate.

Evidence: `reproductions.json`, `direct_hr_015` and `survival_difference_015`.

### F9 — Medium: sensitivity analysis is only partially implemented in effect and duration modes

The sensitivity toggle is available in all modes. However, [line 2158](../R/survivalPower.b.R#L2158) returns NA for modes other than sample size/power. Effect and duration modes consequently display four scenario rows with the generic statement “Parameter change affects results,” without solving the scenario effects or durations. The adjacent sensitivity plot continues to show required sample sizes.

**Proposed repair:** gate the control and outputs to implemented modes, or solve and label the selected estimand for every scenario. The current table is a partial placeholder in these modes, despite the analysis as a whole being functional.

Evidence: `sensitivity-modes.json`.

### F10 — Medium: the public R preset argument promises behavior implemented only in JavaScript

Changing only `clinical_preset='cardio_prevention'` in the backend produces exactly the default results. The backend helper intentionally defers to UI events ([line 4238](../R/survivalPower.b.R#L4238)), but [man/survivalPower.Rd:48](../man/survivalPower.Rd#L48) says selecting a preset automatically populates parameters without an R-interface qualification. The JavaScript preset implementation and its schema/behavior tests work.

**Proposed repair:** either implement preset resolution before constructing read-only R options, with documented precedence for explicit arguments, or document the preset as a UI-only convenience and require explicit assumptions for R calls. Update `.a.yaml` descriptions and regenerate help; do not hand-edit generated files.

Evidence: `argument-behavior.csv`, first row, and `jamovi/js/survivalPower.events.js`.

## Argument behavior matrix

Each option was changed alone from the YAML defaults. Conditional comparisons were then run where necessary. YES means an observable calculation, panel, label, or validation-path change; it does not certify accuracy. “Blocked” means the option belongs to an explicitly unsupported configuration. Full output diffs and contextual numeric values are in [argument-behavior.csv](survivalPower-check-full-2026-09-12/argument-behavior.csv), with result contents in `differential-snapshots.rds`.

<!--ARGUMENT_MATRIX-->

## Output population matrix

Every setter site was checked against the source, then population was confirmed in runtime result snapshots. `clearWith` includes all 32 calculation/selection/simulation options for all results; each optional HTML result also clears with its own display toggle. There are no undefined reference keys or permanently invisible outputs. The schema uses declarative visibility, not failure-driven hiding.

<!--OUTPUT_MATRIX-->

For Images, the associated renderers are `.plot_power_curves`, `.plot_sample_size_curves`, `.plot_expected_survival`, `.plot_study_timeline`, and `.plot_sensitivity_analysis`. Full crosswalk: [output-population.csv](survivalPower-check-full-2026-09-12/output-population.csv). Render evidence: `plot-rendering.json` and `rendered-plots.pdf`.

## Notices coverage matrix

The schema-declared Preformatted collector is intentional: repository guidance documents runtime serialization limitations of dynamically inserted `jmvcore::Notice` objects. Do not replace it with `type: Notice` or `type: Notification`. Its multi-line formatting is appropriate for Preformatted content; it is not a violation of the single-line native-Notice restriction.

| Trigger | Severity | Location | Present? | Assessment |
|---|---|---|---|---|
| Missing dataset columns | Not applicable | — | N/A | Numeric planning tool; no dataset columns required |
| Invalid effect, power, alpha or median | ERROR | Top collector | Yes | Specific validation; many bounds also enforced by Options |
| Unsupported test/distribution/accrual/crossover/NI margin type | ERROR | Top collector | Yes | Stops before successful numeric outputs; states supported alternative |
| Unsolvable primary result | ERROR | Top collector | Yes | Finite-result gate prevents normal downstream output population |
| Extreme HR, high dropout, optimistic assumptions | STRONG_WARNING | Top collector | Partial | Triggers exist; severity is displayed with the same WARNING prefix |
| Too few expected events/independent clusters | Strong warning or design-specific error | Top preferred | No | F4/F5; thresholds should match planning method, not fitted-model EPV |
| Low calculated power | WARNING | Near power result preferred | No | Existing trigger tests the requested input instead; F1 |
| Multiplicity approximation and unsupported simulation | WARNING | Top collector | Yes | Explicitly explains Bonferroni approximation/limited simulator support |
| Interim looks without spending | WARNING | Top collector | Yes | Explains that boundaries/inflation cannot be calculated |
| Unequal allocation and NI sidedness | INFO | Top collector | Yes | Useful assumptions; method wording should match Cox method selection |
| Numerical simulation warnings/failed replicates | WARNING | Simulation output preferred | No | 984 warnings in sparse-event reproduction; F5 |
| Methodology/completion | INFO | Top collector | Yes | Useful assumption summary; not placed at bottom as the generic playbook suggests |

The collector retains emission order rather than sorting by severity, so a later error may follow earlier warnings. Preserve the supported collector while sorting severity, distinguishing strong warnings, and keeping completion information from obscuring material limitations. Some inner plot-data helpers catch errors and return NULL before the outer warning handler can see them; propagate a reason for unavailable plots instead of silently omitting state.

## Placeholder assessment and code quality

**Classification: FUNCTIONAL, with partially implemented ancillary behavior.** Data columns are not used because this is a numeric trial-planning analysis. Options drive real Schoenfeld/event-probability calculations, gsDesign calls, numerical inversions, correlated multi-arm approximations, and seeded Monte Carlo log-rank tests. Results change materially with assumptions; the analysis is not a constant-output scaffold. The inactive Weibull shape and blocked specialized methods are disclosed limitations. F9 and F10 identify narrower incomplete behavior.

The cached primary numeric results, cached seeded simulation, cancellation checkpoints, fixed row keys, clearWith coverage, and explicit unsupported-method guards are useful foundations. The main maintainability problem is duplicated design resolution across headlines, tables, curves and text. A shared resolved-design object and explicit method selection would directly address F1–F3, F6 and F7.

Observed ordinary differential runs took roughly **0.01–0.03 seconds** in this environment, while the enabled 2000-run simulation took about **2.94 seconds**. These are local observations, not performance guarantees. The sample-size curve is computed even when its analysis-type visibility rule hides it; gate that work. Maximum 100,000-run simulations were not benchmarked.

## Documentation and library rendering gate

| Check | Assessment |
|---|---|
| Image state guards | Pass; all five renderers handle NULL state |
| Theme-safe HTML / supported entities | Pass; no opaque HTML fills or nonstructural named entities found; module contract passes |
| Failure-driven setVisible(FALSE) | Pass; no calls in this backend |
| Fixed rows created in .init | Pass; only addRow site is the shared initializer at line 106 |
| R warnings shown meaningfully | Needs work; no explicit backend warning() calls, but dependency warnings escape in sparse-event simulation |
| Translation | Needs work; no `.()` translation calls in the backend; long user-visible text remains untranslated |
| Namespace dependencies | Pass; every explicit `::` package in the backend is in Imports |
| UI/schema/events | Pass for wiring; all options represented, preset keys valid, panel title matches analysis title |
| Control wording | Improvement needed; many labels use Title Case and “Show…” action phrasing rather than sentence case/noun labels |
| Citation keys | Pass; all six keys resolve with nonempty author/year |
| Citation content/version alignment | Needs work; narrative says Anderson 2022, registry says 2026; survival registry mentions 3.8-9 while installed runtime is 3.8.11 |
| Method citations | Needs work; Schoenfeld (1983) and Rothmann et al. (2003) appear in prose without corresponding analysis refs keys |
| Version wording | Beta 0.4.0 banner coexists with older 0.3.0 source comments/test messages; reconcile as documentation maintenance |

The NI narratives, hazard/risk glossary, solved-design displays, and R preset help need substantive corrections as detailed above. The six resolved refs are ClinicoPathJamoviModule, LachinAndFoulkes1986, gsDesign, survival, ggplot2 and scales; see `citation-integrity.csv`. Citation metadata should describe the intended supported/runtime package version, rather than being updated blindly to the audit machine's versions.

## External documentation comparison

Targeted comparisons used the primary gsDesign reference documentation and NEWS, plus installed package execution. This is an intentional jamovi wrapper, so matching upstream argument names/order is not itself a requirement.

| Aspect | Local implementation | Upstream | Assessment / action |
|---|---|---|---|
| Allocation | Control:experimental option; reciprocal passed to gsSurv | Experimental:control | Intentional and correctly mapped; retain explicit help |
| Alpha/sidedness | Superiority alpha halved for one-sided upper-bound design; NI uses entered one-sided alpha | Explicit sidedness and test.type controls | Document the symmetric-test interpretation and which crossing probabilities are counted |
| Variance method | Cox headline inherits Lachin–Foulkes; other outputs use Schoenfeld | Multiple explicitly named methods | F7: unify method or clearly label separate estimates |
| Power for fixed N | Inverts gsSurv beta over 0.01–0.60, then falls back | gsSurvPower computes achieved power directly | Evaluate a version-gated direct path and report any fallback/provenance |
| Parameter scope | Exponential failure, uniform enrollment, common-effect approximations | Richer piecewise failure/enrollment and timing controls | Intentional narrowed scope; do not imply full upstream coverage |
| Examples | 12 fixed-design combinations compared numerically | gsSurvPower with matched enrollment, hazards and timing | Agreement near target power within 0.00003; supporting curve disagreement is local |

The [gsDesign design reference](https://keaven.github.io/gsDesign/reference/gsDesign.html) distinguishes one-sided efficacy and symmetric two-sided designs. A comparison using only the reported upper boundary would omit opposite-tail rejection for a two-sided claim. The corrected independent comparison uses both ± boundaries: across 12 O'Brien–Fleming/Pocock scenarios, the local sequential approximation differed from direct crossing probabilities by at most **0.00080 absolute power** (0.080 percentage points). This is a small measured approximation difference, not the much larger apparent mismatch obtained by omitting the opposite tail. See `sequential-two-sided.csv`; the earlier `sequential-power-comparison.csv` is directional diagnostic evidence only.

The [gsSurvPower reference](https://keaven.github.io/gsDesign/reference/gsSurvPower.html) documents direct achieved-power computation with fixed enrollment, dropout and timing, including a fixed-design path for k=1. [gsDesign NEWS](https://keaven.github.io/gsDesign/news/index.html) documents its addition in the 3.11.0 release. The local comment claiming beta inversion is the only available approach is therefore outdated for the installed version. An update should preserve compatibility with older supported versions or declare an appropriate minimum dependency version; no such update was made in this audit.

## Actionable repair sequence and regression checklist

1. **Resolve and label the actual design:** centralize achieved/target power, solved HR, total duration, minimum follow-up, actual versus effective N/events, method and sidedness. Repair F1–F3 and method-aware narratives in F6.
2. **Handle limited information:** add cluster allocation/method limitations and sparse-event simulation diagnostics; use a defensible finite-simulation interval (F4/F5).
3. **Unify calculation paths:** use one method across Cox headlines, inversions, tables and curves, with an explicit gsDesign-version fallback (F7).
4. **Constrain unfinished controls:** fix effect-specific bounds, gate or implement sensitivity modes, and align R preset behavior with its documentation (F8–F10).
5. **Finish presentation:** preserve notice severity/order, improve translation and labels, gate hidden computation, and synchronize citations/version text.

Required acceptance tests after repair should exercise behavior independently rather than only asserting that outputs exist:

- [ ] A 5% achieved-power design is never assessed as having 80% power; changing a target-only option in power mode cannot change the achieved-power assessment.
- [ ] Solved 45.5756-month duration is used consistently in table, summary and plot; the independently integrated half-event time is reproduced during accrual.
- [ ] The detectable-HR design uses approximately 123.0646 expected events for HR 0.603459 and N=200, irrespective of an irrelevant same-direction starting HR.
- [ ] Cluster results specify feasible whole clusters per arm and explain approximation limitations.
- [ ] Zero simulated rejections yield a nondegenerate interval and explicit sparse-event/failure information; actual event counts survive test failures.
- [ ] Every NI paragraph states the margin-based objective, assumed HR, and one-sided alpha; the glossary distinguishes hazards from risks.
- [ ] Cox headline and curve agree at the same design point for unequal allocation and both effect directions.
- [ ] Equivalent HR, median-ratio, survival-difference and RMST inputs give compatible designs throughout supported ranges.
- [ ] Sensitivity either returns the selected effect/duration estimand or is unavailable with a clear explanation.
- [ ] Preset behavior is correct and documented for both the GUI and public R interface.
- [ ] Re-run focused tests, rendering/state persistence, schema preparation and representative interactive jamovi workflows.

No implementation patch is included because this command is audit-only. These are proposed repairs, not completed changes.

## Reproduction and evidence

Run these report-only harnesses from the repository root with the audited package dependencies installed:

```sh
Rscript development-scripts/audit_survivalPower_full.R
Rscript development-scripts/audit_survivalPower_cases.R
Rscript development-scripts/audit_survivalPower_consistency.R
Rscript development-scripts/audit_survivalPower_outputs.R
```

The cases harness intentionally reproduces warnings in the sparse-event case. The consistency harness captures ordinary-case warnings separately. The output harness checks setter sites and renders plots with the required theme arguments. The focused test suite was run with the full package namespace loaded in the temporary source copy, whose five core survivalPower files matched the audited source, to exercise real save/load serialization rather than a source-only approximation. Its results are in `regression-results.csv`; setup diagnostics and execution output are in `regression.log`. Build output is in `prepare.log`. Source files were checked again against `source-fingerprints.csv` after validation.

**Readiness:** file integration passes; numerical and narrative consistency need repair; notices are incomplete for important edge cases; the analysis should remain beta until the High findings are resolved. The successful existing tests establish a sound integration baseline but do not cover the newly reproduced inconsistencies.
