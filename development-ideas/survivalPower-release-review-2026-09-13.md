# survivalPower release review

Started 2026-09-13; completed 2026-09-14. Scope: the umbrella analysis and its support files,
with read-only inspection of configured submodules. This review follows the earlier repairs;
it records newly confirmed defects and fresh verification of the resulting source.

## 1. Overall verdict

**Ready after specified minor actions: regeneration and a smoke test of the packaged analysis.**
The corrected source passes the release checks for its supported exponential-survival,
uniform-entry planning models. The core numerical calculations agree with independent
integration, published event formulas and installed `gsDesign` calculations. A significant UI
binding defect was repaired. The source is analysis version **0.4.1**, while the generated R
base still declares **0.4.0**; a completed, installed 0.4.1 release has therefore **not** been
verified. Clinical use remains prospective study planning under the disclosed assumptions,
with the approximation limits described below.

## 2. Findings

### Critical

No new materially wrong estimator or supported-path crash remained after this review.

### Major

- **Broken enable bindings in 16 UI controls — fixed.**
  [survivalPower.u.yaml](../jamovi/survivalPower.u.yaml:37) used `==`, `!=` and `>` in
  short-form bindings. Jamovi's installed `_resolveBindPart` treats those expressions as
  nonexistent control names. For example, the default effect-size selector resolved false
  with `Cannot bind to 'test_type != "non_inferiority"'`. The affected controls were
  `effect_size_type`, `effect_size`, `power_level`, `sample_size_input`, `weibull_shape`,
  `ni_margin`, `ni_type`, `rmst_tau`, `rmst_difference`, `number_of_arms`,
  `multiple_comparisons`, `cluster_size`, `icc`, `alpha_spending`,
  `sensitivity_analysis`, and `run_simulation_validation`.
  The scratch truth table had 23 incorrect enabled states out of 48 cases and binding
  warnings in 44 cases. Supported colon/negation expressions now pass **48/48 cases with
  no warnings**, including the NI inverse exceptions.
  [Before](survivalPower-release-review-2026-09-13/ui-bindings-before.json),
  [after](survivalPower-release-review-2026-09-13/ui-bindings-after.json).

### Moderate

- **Unused survival-difference input described as a computed effect in NI inverse mode — fixed.**
  [Validation](../R/survivalPower.b.R:419) emitted a landmark notice claiming HR 1 even though
  that value was only an internal placeholder and the effect was being solved. The notice
  is now restricted to modes that actually use the input conversion. NI validation also
  reuses the already resolved value instead of converting the ignored effect again.
- **Target-power reference line nearly invisible in dark mode — fixed.**
  [Power renderer](../R/survivalPower.b.R:3148) drew a translucent black dashed line on a dark
  background. It now uses the existing orange palette at full opacity, keeping the dashed
  shape and target unchanged. The
  [updated dark plot](survivalPower-release-review-2026-09-13/power_curve_plot-dark.png)
  was inspected visually.

### Minor

- **NI inverse precision — fixed.** The default `uniroot` tolerance returned a design with
  independently calculated power `0.7999422737` for target `0.8` (a 0.00577 percentage-point
  difference, below the displayed precision). An explicit `tol = 1e-8` at
  [the NI root solve](../R/survivalPower.b.R:1364) yields `0.800000000020`.
  The old rounded HR regression anchor was replaced by an independently integrated root.
- **Missing citation URL — fixed.** [Schoenfeld1983](../jamovi/00refs.yaml:910) had a DOI but
  no `url`, failing the library metadata gate. Added the matching
  [PubMed article record](https://pubmed.ncbi.nlm.nih.gov/6354290/).
  Its other caller, `advancedSurvivalPower`, cites the same paper; its analysis was not changed.
- **Stale distribution comment — fixed.**
  [The update configuration](../_updateModules_config.yaml:173) said `SurvivalT` and implied
  explicit helper listing. It now reflects `PowerT` and automatic companion-file copying.
  Distribution behavior itself was already correct.
- **Generated version update pending.**
  [Source version](../jamovi/survivalPower.a.yaml:8) is 0.4.1; the
  [generated base](../R/survivalPower.h.R:1635) remains 0.4.0 until user regeneration.

## 3. Changes made

Repaired the 16 UI conditions, suppressed the false NI landmark notice, increased NI inverse
precision, made the target-power line readable in dark mode, completed the citation metadata,
corrected the routing comment and bumped the analysis version to 0.4.1. Statistical defaults
and supported methods were preserved. The NI tolerance changes unrounded values only.

Added [UI binding regressions](../tests/testthat/test-survivalPower-ui-bindings.R) and
[release-gate regressions](../tests/testthat/test-survivalPower-release-gates.R).
The GUI test executes methods extracted unchanged from the installed jamovi client and checks
50 enabled/disabled scenarios. On machines without that client it skips the runtime portion;
the portable operator, option-name and list-level checks still run. The scratch comparison
separately covers 48 before/after scenarios. The existing NI regression now compares against
an independent integral rather than a low-precision literal.

Timestamped source backups are at `/tmp/survivalPower-release-backup-20260913-212242`.
[Source verification](survivalPower-release-review-2026-09-13/release-source-verification.csv)
and the [release-only source patch](survivalPower-release-review-2026-09-13/release-source.patch)
distinguish these changes from preceding work. No generated file was edited or regenerated
in this release-review pass.

## 4. Statistical verification

The throwaway `/tmp/verify_survivalPower_release.R` independently integrates event probability
over uniform entry, including exponential event and dropout hazards. Log-rank power is checked
with the noncentral chi-square distribution, NI power with its one-sided normal rejection
region, and Cox power with both tails from `gsDesign::nSurv(method="LachinFoulkes")`.
The reference equations are supported by
[Schoenfeld's paper](https://www.biostat.wisc.edu/~chappell/641/papers/paper31.pdf) and the
[official gsDesign survival documentation](https://keaven.github.io/gsDesign/reference/nSurv.html).
This checks the declared planning approximation; it is not evidence that a specific clinical
trial satisfies its assumptions.

Default assumptions below: control median 12 months, uniform accrual over 24 months,
12 additional months, annual dropout probability 0.05, equal allocation and 80% target power.
Superiority alpha is two-sided 0.05; NI alpha is one-sided 0.025, margin 1.25.

| Quantity | Implementation | Independent reference |
|---|---:|---:|
| Log-rank power, N=600, HR=0.75 | 0.812163674420 | 0.812163674420 |
| Cox power, N=600, HR=0.75 | 0.813106231667 | 0.813106231667 |
| NI power, N=600, true HR=1 | 0.628666816991 | 0.628666816991 |
| Log-rank required events / enrollment, HR=0.75 | 380 / 583 | 380 / 583 |
| NI required events / enrollment, true HR=1 | 631 / 901 | 631 / 901 |
| Log-rank total duration, N=600 | 34.724563 months | 34.724574 months |
| Cox total duration, N=600 | 34.557519835 months | 34.557519834 months |
| NI total duration, N=1000, true HR=1 | 31.713680 months | 31.713678 months |
| Power at solved NI HR, N=1000 | 0.800000000020 | 0.800000000000 |

**51/51 scratch reference comparisons pass.** Coverage includes allocation ratios 0.2, 1 and
5; superiority HRs 0.1, 0.75, 1, 1.5 and 5; NI HRs 0.75, 1 and 1.2; all four calculation modes;
zero and positive additional-follow-up solutions. Power comparison tolerances are `1e-7`,
inverse-power checks `2e-5`, and duration checks `1e-4` months.
[All numbers and tolerances](survivalPower-release-review-2026-09-13/independent-statistics.csv).
Six new NI regression scenarios additionally meet a `1e-7` target-power tolerance.

Fresh supplementary cases confirm correct whole-study multi-arm event totals, conservative
multiplicity, two-sided fixed family power, low-power NI wording and preserved analytical RNG.
For example, ten-arm effect inversion reports 499 whole-study events, matching independently
integrated 498.7075, and the ten-arm null family-alpha 0.10 case gives approximately 7.374785%
family rejection probability. The full regression suite also tests sequential Cox against
`gsSurvPower`, exact-binomial simulation uncertainty, adverse direction and sparse-event paths.

## 5. Data-flow audit

All **37 options** have exactly one named UI control and a backend read. There are no
undeclared option reads, orphan UI controls, missing result renderers or invalid `clearWith`
entries. Defaults remain unchanged and the R wrapper arguments still match the source schema;
the only pending analysis-schema generation change is the version number.

In the table, **Primary** means `power_summary` plus the selected
`sample_size_results`, `power_results`, `effect_size_results` or `study_duration_results`,
with the corresponding resolved-design plots and requested narratives.
The [CSV](survivalPower-release-review-2026-09-13/data-flow.csv) also records schema type,
UI grouping, exact enable expression and source line numbers.

| Option / schema default | UI control | Backend computation | Results |
|---|---|---|---|
| [`clinical_preset`](../jamovi/survivalPower.a.yaml:35) / `"custom"` | [ComboBox](../jamovi/survivalPower.u.yaml:15) | [`.apply_clinical_preset`](../R/survivalPower.b.R:4178) — UI callback fills explicit options; R calls only disclose the chosen worked example | notices; instructions; UI controls |
| [`analysis_type`](../jamovi/survivalPower.a.yaml:60) / `"sample_size"` | [ComboBox](../jamovi/survivalPower.u.yaml:20) | [`.calculate_primary_result`](../R/survivalPower.b.R:837) — Dispatch sample size, power, detectable HR or duration | Primary; all result visibility |
| [`test_type`](../jamovi/survivalPower.a.yaml:76) / `"log_rank"` | [ComboBox](../jamovi/survivalPower.u.yaml:22) | [`.calculate_primary_result`](../R/survivalPower.b.R:837) — Log-rank, Lachin-Foulkes/Cox or NI; four unsupported choices stop visibly | Primary; specialized tables; notices |
| [`study_design`](../jamovi/survivalPower.a.yaml:99) / `"two_arm_parallel"` | [ComboBox](../jamovi/survivalPower.u.yaml:26) | [`.adjust_sample_for_design`](../R/survivalPower.b.R:3423) — Two-arm, common-control multi-arm or approximate cluster scaling; stratified caveat; crossover blocked | Primary; multi_arm_table; notices |
| [`primary_endpoint`](../jamovi/survivalPower.a.yaml:117) / `"overall_survival"` | [ComboBox](../jamovi/survivalPower.u.yaml:28) | [`.format_primary_endpoint`](../R/survivalPower.b.R:818) — Endpoint labels only; does not infer a different event model | power_summary; clinical_interpretation; natural_language_summary |
| [`effect_size_type`](../jamovi/survivalPower.a.yaml:136) / `"hazard_ratio"` | [ComboBox](../jamovi/survivalPower.u.yaml:38) | [`.get_effect_hr`](../R/survivalPower.b.R:3011) — Convert HR, reciprocal median ratio, landmark survival difference or exponential RMST difference; ignored for NI inverse | Primary; survival_curves_plot |
| [`effect_size`](../jamovi/survivalPower.a.yaml:152) / `0.75` | [TextBox](../jamovi/survivalPower.u.yaml:41) | [`.get_effect_hr`](../R/survivalPower.b.R:3011) — Assumed effect, or direction for superiority HR inversion; unused in NI inversion | Primary; all relevant plots |
| [`alpha_level`](../jamovi/survivalPower.a.yaml:170) / `0.05` | [TextBox](../jamovi/survivalPower.u.yaml:47) | [`.adjust_alpha_for_multiplicity`](../R/survivalPower.b.R:3398) — Two-sided superiority / one-sided NI; multiplicity and interim boundaries | Primary; regulatory_table; interim_analysis_table; multi_arm_table |
| [`power_level`](../jamovi/survivalPower.a.yaml:179) / `0.8` | [TextBox](../jamovi/survivalPower.u.yaml:52) | [`.calculate_primary_result`](../R/survivalPower.b.R:837) — Planning target for sample size, HR and duration; target line where applicable | Primary; power_curve_plot; narratives |
| [`allocation_ratio`](../jamovi/survivalPower.a.yaml:188) / `1.0` | [TextBox](../jamovi/survivalPower.u.yaml:58) | [`.allocation_props`](../R/survivalPower.b.R:3474) — Control:each-experimental allocation; inverse passed to gsDesign | Primary; multi_arm_table; simulation_validation_table |
| [`sample_size_input`](../jamovi/survivalPower.a.yaml:197) / `200` | [TextBox](../jamovi/survivalPower.u.yaml:62) | [`.resolved_design`](../R/survivalPower.b.R:266) — Known total enrollment for power, HR and duration modes | power_results; effect_size_results; study_duration_results; plots |
| [`control_median_survival`](../jamovi/survivalPower.a.yaml:207) / `12.0` | [TextBox](../jamovi/survivalPower.u.yaml:75) | [`.get_distribution_parameters`](../R/survivalPower.b.R:3316) — Control hazard log(2)/median, exponential model | Primary; survival_curves_plot; assumptions_table |
| [`survival_distribution`](../jamovi/survivalPower.a.yaml:216) / `"exponential"` | [ComboBox](../jamovi/survivalPower.u.yaml:79) | [`.validate_inputs`](../R/survivalPower.b.R:372) — Only exponential accepted; other listed distributions stop before estimates | notices; assumptions_table; Primary |
| [`weibull_shape`](../jamovi/survivalPower.a.yaml:237) / `1.0` | [TextBox](../jamovi/survivalPower.u.yaml:81) | [`.get_distribution_parameters`](../R/survivalPower.b.R:3316) — Dormant for released calculations: Weibull is blocked; retained helper/preset compatibility | notices when Weibull chosen; no supported-mode numeric effect |
| [`accrual_period`](../jamovi/survivalPower.a.yaml:247) / `24.0` | [TextBox](../jamovi/survivalPower.u.yaml:94) | [`.event_probability`](../R/survivalPower.b.R:3488) — Uniform-entry integration and recruitment duration | Primary; accrual_timeline_plot; sensitivity outputs |
| [`follow_up_period`](../jamovi/survivalPower.a.yaml:256) / `12.0` | [TextBox](../jamovi/survivalPower.u.yaml:98) | [`.event_probability`](../R/survivalPower.b.R:3488) — Additional follow-up after recruitment; landmark for survival-difference conversion; duration initial search | Primary; survival_curves_plot; accrual_timeline_plot |
| [`accrual_pattern`](../jamovi/survivalPower.a.yaml:265) / `"uniform"` | [ComboBox](../jamovi/survivalPower.u.yaml:102) | [`.validate_inputs`](../R/survivalPower.b.R:372) — Uniform entry only; other selections rejected | notices; assumptions_table |
| [`dropout_rate`](../jamovi/survivalPower.a.yaml:281) / `0.05` | [TextBox](../jamovi/survivalPower.u.yaml:104) | [`.dropout_hazard`](../R/survivalPower.b.R:3481) — Annual probability converted to monthly competing hazard | Primary; assumptions_table; simulation_validation_table |
| [`ni_margin`](../jamovi/survivalPower.a.yaml:291) / `1.25` | [TextBox](../jamovi/survivalPower.u.yaml:116) | [`.calculate_non_inferiority`](../R/survivalPower.b.R:1227) — Relative HR margin; true HR must be below it; maximum HR inversion | Primary; non_inferiority_table; regulatory_table; narratives |
| [`ni_type`](../jamovi/survivalPower.a.yaml:300) / `"relative_margin"` | [ComboBox](../jamovi/survivalPower.u.yaml:120) | [`.validate_inputs`](../R/survivalPower.b.R:372) — Relative HR margin only; absolute/retention options blocked | notices; non_inferiority_table |
| [`rmst_tau`](../jamovi/survivalPower.a.yaml:315) / `36.0` | [TextBox](../jamovi/survivalPower.u.yaml:123) | [`.get_effect_hr`](../R/survivalPower.b.R:3011) — Restriction time for exponential RMST-to-HR conversion; ignored in NI inverse | Primary; survival_curves_plot; notices |
| [`rmst_difference`](../jamovi/survivalPower.a.yaml:324) / `3.0` | [TextBox](../jamovi/survivalPower.u.yaml:127) | [`.get_effect_hr`](../R/survivalPower.b.R:3011) — Signed months difference converted to HR; not an RMST-test power calculation | Primary; survival_curves_plot; notices |
| [`number_of_arms`](../jamovi/survivalPower.a.yaml:334) / `3` | [TextBox](../jamovi/survivalPower.u.yaml:139) | [`.multi_arm_factor`](../R/survivalPower.b.R:3453) — One control plus equally sized experimental arms, shared assumed HR | Primary; multi_arm_table; whole-study event counts |
| [`multiple_comparisons`](../jamovi/survivalPower.a.yaml:343) / `"dunnett"` | [ComboBox](../jamovi/survivalPower.u.yaml:143) | [`.adjust_alpha_for_multiplicity`](../R/survivalPower.b.R:3398) — None or Bonferroni; Holm/Dunnett conservatively approximate Bonferroni with notice | Primary; multi_arm_table; notices |
| [`interim_analyses`](../jamovi/survivalPower.a.yaml:360) / `0` | [TextBox](../jamovi/survivalPower.u.yaml:154) | [`.gs_info_design`](../R/survivalPower.b.R:3761) — Number of equally spaced information looks plus final; efficacy only | Primary; interim_analysis_table; accrual_timeline_plot |
| [`alpha_spending`](../jamovi/survivalPower.a.yaml:369) / `"none"` | [ComboBox](../jamovi/survivalPower.u.yaml:157) | [`.gs_info_design`](../R/survivalPower.b.R:3761) — None=fixed design with explanation, or LD OBrien-Fleming/Pocock spending | Primary; interim_analysis_table; notices |
| [`cluster_size`](../jamovi/survivalPower.a.yaml:383) / `50` | [TextBox](../jamovi/survivalPower.u.yaml:146) | [`.cluster_allocation`](../R/survivalPower.b.R:308) — Whole-cluster allocation and approximate design effect | Primary; notices; regulatory_table |
| [`icc`](../jamovi/survivalPower.a.yaml:396) / `0.05` | [TextBox](../jamovi/survivalPower.u.yaml:150) | [`.adjust_sample_for_design`](../R/survivalPower.b.R:3423) — Inflation 1+(cluster size-1)*ICC; no small-cluster df correction | Primary; notices |
| [`sensitivity_analysis`](../jamovi/survivalPower.a.yaml:406) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:160) | [`.sensitivity_applicable`](../R/survivalPower.b.R:350) — Numerical grids for sample-size and power modes only | sensitivity_analysis_table; sensitivity_plot; assumptions_table |
| [`run_simulation_validation`](../jamovi/survivalPower.a.yaml:413) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:164) | [`.run_simulation_analysis`](../R/survivalPower.b.R:3889) — Optional fixed two-arm exponential log-rank/Cox Monte Carlo validation | simulation_validation_table; assumptions_table; notices |
| [`simulation_runs`](../jamovi/survivalPower.a.yaml:425) / `2000` | [TextBox](../jamovi/survivalPower.u.yaml:168) | [`.run_simulation_analysis`](../R/survivalPower.b.R:3889) — Configured number of trials, MC error and exact binomial interval | simulation_validation_table; notices |
| [`simulation_seed`](../jamovi/survivalPower.a.yaml:436) / `42` | [TextBox](../jamovi/survivalPower.u.yaml:172) | [`.run_simulation_analysis`](../R/survivalPower.b.R:3889) — Local reproducible seed; restores caller RNG | simulation_validation_table |
| [`show_interpretation`](../jamovi/survivalPower.a.yaml:447) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:184) | [`.generate_interpretation`](../R/survivalPower.b.R:2208) — Optional complete study description and copy-ready report | clinical_interpretation |
| [`show_summary`](../jamovi/survivalPower.a.yaml:456) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:187) | [`.generate_clinical_friendly_outputs`](../R/survivalPower.b.R:4197) — Optional plain-language explanation using resolved numbers | natural_language_summary |
| [`show_explanations`](../jamovi/survivalPower.a.yaml:463) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:190) | [`.generate_clinical_friendly_outputs`](../R/survivalPower.b.R:4197) — Optional educational notes | educational_explanations |
| [`show_glossary`](../jamovi/survivalPower.a.yaml:470) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:193) | [`.generate_clinical_friendly_outputs`](../R/survivalPower.b.R:4197) — Optional statistical glossary | statistical_glossary |
| [`guided_mode`](../jamovi/survivalPower.a.yaml:477) / `false` | [CheckBox](../jamovi/survivalPower.u.yaml:196) | [`.generate_clinical_friendly_outputs`](../R/survivalPower.b.R:4197) — Optional guided workflow; no numerical effect | guided_workflow |

**Unused options:** no globally unread options. `weibull_shape` is intentionally inactive in
all supported public calculations because non-exponential distributions are blocked.
`primary_endpoint` is descriptive metadata; `clinical_preset` is a UI worked-example action,
not an R-side numeric override. `ni_type` and `accrual_pattern` currently expose one accepted
choice plus explicitly rejected future choices. Effect inputs are unused in NI effect inversion;
`sample_size_input` is unused for sample-size calculation; target power is not an input to
fixed power estimation. Method/design-specific controls are irrelevant outside their branch.
These are deliberate conditional inputs rather than silently ignored statistical features.

**Backend behavior with no exposed control:** equal information spacing for interim looks;
efficacy-only monitoring without futility; common HR and equal experimental-arm allocation
in multi-arm designs; Bonferroni approximation for Holm/Dunnett; fixed correlation model for
family power; no stratification efficiency adjustment; equal-cluster design-effect approximation;
a 95% binomial Monte Carlo interval; bounded HR inversion domain and numerical tolerances;
checkpoint and cache policy. Distribution helpers retain Weibull/log-normal/piecewise functions
for compatibility and development tests, but those methods are not enabled in the public
analysis. Cox is a two-group survival planning calculation, with no covariate-effect modeling
control. No new controls were added.

All **24 result items** are populated in at least one valid test scenario:

| Result group | Backend / renderer | Visibility and lifecycle |
|---|---|---|
| `notices`, `instructions` | `.renderNotices`, `.update_instructions` | Always available; visible validation failures; notices reset each run |
| `power_summary` | `.populate_power_summary` | Always visible; main result mode and labels |
| Four primary detail tables | `.populate_sample_size_results`, `.populate_power_results`, `.populate_effect_size_results`, `.populate_duration_results` | Selected analysis mode; fixed rows initialized in `.init()` |
| `assumptions_table`, `regulatory_table` | `.populate_assumptions`, `.populate_regulatory_considerations` | Option-sensitive disclosures and numerical summaries |
| `simulation_validation_table` | `.populate_simulation_comparison` | Simulation requested and applicability checked |
| `non_inferiority_table`, `multi_arm_table`, `interim_analysis_table` | Corresponding `.populate_*` methods | Method/design conditions; unavailable sequential family power explicitly disclosed |
| `sensitivity_analysis_table` | `.populate_sensitivity_analysis_table` | Sensitivity requested; sample-size or power branch |
| `power_curve_plot` | `.plot_power_curves` | Relevant primary modes; serialized values and target; NULL/empty guard |
| `sample_size_plot` | `.plot_sample_size_curves` | Sample-size mode; NULL/empty guard |
| `survival_curves_plot` | `.plot_expected_survival` | Resolved HR, including solved effect; NULL/empty guard |
| `accrual_timeline_plot` | `.plot_study_timeline` | Resolved duration and recruitment; NULL/empty guard |
| `sensitivity_plot` | `.plot_sensitivity_analysis` | Requested and applicable; NULL/empty guard |
| `clinical_interpretation`, `natural_language_summary` | `.generate_interpretation`, `.generate_natural_language_summary` | Explicit output toggles; resolved numbers |
| `educational_explanations`, `statistical_glossary`, `guided_workflow` | Corresponding `.generate_*` methods | Explicit output toggles |

The analysis has **no Data, Variable, Variables or Level options**. An empty data frame is a
normal valid input. Row filtering, subject-level missingness, factor coding and row alignment
are therefore not applicable; enrollment is an explicit numeric design input. Sparse simulated
events and unsupported parameter combinations have dedicated regression coverage. Saved-state
regressions exercise jamovi's protobuf/load lifecycle, including invalidation after a valid
design becomes unsupported, rather than merely changing a private helper value.

## 6. Test results

| Check | Result / evidence |
|---|---|
| All `test-survivalPower*.R` files, with current backend loaded through an isolated package stage | **13 files, 119 test blocks, 1,320 assertions; 0 failures, errors, skips or test warnings.** [Log](survivalPower-release-review-2026-09-13/regression.log), [per-test results](survivalPower-release-review-2026-09-13/regression-results.csv) |
| `Rscript /tmp/verify_survivalPower_release.R` | **51/51 independent comparisons passed** |
| `test-survivalPower-ui-bindings.R` | **78 assertions passed**, including 50 real-parser truth-table scenarios |
| Updated NI/citation tests plus existing review regressions | **41 + 296 assertions passed**; [log](survivalPower-release-review-2026-09-13/precision-regression.log) |
| `test-zzz-results-rendering-contract.R` | **6 assertions passed**, no failures/errors/skips/warnings |
| `python3 tools/check_state_guards.py R/survivalPower.b.R` | `0 unguarded image$state read(s)` |
| `python3 tools/check_uyaml_duplicate_names.py jamovi/survivalPower.u.yaml` | No duplicate control names |
| `python3 tools/theme_safe_html.py R/survivalPower.b.R` | Dry run: no pale-background or chip changes required |
| `tools/ui_harness/render_ui.sh survivalPower agreement tableone` | Target and events-bearing `agreement`: identical `ReferenceError: require is not defined`; no-events `tableone`: `placeholder present = false ... errors = undefined` |
| Direct installed-client binding verification | **48/48 before/after cases pass after repair**, no unknown-control warnings |
| `node tools/ui_harness/compile_ui.mjs . survivalPower /tmp` | Current UI compiles successfully into temporary output |
| `Rscript development-scripts/validate_survivalPower_fixes.R <evidence-dir>` | All **24 outputs populated**; five renderers pass light/dark/NULL states; older power-plot state renders; 12 PNGs and PDF generated |
| `Rscript development-scripts/review_survivalPower_static.R <evidence-dir>` | No missing imports/options, unmarked non-ASCII, scalar/vector condition bugs or missing affected Turkish translations; 13 brace and 3 semicolon style findings only |

The headless UI harness cannot load CommonJS event modules; this is a confirmed harness
limitation, not a remaining target-panel failure. Event handlers use CommonJS according to
[jamovi's official UI documentation](https://dev.jamovi.org/ui/advanced-customisation/).
The parser fallback executes the installed client logic and the existing preset tests execute
the real JS callback against valid/recovery configurations. A full packaged desktop smoke test
is still listed below.

All eight cited keys now resolve with exact case and nonempty title/author/URL. All result
`clearWith` names exist, all five `renderFun` names resolve, and `compilerMode: tame` is set.
There are no direct user-facing `warning()` calls, failure-driven `setVisible(FALSE)` calls or
nonstructural named HTML entities in this backend. Fixed table rows are added in `.init()`.

Package metadata agrees at **1.0.81.01** across `DESCRIPTION`, generated module metadata and
`CITATION.cff`. The analysis has its separate version, **0.4.1** in source. Canonical casing is
`survivalPower`; the generated wrapper calls `survivalPowerClass`, the class exists, scoped
Collate entries match tracked filenames exactly, the Rd topic/export use the canonical case,
and there are no scoped case collisions or tracked `.jmo`/`.tar.gz` archives.
[Metadata audit](survivalPower-release-review-2026-09-13/release-metadata.json),
[case audit](survivalPower-release-review-2026-09-13/packaging-case-audit.json).

All six configured submodule directories were inspected read-only. None currently contains
this analysis; `jsurvival` retains only a historical documentation file matching the name.
The test route is `PowerT`; `_updateModules.R` discovers the `_distributions.R` companion and
`.events.js` automatically. No shipped submodule version was claimed equivalent to this source,
and no neighboring analysis or submodule was changed.

Runtime: R 4.6.0, jmvcore 2.7.38, gsDesign 3.11.0, survival 3.8.11, testthat 3.3.2.
Broad package loading emits existing glmmTMB/TMB version and unavailable XQuartz-display
warnings; no focused-test warning was recorded. The library rendering contract is a focused
check, not a full `R CMD check`. A module-wide check/build/install was not performed.

During this audit, default log-rank execution took about 0.06 seconds, five-interim Cox/Pocock
about 3.1 seconds, that design with sensitivity about 5.1 seconds, and 2,000 simulated trials
about 4.9 seconds. These local executions ran alongside other audit work and are not formal
performance guarantees. The cache remains bounded and simulation/long grids checkpoint.

## 7. Remaining limitations

- **Generated release metadata is stale.** The existing `.h.R` API matches the unchanged
  options/results, so backend tests are valid, but it still identifies 0.4.0. The 0.4.1
  source has not been built/distributed/installed into a target submodule.
- **Manual desktop confirmation remains.** The harness control experiment and real binding
  parser validate this fix, but do not replace opening the packaged analysis and switching
  modes/presets in jamovi.
- Only exponential survival, uniform entry and the stated three statistical methods are
  supported. Unsupported tests/distributions/accrual/NI-margin types and crossover stop
  visibly. An RMST difference can specify an exponential HR; it does not enable an RMST test.
- Cluster calculations are simple design-effect approximations, with explicit small-cluster
  warnings; stratified calculations do not model strata. Multi-arm fixed family power uses
  a common-control normal approximation, while sequential family power is intentionally
  withheld. Holm/Dunnett are disclosed conservative Bonferroni approximations.
- Simulation validation is limited to fixed two-arm exponential log-rank/Cox designs.
  Sparse events may make asymptotic calculations unreliable; failed simulation fits and
  Monte Carlo uncertainty are disclosed. Illustrative presets and NI margins still require
  study-specific justification.
- Repaired messages have translation coverage, but legacy educational HTML and labels are
  not a claim of complete localization.

## 8. Release recommendation

**Ready after specified minor actions.** No confirmed source defect remains from this pass.
Before shipping 0.4.1:

1. Regenerate from the repository root:

   ```bash
   Rscript -e "Sys.unsetenv('ELECTRON_RUN_AS_NODE'); jmvtools::prepare(home = '/Applications/jamovi.app')"
   Rscript -e "devtools::document()"
   ```

2. Confirm generated `survivalPower.h.R` declares `c(0,4,1)` and review the generator diff.
   Build/distribute through the normal module workflow; the user retains control of moving
   the analysis out of the `PowerT` test menu.
3. Smoke-test the packaged analysis in jamovi: default effect inputs enabled; sample-size
   input enabled in power/effect/duration; NI margin enabled for NI; arm/cluster controls
   respond to design; spending enabled after an interim is added; presets recover usable
   values; dark-mode target line is visible; saved results restore and clear on invalid input.

Regeneration was deliberately left to the user because the invoked
[release-review skill](../.claude/skills/release-review-function/SKILL.md) explicitly says
**“stop and ask the user to run them”** when analysis YAML changes require generated files.
Its stated reason is that prepare/document rewrite module-wide files and can fold in other
in-flight edits. The review's source repairs and independent validation are complete; the
remaining action is now concrete and reviewable.
