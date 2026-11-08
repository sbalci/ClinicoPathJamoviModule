# Testing Checklist: IHC Heterogeneity Analysis (`ihcheterogeneity`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution | Standard valid dataset | Clean table and plot outputs populated |
| **TC-02** | Missing values handling | Dataset with 5-15% NAs | Proper omission/imputation notice and valid calculations |
| **TC-03** | Single-level factor / edge case | Factor with 1 observed level | Graceful advisory notice, no fatal crash |
| **TC-04** | Empty dataset / zero rows | Dataset with 0 rows | Error notice shown, results hidden gracefully |
| **TC-05** | Special characters in variable names | Column names with spaces, hyphens, parentheses | Correctly escaped, executed without parsing errors |
| **TC-06** | Full option permutations | All non-default options enabled | All child tables and visual layers rendered accurately |

## 2. Automated Test Execution

Sixteen files under `tests/testthat/` cover `ihcheterogeneity`:

```r
# Run all sixteen ihcheterogeneity test suites
testthat::test_dir("tests/testthat", filter = "ihcheterogeneity")
```

| File | What it covers |
| :--- | :--- |
| `test-ihcheterogeneity.R` | Core integration tests across reference and inter-regional designs. |
| `test-ihcheterogeneity-basic.R` | Smoke coverage, result object creation, default option behavior, and required-variable onboarding. |
| `test-ihcheterogeneity-arguments.R` | Full option permutation matrix (`analysis_type`, `sampling_strategy`, thresholds, variance components, sample size, spatial compartments, and narrative panels). |
| `test-ihcheterogeneity-edge-cases.R` | Missing values, small sample sizes, constant measurements, near-zero case means, special characters in column names, and ordinal `0-3+` scores. |
| `test-ihcheterogeneity-enhancements.R` | Spatial compartment comparisons, omnibus and pairwise post-hoc tests, and narrative report sentences. |
| `test-ihcheterogeneity-fixes.R` | Early numerical and notice regression fixes. |
| `test-ihcheterogeneity-Audit-Fixes.R` | Audit regressions for ICC(2,1)/ICC(3,1), within-case RMS CV, and variance components. |
| `test-ihcheterogeneity-check-fixes.R` | Schema wiring and option validation checks. |
| `test-ihcheterogeneity-i18n-checks.R` | Internationalization catalog and `.()` wrapper checks. |
| `test-ihcheterogeneity-jamovify.R` | `jmvcore` helper migration and `toNumeric` safety checks. |
| `test-ihcheterogeneity-release-review.R` | Primary-source mathematical oracle checks: McGraw & Wong (1996) ICC(2,1)/ICC(3,1), Bonett & Wright (2000) Spearman CIs, Fieller 90% TOST equivalence and Bonferroni material-bias intervals, Bland & Altman (1999) LoA CIs, and Bonett (2002) ICC sample size. |
| `test-ihcheterogeneity-release-fixes.R` | Release pass regressions for constant reference handling, missingness disclosures, and notice hygiene. |
| `test-ihcheterogeneity-release-profile-fixes.R` | Pairwise inter-regional comparisons without a reference, single-region outlier attribution, and compartment consensus grading. |
| `test-ihcheterogeneity-proportional-bias.R` | HC3 heteroscedasticity-robust regression (MacKinnon & White 1985) and Krouwer (2008) three-slope proportional-bias decision rules. |
| `test-ihcheterogeneity-release-20261019.R` | Pre-release critical review regressions (2026-10-19). |
| `test-ihcheterogeneity-presubmission-20261107.R` | Pre-submission review regressions (2026-11-07): Krouwer three-slope rule for inter-regional pairs, Allred (`0, 2-8`) ordinal scale guards across all tables, Bonett-Wright CIs and sparse-pair (`3 <= n < 5`) handling for inter-regional Spearman correlations, reference-design CV labelling, and discretization margin checks. |

## 3. QA Sign-Off Criteria

Status recorded 2026-11-07:

- [x] **testthat, all sixteen `ihcheterogeneity` files**: 100% passing (0 failures, 0 errors).
- [x] **Release gate (`tools/release_gate.py --analysis ihcheterogeneity`)**: 0 blocking findings.
- [x] **UI labels and schemas reviewed**: all 24 options in `jamovi/ihcheterogeneity.a.yaml` have matching controls in `jamovi/ihcheterogeneity.u.yaml` and match `jamovi/ihcheterogeneity.r.yaml`.

