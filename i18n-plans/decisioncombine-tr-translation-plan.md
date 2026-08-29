# `decisioncombine` Turkish Translation Plan

## Scope and implementation status

Target analysis: `decisioncombine`

Files reviewed:

- `R/decisioncombine.b.R`
- `jamovi/decisioncombine.a.yaml`
- `jamovi/decisioncombine.u.yaml`
- `jamovi/decisioncombine.r.yaml`
- `jamovi/i18n/catalog.pot`
- `jamovi/i18n/en.po`
- `jamovi/i18n/tr.po`

The backend imports `jmvcore::.` through roxygen. User-facing validation notices,
recommendation explanations, strategy labels, table labels, and plot labels are wrapped
for extraction. Dynamic values use named `{placeholder}` tokens with
`jmvcore::format()` so translators can reorder them safely. Programmatic identifiers,
result keys, option values, and internal `Positive`/`Negative` comparison values remain
untranslated.

## Terminology

Use formal medical Turkish and keep the following translations consistent:

| English | Turkish |
|---|---|
| Gold standard | Altın standart |
| Sensitivity | Duyarlılık |
| Specificity | Özgüllük |
| Positive predictive value (PPV) | Pozitif öngörü değeri (PPV) |
| Negative predictive value (NPV) | Negatif öngörü değeri (NPV) |
| Accuracy | Doğruluk |
| Balanced accuracy | Dengeli doğruluk |
| Youden's J | Youden J indeksi |
| Positive likelihood ratio | Pozitif olabilirlik oranı |
| Negative likelihood ratio | Negatif olabilirlik oranı |
| Diagnostic odds ratio | Tanısal odds oranı |
| Confidence interval | Güven aralığı |
| Complete-case analysis | Tam olgu analizi |
| Parallel strategy | Paralel strateji |
| Serial strategy | Seri strateji |
| Majority rule | Çoğunluk kuralı |
| Test pattern | Test deseni |

Preserve statistical abbreviations used in the output (`PPV`, `NPV`, `LR+`, `LR-`,
`DOR`) and preserve placeholders exactly, including their braces.

## High-priority message translations

Generated mechanically from `jamovi/i18n/catalog.pot`: every entry whose `#:` reference
lines include `R/decisioncombine.b.R`. 108 rows. A blank Turkish cell is untranslated and
is outstanding work; filled cells were carried over from the previous revision of this
plan or from `tr.po`. Some rows are shared msgids that other analyses also reference, so
a translation there changes those analyses too.

| English source | Suggested Turkish |
|---|---|
| {n} rules tie on Youden's J ({rules}); "{shown}" is displayed only because it comes first. |  |
| {variable} has {n} levels |  |
| A gold standard variable is required. Select a reference test. |  |
| A Haldane-Anscombe continuity correction of 0.5 was applied to {n} pattern(s) with at least one zero cell ({patterns}). The correction affects LR+, LR-, the diagnostic odds ratio and their confidence intervals only; sensitivity, specificity, PPV and NPV on the same rows use the observed counts. |  |
| Accuracy | Doğruluk |
| All Zero Counts |  |
| At least four cases are required for analysis. |  |
| At least four complete cases are required for combination analysis; only {used} of {total} cases remain after excluding missing values. |  |
| Balanced Accuracy |  |
| Complete-case analysis uses {used} of {total} cases ({percent}%) for the combination analysis. Cases missing the gold standard or any selected test were excluded. Individual-test tables use their own pairwise-complete denominators. If data are not missing completely at random, investigate the missingness pattern. |  |
| Continuity Correction |  |
| Decision Space - Sensitivity vs Specificity |  |
| Descriptive Youden ranking |  |
| Diagnostic Performance Comparison |  |
| DOR |  |
| epiR Package Missing |  |
| Estimate (95% CI) |  |
| Every complete case has gold standard "{level}", so this sample contains no disease-absent cases. Specificity and NPV cannot be estimated and are reported as blank. Diagnostic accuracy assessment requires both diseased and non-diseased cases. |  |
| Every complete case has gold standard "{level}", so this sample contains no disease-present cases. Sensitivity and PPV cannot be estimated and are reported as blank. Diagnostic accuracy assessment requires both diseased and non-diseased cases. |  |
| Every pattern and strategy is reported together with no adjustment for multiple comparisons. Treat the best-looking row as a hypothesis to confirm in new data, not as an established result. |  |
| Extreme Disease Prevalence |  |
| Extreme disease prevalence in the combination analysis: {percent}% ({diseased}/{observed} complete cases). PPV and NPV are highly sensitive to prevalence and may not generalize to populations with different disease rates. Sensitivity and specificity can also vary across settings and case mix, and with so few cases in one arm the likelihood ratios and diagnostic odds ratios for every pattern rest on a very small denominator. Individual-test tables use their own pairwise denominators, so their prevalence may differ. |  |
| Forest Plot - 95% Confidence Intervals |  |
| Forest Plot Not Available for Selected Statistic |  |
| gold standard |  |
| Gold Standard Has Only One Outcome |  |
| Individual Test {test} statistics use {used} of {total} cases with both the test and reference standard observed. |  |
| Insufficient Complete Cases |  |
| Insufficient Data |  |
| Invalid Counts |  |
| Invalid counts were detected for pattern "{pattern}". This combination was skipped. |  |
| Invalid counts were detected for Test {test}. The individual analysis was skipped. |  |
| Its advantage is not established: the lower bound of this rule's Youden's J ({lower}) falls at or below the next-best rule's point estimate ({runnerUp}), so the ranking may reflect sampling variation rather than a real difference. |  |
| LR- |  |
| LR+ |  |
| LR+, LR- and the diagnostic odds ratio are computed with a Haldane-Anscombe 0.5 continuity correction when a cell is zero, so they stay finite; sensitivity, specificity, PPV and NPV on the same row use the observed counts. The two therefore need not agree exactly at a zero cell. |  |
| Majority (>=2/3 pos) |  |
| Missing Level | Eksik Düzey |
| Negative |  |
| No candidate rule has an estimable Youden index with all 2-by-2 cell counts at least 5. |  |
| No Complete Cases | Tam Olgu Yok |
| No complete cases remain after removing missing data. | Eksik veriler çıkarıldıktan sonra tam olgu kalmadı. |
| No Data |  |
| No data are available. Load data before running the analysis. |  |
| No Gold Positive Level |  |
| No Gold Standard |  |
| No observations were found for pattern "{pattern}". This combination was skipped. |  |
| No Rule Performs Better Than Chance |  |
| No Test 1 |  |
| No Test 1 Positive Level |  |
| No Test 2 Positive Level |  |
| No Test 3 Positive Level |  |
| No valid observations were found for Test {test}. The individual analysis was skipped. |  |
| None of the {n} eligible candidate rules has a Youden's J above zero, so none discriminates better than chance in this sample and no rule is ranked. A rule with a negative Youden's J is anti-predictive: its result would have to be reversed to carry information. Review the positive-level assignments for the reference standard and each test before interpreting these results. |  |
| NPV |  |
| Observed sensitivity and specificity are both above 70%. |  |
| Observed sensitivity and specificity are both above 80%. |  |
| Parallel (>=1 pos) |  |
| Pattern | Desen |
| Pattern "{pattern}" produced no variation and was omitted from the combination results. |  |
| Pattern Omitted |  |
| Performance Heatmap | Performans Isı Haritası |
| Positive |  |
| Positive and negative predictive values are calculated using the sample prevalence. Interpret them cautiously if the sample does not represent the target clinical population. |  |
| PPV |  |
| PPV/NPV Interpretation |  |
| Prevalence |  |
| Removed {n} case(s) with missing values |  |
| Rows whose label is a result pattern (e.g. "+/-") are mutually exclusive groups, not decision rules: for those rows "Sensitivity" is the proportion of diseased patients showing that exact pattern, and the columns should be read that way. The named rows -- Parallel (>=1 pos), Majority (>=2/3 pos), and the all-positive pattern, which is the Serial (AND) rule -- are the strategies you can apply to a patient. |  |
| Select the disease-present level for the gold standard. |  |
| Select the positive level for Test 1. |  |
| Select the positive level for Test 2. | Test 2 için pozitif düzeyi seçin. |
| Select the positive level for Test 3. | Test 3 için pozitif düzeyi seçin. |
| Sensitivity |  |
| Serial (all pos) |  |
| Single test |  |
| Sparse Strategy Counts | Seyrek Strateji Sayımları |
| Specificity | Özgüllük |
| Strategy |  |
| Strategy Ranking Unavailable | Strateji Sıralaması Kullanılamıyor |
| Test {test} cannot be summarized because no case has both the test and reference-standard result. |  |
| Test {test} Has No Complete Cases |  |
| Test {test} Pairwise Denominator |  |
| Test 1 |  |
| Test 1 is required. Select at least one test variable. |  |
| Test 2 |  |
| Test 2 Required Before Test 3 |  |
| Test 3 |  |
| Test 3 cannot be combined without Test 2. Select Test 2 and its positive level, or remove Test 3. |  |
| Test Negative |  |
| Test Pattern |  |
| Test Positive |  |
| The epiR package is required for combination analysis. Install it with install.packages("epiR"). |  |
| The epiR package is required for diagnostic test analysis. Install it with install.packages("epiR") or disable individual test statistics. |  |
| The forest plot is not drawn for "{statistic}" because this analysis does not calculate a confidence interval for that statistic. The bar chart and heatmap can still display it. |  |
| The highest observed Youden's J among the eligible candidate rules was {youden}. |  |
| The observed results involve a trade-off between sensitivity and specificity. |  |
| The reference standard and tests must use different variables. Select a different variable for: {variables}. |  |
| The specified positive level "{level}" is not defined for variable "{variable}" ({label}). Select a level that exists in the data. |  |
| These testing strategies have a 2-by-2 cell count below 5 (smallest cell {minimum}): {strategies}. Their likelihood-ratio and diagnostic-odds-ratio estimates and confidence intervals may be unstable, and they are excluded from the candidate-rule ranking. Treat these results as exploratory and validate them in a larger independent sample. |  |
| This is a descriptive ranking of {n} candidate rule(s) with all 2-by-2 cell counts at least 5 and a Youden's J above zero; no significance test or multiplicity correction was applied. |  |
| This is a descriptive, sample-dependent ranking of exact-pattern rules and named testing strategies. It is not a clinical guide or validated recommendation. |  |
| This sample-dependent ranking is an analytical summary, not a clinical guide or validated recommendation. |  |
| Total |  |
| Value |  |
| Variable "{variable}" ({label}) has {n} levels: {levels}. Only "{positive}" is treated as positive; every other level ({others}) is counted as NEGATIVE. If any of those levels represent equivocal or indeterminate results, this recoding can bias sensitivity, specificity, predictive values, and likelihood ratios and make them difficult to interpret. Recode the variable to two levels and set equivocal results to missing if that is not what you intend. |  |
| Variables Must Be Distinct |  |
| Youden's J |  |

Long notices should be translated as complete sentences. Do not split a sentence around
a placeholder or concatenate translated fragments.

## UI and output translations

Generated mechanically from `jamovi/i18n/catalog.pot`: every entry whose `#:` reference
lines point at `decisioncombine/...` or `package/analyses/decisioncombine...` (the
`.a.yaml`, `.u.yaml` and `.r.yaml` strings) and not at the backend. 98 rows, same blank-cell
convention as above.

| English source | Suggested Turkish |
|---|---|
| 95% CI |  |
| Add a new column to the dataset containing the test combination pattern for each case (e.g., "+/+", "+/-", "-/-"). |  |
| Advanced medical diagnostic test combination analysis for categorical tests  with comprehensive clinical interpretation. This function systematically  evaluates all possible test result combinations (2-test: 4 patterns,  3-test: 8 patterns) against a gold standard using state-of-the-art  statistical methods. Features include Wilson score confidence intervals  for enhanced accuracy, performance heatmaps, decision trees, and  publication-quality visualizations with clinical decision thresholds.  Provides actionable recommendations for screening vs. confirmatory testing  strategies with detailed clinical interpretation guidelines. Essential for  evidence-based diagnostic protocol development and categorical test  validation studies. |  |
| All Patterns |  |
| All Tests Negative |  |
| All Tests Positive |  |
| Bar chart |  |
| Bar Chart - Performance Comparison |  |
| Color-coded heatmap showing all diagnostic metrics for each test pattern |  |
| Combine and evaluate test patterns |  |
| Combine Medical Decision Tests |  |
| Combine Medical Decision Tests 1 |  |
| Combine tests and evaluate performance |  |
| Contingency Table |  |
| Count |  |
| Counts and diagnostic performance metrics for each test combination pattern and clinical strategy, including prevalence, balanced accuracy, Youden's J, likelihood ratios, and diagnostic odds ratios |  |
| Cross-tabulation showing how test combination patterns align with gold standard results |  |
| Decision |  |
| Decision space (sensitivity vs specificity) |  |
| Decision Space: Sensitivity vs Specificity |  |
| Decision-space scatter plot positioning each test pattern by its sensitivity and specificity, with point size scaled by Youden's J |  |
| Default Metric Set |  |
| Descriptive candidate-rule ranking |  |
| Descriptive Candidate-Rule Ranking |  |
| Diagnostic OR |  |
| Diagnostic Statistics |  |
| Disease Present Level |  |
| Display a decision-space scatter plot positioning each test pattern by its sensitivity and specificity, with point size scaled by Youden's J. |  |
| Display diagnostic statistics for each individual test before combinations. |  |
| Display forest plot showing confidence intervals for key diagnostic metrics across patterns. |  |
| Display frequency distribution tables for the gold standard and cross-tabulation of test results. |  |
| Display grouped bar chart comparing performance metrics across test combinations. |  |
| Display heatmap showing all metrics for all test patterns with color-coded performance values. |  |
| Display Options |  |
| Estimate | Tahmin |
| Filter by pattern type |  |
| Filter by statistic |  |
| Filter the bar chart, heatmap and forest plot by pattern type: all patterns, all tests positive (+/+, +/+/+), all tests negative (-/-, -/-/-), or mixed/discordant patterns. Named strategy rows are not result patterns and are excluded whenever a specific pattern type is selected. The performance tables always show every pattern, and the decision-space plot always shows every row so patterns remain comparable. |  |
| Filters |  |
| First diagnostic test to evaluate. Must have at least 2 levels. |  |
| FN |  |
| Forest plot |  |
| Forest Plot - Confidence Intervals |  |
| Forest plot displaying 95 percent confidence intervals for key diagnostic metrics |  |
| FP |  |
| Frequency distribution of the gold standard (reference) test showing counts and percentages for each level |  |
| Frequency tables |  |
| Gold Negative |  |
| Gold Positive |  |
| Gold Standard (Reference Test) |  |
| Gold Standard Frequency Distribution |  |
| Grouped bar chart comparing sensitivity, specificity, PPV, NPV, and accuracy across test combinations |  |
| Heatmap |  |
| Heatmap - All Metrics by Pattern |  |
| Highest-Ranked Rule |  |
| Individual test statistics |  |
| Interpretation |  |
| Level |  |
| Likelihood Ratios with 95% Confidence Intervals |  |
| Log-scale 95 percent confidence intervals for LR+, LR- and the diagnostic odds ratio. These are ratios on an unbounded scale, so they are reported separately from the proportions above rather than sharing a column with them. |  |
| Lower |  |
| meddecideT |  |
| Mixed/Discordant |  |
| Notices |  |
| Optional third test for 3-way combination analysis (8 patterns). |  |
| Pattern type |  |
| Percent |  |
| Positive Level |  |
| Proportions with 95% Confidence Intervals |  |
| Rank eligible exact-pattern rules and named parallel, serial, and majority strategies descriptively by observed Youden's J. This sample-dependent summary is not a clinical guide or validated recommendation. |  |
| Row |  |
| Sample-dependent descriptive ranking of eligible exact-pattern rules and named testing strategies by observed Youden index; this is not a clinical guide or validated recommendation |  |
| Second diagnostic test for combination analysis. Leave empty for single test only. |  |
| Select one metric, or use the plot-specific default metric set. The bar chart defaults to sensitivity, specificity, PPV, NPV, and accuracy; the heatmap also includes prevalence, balanced accuracy, and Youden's J. The forest plot supports sensitivity, specificity, PPV, NPV, accuracy, LR+, LR-, and diagnostic OR, for which confidence intervals are available. Unsupported forest selections produce an explanatory notice rather than silently showing other statistics. The decision-space plot has fixed sensitivity and specificity axes and is unaffected. |  |
| Select the level indicating disease presence (e.g., "1", "positive", "malignant"). |  |
| Select the level representing a positive result for Test 1. |  |
| Select the reference standard (e.g., biopsy, final diagnosis). This represents the reference classification. Binary variables are intended; with additional levels, all non-positive levels are grouped as negative and a warning is shown. |  |
| Selection Method |  |
| Statistic |  |
| Systematic evaluation of diagnostic test combinations. Analyzes all possible test result patterns (2-test: 4 patterns, 3-test: 8 patterns) against a gold standard and summarizes named parallel, serial, and majority strategies. Calculates sensitivity, specificity, predictive values, likelihood ratios, accuracy, and uncertainty intervals. Descriptive rankings are sample-dependent analytical summaries, not clinical guides or validated recommendations. |  |
| Test 1 (Required) |  |
| Test 1 Performance |  |
| Test 1 Positive Level |  |
| Test 2 (Required for Combinations) |  |
| Test 2 Performance |  |
| Test 2 Positive Level |  |
| Test 3 (Optional) |  |
| Test 3 Performance |  |
| Test 3 Positive Level |  |
| Test Combination Performance | Test Kombinasyonu Performansı |
| Test pattern column | Test deseni sütunu |
| Test Pattern Column |  |
| Test Results Cross-Tabulation |  |
| TN |  |
| TP |  |
| Upper |  |
| Visualizations |  |
| Wilson score 95 percent confidence intervals for sensitivity, specificity, PPV, NPV and accuracy, shown as percentages to match the combination table above. Likelihood ratios and the diagnostic odds ratio are unbounded ratios rather than proportions, so they appear in their own table below. |  |

## Quality-assurance checklist

- [x] Backend translation import is declared through roxygen.
- [x] Validation errors and warnings are wrapped.
- [x] Recommendation rationale sentences and placeholders are wrapped.
- [x] Plot titles, axes, legends, captions, and metric display labels are wrapped.
- [x] YAML labels are human-readable and extractable.
- [x] Internal option/result identifiers remain stable.
- [ ] Update `catalog.pot`, `en.po`, and `tr.po` after the final source changes. NOT DONE.
  Verified 2026-08-28 against the working tree: 16 `.()` strings in `R/decisioncombine.b.R`
  are absent from all three catalogs, and a further set of msgids whose only reference is
  `R/decisioncombine.b.R` are still in the catalogs but no longer occur in the source
  (18 at the time of writing; the backend is still being edited, so re-count before extracting).
  Both tables above therefore describe the catalog as extracted, not the current source.
- [x] Preserve named placeholders during extraction; new Turkish entries remain queued for translation review.
- [ ] Translate the queued entries. Of the 206 `decisioncombine` msgids in `catalog.pot`,
  all 206 exist in `en.po` and `tr.po`, but `tr.po` carries only 2 non-empty translations
  (`Accuracy`, `Specificity`) and `en.po` carries 0.
- [ ] Open the analysis under Turkish locale and check long notices and plot labels for clipping.
- [ ] Confirm decimal and percent formatting follows jamovi locale behavior.

## Catalog maintenance

From the package root, extract after source and YAML changes:

```r
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")
```

Catalog updates must retain existing translations. Any new untranslated entry should use
the terminology above and should be reviewed in the running jamovi analysis before release.
