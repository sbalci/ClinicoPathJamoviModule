# Statistical review: Stoieva and Fraga — IHC accuracy and reader reproducibility

Review date: 2026-10-03. Repository: ClinicoPathJamoviModule, HEAD `b063b98b0`, with the existing working tree inspected. Deliverable: article appraisal and implementation roadmap; no analysis code or menu routing changed.

Follow-up: [staged implementation plan](./Stoieva-2026-IHC-reader-reproducibility-implementation-plan.md), including data preparation, compatibility decisions, phase dependencies, and acceptance criteria.

## Article summary and scope

**The practical recommendation is sound: assess diagnostic accuracy and independent-reader agreement together, retaining disagreements at the clinical cutoff.** Nonreporting of agreement does not establish poor agreement or prove that independent readings were never performed. The proposed composite should remain exploratory.

This systematic review examined dermatopathology IHC diagnostic-biomarker studies published during 2015–2025. PubMed, Embase, and Web of Science searches yielded 1,461 abstracts and 84 included studies. Agreement reporting was absent in 73 studies (86.9%); Cohen's kappa was the most common named agreement statistic. The geometric mean of balanced accuracy and Cohen's kappa was calculable for three studies, with values 0.62–0.93. These are study-level reporting findings, not patient-level estimates of marker performance. [Article abstract and record](https://pubmed.ncbi.nlm.nih.gov/42358041/)

**Evidence boundary:** the abstract, PubMed XML, Crossref metadata, Europe PMC record, and publisher abstract page were inspected. Full methods, tables, figures, supplements, and extraction data were not retrieved. Consequently, this is an **abstract-based appraisal with a source-code coverage review**, not a full-text statistical audit. Recommendations below are reviewer proposals unless explicitly identified as article findings.

## Verified citation

| Field | Value |
|---|---|
| Title | Underreporting of Interrater Agreement in Diagnostic Accuracy Studies of Immunohistochemistry in Dermatopathology: A Systematic Review |
| Authors | Margaryta Stoieva; Garth Fraga |
| Journal | The American Journal of Dermatopathology |
| Year; volume; issue; pages | 2026; 48; 9; 710–716 |
| Issue date | September 2026 |
| Electronic publication | June 4, 2026 |
| DOI | [10.1097/DAD.0000000000003334](https://doi.org/10.1097/DAD.0000000000003334) |
| PMID | [42358041](https://pubmed.ncbi.nlm.nih.gov/42358041/) |
| Publisher | Wolters Kluwer Health; Crossref depositor: Ovid Technologies |
| ISSN | Print 0193-1091; electronic 1533-0311 |
| Retraction/correction check | No retraction publication type or linked correction in the retrieved PubMed XML; Pendar also returned `is_retracted: false`. This is a record check as of the review date, not a guarantee against later notices. |
| MeSH | Immunohistochemistry; Observer Variation; Humans; Reproducibility of Results; Dermatology; Skin Diseases—diagnosis/pathology; Pathology, Clinical |

Publication details agree across [PubMed](https://pubmed.ncbi.nlm.nih.gov/42358041/), the [Crossref DOI record](https://api.crossref.org/works/10.1097/DAD.0000000000003334), and the [publisher landing page](https://www.ovid.com/10.1097/DAD.0000000000003334). Use the author spelling supplied by these primary records; an aggregator expanded Fraga's given name.

BibTeX generated from the verified Crossref metadata:

```bibtex
@article{Stoieva2026InterraterIHC,
  author  = {Stoieva, Margaryta and Fraga, Garth},
  title   = {Underreporting of Interrater Agreement in Diagnostic Accuracy Studies of Immunohistochemistry in Dermatopathology: A Systematic Review},
  journal = {The American Journal of Dermatopathology},
  year    = {2026},
  volume  = {48},
  number  = {9},
  pages   = {710--716},
  doi     = {10.1097/DAD.0000000000003334},
  pmid    = {42358041}
}
```

## Source limitations and skipped material

| Source/material | Outcome | Consequence |
|---|---|---|
| User's PubMed URL | Direct browser extraction was incomplete; indexed content and official EFetch XML recovered the abstract and metadata. | Article summary and bibliographic verification are supported. |
| Publisher full text | The DOI landing page redirected to an abstract with an access prompt. Europe PMC listed subscription access and no PMC full text. | No page-specific appraisal of methods or tables. |
| Supplements and study-level extraction data | Not supplied or recovered. Europe PMC did not list a supplement. This does not establish that none exists. | Cannot reproduce inclusion decisions, individual kappas, or the three composite values. |
| MarkItDown fallback | Installed package failed to expose `MarkItDown`; a standard-library HTML parser recovered the publisher abstract. | Conversion did not uncover full text. Publisher machine-generated “In Brief” text was excluded from evidence. |

For a subsequently obtained full-text PDF, a conversion command using a working MarkItDown installation is:

```sh
markitdown "Stoieva-2026-full-text.pdf" > "Stoieva-2026-full-text.md"
```

This command was not executed. No PDF was available to convert.

## Extracted statistical methods

| Method | Role | What is established | What remains unassessed | Source location |
|---|---|---|---|---|
| Counts and proportions of reporting | Primary | Study-level descriptive audit | Extraction definitions, duplicate cohorts, screening/extraction safeguards | Abstract |
| Sensitivity and specificity | Measures reported by included studies | Accuracy and agreement are distinct targets | Reference standards, reader-specific estimates, cutoff selection, precision | Abstract |
| Cohen's kappa | Agreement measure in included studies | Most frequently named quantitative agreement measure | Weighting, reader design, complete-case rules, confidence intervals | Abstract |
| Qualitative concordance | Other reporting identified | Narrative agreement descriptions were encountered | How narrative and numerical reports were classified | Abstract |
| Balanced accuracy and kappa geometric mean | Exploratory secondary analysis | Composite definition and a range across three studies | Which reader accuracy estimate was used, matching populations/cutoffs, handling of negative kappa, uncertainty | Abstract |

No hypothesis tests, regression models, multiplicity corrections, pooled diagnostic meta-analysis, or software versions can be established from the accessible abstract. Their absence from the abstract is not evidence that they were omitted from the full paper.

## Critical evaluation

**Overall assessment: appropriate descriptive question; full methodological quality unassessable.** The accessible evidence supports improved agreement reporting. It does not validate a composite score or establish a clinical acceptance threshold. A major concern would arise if the composite were used for clinical ranking or acceptance without further validation; this review does not claim that the authors did so.

| Aspect | Assessment / score | Evidence and recommendation |
|---|---|---|
| Design–method alignment | Appropriate in principle; not fully scored | A study-level audit fits a reporting question. Verify eligibility, database search dates, duplicate-study handling, independent screening/extraction, and the operational definition of agreement reporting. |
| Assumptions and diagnostics | Unassessable | Check rating scale, reader allocation, marginal category frequencies, and clustering. Normality tests are not the relevant requirement for categorical agreement. |
| Sample size and power | Partly assessable; not fully scored | An exhaustive review does not need a conventional patient-study power calculation. Search completeness and denominator validity matter. Three computable examples are insufficient to validate a new metric. |
| Multiplicity | Not applicable to the described counts/range | Assess multiplicity only if full text contains subgroup testing, multiple marker comparisons, or cutoff searches. |
| Model specification and confounding | Regression adjustment not applicable; composite specification unassessable | Define the readers, reference standard, case set, scale, threshold, and accuracy aggregation entering each composite. |
| Missing data | Partly assessable; not fully scored | Missing reporting is the primary outcome; missing inputs also select the composite subset. Do not replace an unreported kappa with zero. Distinguish qualitative reporting from an estimable coefficient. |
| Effect estimates and confidence intervals | Unassessable beyond point summaries | A range across studies is not a confidence interval. Inspect uncertainty for component estimates and any proposed composite. |
| Validation and calibration | Composite validation unassessable; predictive calibration not applicable | Mathematical characterization, simulation, and independent empirical evaluation are needed before interpreting composite thresholds. |
| Reproducibility and transparency | Unassessable beyond databases and publication years | Request/check complete search strategies, screening flow, exclusion reasons, extraction data, formulas, and code/software. |

The playbook's 0–2 rubric means 2 = adequate evidence, 1 = minor demonstrated issues, and 0 = major demonstrated issues. **No total out of 18 is assigned:** unobserved and inapplicable domains cannot honestly be turned into numerical quality scores.

Use [PRISMA 2020](https://www.prisma-statement.org/prisma-2020-statement) to assess the review's reporting. For the underlying primary studies, [STARD 2015](https://pmc.ncbi.nlm.nih.gov/articles/PMC4623764/) covers diagnostic-accuracy reporting and [GRRAS](https://www.equator-network.org/reporting-guidelines/guidelines-for-reporting-reliability-and-agreement-studies-grras-were-proposed/) covers reliability/agreement reporting. These reporting frameworks do not replace a design-specific risk-of-bias assessment. Abstract omissions do not establish noncompliance.

### Mathematical appraisal of the proposed score

Writing sensitivity and specificity as proportions,

\[
BA=(Se+Sp)/2,\qquad KBS=\sqrt{BA\,\kappa}.
\]

The following are reviewer-derived properties of that definition, not reproduced findings or allegations about the authors' calculations:

1. **Negative kappa:** when `BA > 0` and `kappa < 0`, the square root is not real. Degenerate reader marginals can also make kappa itself undefined. Never silently use an absolute value, truncate negative kappa to zero, or omit such cases from a comparison.
2. **Zero collapse:** `kappa = 0` makes the score zero for every finite balanced accuracy. Different accuracy profiles become indistinguishable.
3. **No familiar clinical scale:** `BA = 0.5` and `kappa = 1` produce `KBS = 0.7071`. Readers can agree perfectly while their classifications are uninformative against the reference. Kappa interpretation bands and accuracy percentages cannot be transferred to this score.
4. **Different components can give the same result:** `(BA, kappa) = (0.95, 0.50)` and `(0.50, 0.95)` both produce `0.6892`. Even BA alone hides how sensitivity trades against specificity. Equal logarithmic weighting does not establish clinical interchangeability.
5. **Case mix still matters:** kappa depends on reader marginals and category prevalence, so combining it with BA does not remove those dependencies. Always retain the contingency table and marginal frequencies. [Byrt, Bishop and Carlin, 1993](https://pubmed.ncbi.nlm.nih.gov/8501467/)
6. **Joint uncertainty:** estimates obtained from the same cases are dependent. A joint bootstrap must retain the reference and all reader measurements together; resample independent patients/clusters when necessary. Generalization to a population of readers requires a design that also accounts for reader sampling. Combining separate confidence-limit endpoints is not a valid substitute.

The computable subset may differ systematically from studies without agreement inputs. The direction and magnitude of this selection cannot be determined here. These observations support exploratory use with all components shown; they do not support a new reporting requirement or a biomarker ranking system.

## Study design and reporting recommendation for a new marker

Prespecify the intended use, case spectrum, reference standard, scoring instructions, reader expertise/training, cutoff, and equality rule. Obtain independent readings before consensus or adjudication, record what clinical/reference information readers can see, and retain original scores and indeterminate results. Report reader-specific accuracy separately from any consensus-reader accuracy. STARD specifically addresses positivity definitions, information available to readers, missing data, and estimate precision. [STARD explanation and elaboration](https://pmc.ncbi.nlm.nih.gov/articles/PMC5128957/)

| Layer | Recommended output | Interpretation |
|---|---|---|
| Accuracy versus reference | TP/FP/FN/TN, disease-positive sensitivity, specificity, and 95% CIs for each reader | Whether each reader's interpretation identifies the target condition |
| Agreement at the clinical cutoff | Reader-by-reader 2×2 table, observed agreement, Cohen kappa with CI for two readers, and positive/negative specific agreement | Whether readers make the same clinically relevant call |
| Original ordinal scores | Full cross-tabulation and explicitly justified weighted kappa, preserving level order | How far scores differ; an adjacent-category difference can still cross the clinical cutoff |
| Disagreement location | Counts and directions of cutoff crossings; original scores and case IDs; denominator overall and in a prespecified near-cutoff band | Where reproducibility can change a clinical classification |
| Missing and indeterminate readings | Counts by reader, reasons, available-case and shared-case denominators | Whether different outputs describe the same cases |

For two binary readers with counts `a = both positive`, `b = reader 1 positive only`, `c = reader 2 positive only`, and `d = both negative`, positive specific agreement is `2a/(2a+b+c)` and negative specific agreement is `2d/(2d+b+c)`. These are symmetric agreement measures, not sensitivity and specificity against a reference. A zero denominator means unavailable, not perfect agreement.

Define near-cutoff membership in advance—for example, either reader's numeric score within `delta` of the threshold for each reader pair—and report that rule and denominator. These conditional results are descriptive. If difficult or borderline cases are deliberately enriched, present them separately from a representative practice sample. Plan both agreement precision and adequate reference-positive/reference-negative counts for accuracy; one kappa-based sample-size calculation cannot satisfy all objectives. GRRAS supports explicit subject/reader selection and measurement design. [GRRAS](https://www.equator-network.org/reporting-guidelines/guidelines-for-reporting-reliability-and-agreement-studies-grras-were-proposed/)

For continuous scores, choose agreement methods suited to the measurement scale rather than categorizing solely to obtain kappa. For more than two readers, select a multireader estimand appropriate to the reader design; Cohen's two-reader statistic and the paper's composite do not automatically generalize.

## ClinicoPath catalog and coverage matrix

All 390 sets of analysis/options, results, UI, and backend files were scanned for relevant methods; matching implementations were then inspected. **Coverage below concerns current source capability.** Production routing is not proof of release validation. `T` denotes test routing, while `P` and `D` denote pending/draft analyses. No menus were changed.

Legend: ✅ equivalent component calculation exists; 🟡 manual steps, restricted estimands, or development-only workflow; ❌ no identified implementation. Proposed study extensions are distinguished from methods established in the article.

| Method or workflow | Function(s) and current route | Coverage | Exact limitation or workaround |
|---|---|:---:|---|
| Article: descriptive reporting counts | `crosstable` (Exploration); standard jamovi frequencies | ✅ | Requires one coded row per included study. Study data were not retrieved, so only reported arithmetic was checked. |
| Article: sensitivity/specificity from a 2×2 table | `decisioncalculator` (meddecide production route) | ✅ | TP/TN/FP/FN input; optional CIs. Counts cannot recover interreader agreement or identify cases. |
| Article: balanced accuracy | `decisioncalculator` | ✅ point estimate | Its result explicitly labels BA confidence intervals as not computed. |
| Article: Cohen kappa and reader cross-tabulations | `agreement` (meddecideT), `pathagreement` (OncoPathT) | ✅ source / 🟡 deployment | Implemented agreement methods; currently test-routed. Actual original-paper weighting/CI specifications remain unknown. |
| Article: qualitative agreement descriptions | Narrative reporting | 🟡 | Cannot convert a statement of concordance into an estimable kappa without paired counts/data. |
| Article: proposed kappa-balanced score | Components available separately | ❌ dedicated output / 🟡 manual | No identified KBS implementation. Manual calculation requires compatible cases, readers, scale, cutoff, and an explicit choice of accuracy estimate. |
| Extension: per-reader binary accuracy | `decision` and `decisioncompare` (meddecideT) | 🟡 | Separate reader runs, or up to three tests against one reference. Inspect available-case versus paired denominators. |
| Extension: integrated reference and agreement report | `pathagreement` (OncoPathT) | 🟡 | Existing per-reader sensitivity/specificity are macro-averaged over reference categories, including binary data; they are not disease-positive estimates. |
| Extension: weighted ordinal and category-specific agreement | `agreement`, `pathagreement` | 🟡 | Point-estimate paths exist in test menus. Specific-agreement CI implementation needs targeted validation; do not equate ordinal agreement with cutoff agreement. |
| Extension: prespecified score cutoff | `categorize` (Exploration), `enhancedROC` (meddecide) | 🟡 pipeline | Manual score categorization/threshold accuracy exists; no integrated cutoff-disagreement report. Apply the same direction and equality rule to every reader. |
| Extension: disagreement case list | `pathagreement` | 🟡 | Existing list reports dissent from the modal category and shows at most 50 cases; no prespecified numeric near-cutoff band. |
| Extension: agreement sample-size planning | `kappaSizeCI`, `kappaSizePower`, `kappaSizeFixedN` (Power, routed to meddecide) | 🟡 design-specific | Existing tools, not missing functionality. `kappaSizeCI` uses common-correlation/Fleiss-type unweighted kappa; Cohen equivalence requires two readers with equal marginals. |

`cohenskappa` is draft-routed (`meddecideExtraD`) and its weighted paths are not a working substitute: it passes string weights to `psych::cohen.kappa()` and extracts the unweighted returned fields. Nearby IHC/threshold drafts likewise do not establish production coverage. `diagnosticmeta` should not be recommended merely because this article is a systematic review; pooling diagnostic performance is a different task.

### Implementation evidence

| Finding | Source anchors |
|---|---|
| Count-based diagnostic calculator and BA | [Options](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/decisioncalculator.a.yaml:40); [BA interval limitation](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/decisioncalculator.b.R:91) |
| Per-case accuracy with explicit class selection | [decision options](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/decision.a.yaml:28); [comparison implementation](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/decisioncompare.b.R:1173) |
| Agreement and specific agreement | [Agreement options](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/agreement.a.yaml:1204); [specific-agreement formula and approximate CI](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/agreement.b.R:4394) |
| Macro-averaged per-reader accuracy | [pathagreement backend](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathagreement.b.R:2139) |
| Capped disagreement list | [pathagreement case extraction](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathagreement.b.R:2027) |
| Weighted-kappa draft defect | [cohenskappa backend](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/cohenskappa.b.R:197) |
| Prespecified ROC cutoffs | [enhancedROC backend](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/enhancedROC.b.R:1477) |
| Manual cutoff equality convention | [categorize options](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/categorize.a.yaml:212) |
| Kappa precision-planning estimand | [kappaSizeCI explanation](/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/kappaSizeCI.b.R:711) |

A practical current-source sequence is `categorize` for prespecified binary calls → `decision` per reader or `decisioncompare` for reference accuracy → `agreement`/`pathagreement` for original-score and binary agreement → explicit near-cutoff subgroup reporting. The accuracy and agreement steps here require test-routed analyses. For published aggregate data alone, `decisioncalculator` is useful but cannot reconstruct missing reader agreement.

## Gap analysis and concrete implementation roadmap

These are proposed edits, **not implemented changes**. They extend existing analyses; generated `.h.R`/`.Rd` files should be regenerated through the normal tools rather than edited manually. Existing test routing should remain until release checks are satisfied.

### Priority 1: explicit binary per-reader reference accuracy

**Target:** `pathagreement`. Preserve the existing macro-category estimand and add a separately named binary view. For binary data, macro-averaging gives `(Se + Sp)/2` for both sensitivity and specificity, so simply relabeling that result as disease-positive accuracy would be incorrect.

| File | Proposed edit |
|---|---|
| `jamovi/pathagreement.a.yaml` | Add `referenceMetricMode` (`macro`, `binary`), `referencePositive`, `referenceNegative`, and `readerPositiveLabel`/`readerNegativeLabel` strings for already categorical reader columns. For numeric cutoff mode below, derived calls already have explicit positive/negative coding. Require exactly two selected reference categories; count other/indeterminate values as excluded. |
| `R/pathagreement.b.R` | Branch before macro averaging. Calculate TP/FP/FN/TN, class denominators, Se/Sp, BA, and method-labelled intervals per reader. Keep reader-reference kappa separate from reader-reader kappa. Report both per-reader available-case N and an optional shared-case analysis; never silently align different populations. |
| `jamovi/pathagreement.r.yaml` | Add `binaryAccuracyTable`: reader, positive/negative labels, TP/FP/FN/TN, missing/excluded N, Se/Sp with CIs, BA and its interval status. Give the existing output an explicit macro-average title. |
| `jamovi/pathagreement.u.yaml` | Add the mode selector and reference `LevelSelector` controls; show categorical-reader label fields only in categorical binary mode. Enable the existing reference target for this workflow. |

Schema fragment under `options:`:

```yaml
- name: referenceMetricMode
  title: Reference accuracy summary
  type: List
  options:
    - {name: macro, title: Macro average across categories}
    - {name: binary, title: Disease-positive binary accuracy}
  default: macro
- name: referencePositive
  title: Disease present
  type: Level
  variable: (referenceStandard)
- name: referenceNegative
  title: Disease absent
  type: Level
  variable: (referenceStandard)
```

`Level` options must have no `default`; generated R callers must supply them explicitly, including `NULL` where the selected mode permits it. Validate that positive and negative labels are distinct and observed/declared as appropriate. Do not rely on alphabetical level order.

**Validation:** a hand-counted fixture with TP=9, FN=1, TN=6, FP=4 must show Se=0.90 and Sp=0.60, whereas both macro averages are 0.75. Cross-check component intervals with `stats::binom.test()` or the selected `epiR` method. Test missing references, missing readers, third/indeterminate categories, reversed labels, and absent reference-positive or reference-negative cases.

### Priority 1: clinical-cutoff disagreement and joint uncertainty

**Target:** `pathagreement`, using the binary accuracy path above when a reference is supplied.

| File | Proposed edit |
|---|---|
| `.a.yaml` | Add `cutoffAnalysis` Bool (default false), numeric `readerScores` Variables, `clinicalCutoff` Number, `positiveRule` List (`ge`, `gt`, `le`, `lt`), `nearCutoffWidth` nonnegative Number, and optional `clusterID` Variable. Reuse `caseID`, `bootstrapSamples`, and `seed`; add an explicit cutoff-interval toggle. |
| `.b.R` | Route numeric mode before existing factor-only validation. Preserve raw scores and row IDs. Derive calls once with the chosen inequality. For each pair, use both-observed cases for agreement; define near-band membership as either observed score within the stated width. Keep ordinal and numeric-distance concepts separate. Resample entire patient clusters, or cases if independent, preserving all readings and reference labels. Expose the effective denominator for every estimate. |
| `.r.yaml` | Add `cutoffPairTable` with pair, band, N, four counts, discordant N/rate, observed agreement, kappa/CI, and positive/negative agreement/CI. Add `cutoffCaseTable` with case ID, pair, original scores, calls, band flag, and disagreement direction. Show total discordant N even when the displayed list is capped. |
| `.u.yaml` | Add a collapsed “Agreement at clinical cutoff” panel with numeric targets, cutoff, inequality, band width, optional cluster ID, interval toggle and resample controls. Use `TextBox` for numeric options. Display the actual inequality in results. |

Core derivation sketch, after input validation:

```r
calls <- switch(positive_rule,
  ge = scores >= cutoff,
  gt = scores > cutoff,
  le = scores <= cutoff,
  lt = scores < cutoff
)
observed <- !is.na(scores[, i]) & !is.na(scores[, j])
near <- observed & (
  abs(scores[, i] - cutoff) <= band_width |
  abs(scores[, j] - cutoff) <= band_width
)
discordant <- observed & calls[, i] != calls[, j]
```

This is a sketch for numeric scores, not a complete backend. Reject nonfinite values deliberately, preserve missing values, and never infer numeric distances from nominal factor codes. The near-band estimand changes the case mix and must be labelled as conditional.

**Validation:** exact-cutoff ties for all four inequalities, reversed direction, two and three readers, empty bands, asymmetric margins, all-one-category readers, incomplete pairs, repeated specimens per patient, and omitted cluster IDs. Compare counts with hand tabulations and kappa with an independent implementation. Use seeded simulations to assess interval coverage for ordinary and boundary cases. Report undefined bootstrap draws; do not silently discard them and claim nominal coverage. Preserve reproducibility across output toggles and `.omv` reopening.

### Priority 2: validate existing agreement uncertainty and consolidate draft coverage

**Target:** `agreement` and, only if retained, `cohenskappa`.

| File type | Proposed edit |
|---|---|
| `.a.yaml` | Add an explicit `specificAgreementCIMethod` choice after selecting and validating a paired-data method; reuse confidence level and seed. Keep experimental options off by default. |
| `.b.R` | Replace or qualify the current effective-N Wilson approximation for specific agreement. Evaluate a paired-multinomial method or case/cluster bootstrap with stated boundary behavior. For the draft `cohenskappa`, pass numeric weight matrices to `psych::cohen.kappa()` and extract `weighted.kappa` and corresponding variance, or consolidate into the existing agreement engine. |
| `.r.yaml` | Label the estimator, CI method, N, and unavailable estimates; retain component tables. |
| `.u.yaml` | Expose the CI method and any needed cluster target; explain ordinal weights without automatic clinical acceptance bands. |

**Validation:** hand-computable ordered tables; linear versus quadratic weights; unequal margins; zero positives/negatives; small N; simulation coverage. The local weighted API check with `psych` 2.6.5 reproduced `non-numeric argument to binary operator` for both current string-weight calls. This is a scoped finding, not a complete audit of either analysis.

No new agreement sample-size analysis is proposed. The existing `kappaSize*` options cover specific planning questions; documentation should direct unequal-marginal, weighted, clustered, and broader reader-population designs to design-specific planning or simulation.

### Priority 3: optional exploratory composite

**Target:** `pathagreement`, after the preceding workflow is validated and the desired reader/accuracy estimand is specified.

| File | Proposed edit |
|---|---|
| `.a.yaml` | Add `showKappaBalanced` Bool, default false. Require an explicit documented mapping from the selected reader pair to the accuracy estimate. Do not invent the original paper's aggregation rule. |
| `.b.R` | Compute only from compatible component estimates on a common case set. Accept finite components in [0,1]; negative/undefined kappa produces an unavailable result and explanation. Preserve valid zero-valued components. If estimating uncertainty, use joint resampling and predefine treatment of invalid draws. |
| `.r.yaml` | Add an “Exploratory kappa-balanced score” table with both components, Se/Sp, reader pair, accuracy definition, N, cutoff/scale, composite, and interval status. |
| `.u.yaml` | Place the opt-in control under an exploratory section, with no clinical grade or ranking controls. |

Point-calculation sketch:

```r
kappa_balanced <- function(ba, kappa) {
  if (length(ba) != 1L || length(kappa) != 1L ||
      !is.finite(ba) || !is.finite(kappa) ||
      ba < 0 || ba > 1 || kappa < 0 || kappa > 1) {
    return(NA_real_)
  }
  sqrt(ba * kappa)
}
```

The UI must explain an unavailable result rather than display an unexplained blank. For jamovi tables, use the established missing-cell convention; do not fabricate CIs. Numerical guards alone do not validate the score. Avoid borrowing another coefficient's interpretation categories or substituting weighted/multireader kappa without separately defining and evaluating that new metric.

## Dependencies, performance, and test plan

No new runtime package is necessary for the proposed core workflow. Existing dependencies include `irr`/`irrCAC` for relevant agreement estimators, `epiR` for diagnostic summaries, `boot` for resampling, and `ggplot2` for optional plots; `stats` handles exact binomial checks. `kappaSize` already supports the current planning tools. Use only APIs appropriate to the selected estimand, and ensure dependencies remain declared in every destination module when work is later promoted.

| Test level | Planned checks |
|---|---|
| Numerical | Hand-counted 2×2 and ordinal fixtures; reader swapping; explicit positive labels; asymmetric Se/Sp; composite examples and invalid domains |
| Data integrity | Shared versus available-case masks; missing/indeterminate values; empty strata; persistent original row IDs; duplicate specimens within patients |
| Inferential | Interval coverage simulations, case/cluster resampling, few-cluster behavior, reader-population versus fixed-reader target, undefined bootstrap draws |
| Integration | Targeted testthat files; schema compilation; real option/result wiring; output invalidation; `.omv` save/reopen; documented reproducible examples |
| Performance | Stress-test 50,000 cases × 50 readers without bootstrap first (1,225 pairs); evaluate selected-pair bootstrapping separately. Avoid materializing all case-by-pair rows or storing raw study data/fits in image state; checkpoint long loops and cap displayed case lists transparently. |
| Release | When code changes are implemented, run focused tests and required prepare/document checks, then release gates before promotion. This source review is not release validation. |

Suggested future tests: `test-pathagreement-binary-accuracy.R`, `test-pathagreement-clinical-cutoff.R`, and focused extensions to existing agreement validation tests. These tests were not created as part of this review.

**Checks actually performed:** citation cross-verification; source/catalog inspection; basic article arithmetic and composite examples in R 4.6.0; bounded checks of the draft weighted-kappa calls; parsing of the proposed YAML fragment; local-link and code-fence checks. No full-paper numbers were reproduced, no complete jamovi runtime validation was performed, and no package build was needed for this Markdown-only deliverable.

As a reviewer calculation, `73/84 = 86.9048%`. A conventional independent-binomial Wilson interval would be 78.1%–92.5%, but the review is not established to be a random sample of a larger publication population. This interval is therefore only an illustrative calculation, not an article result or a remedy for search/selection bias.

## Prioritized backlog

1. **High impact, moderate effort:** explicit disease-positive per-reader accuracy and visible denominators in `pathagreement`.
2. **High impact, moderate effort:** clinical-cutoff disagreement, near-cutoff counts, and preservation of independent readings.
3. **High impact, moderate effort:** validated agreement uncertainty, scoped draft repairs, and release-readiness checks for existing tools.
4. **High impact, low effort:** a reproducible independent-reader example and reporting guide using existing sample-size tools with their actual assumptions.
5. **Exploratory impact, additional validation burden:** opt-in composite only after its inputs and intended interpretation are defined.

Priorities reflect clinical relevance and reuse across visually scored pathology markers; no separate bibliometric survey of method frequency was conducted. New composite output is lower priority than reporting the established components correctly.

## Skills and agents invoked

| Skill | Use |
|---|---|
| `review-article-stats` | Canonical article-review workflow, coverage mapping, roadmap, and saved report |
| `citation-management` | Crossref/PubMed verification and metadata-derived BibTeX |
| `markitdown` | Attempted fallback for publisher HTML; package API unavailable, standard-library parsing used |
| `statistical-analysis` | Estimands, composite properties, paired uncertainty, and study-design recommendations |
| `peer-review` | Reporting-framework assessment and explicit abstract-only limitations |
| `scientific-critical-thinking` | Distinguishing nonreporting from poor reproducibility and identifying selection/generalization limits |

| Agent | Independent task | Output |
|---|---|---|
| `agreement_coverage` | Repository catalog and focused implementation inspection | Routing-aware coverage, concrete limitations, bounded API check, roadmap |
| `statistical_appraisal` | Statistical critique and primary methodological references | Nine-domain assessment, mathematical properties, study/reporting recommendations |

The unavailable `pubmed-database` skill was searched for locally; official EFetch XML and Europe PMC provided the needed record checks directly. Pendar's verification connector provided an additional DOI/retraction cross-check; its open-access flag conflicted with the actual publisher/Europe PMC access evidence and was not used to infer that full text had been read. No agents modified repository files.
