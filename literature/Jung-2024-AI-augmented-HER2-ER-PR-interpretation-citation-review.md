# Citation Review: AI-Augmented Interpretation of HER2, ER, and PR in Breast Cancer

---

## ARTICLE SUMMARY

- **Title/Label**: Augmented interpretation of HER2, ER, and PR in breast cancer by artificial intelligence analyzer: enhancing interobserver agreement through a reader study of 201 cases
- **Design & Cohort**: AI-assisted reader study; N = 201 breast cancer cases from Kyung Hee University Hospital (Jan 2018 - Dec 2021). All IHC types (HER2, ER, PR) on same patients. External validation cohort. Three board-certified pathologists from different hospitals independently scored WSIs using digital visualizer. Two-phase design: (1) initial evaluation without AI, (2) revised evaluation with AI assistance for discordant cases only. AI models: Lunit SCOPE HER2 (trained on 1259 WSIs) and Lunit SCOPE ER/PR (trained on 1210 WSIs). DeepLabv3+ segmentation + ResNet-34/101 feature extraction.
- **Key Analyses**:
  - Agreement rate (% concordant cases among pathologists)
  - Quadratic weighted kappa for inter-observer agreement
  - F1 score and IoU for AI cell detection and tissue segmentation models
  - Chi-square / Fisher's exact test for categorical comparisons
  - McNemar test implied (comparing pre/post AI concordance rates)
  - Confusion matrices (initial vs revised consensus for HER2, ER, PR)
  - Molecular subtype reclassification analysis

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Augmented interpretation of HER2, ER, and PR in breast cancer by artificial intelligence analyzer: enhancing interobserver agreement through a reader study of 201 cases |
| Journal | Breast Cancer Research |
| Year | 2024 |
| Volume | 26 |
| Pages | 31 |
| DOI | 10.1186/s13058-024-01784-y |
| PMID | TODO |
| Publisher | BioMed Central / Springer Nature |
| First Author | Minsun Jung |
| Corresponding Author | So-Woon Kim (swkdek@gmail.com) |
| Received / Accepted | Dec 6, 2023 / Feb 11, 2024 |
| License | CC BY 4.0 |

---

## Skipped Sources

*None -- PDF was read successfully (14 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Agreement rate (% concordant) | Primary -- concordance among pathologists | 3 levels: concordant (3/3 agree), partially concordant (2/3 agree), discordant (all differ); initial vs revised | Simple proportion; applied to HER2, ER, PR separately | Results (p4-6), Fig 2 |
| Quadratic weighted kappa | Primary -- inter-observer agreement | Pairwise between each pair of pathologists; initial vs revised evaluation; reported for each biomarker | Ordinal data (HER2: 0/1+/2+/3+; ER/PR: negative/low positive/positive) | Results (p5-6), Figs S2-S4 |
| F1 score | Secondary -- AI model cell detection performance | Per tumor cell class (3+, 2+, 1+, 0, OT for HER2; similar for ER/PR) | Harmonic mean of precision and recall; standard ML metric | Methods (p3), Supp Tables S4-S7 |
| Intersection over Union (IoU) | Secondary -- AI tissue segmentation performance | For CA (cancer area), CIS, BG tissue classes | Standard segmentation metric | Methods (p3), Supp Tables S4-S7 |
| Chi-square / Fisher's exact test | Secondary -- categorical comparisons | Used for comparing proportions (e.g., concordance rates initial vs revised) | Expected cell count determines test choice | Methods (p4) |
| Concordance rate change (p values) | Primary -- AI impact assessment | HER2: 49.3→74.1% (p<0.001); ER: 93.0→96.5% (p=0.096); PR: 84.6→91.5% (p=0.006) | McNemar-like paired comparison of proportions before/after AI | Results (p5-6), Fig 2 |
| Confusion matrix (initial vs revised consensus) | Secondary -- reclassification analysis | HER2, ER, PR separately; shows how consensus changed after AI assistance | All cases vs revised cases only | Results (p6-7), Fig 3 |
| Molecular subtype reclassification | Secondary -- clinical impact | 6 subtypes (HER2+, HER2-equivocal/HR+, HER2-equivocal/HR-, HR+, TNBC, no consensus); initial vs revised | Composite of HER2, ER, PR consensus results | Results (p8-9), Fig 4 |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Agreement rate (% concordant) | `agreement`, `pathologyagreement` | ✅ | Percent agreement standard output |
| Quadratic weighted kappa | `agreement` (kappa2 with weight="squared") | ✅ | Weighted kappa fully supported |
| Fleiss' kappa (3 raters) | `agreement` (kappam.fleiss) | ✅ | Multi-rater kappa available |
| Chi-square / Fisher's exact test | `crosstable`, base jamovi | ✅ | Standard categorical analysis |
| McNemar test (paired proportions) | `crosstable` (may include) | 🟡 | McNemar test for paired nominal data -- need to verify if available in `crosstable` or `agreement` |
| Confusion matrix | `agreement` (multiClassConfusionMatrix) | ✅ | Multi-class confusion matrix implemented 2026-02-08 |
| F1 score (AI model evaluation) | `segmentationmetrics` (Dice/F1) | ✅ | F1/Dice coefficient available in segmentation metrics |
| IoU (segmentation) | `segmentationmetrics` (IoU/Jaccard) | ✅ | IoU/Jaccard index available |
| Molecular subtype classification | — | ❌ | Clinical classification logic, not statistical method |
| Paired agreement comparison (pre/post AI) | `agreement` (pairedAgreementComparison) | ✅ | Paired agreement comparison implemented 2026-02-08; compares kappa between two conditions |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟡 Moderate (11/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 1/2 | n=201 reasonable for primary analysis but small for subgroup analysis (HER2 1+: n=34; TNBC: n=9) |
| Multiple testing correction | 0/2 | Multiple p-values (HER2, ER, PR × initial/revised × pairwise) without correction |
| Effect sizes with CIs | 1/2 | Kappa values reported but CIs not always shown; agreement rates without CIs |
| Assumption checking | 1/2 | Quadratic weighted kappa appropriate for ordinal data; but no discussion of prevalence effects on kappa |
| Appropriate test selection | 2/2 | Weighted kappa for ordinal agreement; agreement rate for clinical interpretation; F1/IoU for AI validation |
| Reproducibility | 2/2 | 3 pathologists from different institutions; external test set; AI models reproducible (Lunit commercial) |
| Handling of missing data | 1/2 | Cases with "no consensus" reported (n=3-4); not excluded from analysis |
| Model diagnostics | 1/2 | AI model performance reported (F1, IoU) but no calibration; kappa interpretation follows standard thresholds |
| Clinical significance assessment | 1/1 | Molecular subtype reclassification (58.2→78.6%, p<0.001) -- directly clinically meaningful |
| External validity | 1/1 | External test set from Kyung Hee University Hospital; AI models from Lunit (commercial, multi-institution training) |
| Bias assessment | 0/1 | Selection bias from university hospital; AI errors analyzed (10 complete failures) but systematic bias not formally assessed |

### Strengths
1. **Paired pre/post AI design**: Same cases scored initially without AI, then revised with AI assistance -- clean comparison
2. **Multi-biomarker concurrent evaluation**: HER2, ER, PR all assessed on same patient cohort (rare)
3. **Multi-institution pathologists**: 3 pathologists from different hospitals -- more realistic than single-institution
4. **Molecular subtype impact**: Demonstrates that improved individual biomarker agreement translates to improved subtype classification
5. **AI failure analysis**: 10 complete AI failures analyzed with photomicrographic examples -- honest and educational
6. **Clinical workflow design**: AI used as "second reader" for discordant cases only -- practical implementation model
7. **HER2-low focus**: Improvement especially at HER2 2+ and 1+ (26.5→70.7% and 46.2→68.4%) -- clinically critical for ADC eligibility

### Weaknesses
1. **No blinding between phases**: Pathologists knew they were re-evaluating with AI -- may introduce bias toward accepting AI opinion
2. **Only discordant cases revisited**: Cases where pathologist agreed with AI were not revisited; potential for AI-induced errors in "concordant" cases not assessed
3. **Small subgroups**: HER2-equivocal/HR-negative (n=4), TNBC (n=9) too small for reliable subtype-specific conclusions
4. **No long-term clinical outcome**: Agreement improvement assumed to improve patient outcomes; not directly measured
5. **Commercial AI (Lunit)**: Competing interests declared (employees of Lunit among authors); generalizability to other AI platforms unknown

---

## GAP ANALYSIS

### Gap 1: McNemar Test for Paired Proportions (Pre/Post Comparison)

**What the article used**: Compared concordance rates before vs after AI assistance (e.g., HER2: 49.3% → 74.1%, p<0.001). This requires a paired test since the same cases are evaluated in both conditions. The article likely used McNemar's test or a similar paired proportion test.

**Current ClinicoPath coverage**: The `pairedAgreementComparison` feature (implemented 2026-02-08) compares kappa between two conditions using bootstrap. For comparing concordance rates (proportions), McNemar's test would be appropriate. It's unclear whether `crosstable` includes McNemar's test.

**Gap**: If McNemar's test is not available in `crosstable`, it would be a useful addition for before/after study designs. However, the existing `pairedAgreementComparison` in `agreement` provides a more sophisticated analysis (comparing kappa rather than raw concordance rates).

**Priority**: Low -- the existing paired agreement comparison covers the core use case; McNemar's test is a simpler alternative that adds marginal value.

---

## IMPLEMENTATION ROADMAP

### Not Recommended: Gap 1 (McNemar's test)

The existing `pairedAgreementComparison` feature in `agreement` already handles the core use case of comparing agreement between two conditions (e.g., manual vs AI-assisted). McNemar's test adds only marginal value for comparing raw concordance rates.

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟡 Moderate (11/18) |
| Methodological novelty | High (concurrent HER2+ER+PR evaluation; AI as second reader workflow; subtype reclassification) |
| Clinical relevance | Very High (HER2-low identification for ADC therapy; molecular subtype accuracy) |
| Coverage by ClinicoPath | Excellent (8 covered, 1 partial, 1 outside-scope) |
| Implementation priority | None -- all statistical methods effectively covered |

### Key Takeaway for ClinicoPath Development

This article demonstrates a compelling clinical use case for the `agreement` function's paired comparison feature (implemented 2026-02-08). The before/after AI assistance comparison maps directly to the `pairedAgreementComparison` option, which compares kappa between two conditions.

The article also validates the clinical importance of the `segmentationmetrics` function's F1/IoU metrics for evaluating AI model performance. The multi-class confusion matrix feature handles the consensus reclassification analysis shown in the article's figures.

Overall, ClinicoPath's existing toolkit comprehensively covers the statistical needs of AI-assisted pathology reader studies.
