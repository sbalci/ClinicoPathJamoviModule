# Thyroid FNA AI observer study: statistical review and ClinicoPath opportunities

Reviewed 4 October 2026 using the current working tree. Scope: article evaluation,
numerical checks of published aggregate counts, and an implementation roadmap.
The proposed analysis changes below have **not** been implemented. Existing unrelated
working-tree changes were preserved.

**Implementation-plan update:** the [subsequent plan](satturwar-2026-thyroid-fna-implementation-plan.md)
preserves the binary `decision`/`decisioncompare` calculation paths and places nondiagnostic
status accounting in a separate, opt-in reader workflow. It supersedes the architectural
suggestion below to propagate a 3 × 2 workflow into those diagnostic wrappers.

## Assessment

**Major concerns for confirmatory claims of improved diagnostic performance; useful
exploratory evidence about reader behavior and workflow.** The most valuable software
extension is a comparator-aware, three-outcome reader workflow built on `pathagreement`
and `decisioncompare`. Much of the necessary calculation infrastructure already exists.

Three distinctions must survive every table, plot, and exported interpretation:

1. Agreement with consensus versus accuracy against disease status.
2. Consensus Bethesda III+ triage versus histology concordance at Bethesda V–VI.
3. Independent cases versus multiple readings of each case.

The arithmetic is largely reproducible. The key concerns are the estimands, dependence,
and design limitations, rather than widespread calculation errors.

## Article summary and citation

Retrospective observer study: 100 selected ThinPrep cases, three readers, three sequential
modalities, and two-week washouts. Development used 47 slides separately from the test
cohort. The primary comparator was expert consensus; histology supplied a secondary
comparison. Main analyses were category tables, binary metrics, Fleiss kappa, and paired
Wilcoxon timing comparisons. [Article, Methods and Results](https://doi.org/10.1002/cncy.70149)

| Field | Verified value |
|---|---|
| Title | Artificial intelligence–assisted digital thyroid FNA cytology: Improved agreement and sensitivity for higher-risk Bethesda categories with enhanced screening efficiency |
| Authors | Satturwar, Li, Yang, et al.; full author list in the linked BibTeX |
| Journal | Cancer Cytopathology |
| Publication | Online 23 September 2026; October 2026, volume 134, issue 10, e70149 |
| DOI | [10.1002/cncy.70149](https://doi.org/10.1002/cncy.70149) |
| PMID / PMCID | [42779199](https://pubmed.ncbi.nlm.nih.gov/42779199/) / [PMC13601818](https://pmc.ncbi.nlm.nih.gov/articles/PMC13601818/) |
| Publisher / ISSN | Wiley / 1934-6638 |
| Bibliography | [Validated BibTeX](satturwar-2026-thyroid-fna.bib) |
| Correction/retraction check | No linked correction or retraction found in the checked PubMed and Crossref records on 4 October 2026; this is not proof that none exists |
| Supplement | Table S1 DOCX retrieved through [Europe PMC](https://www.ebi.ac.uk/europepmc/webservices/rest/PMC13601818/supplementaryFiles); [local counts and provenance](satturwar-2026-thyroid-fna-supplement.txt) |

No source remained unreadable. The publisher's supplement download failed, but Europe
PMC supplied the DOCX. No individual case–reader records, modality-paired classifications,
or raw timing observations were available for this review.

## Extracted methods

| Method | Role / variant | Required qualification | Article location |
|---|---|---|---|
| Category-stratified sampling | Test-cohort construction, restricted to cases with histology | Selected spectrum; sample predictive values are not routine-clinic predictive values | Methods |
| Slide-level training / validation split | Model development; patient independence described | Confirm separation through annotation, model selection, and threshold setting | Methods |
| Multicategory confusion matrices and exact agreement | Primary categorical comparison | Retain category order and absent categories; distinguish reference and reader axes | Tables 1–4 |
| Binary proportions with 95% CIs | Consensus III+ versus II | Comparator-conditioned agreement; explicitly retain or exclude abstentions | Table 6 |
| Histology cross-tabulation | Secondary descriptive comparison | S1 uses V–VI for malignant concordance, a different threshold | Table S1 |
| Fleiss kappa | Between-reader binary agreement | Case-level rating matrix and nondiagnostic policy needed; uncertainty not supplied | Results |
| Mean, SD, median, percentage time reduction | Workflow summaries | Ratio of marginal summaries is not a paired individual reduction | Table 5 and Results |
| Wilcoxon signed-rank tests | Paired timing contrasts; two-sided alpha .05 | Pairing unit, symmetry of differences, ties, shared-case dependence, multiplicity | Methods, Table 5 |

R 4.5.0 and several analysis/plotting packages are named. The reporting does not identify
a clustered CI method or an inferential test for the stated sensitivity improvement.
Methods and tables were reviewed in the [full article](https://doi.org/10.1002/cncy.70149).

## Reproducible aggregate-count audit

Run from the repository root:

```sh
Rscript --vanilla literature/satturwar-2026-thyroid-fna-checks.R
```

The [script](satturwar-2026-thyroid-fna-checks.R) transcribes numerical counts, checks
row/column totals, and recreates the published metric fractions. It does not invent
patient records or infer unavailable within-case correlations.

### Nondiagnostic results require a 3 × 2 table

After excluding consensus-I cases, the aggregate counts are:

| Reviewer call / consensus | Microscopy | Single-layer AI | Seven-layer AI |
|---|---:|---:|---:|
| Positive / positive | 108 | 141 | 143 |
| Positive / negative | 13 | 29 | 32 |
| Negative / positive | 63 | 35 | 32 |
| Negative / negative | 67 | 61 | 60 |
| Nondiagnostic / positive | 9 | 4 | 5 |
| Nondiagnostic / negative | 25 | 15 | 13 |
| Eligible reads | 285 | 285 | 285 |
| Reads with a determinate call | 251 | 266 | 267 |

These are our reconstructed counts from Tables 2–4. All **15 Table 6 fractions** agree
with this reconstruction. Only the 15 reads belonging to consensus-I cases are removed
from the reference population. Predicted-I calls among the remaining cases stay in the
PPA/NPA and overall-agreement denominators as failures to agree, but cannot enter the
denominator conditional on a positive or negative call. This is a coherent three-outcome
convention; describing it simply as exclusion of nondiagnostic reads is insufficient.

| Reconstructed metric | Microscopy | Single-layer AI | Seven-layer AI |
|---|---:|---:|---:|
| PPA, all reference-positive reads | 60.00% | 78.33% | 79.44% |
| NPA, all reference-negative reads | 63.81% | 58.10% | 57.14% |
| PPA, determinate calls only | 63.16% | 80.11% | 81.71% |
| NPA, determinate calls only | 83.75% | 67.78% | 65.22% |
| Determinate-call coverage | 88.07% | 93.33% | 93.68% |

The determinate-only rows are a **sensitivity analysis we calculated**, not corrected
versions of the authors' estimand. They condition on different selected subsets across
modalities. The large denominator effect shows why complete-case filtering cannot be
an invisible software default. [Denominator audit](satturwar-2026-thyroid-fna-denominators.csv)

All **15 reported confidence intervals** match independent-read Clopper–Pearson intervals
after rounding to one decimal place. This numerical match strongly suggests ordinary
binomial uncertainty; it does not establish which code the authors ran. Such intervals
do not account for multiple correlated reads of each case. Case IDs and paired data are
needed to obtain defensible replacement intervals. [CI audit](satturwar-2026-thyroid-fna-metrics.csv)

A minor arithmetic discrepancy is isolated: 47/90 is **52.2%**, whereas the article gives
52.5% for single-layer malignant-category agreement. It does not change the direction
of that comparison.

### The histology endpoint changes the positivity rule

We checked the supplement's counts against each modality's main-table row totals.
They reconcile. Calculations below retain the full histology-positive denominator:

| Our reconstruction | Microscopy | Single-layer AI | Seven-layer AI |
|---|---:|---:|---:|
| S1 malignant concordance: V–VI / histology malignant | 40.35% | 55.26% | 55.26% |
| Derived III+ triage sensitivity / histology malignant | 71.05% | 88.60% | 87.72% |
| II calls / histology benign | 52.69% | 46.77% | 44.09% |

The second row is a **new descriptive calculation**, not a published primary endpoint
or a validated clinical estimate. It illustrates that both comparator and threshold
must be held fixed before comparing percentages. S1 does not recover the missing
reader-specific or modality-paired records. [Histology audit](satturwar-2026-thyroid-fna-histology.csv)

## Critical evaluation

### Main concerns and remedies

**1. Match terminology to the scientific target.** A consensus label can be a useful
standard for reproducing a classification, but it does not by itself establish disease
status. For this comparator, label the directional quantities PPA/NPA; label the reverse
conditional quantities as agreement conditional on the reader's call. Do not present
them as a patient's probability of malignancy. FDA guidance supports this distinction.
[FDA diagnostic-test reporting guidance](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/statistical-guidance-reporting-results-studies-evaluating-diagnostic-tests-guidance-industry-and-fda)

**2. Preserve the sampling unit.** The same case contributes ratings from several readers
and modalities. Pooled percentages can be valid descriptive summaries; independent-read
confidence limits do not reflect that design. For inference conditional on these three
readers, resample whole cases, retaining every reader and modality in each draw, and
calculate contrasts within the same draw. If a patient contributes multiple cases,
resample patients. Inference to a population of readers requires a justified multireader
multicase method and adequate reader sampling; three selected readers provide limited
information about reader heterogeneity. Binary endpoints need a method validated for
binary reader outcomes, such as the framework examined by
[Chen et al.](https://pmc.ncbi.nlm.nih.gov/articles/PMC4478886/), rather than an assumed
transfer of an ROC-AUC procedure.

**3. Modality and period cannot be separated here.** The fixed review sequence means that
case familiarity, training, and time period are inseparable from modality. Washout may
reduce recall; it cannot identify the separate effects. Adding period to a regression
cannot fix perfect confounding. A future randomized/counterbalanced design should
address this explicitly. The microscopy-to-AI comparison also bundles image display
and AI assistance; an unaided digital arm would help isolate the latter. The
[FDA CADe study guidance](https://www.fda.gov/media/77642/download) discusses session
randomization and counterbalancing. It is radiology guidance used here as methodological
context, not a cytology-specific regulatory requirement.

**4. Define abstention before analysis.** Keep missing measurements, nondiagnostic
predictions, and nondiagnostic reference classifications as different states. Report
coverage, every denominator, and both the primary and determinate-only estimands where
useful. A lower abstention rate is not proof of safer diagnosis; disagreements on adequacy
need separate adjudication or outcome validation.

**5. Compare modalities directly.** An improvement in marginal percentages does not
supply a paired significance test. Provide PPA/NPA differences with CIs and paired
contrasts on prespecified populations. McNemar can be used within one reader and one
reference stratum; pooled repeated reads require dependence-aware inference. Overlapping
or nonoverlapping separate CIs are not a substitute for the CI of a difference.

**6. Report timing effects and multiplicity transparently.** A reader-specific signed-rank
analysis can be appropriate for matched cases. A pooled analysis must also handle shared
cases. Report the distribution of paired differences, meaningful seconds saved or a
clearly defined time ratio, and uncertainty. Median of paired changes, change in marginal
medians, and a Hodges–Lehmann shift are different estimands. Table 5 alone presents 15
pairwise comparisons; a multiplicity strategy or an explicit exploratory interpretation
is needed. The review did not find a reported adjustment, which is not proof none was used.

**7. Bound generalization and clinical claims.** Histology-based eligibility and category
enrichment restrict the spectrum. Predictive values and overall accuracy depend on that
selection; class-specific performance can also change with spectrum. Reporting time is
only one component of workflow cost. Net clinical benefit and fewer repeat FNAs require
outcomes or decision consequences beyond agreement. Similar point estimates between
the two digital modes do not establish equivalence or noninferiority.

**8. Improve AI reporting without inventing missing analyses.** Request the locked model
version, dataset separation details, reference-reader independence/blinding, threshold
prespecification, reader-level outputs, exclusions, and analysis code. Verify whether the
senior consensus readers and experimental readers overlap; the text does not establish
that overlap. The stated target-category development threshold and lower overall recall
may concern different aggregates and should be clarified rather than declared contradictory.
Calibration is not a compulsory missing endpoint for hard category calls; it becomes
relevant if the system supplies probabilities interpreted as risks.

### Checklist and scoring

Scores describe reporting and suitability for **confirmatory comparative inference**:
2 = good, 1 = partial, 0 = major concern. This is the playbook's review rubric, not a
validated risk-of-bias instrument or formal GRADE assessment.

| Aspect | Assessment / evidence | Score | Recommendation |
|---|---|---:|---|
| Design–method alignment | Paired observations recognized, but consensus labels and disease terminology coexist | 1 | Separate endpoints; retain a paired reader-study design |
| Assumptions / diagnostics | Independent-binomial CI reproduction; no demonstrated handling of clustered reads | 0 | Case/patient resampling, paired contrasts, timing-difference diagnostics |
| Sample size / precision | Useful pilot; small reader panel and limited reference-negative cases; no planning rationale located | 1 | Plan cases and readers separately using intended contrast/precision |
| Multiplicity | Multiple category and modality tests; correction not reported | 0 | Prespecify contrasts and family; Holm or explicit exploratory reporting |
| Model specification / confounding | Fixed modality order; AI and display changes bundled | 0 | Improve design; do not claim regression can separate aliased effects |
| Missing / indeterminate data | Counts reconstruct, but exclusions need more explicit description | 1 | Three-outcome tables and separate missingness flow |
| Effect sizes / CIs | Counts and CIs supplied; comparative CIs and timing-change CIs missing | 1 | Paired absolute differences, ratios where appropriate, clustered CIs |
| Validation / calibration | Separate test cohort is a strength; narrow specimen/site spectrum | 1 | Independent multicenter reader study; probability calibration only if applicable |
| Reproducibility / transparency | Tables, software, supplement, and conflicts disclosed; raw paired data/code unavailable for review | 1 | Share deidentified case–reader data or sufficient covariance/paired summaries |
| **Total** | **Major concerns for strong comparative claims; exploratory findings remain useful** | **6/18** | Reanalysis plus stronger prospective design |

Reported industry relationships warrant transparent independent validation; they are
not evidence that the calculations are wrong. A STARD/STARD-AI-oriented reporting review
is more applicable than treating this as a randomized therapeutic trial. Patient flow,
case spectrum, reference construction, reader study design, AI version and data separation,
indeterminate handling, and uncertainty are the main reporting domains to strengthen.
The [STARD-AI guideline](https://www.nature.com/articles/s41591-025-03953-8) provides the
relevant reporting framework. Its [2026 author correction](https://www.nature.com/articles/s41591-026-04570-9)
adds an omitted steering-committee member and does not change the checklist.

## Current ClinicoPath coverage

The scan covered **390 analysis definitions** and their option, result, UI, and backend
surfaces. See the [full catalog](satturwar-2026-thyroid-fna-function-catalog.tsv) and
[focused catalog](satturwar-2026-thyroid-fna-function-catalog.json). Coverage below is
source-level capability, not a claim that all functions ship or passed release testing.

`T` routes to JamoviTest; `D` is a draft. `decision`, `decisioncompare`, `agreement`, and
`nogoldstandard` currently have `meddecideT`; `pathagreement` has `OncoPathT`.
`multiclassdiagnostics`, `aivalidation`, and `diagnosticsamplesize` have `meddecideExtraD`.

| Article need | Function(s) | Coverage | Exact boundary |
|---|---|:---:|---|
| Binary comparator-conditioned proportions | `decision`, `pathagreement` | 🟡 | Arithmetic exists; labels presume disease/accuracy; no comparator-kind switch |
| Symmetric pair agreement | `agreement`, `pathagreement` | ✅ | Available, but not interchangeable with directional PPA/NPA |
| Retained nondiagnostic predictions in a 3 × 2 analysis | `pathagreement`, `decision`, `decisioncompare` | ❌ | Current filtering/recoding does not reproduce the primary article convention |
| Reader-specific diagnostics and case/patient bootstrap | `pathagreement` | ✅ | Existing fixed-reader workflow; currently excludes unclassified predictions |
| Paired single-reader modality differences | `decisioncompare` | ✅ | McNemar and paired diagnostic differences already exist; comparator labels need extension |
| Joint modality comparisons across readers | `pathagreement` + `decisioncompare` | 🟡 | Need explicit reader/modality mapping and shared resampling for paired differences |
| Generalization to random reader and case populations | Related agreement/mixed-model functions | ❌ | No validated end-to-end binary MRMC workflow identified |
| Multicategory tables / exact agreement | `agreement`, `pathagreement`, `multiclassdiagnostics` | 🟡 | Core building blocks exist; clinical transitions and abstentions need explicit roles |
| Fleiss kappa / ordinal agreement | `agreement`, `pathagreement` | ✅ | For a correctly structured case-by-reader dataset |
| Paired change in interreader agreement | `agreement` | ✅ | Existing joint case-row bootstrap for two complete fixed panels; patient clustering and random-reader inference remain extensions |
| Reader-specific Wilcoxon / Friedman timing analysis | `jjwithinstats` | ✅ | Production; one independent case per row, one reader at a time |
| Time-saving effect table with CIs | `jjwithinstats` | 🟡 | Existing repeated-measure plots/tests; structured seconds/ratio contrasts missing |
| Reader/case timing model | `mixedmodelanova`, `geemodel` | 🟡 | Drafts; fixed-reader models are possible candidates, but extensions/repairs are needed |
| Ordinal mixed model | `ordinalmixedmodel` | 🟡 | Draft CLMM supports multiple random intercepts; some advertised extras unsupported |
| Reader-study precision / power | `diagnosticsamplesize`, `clinicaltrialdesign` | 🟡 | Single-proportion precision is a starting point; not paired MRMC planning |

An available approximation today is to preprocess the comparator and endpoint explicitly,
run `decisioncompare` separately for each reader on determinate paired cases, use
`pathagreement` for reader agreement/bootstrap summaries, and run `jjwithinstats` on
each reader's case-wide timings. That sequence does **not** reproduce the article's
retained-nondiagnostic denominators or supply pooled MRMC inference. The published
aggregate tables alone cannot populate those case-level workflows.

### Evidence from the implementation

| Finding | Source evidence |
|---|---|
| Existing accuracy helper deletes unclassified calls | `R/pathagreement-reader-metrics.R:3` filters both vectors before constructing denominators |
| Existing positive/negative agreement is symmetric | `R/pathagreement-reader-metrics.R:14` computes `2a/(2a+b+c)` and `2d/(2d+b+c)` |
| Unselected binary categories become missing | `R/pathagreement-reader-data.R:64`; labels configured in `jamovi/pathagreement.a.yaml:684` |
| Ordinal cutoff needs an abstention mask | `R/pathagreement-reader-data.R:114` thresholds the ordered score; treating I as below III would make it negative |
| Cluster bootstrap already exposed | `jamovi/pathagreement.a.yaml:800` through reader confidence options; helper files include `pathagreement-bootstrap.R` |
| Paired comparisons already use matched determinate rows | `R/decisioncompare.b.R:172`; per-class comparison reporting at `:296` |
| Existing ordinal directionality | `jamovi/agreement.a.yaml:714`; `R/agreement.b.R:7878` checks ordered paired input and `:7963` calls `irr::rater.bias` |
| Existing paired agreement bootstrap | `jamovi/agreement.a.yaml:1813`; `R/agreement.b.R:14030` uses the same sampled rows for both conditions; complete panels selected at `:13928` |
| Repeated-measure timing tests already paired | `R/jjwithinstats.b.R:390` and `:1258` |
| Continuous mixed model only uses first random grouping in one mode | `R/mixedmodelanova.b.R:104`; nested alternative at `:111` |
| Ordinal mixed model builds multiple random intercepts | `R/ordinalmixedmodel.b.R:180` and `:199` |

Runtime checks of the current pure reader helpers using FDA's 2 × 2 example confirmed
that the sensitivity/specificity fields equal directional PPA/NPA numerically, while
the positive/negative-agreement fields give different, symmetric quantities. The latter
are valid statistics; renaming them PPA/NPA would introduce an error.

For the article's microscopy counts, the present complete-case helper returns 63.16%
and 83.75%, versus 60.00% and 63.81% under the article's retained-abstention convention.
This concretely demonstrates the missing estimand, rather than just a missing label.

## Prioritized implementation roadmap

These are proposed source changes, not completed features. Extend the existing functions
before introducing a separate analysis. Preserve old saved-analysis behavior through an
explicit migration/legacy policy; display the chosen interpretation on old results.

### P0 — Comparator semantics in `decision`, `decisioncompare`, and `pathagreement`

**Impact:** high and broadly reusable; **effort:** small to medium.

Append a `referenceType` option to each `.a.yaml`, with disease, consensus, and method
comparison choices, plus a comparator/target description. A new-analysis default should
not silently imply disease truth. Example schema fragment:

```yaml
- name: referenceType
  title: Comparator interpretation
  type: List
  options:
    - name: unspecified
      title: Specify comparator interpretation
    - name: disease
      title: Reference for disease status
    - name: consensus
      title: Expert consensus classification
    - name: method
      title: Comparative method
  default: unspecified
```

In `.b.R`, use one metric-definition/label provider, also consumed by interpretation,
plots, notes and R-code export. Keep raw counts separate from semantics. In consensus
mode, render directional PPA/NPA and overall agreement; reverse conditional agreement
must not become disease PPV/NPV. Hide disease-risk calculators and clinical likelihood
ratio interpretations in this mode. The arithmetic for determinate binary calls is:

```r
ppa <- a / (a + c)  # comparator-positive denominator
npa <- d / (b + d)  # comparator-negative denominator
psa <- 2 * a / (2 * a + b + c)  # symmetric positive specific agreement
```

Guard zero denominators and report unavailable estimates as missing, never zero.
In each `.r.yaml`, add a metric-definition table with comparator, positive rule,
numerator, denominator and CI method; make every affected output `clearWith` the new
options. In `.u.yaml`, put a `ComboBox` for comparator interpretation above metric choices.
Use “comparator-positive” rather than “disease present” in consensus help text.

**Validation:** FDA Table 4; transposed comparator/test axes; perfect agreement; empty
positive/negative strata; old saved options; no disease-risk prose in consensus mode.
No new statistical dependency is needed. Share helpers as `utils-*.R` only if multiple
analyses actually call them, following repository ownership rules.

### P0 — Explicit nondiagnostic handling in `pathagreement`, then diagnostic wrappers

**Impact:** high; **effort:** medium.

In `.a.yaml`, add `readerIndeterminateLevels`, `referenceIndeterminateLevels`, and
`indeterminatePolicy = determinate_only | retain_nonagreement | both`, with separate
missing-value treatment. For ordered Bethesda input, mask declared nondiagnostic levels
before applying the clinical cutoff. Reference-I exclusion and predicted-I handling
must be distinct operations.

In the backend and reader-data/metrics helpers, retain a state vector with positive,
negative, nondiagnostic and missing values. Construct a 3 × 2 table; do not convert
nondiagnostic predictions to FALSE or silently to missing. If `u_pos` and `u_neg`
are the nondiagnostic cells, the retained-nonagreement estimands are:

```r
ppa <- a / (a + c + u_pos)
npa <- d / (b + d + u_neg)
opa <- (a + d) / (a + b + c + d + u_pos + u_neg)
coverage <- (a + b + c + d) / (a + b + c + d + u_pos + u_neg)
```

The `.r.yaml` changes are a 3 × 2 counts table, population-flow table, and parallel rows
for each selected policy with explicit denominators. `.u.yaml` needs a “Nondiagnostic
and missing results” panel and a policy selector. Interpret differences between policies
as selection effects, not automatic improvement. If paired modalities are compared,
state whether each policy uses available, pair-common, or all-modality-common cases.

**Validation:** the counts in this review, predicted-I in each reference stratum, missing
versus abstention, all-abstention arms, no consensus-V cases, unequal modality coverage,
and a nonobserved ordered category. No new packages required.

### P1 — Paired multireader modality contrasts in `pathagreement`

**Impact:** high for AI observer studies; **effort:** medium to large.

Retain the existing reader bootstrap. Extend `.a.yaml` with an explicit long-format
mode (`caseID`, `readerID`, `modality`, `rating`, optional patient ID), modality contrasts,
and `readerInference = fixed` initially. Reuse existing reference and cutoff options.
Validate unique case × reader × modality keys and case-constant reference labels.
Do not infer identities by splitting column names.

The backend should construct a case × reader × modality array, resample whole cases
or patients **once per replicate**, and compute all modality estimates and differences
from that same sample. Fix the reader weights in the estimand; incomplete reading grids
must not silently change the reader mixture. Offer per-reader effects plus an explicitly
defined fixed-reader average. Preserve strata when the design conditions on a stratified
reference sample. Report failed bootstrap draws and minimum usable replicates.

`.r.yaml`: case count, reader count, read count, modality-specific estimates, paired
percentage-point differences with CIs, reference stratum, retained population, and
reader inference scope. `.u.yaml`: role selectors and a contrast panel; display that
uncertainty is conditional on the selected readers. Reuse `decisioncompare`'s tested
single-reader McNemar/paired-difference logic for appropriate subsets; do not apply
McNemar to pooled reader–case rows as independent observations.

**Validation:** simulated correlated binary data with known modality effects; shared
case resampling across modalities; ID permutation invariance; incomplete grids;
duplicate keys; patient clusters; unequal reader denominators; zero discordance;
Monte Carlo coverage checks. Compare single-reader limits with direct paired methods.
Use existing base R / `boot`; a random-reader MRMC extension needs a separately validated
method. Do not apply an ROC-AUC package to binary endpoints without endpoint support.

### P1 — Interpretable timing changes in `jjwithinstats`; mixed-model extension later

**Impact:** high; **effort:** medium for paired effects, larger for reader models.

Add `pairedSummary`, `differenceEstimator` (mean, median paired difference,
Hodges–Lehmann), confidence method, bootstrap repetitions and optional log time ratio
to `.a.yaml`. In `.b.R`, reuse the existing complete-pair handling and Holm adjustment;
resample independent case rows. A signed-rank CI from `stats::wilcox.test(...,
paired = TRUE, conf.int = TRUE)` must be labeled as a location-shift estimate, not a
median paired difference. Ratios require positive times and an explicit definition.

Add a structured `.r.yaml` table for contrast, number of pairs, unit, effect definition,
estimate, CI, raw/adjusted p and CI method. Put these controls under “Paired changes”
in `.u.yaml`. Report scanning/setup/inference/review times separately if those columns
are supplied; do not infer total workflow benefit from review time alone.

For a subsequent `mixedmodelanova` extension, add crossed grouping and targeted
modality contrasts to `.a.yaml`; extend backend formula generation, convergence and
singularity checks, and `emmeans` outputs; add case/reader/read counts and a formula
table to `.r.yaml`; add a reader-study preset to `.u.yaml`. A candidate fixed-reader
model is `log(time) ~ modality * reader + (1 | case)`, with additional justified
case–reader structure considered in validation. Its coefficients cannot disentangle
modality from the fixed study order.

**Validation:** known paired shifts, skew, ties, all-zero differences, incomplete pairs,
zero times, retransformation, row permutations, and simulated case–reader dependence.
Existing dependencies include `lme4`, `lmerTest`, `emmeans` and `boot`.

### P2 — Ordinal transitions in `agreement` / `pathagreement`

**Impact:** medium to high; **effort:** medium.

Add baseline/comparison roles, ordered diagnostic categories, nondiagnostic levels,
and clinical cutoff(s) to `.a.yaml`. Reuse existing directional discordance and
marginal-homogeneity infrastructure in `.b.R`, adding transitions by magnitude and
clinical threshold. Up/down directionality and marginal homogeneity test different
questions. Bethesda I must remain an adequacy category outside the severity order.

`.r.yaml`: full transition matrix, unchanged/up/down counts, denominator, threshold
crossings, and explicitly unavailable zero-support class estimates. `.u.yaml`: a
“Category changes” panel naming the comparison direction. Do not interpret all
upgrades as benefit or treat equal-spaced ordinal weights as clinically validated costs.

**Validation:** reversed direction, absent consensus-V, disagreement only on adequacy,
matched order requirements, asymmetric transitions with unchanged total agreement,
and reuse of the existing paired case bootstrap for differences in kappa. Extend that
bootstrap to patient clusters where needed rather than duplicating it. Existing
`irr`/`irrCAC` and base tables are sufficient starting points.

### P2 — Repair candidate drafts before reuse; extend precision planning

These source-level blockers were identified during mapping, not fixed in this review:

| Target | Required backend correction | Schema / results / UI consequences |
|---|---|---|
| `geemodel` | `R/geemodel.b.R:250` uses `Gamma()` (inverse link) while labels advertise log; use `Gamma(link = "log")` for that option. Wire the collected `time_var` to validated ordering/waves where appropriate; add targeted terms/contrasts | Align `.a.yaml` family/link choices, `.r.yaml` link and contrast outputs, `.u.yaml` correlation/order controls; validate against direct `geeglm` |
| `mixedmodelanova` | First-group-only versus nested grouping at `:104`–`:115` cannot represent arbitrary crossed effects | Explicit crossed mode in `.a/.u.yaml`; formula and grouping diagnostics in `.r.yaml` |
| `ordinalmixedmodel` | Calls `nominal_test` on a CLMM at `:342` and unsupported `predict.clmm` dispatch at `:405` | Replace with supported methods or remove/disable promises in all schemas; retain honest unavailable outputs |
| `clinicaltrialdesign` | Paired t/McNemar branches call helpers not defined in its backend at `:289` and `:301` | Implement and validate, or disable those `.a/.u.yaml` choices and corresponding `.r.yaml` outputs |
| `diagnosticsamplesize` | Existing interval-width calculations address one proportion, not paired MRMC differences | Add comparator terminology and reference-positive fraction to `.a/.u.yaml`; show independent-case assumptions in `.r.yaml`; keep reader-study simulation planning separate |

Local R API inspection confirmed the Gamma default and the missing CLMM dispatch
methods in the installed package. These findings are a reason to avoid counting menu
labels as validated coverage. General paired power must not be inferred from an option
whose calculation helper is absent.

`nogoldstandard` is not a shortcut for relabeling this study's results. Latent disease
models require their own identifiability and conditional-dependence assumptions; they
answer a different question from agreement with a chosen comparator.

## Validation, dependencies, and release sequence

**Completed in this review:** aggregate-count R script passed; 15/15 metric fractions
reconstructed; 15/15 CIs matched at published precision; S1 row totals reconciled;
directional versus symmetric reader-helper checks passed; bibliography validated.
No full package build or test suite was run because analysis source was not changed.

For implementation, extend existing focused reader/agreement/decisioncompare and
`test-jjwithinstats-release-review.R` coverage. Add independent reference calculations
and regression fixtures for the article's 3 × 2 tables. Keep bootstrap seeds deterministic,
test cancellation/checkpoints, validate 50,000-row performance, and never expand weighted
counts into synthetic independent patient data for inferential testing.

Most required packages already appear in the umbrella `DESCRIPTION`: `boot`, `binom`,
`irr`, `irrCAC`, `geepack`, `lme4`, `lmerTest`, `emmeans`, and `ordinal`. Presence in the
umbrella does not certify a sibling module's dependencies. Use the module updater's
parser and dry run when shipping; do not infer dependencies from text matches alone.

Order the work as follows:

1. Comparator labels, denominator transparency and nondiagnostic states.
2. Fixed-reader paired modality contrasts using the existing cluster bootstrap.
3. Paired timing effect tables with CIs.
4. Ordinal transition summaries and draft-model repairs.
5. Random-reader MRMC inference and design-specific precision/power simulation.

This order reflects reuse and clinical interpretability, not a measured bibliometric
frequency ranking. Before promotion, regenerate `.h.R`/`.Rd` through the supported tools,
run focused numerical tests and serialization checks, run `jmvtools::prepare()` and
required release gates. Do not edit generated files or promote a `T`/`D` analysis as a
side effect of this literature review. No new File/Text feature or `minApp` increase
is needed for the first extensions.

## Caveats

This is a source audit of the current working tree and a reproducibility check of
published aggregate data. It is not a full release review of every mapped function.
The aggregate matrices cannot reproduce reader-specific kappa, paired McNemar tests,
clustered intervals, signed-rank p-values, or timing effect CIs. No such replacement
results are claimed. Suggestions concerning study design are methodological judgments,
not evidence that an unreported procedure was definitely omitted.

## Skills and agents used

| Skill / agent | Purpose |
|---|---|
| `review-article-stats` | Canonical review, coverage mapping and roadmap workflow |
| `statistical-analysis` | Paired/crossed data structure, effect definitions, inference and precision |
| `peer-review` | Reporting completeness, constructive critique and reproducibility |
| `scientific-critical-thinking` | Confounding, spectrum, target validity and claim strength |
| `citation-management` | Crossref/Wiley/PubMed metadata and BibTeX validation |
| `docx` | Supplementary table extraction |
| `coverage_audit` agent | Repository catalog and diagnostic/agreement backend inspection |
| `repeated_measure_coverage` agent | Timing, mixed-model, ordinal and planning capability audit |
| `citation_context` agent | Citation checks, supplement retrieval and primary methodological sources |

The user-invoked playbook explicitly permits parallel specialist agents. Primary
statistical synthesis and all aggregate numerical checks were performed by the main agent.
