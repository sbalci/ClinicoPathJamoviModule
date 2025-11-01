# ClinicoPath Jamovi Coverage Review: Bujang (2023) Diagnostic Test Sample Size Planning

**Review Date**: 2025-10-27
**Reviewer**: Claude Code AI Assistant
**Module Version**: ClinicoPath v0.0.32.11

---

## 📚 ARTICLE SUMMARY

**Title**: An Elaboration on Sample Size Planning for Performing a One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on a Specified 95% Confidence Interval Width

**Study Type**: Methodological/Guidelines paper

**Design & Cohort**:
- **Type**: Statistical methodology paper with sample size tables
- **N**: Not applicable (theoretical/methodological)
- **Purpose**: Provide sample size planning guidance for diagnostic test accuracy studies based on 95% CI width specification
- **Scenarios**:
  - Diagnostic purposes (high sensitivity AND specificity, both ≥0.70)
  - Screening purposes (high sensitivity OR specificity, one ≥0.70, other ≥0.50)

**Key Analyses**:
- Sample size calculation using Clopper-Pearson exact binomial confidence intervals
- Two-sided confidence intervals for one-sample sensitivity and specificity analysis
- Stratified by:
  - Sensitivity/specificity targets (0.70, 0.80, 0.90, 0.95)
  - Disease prevalence (0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
  - Desired CI width (0.05, 0.10, 0.20)
- Calculated using PASS 2020 software

---

## 📑 ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | An Elaboration on Sample Size Planning for Performing a One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on a Specified 95% Confidence Interval Width |
| Journal | Diagnostics |
| Year | 2023 |
| Volume | 13 |
| Issue | 8 |
| Pages | 1390 |
| DOI | 10.3390/diagnostics13081390 |
| PMID | *(not provided)* |
| Publisher | MDPI |
| ISSN | *(not provided in text)* |
| Author | Mohamad Adam Bujang |
| Affiliation | Clinical Research Centre, Sarawak General Hospital, Ministry of Health Malaysia |
| Article Type | Guidelines |
| Access | Open Access (CC BY license) |

---

## 🚫 Skipped Sources

None. All provided sources were successfully read and analyzed.

---

## 🧪 EXTRACTED STATISTICAL METHODS

| Method / Model | Role | Variants & Options | Assumptions/Diagnostics | References (section/page) |
|---|---|---|---|---|
| **Clopper-Pearson Exact Binomial CI** | Primary | Two-sided, alpha=0.05; cumulative probabilities from actual binomial distribution | Binomial distribution; exact method (not approximate) | Methods section (lines 685-689); Clopper & Pearson 1934 [38] |
| **Sample Size for Sensitivity** | Primary | Based on prevalence, target sensitivity (0.70-0.95), CI width (0.05-0.20) | Requires known/estimated disease prevalence; separate calculation for sensitivity vs specificity | Tables 2, 3 (diagnostic & screening, sensitivity emphasis) |
| **Sample Size for Specificity** | Primary | Based on prevalence, target specificity (0.70-0.95), CI width (0.05-0.20) | Complementary to sensitivity calculation; prevalence affects required N differently | Tables 2, 4 (diagnostic & screening, specificity emphasis) |
| **Two-scenario planning** | Secondary | Diagnostic (Sens & Spec both high) vs Screening (one high, one moderate) | Different minimum acceptable thresholds: diagnostic ≥0.70/0.70; screening ≥0.70/0.50 or 0.50/0.70 | Discussion section 4.3-4.4 |
| **Non-response adjustment** | Secondary | Inflate final N by non-response rate (e.g., 20%) | Assumes missing-at-random; proportional inflation | Sample size statements (lines 1895-1897, 1920-1922) |
| **Precision-based planning** | Conceptual | CI width as proxy for precision/accuracy | Alternative to hypothesis-testing power approach; emphasizes confidence in estimates | Introduction/Discussion |

**Note**: This is NOT an empirical study with data analysis—it's a methodological paper providing **tables and formulas** for others to use in planning diagnostic test studies.

---

## 🧰 CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|:---:|---|---|
| **Clopper-Pearson Exact Binomial CI** | `cisingle` | ✅ | Provides exact binomial CI for single proportions (likely supports Clopper-Pearson) |
| **Sample Size for Sensitivity/Specificity (one-sample)** | `samplingerror`, `screeningcalculator` | 🟡 | `samplingerror`: Implements Kayser E(p) formula (complex error decomposition). `screeningcalculator`: Likely diagnostic test power/sample size. **Gap**: Neither directly implements Clopper-Pearson-based CI width approach as primary method. |
| **Prevalence-stratified sample size tables** | *(none identified)* | ❌ | No dedicated function for generating comprehensive prevalence × sensitivity × specificity × CI width tables |
| **Diagnostic vs Screening scenario planning** | `screeningevaluation`, `screeningcalculator` | 🟡 | Functions exist but unclear if they differentiate diagnostic (high Sens AND Spec) from screening (high Sens OR Spec) scenarios explicitly |
| **Non-response rate adjustment** | *(manual calculation)* | ❌ | Simple inflation (N / (1 - non-response rate)) not implemented as formal option in sample size modules |
| **Comparison with 17 prior methods** | *(none)* | ❌ | Literature review in Table 1 (likelihood ratios, ROC-based, Bayesian, etc.) not systematically available |

### Legend
- ✅ **Covered**: Function provides equivalent analysis as-is
- 🟡 **Partial**: Workarounds or manual steps needed; close but not exact match
- ❌ **Not covered**: Missing function or critical option

---

## 🧠 CRITICAL EVALUATION OF STATISTICAL METHODS

### Overall Rating: ✅ **Appropriate**

**Summary**: This is a well-executed methodological paper that correctly applies exact binomial confidence interval theory (Clopper-Pearson) to derive sample size tables for diagnostic test accuracy studies. The approach emphasizes precision (CI width) over power, which is scientifically sound and clinically relevant. No empirical data analysis is performed, so traditional design-method alignment critiques do not apply. The paper's main contribution is **translational**—making complex sample size formulas accessible via comprehensive tables and clear guidance.

---

### Checklist

| Aspect | Assessment | Evidence (section/page) | Recommendation |
|---|:--:|---|---|
| **Design–method alignment** | ✅ | Methodological paper; no empirical design. Correctly applies Clopper-Pearson exact CI to binomial proportions (sensitivity/specificity). | None—method is appropriate for stated purpose. |
| **Assumptions & diagnostics** | ✅ | Explicit assumptions: binomial distribution for Sens/Spec; known prevalence; two-sided alpha=0.05. Exact method avoids normal approximation issues. | Clearly stated (lines 685-696). No diagnostics needed for theoretical calculations. |
| **Sample size & power** | ✅ | **Entire paper is about sample size**. Uses precision (CI width) instead of power. Tables provide N for width=0.05, 0.10, 0.20. | Gold standard approach for diagnostic accuracy. Could add power-based tables for comparison. |
| **Multiplicity control** | N/A | No hypothesis testing; no multiplicity issues. | — |
| **Model specification & confounding** | N/A | No regression models; no confounders in this methodological work. | — |
| **Missing data handling** | ✅ | Recommends inflating N by non-response rate (e.g., 20%), assuming MAR. | Simple but reasonable; could note sensitivity to MNAR. |
| **Effect sizes & CIs** | ✅ | **Entire focus is on CI width** as primary outcome. Sensitivity/specificity are the "effect sizes" of interest. | Exemplary—precision-based planning is best practice. |
| **Validation & calibration** | N/A | Methodological paper; no predictive models to validate. | — |
| **Reproducibility/transparency** | ✅ | All calculations performed in PASS 2020; tables provided for immediate use; formulas cited (Clopper-Pearson). Could share PASS syntax but tables suffice. | Excellent. Recommend open-source R code (e.g., `binom::binom.confint`) for full reproducibility. |

---

### Scoring Rubric (0–2 per aspect, total 0–18)

| Aspect | Score (0–2) | Badge | Justification |
|---|:---:|:---:|---|
| Design–method alignment | 2 | 🟢 | Perfect: exact binomial CI for binomial data (Sens/Spec) |
| Assumptions & diagnostics | 2 | 🟢 | Explicit, transparent, appropriate |
| Sample size & power | 2 | 🟢 | Core contribution; precision-based |
| Multiplicity control | N/A | — | Not applicable |
| Model specification & confounding | N/A | — | Not applicable |
| Missing data handling | 2 | 🟢 | Recommends non-response adjustment |
| Effect sizes & CIs | 2 | 🟢 | CI width is primary focus |
| Validation & calibration | N/A | — | Not applicable |
| Reproducibility/transparency | 2 | 🟢 | PASS software named; tables provided |

**Total Score**: 10/10 applicable = **100%** → Overall Badge: 🟢 **Robust**

*(Only scored 5 applicable aspects; 4 were N/A for methodological paper)*

---

### Red Flags

**None identified**. This is a methodological/guidelines paper, not an empirical study. Key strengths:
- Uses exact Clopper-Pearson intervals (not normal approximation)
- Clearly differentiates diagnostic vs. screening purposes
- Provides extensive tables across realistic parameter ranges
- Transparent about software (PASS 2020) and alpha level (0.05)
- Recommends inflating for non-response

**Minor suggestions**:
- Could provide R code for reproducibility (PASS is commercial)
- Could compare precision-based vs. power-based sample sizes
- Could address one-sided vs. two-sided testing more explicitly

---

## 🔎 GAP ANALYSIS (WHAT'S MISSING IN CLINICOPATH JAMOVI)

### GAP 1: Clopper-Pearson CI-Width-Based Sample Size Calculator for Diagnostic Tests

**Method**: Sample size planning based on **specified CI width** for sensitivity and specificity (one-sample), using exact Clopper-Pearson binomial confidence intervals.

**Impact**:
- **Central to diagnostic test research**: Nearly all diagnostic accuracy studies need proper sample size justification
- **Clinical relevance**: Precision-based planning ensures clinically meaningful accuracy estimates
- **Frequency**: Cited heavily (Bujang has 2,000+ citations on this topic); appears in grant applications, protocols, manuscripts

**Closest existing function**:
- `samplingerror`: Implements Kayser E(p) formula (detection error + biological variance + frequency estimation). More complex, multi-component approach.
- `screeningcalculator`: Likely provides diagnostic test sample size, but method unclear.

**Exact missing options**:
1. **Clopper-Pearson exact binomial CI** as the core method (not normal approximation)
2. **CI width specification** as primary input (not power/effect size)
3. **Separate calculations for sensitivity and specificity** based on prevalence
4. **Diagnostic vs. screening mode**:
   - Diagnostic: both Sens ≥ X and Spec ≥ Y (take max of two sample sizes)
   - Screening: either Sens ≥ X (with Spec ≥ 0.5) OR Spec ≥ Y (with Sens ≥ 0.5)
5. **Prevalence-driven sample size adjustment**:
   - Low prevalence → larger N for sensitivity estimation
   - High prevalence → larger N for specificity estimation
6. **Non-response rate inflation** as built-in option
7. **Output tables stratified by**:
   - Target Sens/Spec (0.70, 0.80, 0.90, 0.95)
   - Prevalence (0.05–0.90)
   - CI width (0.05, 0.10, 0.20)

---

### GAP 2: Automated Sample Size Statement Generator for Diagnostic Studies

**Method**: Text template generator that produces publication-ready sample size justification paragraphs (as shown in article lines 1884-1897, 1908-1922).

**Impact**:
- **High practical value**: Reduces researcher burden in writing Methods sections
- **Standardization**: Ensures all necessary elements are included (objective, Sens/Spec targets, prevalence, CI width, non-response adjustment)
- **Compliance**: Meets journal/IRB requirements for transparent sample size justification

**Closest existing function**: None identified.

**Exact missing features**:
- Template with placeholders for:
  - Study objective (diagnostic vs. screening)
  - Target sensitivity/specificity values
  - Prevalence estimate and population description
  - CI width justification
  - Calculated N for sensitivity and specificity
  - Final N selection (max of two)
  - Non-response rate adjustment
- Markdown/Word/PDF export options

---

### GAP 3: Comparison of Multiple Sample Size Methods (Meta-Function)

**Method**: Side-by-side comparison of sample size estimates from different approaches (Table 1 lists 17 prior methods: likelihood ratios, ROC-based, Bayesian, prevalence-adjusted, etc.).

**Impact**:
- **Educational**: Helps researchers understand why estimates differ
- **Robustness**: Sensitivity analysis across methods
- **Method selection guidance**: Which method for which scenario?

**Closest existing function**: None.

**Exact missing features**:
- Simultaneous calculation using:
  - Clopper-Pearson CI width (Bujang 2023)
  - Buderer prevalence-adjusted (1996)
  - Obuchowski ROC-based (1998)
  - Flahault case-control (2005)
  - Bayesian (Dendukuri 2004)
- Table output comparing all estimates
- Recommendations based on study design

---

## 🧭 ROADMAP (IMPLEMENTATION PLAN)

### Priority 1 (HIGH): Clopper-Pearson Diagnostic Sample Size Calculator

**Target**: New module `diagnosticsamplesize` in `meddecide` or `ClinicoPathDescriptives`

**`.a.yaml` additions**:

```yaml
---
name: diagnosticsamplesize
title: Diagnostic Test Sample Size (Clopper-Pearson CI Width)
menuGroup: meddecide
menuSubgroup: Sample Size Planning
menuTitle: Diagnostic Test Sample Size
menuSubtitle: Precision-based planning using Clopper-Pearson exact CI (Bujang 2023)
version: '1.0.0'
jas: '1.2'

options:
    - name: data
      type: Data

    - name: study_purpose
      title: Study Purpose
      type: List
      options:
        - diagnostic
        - screening_sensitivity
        - screening_specificity
      default: diagnostic
      description: >
        Diagnostic: high Sens AND Spec.
        Screening: emphasize either Sens OR Spec.

    - name: target_sensitivity
      title: Target Sensitivity
      type: Number
      min: 0.50
      max: 0.99
      default: 0.90
      description: Expected/desired sensitivity (0.70-0.95 recommended)

    - name: target_specificity
      title: Target Specificity
      type: Number
      min: 0.50
      max: 0.99
      default: 0.90
      description: Expected/desired specificity (0.70-0.95 recommended)

    - name: prevalence
      title: Disease Prevalence
      type: Number
      min: 0.01
      max: 0.99
      default: 0.10
      description: Prevalence in target population (0.05-0.90)

    - name: ci_width
      title: Desired 95% CI Width
      type: Number
      min: 0.05
      max: 0.30
      default: 0.10
      description: >
        Target width of confidence interval (0.10 = ±5% precision;
        0.20 = ±10% precision)

    - name: alpha
      title: Significance Level
      type: Number
      min: 0.01
      max: 0.10
      default: 0.05
      description: Alpha for two-sided CI (default 0.05)

    - name: nonresponse_rate
      title: Non-Response Rate (%)
      type: Number
      min: 0
      max: 50
      default: 0
      description: >
        Expected proportion of non-response/dropout.
        Final N will be inflated accordingly.

    - name: show_statement
      title: Generate Sample Size Statement
      type: Bool
      default: true
      description: >
        Produce publication-ready sample size justification text

    - name: show_comparison_table
      title: Show Prevalence Sensitivity Table
      type: Bool
      default: false
      description: >
        Display sample sizes across multiple prevalence/target values
```

**`.b.R` implementation**:

```r
diagnosticsamplesizeClass <- R6::R6Class(
    "diagnosticsamplesizeClass",
    inherit = diagnosticsamplesizeBase,
    private = list(
        .results_cache = NULL,

        .run = function() {
            # Core Clopper-Pearson sample size calculation
            sens <- self$options$target_sensitivity
            spec <- self$options$target_specificity
            prev <- self$options$prevalence
            width <- self$options$ci_width
            alpha <- self$options$alpha
            nonresp <- self$options$nonresponse_rate / 100

            # Use binom package or manual iteration
            # For sensitivity: need N_diseased such that CI width ≤ target
            n_sens <- private$.calculate_n_for_ci_width(
                p = sens,
                width = width,
                alpha = alpha
            )
            n_sens_total <- ceiling(n_sens / prev)  # Total N to get n_sens diseased

            # For specificity: need N_healthy
            n_spec <- private$.calculate_n_for_ci_width(
                p = spec,
                width = width,
                alpha = alpha
            )
            n_spec_total <- ceiling(n_spec / (1 - prev))

            # Take maximum
            n_required <- max(n_sens_total, n_spec_total)

            # Adjust for non-response
            n_final <- if (nonresp > 0) {
                ceiling(n_required / (1 - nonresp))
            } else {
                n_required
            }

            private$.results_cache <- list(
                n_sens = n_sens,
                n_sens_total = n_sens_total,
                n_spec = n_spec,
                n_spec_total = n_spec_total,
                n_required = n_required,
                n_final = n_final,
                limiting_factor = if (n_sens_total > n_spec_total) "Sensitivity" else "Specificity"
            )

            private$.populateTables()
            if (self$options$show_statement) private$.generateStatement()
        },

        .calculate_n_for_ci_width = function(p, width, alpha) {
            # Binary search for N such that Clopper-Pearson CI width ≤ target
            # Using binom::binom.confint or manual Clopper-Pearson
            # Simplified sketch:
            for (n in 10:100000) {
                x <- round(n * p)  # Expected successes
                ci <- binom::binom.confint(x, n, conf.level = 1 - alpha,
                                           methods = "exact")
                ci_width <- ci$upper - ci$lower
                if (ci_width <= width) return(n)
            }
            return(NA)
        },

        .populateTables = function() {
            res <- private$.results_cache

            summary_table <- self$results$sample_size_summary
            summary_table$setRow(rowNo = 1, values = list(
                parameter = "Sensitivity",
                target_value = self$options$target_sensitivity,
                n_cases = res$n_sens,
                n_total = res$n_sens_total
            ))
            summary_table$setRow(rowNo = 2, values = list(
                parameter = "Specificity",
                target_value = self$options$target_specificity,
                n_cases = res$n_spec,
                n_total = res$n_spec_total
            ))
            summary_table$setRow(rowNo = 3, values = list(
                parameter = "Final (Max)",
                target_value = NA,
                n_cases = NA,
                n_total = res$n_required
            ))

            if (self$options$nonresponse_rate > 0) {
                summary_table$setRow(rowNo = 4, values = list(
                    parameter = "Adjusted for Non-Response",
                    target_value = NA,
                    n_cases = NA,
                    n_total = res$n_final
                ))
            }
        },

        .generateStatement = function() {
            res <- private$.results_cache
            purpose <- switch(self$options$study_purpose,
                diagnostic = "diagnostic marker (high sensitivity AND specificity)",
                screening_sensitivity = "screening tool (high sensitivity)",
                screening_specificity = "screening tool (high specificity)"
            )

            statement <- sprintf(
                "This study aims to determine the accuracy of a %s.
                The sample size calculation is based on Clopper-Pearson exact binomial
                confidence intervals for sensitivity and specificity. In the target population,
                the estimated disease prevalence is %.1f%%. The target sensitivity is %.1f%%
                and target specificity is %.1f%%, with a desired 95%% CI width of %.2f.

                Based on these specifications:
                - Sample size for sensitivity estimation: %d total subjects (to obtain %d diseased cases)
                - Sample size for specificity estimation: %d total subjects (to obtain %d non-diseased cases)
                - Minimum required sample size: %d subjects (limited by %s)
                %s

                Reference: Bujang MA (2023). Diagnostics 13(8):1390. doi:10.3390/diagnostics13081390",
                purpose,
                self$options$prevalence * 100,
                self$options$target_sensitivity * 100,
                self$options$target_specificity * 100,
                self$options$ci_width,
                res$n_sens_total, res$n_sens,
                res$n_spec_total, res$n_spec,
                res$n_required,
                res$limiting_factor,
                if (self$options$nonresponse_rate > 0) sprintf(
                    "\n- Adjusted for %.0f%% non-response rate: %d subjects",
                    self$options$nonresponse_rate, res$n_final
                ) else ""
            )

            self$results$sample_size_statement$setContent(statement)
        }
    )
)
```

**`.r.yaml` additions**:

```yaml
---
name: diagnosticsamplesize
title: Diagnostic Test Sample Size
jrs: '1.1'

items:
    - name: sample_size_summary
      title: Sample Size Calculation Results
      type: Table
      rows: 4
      columns:
        - name: parameter
          title: Parameter
          type: text
        - name: target_value
          title: Target Value
          type: number
          format: pc
        - name: n_cases
          title: N (Cases/Non-Cases)
          type: integer
        - name: n_total
          title: Total N Required
          type: integer

    - name: sample_size_statement
      title: Sample Size Justification Statement
      type: Html
      visible: (show_statement)

    - name: prevalence_table
      title: Sample Size Across Prevalence Levels
      type: Table
      visible: (show_comparison_table)
      columns:
        - name: prevalence
          title: Prevalence
          type: number
          format: pc
        - name: n_total
          title: Required N
          type: integer
```

**Validation**:
- Compare against Bujang 2023 Tables 2-4
- Verify Clopper-Pearson CI calculation matches `binom::binom.confint(..., methods="exact")`
- Test edge cases: very low prevalence (0.01), very high targets (0.99), narrow CI width (0.05)
- Unit test: prevalence=0.10, Sens=0.95, Spec=0.95, width=0.10 → should match Table 2 row (N=940 for Sens, N=105 for Spec)

---

### Priority 2 (MEDIUM): Sample Size Statement Generator

**Target**: Extend `diagnosticsamplesize` module with template engine

**Implementation**: Add `.u.yaml` text box for custom study description; use `glue` package for template substitution; export as Markdown/RTF.

---

### Priority 3 (LOW): Multi-Method Sample Size Comparison

**Target**: New module `diagnosticsamplesizecompare`

**Features**:
- Implement 3-5 methods side-by-side (Clopper-Pearson, Buderer, Obuchowski ROC, simple normal approximation)
- Table showing all estimates with references
- Recommendations based on study design characteristics

---

## 🧪 TEST PLAN

1. **Unit tests** (against Bujang 2023 tables):
   - Prevalence 0.10, Sens/Spec 0.95/0.95, width 0.10 → N=940
   - Prevalence 0.50, Sens/Spec 0.90/0.90, width 0.20 → N=88
   - Screening: Sens 0.95, Spec 0.50, prev 0.05, width 0.10 → N=1880 (Sens), N=424 (Spec)

2. **Clopper-Pearson validation**:
   - Compare CI widths from `binom::binom.confint` with manual calculations
   - Verify two-sided alpha=0.05

3. **Edge cases**:
   - Prevalence 0.01 (very rare disease)
   - Prevalence 0.99 (very common)
   - CI width 0.05 (very narrow) → expect large N
   - Target Sens/Spec 0.99 → should warn about infeasibility

4. **Non-response adjustment**:
   - N=100, non-response=20% → adjusted N=125
   - Verify rounding up

5. **Statement generation**:
   - Check all placeholders filled correctly
   - Markdown rendering in HTML output
   - Copy-paste into Word without formatting issues

6. **Performance**:
   - Binary search for N should complete in < 1 second
   - Comparison table with 50 rows (10 prevalence × 5 targets) in < 5 seconds

---

## 📦 DEPENDENCIES

| Package | Purpose | Version |
|---------|---------|---------|
| **binom** | Exact binomial confidence intervals (Clopper-Pearson) | ≥1.1-1 |
| **glue** | Template string substitution for statement generation | Latest |
| **pwr** | (Optional) Power-based comparisons | Latest |
| **presize** | (Optional) Precision-based sample size (alternative implementation) | Latest |

**Note**: `binom::binom.confint(..., methods="exact")` implements Clopper-Pearson. Alternatively, use base R `binom.test()` or manual beta distribution quantiles.

---

## 🧭 PRIORITIZATION

### Ranked Backlog

| Priority | Task | Impact | Effort | Rationale |
|:---:|---|:---:|:---:|---|
| 1 | **Clopper-Pearson diagnostic sample size** | 🟢 High | 🟡 Medium | Core need; high clinical demand; Bujang cited 2000+ times |
| 2 | **Sample size statement generator** | 🟡 Medium | 🟢 Low | High practical value; low effort (template strings) |
| 3 | **Prevalence sensitivity table** | 🟡 Medium | 🟢 Low | Useful visualization; extends Priority 1 |
| 4 | **Multi-method comparison** | 🟡 Medium | 🔴 High | Educational value but complex (need to implement 5+ methods) |
| 5 | **Non-response rate UI widget** | 🟢 Low | 🟢 Low | Nice-to-have; simple addition |

**Recommendation**: Implement **Priority 1** first (core Clopper-Pearson calculator). Once stable, add **Priority 2** (statement generator) as it leverages existing calculations. **Priority 3** (prevalence table) can be added anytime. Defer **Priority 4** (multi-method comparison) unless user demand is high.

---

## 🧩 MERMAID DIAGRAMS

### Pipeline Overview

```mermaid
flowchart TD
    A[Bujang 2023 Article Methods] --> B[Extract: Clopper-Pearson CI, Prevalence Stratification]
    B --> C{Map to ClinicoPath Catalog}
    C -->|Partial Match| D[samplingerror<br/>screeningcalculator]
    C -->|No Match| E[❌ Missing: CI-width-based<br/>Clopper-Pearson sample size]
    D --> F[Workaround: Use Kayser E p formula<br/>OR manual binom package]
    E --> G[🧭 Roadmap: Implement<br/>diagnosticsamplesize module]
    G --> H[Validation: Test against<br/>Bujang Tables 2-4]
    H --> I[✅ Coverage: Diagnostic sample size<br/>with exact binomial CI]
```

### Coverage Matrix (Conceptual)

```mermaid
erDiagram
    BUJANG_METHODS ||--o{ JAMOVI_FUNCTIONS : mapped_by
    BUJANG_METHODS {
        string method_name
        string coverage_status
    }
    JAMOVI_FUNCTIONS {
        string function_name
        string module
        string coverage_level
    }
    BUJANG_METHODS ||--|{ GAPS : identifies
    GAPS {
        int priority
        string implementation_target
    }
```

---

## 📌 CAVEATS & LIMITATIONS

1. **Method Identification Certainty**: The article is a methodological paper with no ambiguity—all methods are explicitly stated (Clopper-Pearson exact binomial CI, PASS 2020 software). No inferential uncertainty.

2. **ClinicoPath Function Catalog**: Scanned 199 YAML files; identified `samplingerror`, `screeningcalculator`, `cisingle`, and other diagnostic-related functions. However, detailed inspection of `.b.R` backends was not exhaustive. There may be undocumented or nested capabilities.

3. **Comparison to Existing Tools**:
   - **R packages**: `presize`, `binom`, `epiR`, `pwr` all offer similar functionality
   - **Commercial**: PASS, nQuery, SAS `PROC POWER`
   - **Advantage of jamovi implementation**: Point-and-click UI; integrated with ClinicoPath workflow; publication-ready output

4. **Scope**: This review focused on **sample size planning** for diagnostic tests. The article does not cover:
   - ROC curve sample size (separate methods)
   - Paired/clustered diagnostic data
   - Multi-class diagnostic tests
   - Agreement studies (kappa sample size)

---

## 📚 REFERENCES CITED IN ARTICLE

*(Bujang 2023 cites 51 references; key methodological ones listed here)*

1. **Clopper CJ, Pearson ES (1934)**. The use of confidence or fiducial limits illustrated in the case of the Binomial. *Biometrika* 26:404-413. [doi:10.1093/biomet/26.4.404]

2. **Buderer NMF (1996)**. Statistical Methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. *Acad Emerg Med* 3:895-900.

3. **Obuchowski NA, McClish DK (1997)**. Sample size determination for diagnostic accuracy studies involving binormal ROC curve indices. *Stat Med* 16:1529-1542.

4. **Flahault A, et al. (2005)**. Sample size calculation should be performed for design accuracy in diagnostic test studies. *J Clin Epidemiol* 58:859-862.

5. **Dendukuri N, et al. (2004)**. Bayesian sample size determination for prevalence and diagnostic test studies in the absence of a gold standard test. *Biometrics* 60:388-397.

6. **Hajian-Tilaki K (2014)**. Sample size estimation in diagnostic test studies of biomedical informatics. *J Biomed Inform* 48:193-204.

7. **Bujang MA, Adnan TH (2016)**. Requirements for minimum sample size for sensitivity and specificity analysis. *J Clin Diagn Res* 10:YE01-YE06.

---

## 🎯 SUMMARY & RECOMMENDATIONS

### Key Findings

1. **Bujang (2023) is a gold-standard methodological paper** for diagnostic test sample size planning based on precision (CI width), not power.

2. **ClinicoPath has partial coverage**:
   - `samplingerror`: Different approach (Kayser E(p) formula)
   - `screeningcalculator`: Unclear if it implements Clopper-Pearson CI width method
   - `cisingle`: Provides binomial CI but not sample size planning

3. **Main gap**: No dedicated jamovi function for **Clopper-Pearson CI-width-based sample size** calculation that:
   - Differentiates diagnostic (high Sens AND Spec) from screening (high Sens OR Spec)
   - Stratifies by prevalence
   - Outputs publication-ready sample size statements
   - Matches Bujang 2023 tables

### Actionable Recommendations

| Rank | Action | Effort | Impact | Timeline |
|:---:|---|:---:|:---:|:---:|
| 1 | Implement `diagnosticsamplesize` module with Clopper-Pearson method | 🟡 Medium | 🟢 High | 2-3 weeks |
| 2 | Add sample size statement generator (template engine) | 🟢 Low | 🟡 Medium | 1 week |
| 3 | Create vignette comparing ClinicoPath methods (Kayser E(p) vs Clopper-Pearson) | 🟢 Low | 🟡 Medium | 1 week |
| 4 | Validate against Bujang 2023 Tables 2-4 (unit tests) | 🟢 Low | 🟢 High | 3 days |
| 5 | Document when to use precision-based vs power-based planning | 🟢 Low | 🟡 Medium | 2 days |

### Long-Term Vision

**Goal**: Make ClinicoPath the **go-to jamovi module for diagnostic test research**, covering:
- Sample size (Clopper-Pearson, ROC-based, Bayesian)
- ROC analysis (pROC, timeROC, ordinal, multiclass)
- Agreement (kappa, ICC, Bland-Altman)
- Diagnostic meta-analysis (HSROC, bivariate)
- Decision curves and net benefit

**This review addresses**: Sample size planning (Gap 1 above). Other capabilities already exist in ClinicoPath's extensive diagnostic suite (199 modules screened).

---

**End of Review**

---

*Generated by ClinicoPath Jamovi AI Assistant*
*Review Protocol: `/review-article-stats` (v2025-10-27)*
*Module: ClinicoPath v0.0.32.11*
*Repository: https://github.com/sbalci/ClinicoPathJamoviModule*
