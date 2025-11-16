---
name: review-function
description: Detailed code review of a specific jamovi function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to review
    required: true
    autocomplete: functions
usage: /review-function <function_name>
---

_Note: This command intentionally avoids emoji and uses UI toggles to control visibility of natural‑language outputs._

# Detailed Jamovi Function Code Review

You are an expert R package and jamovi developer, and an expert biostatistician working closely with pathologists and clinicians. You are conducting a thorough, critical review of the jamovi function `$ARGUMENTS`, focusing on mathematical and statistical correctness, clinical readiness, code quality, best practices, performance, and maintainability.

## Review Target

Function: **`$ARGUMENTS`**

## Code Review Focus Areas

### Architecture & Design

- R6 class structure and inheritance
- Function modularization and separation of concerns
- Data flow and state management
- Error propagation and handling

### Implementation Quality

- Algorithm efficiency and performance
- Memory usage patterns
- Code readability and maintainability
- Following jamovi and R best practices

### Robustness & Security

- Input validation completeness
- Edge case handling
- Error message quality and helpfulness
- Data sanitization

### Documentation & UX

**Visibility rule:** Natural‑language summaries and educational/explanatory outputs must render **only when** the corresponding UI options are enabled by the user (see the `.u.yaml` checkboxes below). Keep these sections hidden by default unless selected.

- Code comments and self-documentation
- User interface clarity
- Help text and explanatory content
- Accessibility considerations
- Natural‑language summary of results (plain, copy‑ready).
- Explanatory output panel: what the function does, when/how to use it, assumptions/caveats, and a short user guide.

### Performance & Scalability

- Computational complexity
- Memory efficiency
- Large dataset handling
- Optimization opportunities

### Mathematical & Statistical Correctness

- Correctness of formulas, test statistics, and estimators (e.g., OR/HR/RR, AUC, ICC, kappa, RMST, DeLong, Fine–Gray).
- Appropriate choice of statistical methods for the intended design (paired vs independent, one‑sided vs two‑sided, parametric vs non‑parametric, survival vs binary, etc.).
- Proper handling of assumptions (normality, homoscedasticity, proportional hazards, independence, expected cell counts, etc.) and use of robust alternatives where needed.
- Correct construction and interpretation of confidence intervals, p‑values, and effect sizes (including direction and units).
- Treatment of ties, censoring, competing risks, and clustering when applicable.
- Handling of missing data (complete‑case vs imputation vs weighting) and clear communication of what is done.
- Alignment with reference implementations (e.g., base R, survival, pROC, irr, cmprsk, lme4, etc.) for the same inputs.

### Clinical & Release Readiness

- Are defaults clinically sensible for pathologists and oncologists (e.g., common cut‑offs, scales, and summaries)?
- Are outputs interpretable and safe to use in pathology/oncology reports (no misleading labels, correct units, clear directions of effect)?
- Are misuse risks mitigated (e.g., chi‑square with low expected counts, overfitted models, too few events per variable) with strong warnings or errors?
- Are explanatory texts and help sufficient for clinicians without advanced statistics training?
- Has the function behavior been validated against reference analyses and edge cases (documented test set or verification script)?
- Is the function stable and predictable across a range of dataset sizes and realistic clinical scenarios?
- Based on the above, is the function **ready for clinical-facing use and public release**, or does it require further validation/refinement?

### Clinician‑Friendly (Pathologist/Oncologist) Additions

- Plain‑language labels and tooltips for each option (avoid jargon; show examples: e.g., “Select tumor grade (G1/G2/G3)”).
- In‑app micro‑explanations for statistics (what the test answers clinically, assumptions, effect size meaning, minimal sample heuristics).
- Glossary panel (AUC, OR, HR, RMST, FDR, ICC, kappa, DeLong, Fine–Gray, etc.) with 1–2 line clinical interpretations.
- Guided mode (wizard): “Pick your outcome → choose groups → check assumptions → run → interpret outputs.”
- Contextual warnings for misuse (e.g., “Paired test selected but groups are independent”).
- Example interpretations beneath each key result (e.g., “An OR of 2.1 means the odds are ~2× higher in group A”).
- One‑click report sentences (auto‑generated paragraphs with placeholders filled from results; copy to clipboard).
- Defaults tuned to common clinical scenarios; show ‘Recommended’ badges.
- Accessibility & readability: larger font option, color‑blind‑safe palettes, avoid red‑green only.
- Internationalization hooks (TR/EN) for labels, help, and report templates.

### Clinician‑Friendly UX & Explanations

| Area | Status | Notes |
|---|---:|---|
| Plain‑language labels/tooltips | ☐ | |
| Micro‑explanations per option | ☐ | |
| Glossary entries present | ☐ | |
| Guided flow (wizard) | ☐ | |
| Misuse warnings/guards | ☐ | |
| Example interpretations in outputs | ☐ | |
| Report sentence templates | ☐ | |
| Sensible defaults & presets | ☐ | |
| Accessibility (CB‑safe, font) | ☐ | |
| i18n (TR/EN) coverage | ☐ | |
| Natural‑language summary in output | ☐ | |
| About/How‑to section present | ☐ | |
| Caveats & assumptions panel | ☐ | |
| Guidance links/examples | ☐ | |

## Review Response Format

### CODE REVIEW: `$ARGUMENTS`

**Overall Quality**: 1–5 (stars)  

**Maintainability**: HIGH/MEDIUM/LOW  

**Performance**: EXCELLENT/GOOD/NEEDS_WORK  

**User Experience**: EXCELLENT/GOOD/NEEDS_WORK  

**Mathematical/Statistical Correctness**: CORRECT / MINOR_ISSUES / MAJOR_ISSUES  

**Clinical & Release Readiness**: READY / NEEDS_VALIDATION / NOT_READY  

#### STRENGTHS

1. [Specific positive findings with code references]
2. [Well-implemented patterns]
3. [Good practices observed]

#### CRITICAL ISSUES

1. [Mathematical/statistical correctness problems (wrong formulas, tests, CI/p‑value calculations) with file:line references]
2. [Clinical safety or misuse risks (e.g., misleading defaults, lack of guards for low n/events, incorrect labels/units)]
3. [Performance bottlenecks and major design flaws impacting reliability or maintainability]

#### IMPROVEMENT OPPORTUNITIES

1. [Code quality improvements with examples]
2. [Refactoring suggestions]
3. [Performance optimizations]

#### ENHANCEMENT SUGGESTIONS

1. [Feature improvements]
2. [User experience enhancements]
3. [Future-proofing recommendations]

#### **Clinician‑Friendly Improvements:**

- Provide **Example interpretation** blocks under tables/plots.
- Add **guided mode** that enforces a recommended sequence (variables → assumptions → run → interpret).
- Include **copy‑ready report sentences** with placeholders auto‑filled from results.
- Add **misuse detection** (e.g., warn if expected counts < 5 for chi‑square; suggest Fisher’s exact).
- Offer **clinical presets** (e.g., “2×2 diagnostic test,” “KM survival with median & 95% CI,” “ROC with DeLong CI”).
- Provide **TR/EN translations** and ensure medical terminology is consistent.
- Use **color‑blind‑safe** default palettes and increase table readability (thousands separators, units).

**Natural‑language summaries & Explanatory Outputs:**

- Add a top‑level **Summary** box with a plain‑language paragraph that names the test/model, the comparison, key effect (with CI) and p‑value, and one clinical interpretation sentence.
- Add an **About this analysis** panel that briefly explains what the function does, when to use it, inputs required, and typical outputs (with links to docs).
- Add a **Caveats & assumptions** panel that lists assumptions, data requirements (e.g., expected counts, proportional hazards), and common pitfalls; surface contextual warnings if violated.
- Provide a **How to use** checklist (variables → options → run → interpret), and, if possible, a mini example with mock numbers.

#### SPECIFIC RECOMMENDATIONS

**Architecture:**

```r
# Suggested refactoring
```

**Mathematical/Statistical:**

```r
# Corrections to formulas, test choices, CI/p-value computation, or assumptions handling
```

**Clinical & Release Readiness:**

- [What must change before clinicians/pathologists can safely rely on this function]
- [What validation (comparisons, simulations, unit tests) is still required]
- [A clear recommendation: READY / NEEDS_VALIDATION / NOT_READY, with justification]

#### ACTION ITEMS

- [ ] [Specific actionable item]
- [ ] [Another specific item]
- [ ] Add plain‑language tooltips.
- [ ] Insert example‑interpretation blocks for key outputs.
- [ ] Implement misuse guards (e.g., switch to Fisher’s exact when expected counts < 5).
- [ ] Add natural‑language **Summary** box with copy‑ready text.
- [ ] Add **About this analysis** panel (what/when/how/outputs).
- [ ] Add **Caveats & assumptions** panel with contextual warnings.
- [ ] [Enhancement opportunity]
- [ ] [Code quality improvement]

**Performance:**

```r
# Optimization examples
```

**Error Handling:**

```r
# Better error handling patterns
```

**User Experience:**

```yaml
# Panels controlled by checkboxes; render only when enabled.
children:
  - type: ComboBox
    name: test
    label: "Group comparison test"
    options:
      - label: "t‑test (means)"
        value: ttest
      - label: "Mann–Whitney U (medians)"
        value: wilcox
      - label: "Welch t‑test (unequal variances)"
        value: welch
  - type: CheckBox
    name: assume_equal_var
    label: "Assume equal variances"


  - type: CollapseBox
    label: Output Options
    collapsed: true
    children:
      - type: Label
        label: Analysis Output
        fitToGrid: true
        children:
          # When unchecked, Summary/Explanations sections must not be rendered.
          - type: CheckBox
            name: showSummary
            label: "Show Summary (natural‑language)"
          - type: CheckBox
            name: showExplanations
            label: "Show Explanations (educational notes)"
```

# .u.yaml (labels & tooltips)

```yaml
children:
  - type: ComboBox
    name: test
    label: "Group comparison test"
    options:
      - label: "t‑test (means)"
        value: ttest
      - label: "Mann–Whitney U (medians)"
        value: wilcox
      - label: "Welch t‑test (unequal variances)"
        value: welch
  - type: CheckBox
    name: assume_equal_var
    label: "Assume equal variances"


  - type: CollapseBox
    label: Output Options
    collapsed: true
    children:
      - type: Label
        label: Analysis Output
        fitToGrid: true
        children:
          # When unchecked, Summary/Explanations sections must not be rendered.
          - type: CheckBox
            name: showSummary
            label: "Show Summary (natural‑language)"
          - type: CheckBox
            name: showExplanations
            label: "Show Explanations (educational notes)"
```

```yaml
# .r.yaml (report sentences)
items:
  - name: report
    type: Html
    title: "Report sentence"
  - name: summary
    type: Html
    title: "Summary (natural‑language)"
    visible: false
  - name: explanations
    type: Html
    title: "Explanations"
    visible: false
```

```r
# .b.R (auto‑generated interpretation)
# Auto-generated interpretation sentence (always safe to compute; display controlled by UI)
interp <- sprintf(
  "The %s between %s and %s was %s (%.2f, 95%% CI %.2f–%.2f), p = %.3f.",
  if (test == "ttest") "difference in means" else "difference in distributions",
  g1, g2, stat_name, stat_value, ci_low, ci_high, pval
)
self$results$report$setContent(interp)

# Natural-language summary: only render when user enables 'Show Summary'
if (isTRUE(self$options$showSummary)) {
  summary_text <- sprintf(
    "We compared %s vs %s using %s. The key effect was %s (95%% CI %.2f–%.2f), p = %.3f. Clinically, this suggests %s.",
    g1, g2, stat_name, stat_value, ci_low, ci_high, pval, clinical_hint
  )
  self$results$summary$setVisible(TRUE)
  self$results$summary$setContent(summary_text)
} else {
  self$results$summary$setVisible(FALSE)
}

# Explanations (educational notes): only render when user enables 'Show Explanations'
if (isTRUE(self$options$showExplanations)) {
  expl <- paste0(
    "&lt;b&gt;What does this test answer?&lt;/b&gt; ", test_expl, "&lt;br/&gt;",
    "&lt;b&gt;Assumptions:&lt;/b&gt; ", assumptions_text, "&lt;br/&gt;",
    "&lt;b&gt;Effect size meaning:&lt;/b&gt; ", effect_expl
  )
  self$results$explanations$setVisible(TRUE)
  self$results$explanations$setContent(expl)
} else {
  self$results$explanations$setVisible(FALSE)
}
```
