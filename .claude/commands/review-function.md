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

# Detailed Jamovi Function Code Review

You are conducting a thorough code review of the jamovi function `$ARGUMENTS`. Focus on code quality, best practices, performance, and maintainability.

## Review Target

Function: **`$ARGUMENTS`**

## Code Review Focus Areas

### 🏗️ **Architecture & Design**

- R6 class structure and inheritance
- Function modularization and separation of concerns
- Data flow and state management
- Error propagation and handling

### 🔧 **Implementation Quality**

- Algorithm efficiency and performance
- Memory usage patterns
- Code readability and maintainability
- Following jamovi and R best practices

### 🛡️ **Robustness & Security**

- Input validation completeness
- Edge case handling
- Error message quality and helpfulness
- Data sanitization

### 📚 **Documentation & UX**

- Code comments and self-documentation
- User interface clarity
- Help text and explanatory content
- Accessibility considerations
- Natural‑language summary of results (plain, copy‑ready).
- Explanatory output panel: what the function does, when/how to use it, assumptions/caveats, and a short user guide.

### ⚡ **Performance & Scalability**

- Computational complexity
- Memory efficiency
- Large dataset handling
- Optimization opportunities

### **Clinician‑Friendly (Pathologist/Oncologist) Additions**

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

### 🩺 Clinician‑Friendly UX & Explanations

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

### 🔍 CODE REVIEW: `$ARGUMENTS`

**Overall Quality**: ⭐⭐⭐⭐⭐ (1-5 stars)  

**Maintainability**: HIGH/MEDIUM/LOW  

**Performance**: EXCELLENT/GOOD/NEEDS_WORK  

**User Experience**: EXCELLENT/GOOD/NEEDS_WORK  

#### 🏆 **STRENGTHS**

1. [Specific positive findings with code references]
2. [Well-implemented patterns]
3. [Good practices observed]

#### 🚨 **CRITICAL ISSUES**

1. [Security/reliability concerns with file:line references]
2. [Performance bottlenecks]
3. [Major design flaws]

#### ⚠️ **IMPROVEMENT OPPORTUNITIES**

1. [Code quality improvements with examples]
2. [Refactoring suggestions]
3. [Performance optimizations]

#### 💡 **ENHANCEMENT SUGGESTIONS**

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

#### 🔧 **SPECIFIC RECOMMENDATIONS**

**Architecture:**

```r
# Suggested refactoring
```

#### 📋 **ACTION ITEMS**

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
# UI improvements
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
          - type: LayoutBox
            margin: large
            children:
              - type: CheckBox
                name: showSummaries
                label: Analysis Summary
              - type: CheckBox
                name: showExplanations
                label: Show Explanations


```

```yaml
# .r.yaml (report sentences)
items:
  - name: report
    type: Html
    title: "Report sentence"
```

```r
# .b.R (auto‑generated interpretation)
interp <- sprintf(
  "The %s between %s and %s was %s (%.2f, 95%% CI %.2f–%.2f), p = %.3f.",
  if (test == "ttest") "difference in means" else "difference in distributions",
  g1, g2, stat_name, stat_value, ci_low, ci_high, pval
)
self$results$report$setContent(interp)
```

#### 📋 **ACTION ITEMS**

**High Priority:**

- [ ] [Specific actionable item]
- [ ] [Another specific item]
- [ ] Add plain‑language tooltips.
- [ ] Insert example‑interpretation blocks for key outputs.
- [ ] Implement misuse guards (e.g., switch to Fisher’s exact when expected counts < 5).

**Medium Priority:**

- [ ] [Enhancement opportunity]
- [ ] [Code quality improvement]

**Nice to Have:**

- [ ] [Future enhancement]
- [ ] [Documentation improvement]

#### 📊 **METRICS & ASSESSMENT**

| Aspect | Score | Notes |
|--------|-------|-------|
| Code Quality | X/10 | [Brief note] |
| Error Handling | X/10 | [Brief note] |
| Performance | X/10 | [Brief note] |
| Documentation | X/10 | [Brief note] |
| User Experience | X/10 | [Brief note] |

**Recommendation**: APPROVE / APPROVE_WITH_CHANGES / NEEDS_REWORK

Provide specific, actionable feedback with code examples where helpful.
