# Guide Files Index

**Location:** `/vignettes/*_guide.md`

This directory contains comprehensive guides for jamovi module development in the ClinicoPath project.

---

## Primary Guide (START HERE)

### 📘 `jamovi_module_patterns_guide.md` - **Comprehensive Jamovi Development Guide**

**Created:** 2025-01-17
**Source:** Analysis of jmvbaseR official example module + ClinicoPath production implementations

**Use this guide for:**
- Starting a new jamovi analysis
- Understanding module structure
- Declaring package dependencies (DESCRIPTION `Imports` vs `Remotes`)
- Data handling and state management
- Formula building for statistical models
- Plot rendering and state serialization
- Output patterns (tables, plots, HTML, preformatted)
- Best practices and common pitfalls

**Contents:**
1. Module Structure & 4-File Architecture (incl. DESCRIPTION `Imports`/`Remotes`)
2. Data Handling Patterns (jmvcore functions)
3. State Management (why & how)
4. State Serialization Solutions (tibble → list fixes)
5. Formula Building (from jmvbaseR)
6. Syntax Generation (.asSource methods)
7. Output Patterns (4 types)
8. Best Practices (DO/DON'T lists)
9. jmvcore Function Reference

---

## Release Gate (READ BEFORE SUBMITTING TO THE JAMOVI LIBRARY)

### 🚦 `jamovi_library_review_guide.md` - **What the jamovi library reviewer checks**

**Created:** 2026-08-20
**Source:** Five real audit reports in `jamovi-library-audit/` (ClinicoPathDescriptives,
jsurvival, meddecide, OncoPath, jjstatsplot — 2026-07-13 and 2026-08-17/18 rounds)

**Use this guide for:**
- The pre-submission checklist (runnable commands, one per finding class)
- The ten findings that recur across **every** module
- Knowing which fixes the reviewer treats as MEDIUM/HIGH vs. polish

**Contents:**
1. Pre-submission checklist
2. Findings that recur across every module
3. Render functions must NULL-guard `image$state`
4. HTML output must be theme-safe (dark mode)
5. `setVisible(FALSE)` is not an error mechanism
6. Row structure that isn't result-dependent belongs in `.init()`
7. No named HTML entities except the structural five
8. `warning()` is invisible to jamovi users
9. Translatable strings are whole sentences
10. Every package used must be declared — including base packages
11. Dead code must not reference a schema that no longer exists
12. UI label conventions
13. **The `type: Notice` trap** — it does not compile; re-test after every jmvtools upgrade
14. Encoding review findings as tests

**Tooling that goes with it:**
- `tools/check_state_guards.py` — exits 1 on any unguarded `image$state` read
- `tools/theme_safe_html.py` — the dark-theme transform (idempotent; `--apply` to write)
- `tests/testthat/test-zzz-results-rendering-contract.R` — the same rules as tests

---

## Specialized Guides (By Topic)

### File-Specific Guides

#### `jamovi_a_yaml_guide.md` - Analysis Definition (.a.yaml)
- Option types and properties
- Default values
- Descriptions for R and jamovi
- Variable selectors
- List options

#### `jamovi_b_R_guide.md` - Backend Implementation (.b.R)
- R6 class structure
- .init() and .run() methods
- Private vs public methods
- Helper function patterns
- Data access patterns

#### `jamovi_r_yaml_guide.md` - Results Definition (.r.yaml)
- Output types (Table, Image, Html, Preformatted)
- Column definitions
- clearWith dependencies
- Visibility rules

#### `jamovi_u_yaml_guide.md` - User Interface (.u.yaml)
- UI control types
- Layout organization
- VariableSupplier patterns
- Enable/visible conditions

### Feature-Specific Guides

#### `jamovi_tables_guide.md` - Table Output
- Table definitions in .r.yaml
- Populating tables in .b.R
- addRow() and setRow() methods
- Column formatting
- Nested tables

#### `jamovi_plots_guide.md` - Plot/Image Output
- Plot state management
- renderFun implementation
- ggtheme integration
- State serialization for plots
- Multiple plot types

#### `jamovi_notices_guide.md` - User Notices
- jmvcore::Notice API
- NoticeType (ERROR, STRONG_WARNING, WARNING, INFO)
- Positioning strategies
- Single-line content requirement
- Clinical profile notices

#### `jamovi_formula_guide.md` - Statistical Formulas
- Building R formulas from options
- jmvcore::composeTerm() usage
- jmvcore::composeTerms() usage
- Model terms handling
- Full-factorial generation

#### `jamovi_js_guide.md` - Custom JavaScript
- When to use JavaScript
- Event handling
- Dynamic UI updates
- Custom control behavior

#### `jamovi_actions_guide.md` - UI Actions & Events
- onChange events
- UI state updates
- Conditional option visibility
- Custom validation

#### `jamovi_i18n_guide.md` - Internationalization (i18n)
- **Created:** 2026-01-31
- **Source:** jamovi i18n API documentation (dev.jamovi.org)
- Translation architecture and workflow
- Marking strings with `.()` function
- Working with .po and .pot files
- jmvtools translation commands
- Turkish medical terminology glossary
- Clinical pathology translation guidelines
- Weblate integration for collaborative translation
- Complete i18n implementation examples

**Use this guide for:**
- Adding multilingual support to jamovi modules
- Preparing Turkish translations for clinical users
- Setting up translation catalogs (.po files)
- Implementing gettext best practices
- Translating medical and statistical terminology
- Integrating with Weblate translation platform

---

## How to Use These Guides

### Workflow

```
1. Starting new analysis?
   → Read: jamovi_module_patterns_guide.md (overview)
   → Then: jamovi_a_yaml_guide.md + jamovi_u_yaml_guide.md (define interface)

2. Implementing backend logic?
   → Read: jamovi_b_R_guide.md (R6 class patterns)
   → Reference: jamovi_module_patterns_guide.md > Data Handling section

3. Adding table output?
   → Read: jamovi_r_yaml_guide.md (define structure)
   → Read: jamovi_tables_guide.md (populate data)

4. Adding plot output?
   → Read: jamovi_plots_guide.md (state management critical!)
   → Reference: jamovi_module_patterns_guide.md > State Management section

5. Adding user notices?
   → Read: jamovi_notices_guide.md (API usage)
   → Reference: jamovi_module_patterns_guide.md > Best Practices

6. Building statistical models?
   → Read: jamovi_formula_guide.md (formula patterns)
   → Reference: jmvbaseR example implementations
```

### Quick Reference: Common Problems & Solutions

| Problem | Guide to Consult | Section |
|---------|------------------|---------|
| Non-CRAN dependency: `Imports` vs `Remotes`, install fails/redundant | `jamovi_module_patterns_guide.md` | DESCRIPTION: Dependencies |
| Plot doesn't update when options change | `jamovi_module_patterns_guide.md` | State Management |
| `dplyr::pull()` error on plot data | `jamovi_module_patterns_guide.md` | State Serialization |
| Variable names with spaces/special chars | `jamovi_module_patterns_guide.md` | Data Handling |
| Formula building for regression/ANOVA | `jamovi_formula_guide.md` | All |
| "Cannot add bindings to locked environment" | `jamovi_module_patterns_guide.md` | State Management |
| Notice content with line breaks | `jamovi_notices_guide.md` | Content Rules |
| Table not populating | `jamovi_tables_guide.md` | Populate section |
| UI options not appearing | `jamovi_u_yaml_guide.md` | Control Types |
| Translations not appearing | `jamovi_i18n_guide.md` | Troubleshooting |
| Strings not being extracted to .po | `jamovi_i18n_guide.md` | Marking Strings |
| `self` scope issues with `.()` | `jamovi_i18n_guide.md` | Advanced Patterns |

---

## Guide Maintenance

### When to Update Guides

- ✅ New pattern discovered from example modules
- ✅ Solution found for recurring problem
- ✅ jamovi API changes
- ✅ Best practice emerges from production use

### How to Update

1. Update the relevant `*_guide.md` file
2. Add changelog entry at top of guide
3. Cross-reference from `jamovi_module_patterns_guide.md` if widely applicable
4. Update this README_GUIDES.md index if new guide created

### Guide Template

New guides should follow this structure:

```markdown
# Guide Title

**Created:** YYYY-MM-DD
**Last Updated:** YYYY-MM-DD
**Purpose:** One-sentence description

## When to Use This Guide

[Specific scenarios]

## Core Concepts

[Main patterns/concepts]

## Examples

[Code examples from jmvbaseR or ClinicoPath]

## Common Pitfalls

[What NOT to do]

## References

[Links to official docs, example code]
```

---

## References

- **jmvbaseR Example Module:** `/Users/serdarbalci/Documents/GitHub/jmvbaseR`
- **Official jamovi Documentation:** `./dev.jamovi.org-master/`
- **ClinicoPath Examples:** This repository (`R/*.b.R`, `jamovi/*.yaml`)

---

**Last Updated:** 2026-06-22

**Recent additions:**
- 2026-06-22: Documented jmvtools DESCRIPTION `Imports`/`Remotes` behavior - a package may now appear in both fields; jmvtools suppresses the CRAN-mirror download of an import when it also appears in `Remotes` (see `jamovi_module_patterns_guide.md` → "DESCRIPTION: Dependencies")
- 2026-01-31: Added `jamovi_i18n_guide.md` - Comprehensive internationalization guide