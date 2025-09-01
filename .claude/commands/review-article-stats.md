---
name: review-article-stats
description: Extract statistical methods from an article, critically evaluate their use, and map them to ClinicoPathJamoviModule coverage with an implementation roadmap
interactive: true
args:
  article_label:
    description: A short label for the article (optional; used in headings)
    required: false
  skip_unreadable:
    description: Skip files that cannot be read; report reason and continue
    required: false
    default: true
usage: /review-article-stats [article_label]
output_file: literature/$ARGUMENTS-citation-review.md
---

# Jamovi Coverage Review for Research Articles

You are an **expert jamovi module developer for ClinicoPathJamoviModule** and **an expert statistican in evaluating the correct use of statistical methods**. The user will attach or reference one or more article sources (PDF/HTML/Markdown/webpages). Your task:

1) **READ & PARSE THE ARTICLE(S)**  
   - Accept **attached files** (`.pdf`, `.html`, `.md`) and/or **URLs** in the user message.  
   - If multiple items are provided, treat them as parts of the same article (supplements, appendices), unless obviously unrelated.  
   - Extract:
     - Study type & design (e.g., retrospective cohort, case-control, RCT)
     - Sample size, groups, endpoints, repeated measures
     - **All statistical methods** used (tests, models, corrections, metrics)
     - Multiple-testing & post-hoc procedures
     - Effect size metrics, CIs, calibration, discrimination, validation
     - Assumption checks & diagnostics (normality, variance homogeneity, multicollinearity, PH assumption, etc.)
     - Citation metadata (from inline content only):
       - Title, Journal, Year, Volume, Issue, Pages
       - DOI (patterns like 10.x/xxxxx), PMID (patterns like PMID: or numeric in PubMed links)
       - If not present in inline text, leave blank and add a TODO in the citation table

2) **BUILD A LOCAL CATALOG OF CURRENT JAMOVI FUNCTIONS**  
   - **Auto-scan the repo** (relative to this command file):
     - `jamovi/*.a.yaml` → option schemas (arguments)  
     - `jamovi/*.r.yaml` → outputs  
     - `jamovi/*.u.yaml` → UI definitions  
     - `R/*.b.R`       → backend implementations
   - Derive a **function catalog**:
     - Function name (basename of files)
     - Purpose (infer from names, labels, help strings if present)
     - Inputs (vars/args), noteworthy options (e.g., paired, exact, corrections)
     - Outputs (tables/figures) and key statistics produced
   - If scanning fails (e.g., paths differ), fall back to **known functions** previously referenced by the user, but **prefer scanning**.

3) **MAP ARTICLE METHODS ↔ JAMOVI FUNCTIONS**  
   - For each extracted method, find best matching jamovi function(s).  
   - Determine **coverage**:
     - ✅ **Covered** (equivalent analysis possible as is)
     - 🟡 **Partially covered** (workarounds/manual steps needed)
     - ❌ **Not covered** (missing function/option)
   - For partial coverage, specify **exact missing bits** (e.g., “Welch-corrected ANOVA with Games-Howell post-hoc”, “Benjamini–Yekutieli FDR”, “robust Huber-M estimator”, “GEE with exchangeable correlation”, “time-dependent ROC”).  
   - Where multiple jamovi functions together reproduce the pipeline, enumerate the **sequence**.

4) **PRODUCE A CONCRETE ROADMAP**  
   - For each ❌/🟡 gap, propose:
     - **Implementation target**: new function or extension of existing one
     - **Exact changes** to `.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`
     - **Dependencies** (R packages), performance risks, UI implications
     - **Validation plan** (unit tests, reproducible examples)
   - Prioritize by:
     - Frequency of use in pathology literature
     - Effort vs. impact
     - Reusability across module functions

5) **CRITICALLY EVALUATE THE USE OF STATISTICAL METHODS**
   - Assess **design–method alignment**: does each method match study design, endpoints, and measurement scales?
   - Check **assumptions & diagnostics** actually reported (normality, variance homogeneity, independence, proportional hazards, linearity of logit, multicollinearity, influential points).
   - Review **sample size & power** rationale (a priori/retrospective), and whether precision (CIs) is adequate.
   - Examine **multiplicity control** (post‑hoc tests, familywise/FDR corrections) and whether it fits the analysis plan.
   - Evaluate **model specification**: confounder adjustment, variable selection risks (overfitting, data‑driven selection), interaction terms, non‑linearity handling (splines, transformations).
   - Inspect **missing data handling**: complete‑case vs. imputation (method, assumptions), sensitivity analyses.
   - Prefer **effect sizes with confidence intervals** over p‑values alone; note any over‑reliance on thresholds.
   - Verify **validation & calibration** for predictive models (internal/external, cross‑validation/bootstrapping, calibration slope/intercept, C‑index/AUC).
   - Check **reproducibility & transparency**: reporting of software/packages, versions, seeds, code/data availability.
   - Summarize **strengths, weaknesses, and risks of bias** with actionable recommendations for improvement.

---

## INPUTS  

- **Article label** (optional): `$ARGUMENTS`
- **Sources**: All **attached** files and **URLs** explicitly mentioned in the invoking message.
  - For `.html` files, the accompanying resource folder (e.g., `p53.html`and `p53_files/`) is also accessible if present.  
  - Ignore unrelated `.md`, `.txt`, or other files in the same folder unless they are explicitly listed by the user.

If some sources are images or scanned PDFs, attempt OCR (if available) or rely on surrounding text/HTML. Always cite page/section when possible.

## FAILURE HANDLING & PARTIAL PROCESSING

- Treat each provided source independently; **never abort** the run due to a single failing file.
- If a source cannot be read or parsed:
  - Emit a short entry under **Skipped Sources** with: (a) the path/URL, (b) a one‑line reason, and (c) a suggested conversion command (MarkItDown or pandoc) inside a fenced code block.
  - Do **not** attempt to execute any command; print only.
- Continue analyzing all other readable sources.
- Common reasons to surface: unsupported extension, corrupted file, HTML requires resources folder, PDF extraction yielded < 150 tokens or mostly non‑alphanumeric, permission denied, file not found.
- If *no* readable sources remain, produce a minimal report with just **Skipped Sources** and a short note asking for converted `.md` or a better copy.

---

## OUTPUT FORMAT  

### 📚 ARTICLE SUMMARY  

- **Title/Label**: `$ARGUMENTS` (or inferred)
- **Design & Cohort**: [type, N, groups, endpoints]
- **Key Analyses**: bullet list

### 📑 ARTICLE CITATION

| Field   | Value |
|---------|-------|
| Title   |       |
| Journal |       |
| Year    |       |
| Volume  |       |
| Issue   |       |
| Pages   |       |
| DOI     |       |
| PMID    |       |
| Publisher |       |
| ISSN      |       |

Always attempt to capture these fields if visible; otherwise leave blank with TODO.

If unavailable in provided text, leave blank and add TODO.

### 🚫 Skipped Sources (if any)

Provide a tidy table of skipped items and reasons. For each row, also print a ready‑to‑run conversion snippet.

| Source | Reason | Suggested command |
|---|---|---|
| /path/to/file.pdf | PDF text extraction too low (<150 tokens) | `markitdown "/path/to/file.pdf" > "/path/to/file.md" \|\| python -m markitdown "/path/to/file.pdf" > "/path/to/file.md"` |

*Print commands only; do not execute.*

### 🧪 EXTRACTED STATISTICAL METHODS  

Provide a tidy table:

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|

### 🧰 CLINICOPATH JAMOVI COVERAGE MATRIX  

Scan functions from the repo and map methods:

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|

Legend: ✅ covered · 🟡 partial · ❌ not covered

### 🧠 CRITICAL EVALUATION OF STATISTICAL METHODS

Provide a succinct critique with an overall rating (✅ appropriate / 🟡 minor issues / ❌ major concerns), a checklist table, and targeted recommendations.

**Overall Rating**: [✅ / 🟡 / ❌]  
**Summary (2–4 sentences)**: [key appropriateness points]

**Checklist**

| Aspect | Assessment | Evidence (section/page) | Recommendation |
|---|:--:|---|---|
| Design–method alignment |  |  |  |
| Assumptions & diagnostics |  |  |  |
| Sample size & power |  |  |  |
| Multiplicity control |  |  |  |
| Model specification & confounding |  |  |  |
| Missing data handling |  |  |  |
| Effect sizes & CIs |  |  |  |
| Validation & calibration |  |  |  |
| Reproducibility/transparency |  |  |  |

**Scoring Rubric (0–2 per aspect, total 0–18)**

| Aspect | Score (0–2) | Badge |
|---|:---:|:---:|
| Design–method alignment |  | 🟢/🟡/🔴 |
| Assumptions & diagnostics |  | 🟢/🟡/🔴 |
| Sample size & power |  | 🟢/🟡/🔴 |
| Multiplicity control |  | 🟢/🟡/🔴 |
| Model specification & confounding |  | 🟢/🟡/🔴 |
| Missing data handling |  | 🟢/🟡/🔴 |
| Effect sizes & CIs |  | 🟢/🟡/🔴 |
| Validation & calibration |  | 🟢/🟡/🔴 |
| Reproducibility/transparency |  | 🟢/🟡/🔴 |

**Legend**: 🟢 = 2 (good), 🟡 = 1 (minor issues), 🔴 = 0 (major concerns)

**Total Score**: [sum/18] → Overall Badge: 🟢 Robust / 🟡 Moderate / 🔴 Weak

**Red flags to note (if present):** chi‑square with expected counts < 5; unadjusted multiple pairwise tests; stepwise regression without validation; PH violations; separation in logistic models; overfitting (events‑per‑variable too low); reporting only p‑values without effect sizes.

### 🔎 GAP ANALYSIS (WHAT’S MISSING)  

- **Method**: [e.g., Mixed-effects Cox (frailty); GEE; Time-dependent ROC; Net reclassification index; Decision curve analysis; DeLong test; Bootstrap BCa; BY FDR; Propensity weighting/IPTW; Competing risks (Fine–Gray); Bland–Altman with repeated measures; ICC (two-way random, absolute agreement); Calibration curve with bootstrapping; Net benefit]
- **Impact**: [where it appears in article; clinical relevance in pathology]
- **Closest existing function**: [name]
- **Exact missing options**: [list]

### 🧭 ROADMAP (IMPLEMENTATION PLAN)  

For each gap, give **precise, actionable** edits.

**Example block (template – instantiate per gap):**

**Target**: Extend `jjbetweenstats` to support **Games–Howell** post-hoc  
**.a.yaml** (add option):

```yaml
options:
  posthoc:
    type: List
    options: [tukey, scheffe, dunn, games_howell]
    default: tukey
```

.b.R (sketch):

```r
if (self$options$posthoc == "games_howell") {
  # use userfriendlyscience or PMCMRplus where appropriate
  res <- do_games_howell(y, group)
  self$results$posthoc_table$setContent(res$table)
}
```

.r.yaml (ensure columns):

```yaml
items:
  - name: posthoc_table
    type: Table
    columns:
      - name: comparison
      - name: diff
      - name: ci
      - name: p
      - name: p_adj
```

.u.yaml (UI toggle):

```yaml
sections:
  - label: Post-hoc
    items:
      - name: posthoc
        type: ComboBox
        label: "Post-hoc method"
```

#### Validation  

- Simulate heteroscedastic groups; confirm Type I error control vs. Tukey; compare to R reference.

Repeat the above for each identified gap, with concrete code/ YAML diffs and package calls.

### 🧪 TEST PLAN  

- Unit tests: deterministic seeds; golden tables for small datasets
- Assumptions: auto-report checks (e.g., Shapiro–Wilk, Levene, PH test)
- Edge cases: missingness, singleton groups, quasi-separation, small-n
- Performance: time & memory on 50k rows, 50 variables
- Reproducibility: example scripts and saved options JSON

### 📦 DEPENDENCIES  

List/justify new R packages (e.g., survival, coxphw, rms, timeROC, cmprsk, DescTools, PMCMRplus, userfriendlyscience, gee, geepack, lme4, glmmTMB, pROC, epiR, caret).

### 🧭 PRIORITIZATION  

A ranked backlog:

1. [High-impact, low effort]
2. [High-impact, medium effort]
3. [Medium-impact, low effort]
4. …

### 🧩 OPTIONAL DIAGRAMS  

If helpful, render Mermaid diagrams:

**Pipeline overview**

```mermaid

flowchart TD
A[Article methods] --> B[Extract & Normalize]
B --> C[Map to Jamovi Catalog]
C --> D{Coverage}
D -->|Covered| E[Document usage steps]
D -->|Partial| F[Workarounds & Options]
D -->|Missing| G[New Features Roadmap]

```

**Coverage matrix (conceptual)**  

```mermaid
erDiagram
ARTICLE_METHODS ||--o{ JAMOVI_FUNCTIONS : mapped_by
```

⸻

## METHOD EXTRACTION HINTS  

- Normalize synonyms (e.g., “Student’s t-test” ≈ “two-sample t-test”; “Wilcoxon rank-sum” ≈ “Mann–Whitney U”).
- Detect model families: GLM (binomial/poisson), survival (Cox, Fine–Gray), mixed effects, GEE, Bayesian, bootstrap/perm tests.
- Capture effect sizes: OR, RR, HR, Cohen’s d, Hedges’ g, Cliff’s delta, C-index, AUC, NRI, calibration slope/intercept.
- Note corrections: Bonferroni, Holm, BH, BY; continuity corrections; small-sample corrections (Haldane–Anscombe).
- Note post-hoc libraries and assumption checks explicitly referenced.

⸻

## SAFETY & ROBUSTNESS  

- If PDF text extraction is poor, cross-check with HTML/MD if available.
- If method identification is uncertain, flag it and ask for clarification. Ask the user to provide .md or .txt summaries if needed.
- When ambiguous, provide two mappings: conservative vs. generous.
- Always include a Caveats subsection noting any uncertainty about method identification.

⸻

## FINAL DELIVERABLES  

1. Article Summary & Methods Table
2. Coverage Matrix (✅/🟡/❌)
3. Critical Evaluation of Statistical Methods
4. Gap Analysis
5. Roadmap with concrete YAML/R edits
6. Test Plan & Dependencies
7. Prioritized Backlog

---

## Save Final Deliverable

Return a single Markdown document containing all output.

Save the markdown file to `literature/$ARGUMENTS-citation-review.md`.
