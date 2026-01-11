---
description: Generate social media promotion text for a jamovi function (for pathologists and clinicians)
argument-hint: "[function-name|random|surprise] [platform:twitter|linkedin|general]"
---

# Social Media Promotion Generator for ClinicoPath Functions

Generate **concise, clinician-friendly promotional text** for the jamovi function **`$1`** targeting **pathologists and oncologists**.

## Function to Promote

**Function Name**: `$1` (or use "random"/"surprise" for automatic selection)

**Target Platform**: `$2` (default: general if not specified)

**Random Selection Mode**:
- Use `random` or `surprise` (or leave empty) to automatically select a **mature function** to promote
- Only functions with stable menuGroups are selected (excludes testing/development functions ending in "T" or "D")
- Great for discovering lesser-known features or creating regular promotional content

## Analysis Steps

1. **Read the function files** to understand capabilities:
   - `jamovi/$1.a.yaml` - options and parameters
   - `R/$1.b.R` - implementation and statistical methods
   - `jamovi/$1.r.yaml` - outputs and results
   - `jamovi/$1.u.yaml` - user interface workflow

2. **Identify clinical value**:
   - What clinical question does it answer?
   - What pathology/oncology scenarios does it address?
   - What makes it unique or valuable?
   - What are typical use cases?

3. **Extract key features**:
   - Statistical methods used
   - Output types (tables, plots, metrics)
   - Ease of use features
   - Clinical interpretation aids

4. **Identify relevant example datasets**:
   - Check the complete test data catalog for function-specific .omv files
   - Match function name prefix to available test datasets
   - Include 1-2 direct download links with clinical context
   - See "Test Data Reference" section below

## Test Data Reference

**Complete Catalog**: See `vignettes/test-data-complete-catalog.Rmd` for all 945+ test datasets

**Key Function Categories** (examples):
- `timeinterval_*.omv` - Time interval calculations (24 files)
- `ihc_*.omv` - IHC heterogeneity analysis (21 files)
- `outcomeorganizer_*.omv` - Survival outcome organization (20 files)
- `singlearm_*.omv` - Single-arm survival trials (18 files)
- `psychopdaROC_*.omv` - ROC analysis (17 files)
- `survivalcont_*.omv` - Continuous survival predictors (17 files)
- `decision_*.omv` - Decision analysis (16 files)
- `swimmerplot_*.omv` - Swimmer plots (16 files)
- `waterfall_*.omv` - Waterfall plots (16 files)
- `decisioncompare_*.omv` - Decision curve comparison (14 files)
- `linechart_*.omv` - Longitudinal line charts (13 files)
- `pathsampling_*.omv` - Pathology sampling adequacy (13 files)
- `stagemigration_*.omv` - Stage migration analysis (13 files)

**General Clinical Datasets**:
- `histopathology.omv` - General histopathology data
- `histopathologySurvival.omv` - Survival analysis examples
- `histopathologyMedicalDecision.omv` - Decision tree examples
- `rocdata.omv` - ROC curve examples
- `colon.omv` - Colon cancer outcomes
- `melanoma.omv` - Melanoma patient data
- `BreastCancer.omv` - Breast cancer analysis

**GitHub Base URL**: `https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data-raw/non-rda/`

**Usage in Promotional Content**:
1. Look up function name in the catalog (e.g., for `swimmerplot` function, find `swimmerplot_*.omv` files)
2. Select the most clinically relevant example (e.g., `swimmerplot_immuno.omv` for immunotherapy trials)
3. Include direct download link in promotional text
4. Phrase as: "Try it yourself: [dataset name] - [clinical scenario]"
5. Always emphasize "download and test immediately" convenience

## Target Audience

- Pathologists (surgical, clinical, molecular)
- Oncologists and cancer researchers
- Clinical laboratory directors
- Medical statisticians working with clinical teams

## Output Requirements

Generate **3 promotional variants**:

### 🐦 Variant 1: Brief (Twitter/X Format)
**Length**: Maximum 280 characters including hashtags
**Style**: Punchy, direct, engaging
**Structure**:
```
[Hook: clinical problem or benefit] [Key feature] [Result/output]

#Pathology #ClinicalResearch #Jamovi
```

### 📱 Variant 2: Standard (Multi-Platform)
**Length**: 100-150 words
**Style**: Professional but accessible
**Structure**:
```
[Engaging opening - clinical challenge]

[What the function does in plain language]

Key capabilities:
• [Feature 1]
• [Feature 2]
• [Feature 3]

Perfect for: [Clinical use cases]

#Pathology #DigitalPathology #Biostatistics #Jamovi
```

### 💼 Variant 3: Extended (LinkedIn Format)
**Length**: 200-300 words
**Style**: Professional, educational, detailed
**Structure**:
```
[Compelling hook - clinical question or challenge]

[Introduction to the function and its clinical purpose]

🔬 Key Features:
• [Feature 1 with clinical benefit]
• [Feature 2 with clinical benefit]
• [Feature 3 with clinical benefit]

📊 Clinical Applications:
• [Use case 1 - specific pathology scenario]
• [Use case 2 - specific oncology scenario]
• [Use case 3 - research application]

[Why it matters for pathologists/oncologists]

[How to access - mention ClinicoPath jamovi module]

#Pathology #DigitalPathology #ClinicalResearch #Biostatistics #Oncology #Jamovi #OpenScience
```

## Writing Guidelines

### ✅ DO:
- Use clinical terminology pathologists understand
- Focus on clinical value, not technical implementation
- Mention specific pathology/oncology scenarios
- Highlight ease of use and accessibility
- Include relevant outputs (AUC, HR, Kaplan-Meier curves, etc.)
- Emphasize clinical relevance and validation
- Use active voice and strong action verbs
- Make it shareable and engaging

### ❌ DON'T:
- Use heavy statistical jargon without context
- Make exaggerated or unsubstantiated claims
- Focus on code or technical implementation
- Assume advanced statistics knowledge
- Use vague "powerful tool" language
- Include R code or syntax
- Overuse emojis or informal language

## Tone Examples

**Good - Clinical and Engaging**:
> "Struggling with inter-observer agreement in tumor grading? Our kappa statistics tool provides instant reliability metrics with confidence intervals - essential for quality assurance in pathology labs. One click, publication-ready results."

**Bad - Too Technical**:
> "Implementation of Cohen's kappa coefficient with asymptotic standard error estimation for categorical agreement assessment using R6 class architecture and jamovi framework."

**Good - Value-Focused**:
> "Compare survival outcomes across treatment protocols in minutes. Generate publication-ready Kaplan-Meier curves, hazard ratios, and log-rank tests with confidence intervals. Designed specifically for oncology researchers."

**Bad - Generic**:
> "A powerful survival analysis function with many options and advanced statistical capabilities for analyzing time-to-event data."

## Hashtag Strategy

### Core Tags (Always Include):
- #Pathology
- #ClinicalResearch
- #Jamovi

### Function-Specific Tags (Choose 2-3):

**For Diagnostic/ROC Functions**:
- #DiagnosticAccuracy #ROC #Biomarkers #DigitalPathology

**For Survival Analysis**:
- #SurvivalAnalysis #Oncology #CancerResearch #Kaplan­Meier

**For Agreement/QA Functions**:
- #QualityAssurance #InterRaterReliability #PathologyQC

**For Decision Analysis**:
- #ClinicalDecisionSupport #CostEffectiveness #EvidenceBasedMedicine

**For Descriptive Statistics**:
- #Biostatistics #DataAnalysis #MedicalStatistics

**General Research Tags**:
- #OpenScience #ResearchTools #MedStats #DataScience

## Clinical Context by Function Type

### Diagnostic/ROC Functions
"Validate new biomarkers against gold standards. Calculate sensitivity, specificity, PPV, NPV, and AUC with DeLong confidence intervals. Optimize cut-points for clinical decision-making."

### Survival Analysis Functions
"Track patient outcomes and treatment response. Visualize time-to-event data with Kaplan-Meier curves. Compare treatment arms with hazard ratios and log-rank tests."

### Agreement Functions
"Ensure diagnostic reproducibility. Measure inter-observer agreement with Cohen's kappa, ICC, or Fleiss' kappa. Generate Bland-Altman plots for method comparison."

### Decision Analysis Functions
"Model clinical decision pathways. Evaluate cost-effectiveness of diagnostic strategies. Build decision trees with probabilistic sensitivity analysis."

### Descriptive Statistics
"Summarize cohort characteristics for Table 1. Compare groups with appropriate statistical tests. Generate publication-ready summary tables."

## Platform-Specific Adjustments

If `$2` is specified:

### twitter
- Strict 280-character limit
- More casual, engaging tone
- 2-3 hashtags maximum
- Use relevant emojis sparingly (🔬📊💡)
- Focus on single key benefit

### linkedin
- Professional, educational tone
- Use full 200-300 words
- Include detailed use cases
- More hashtags (5-7)
- End with clear call-to-action

### general (or unspecified)
- Balanced approach works across platforms
- 100-200 words
- 3-5 hashtags
- Professional but accessible

## Output Format

Provide all three variants clearly labeled:

```markdown
## 🐦 VARIANT 1: Brief (Twitter/X)
[≤280 character version with hashtags]

---

## 📱 VARIANT 2: Standard (Multi-platform)
[100-150 word version with hashtags]

---

## 💼 VARIANT 3: Extended (LinkedIn)
[200-300 word version with hashtags]

---

## 📊 EXAMPLE DATASETS
- **[Dataset 1 Name]** - [Clinical scenario]
  Download: [GitHub raw URL]
- **[Dataset 2 Name]** - [Clinical scenario]
  Download: [GitHub raw URL]

---

## 📋 USAGE NOTES
- **Primary Audience**: [pathologists/oncologists/researchers]
- **Key Message**: [main clinical benefit in one sentence]
- **Clinical Context**: [typical use case]
- **Best Platform**: [twitter/linkedin/both - with rationale]
- **Suggested Posting Time**: [morning/afternoon - when clinicians are active]
- **Test Data**: [Number of relevant .omv files available for this function]
```

## Important Reminders

- **Accuracy First**: Only promote features that actually exist in the code
- **Clinical Safety**: Never overstate diagnostic or clinical capabilities
- **No Medical Claims**: Don't claim FDA approval or clinical validation unless documented
- **Professional Credibility**: Maintain scientific rigor and honesty
- **Accessibility**: Make it understandable to varied expertise levels
- **Include Test Data**: ALWAYS look up and include relevant .omv example files from the catalog

---

## Execution Steps

### Step 0: Random Function Selection (if needed)

**If `$1` is empty, "random", or "surprise":**

1. Use `Glob` to find all .a.yaml files: `jamovi/*.a.yaml`
2. Filter out system files (0000.yaml, js.yaml)
3. For each function, use `Grep` to check the `menuGroup:` field
4. **Filter to mature functions only**: Exclude functions where menuGroup ends with "T" or "D"
   - "T" suffix = Testing functions (e.g., `menuGroup: meddecideT`)
   - "D" suffix = Development functions (e.g., `menuGroup: SurvivalD`)
5. From the filtered list of mature functions, randomly select one
6. Announce: "🎲 Randomly selected mature function: **[function_name]** (menuGroup: [group])"
7. Use that function for the main execution steps below

**Mature Function Examples:**
- ✅ VALID: `menuGroup: meddecide`, `menuGroup: Survival`, `menuGroup: Descriptives`
- ❌ INVALID: `menuGroup: meddecideT`, `menuGroup: SurvivalD`, `menuGroup: ClinicoPathT`

**Otherwise:** Use the specified function name from `$1`

### Main Execution Steps:

1. **Read function files**: Analyze `jamovi/$1.a.yaml`, `R/$1.b.R`, `jamovi/$1.r.yaml`, `jamovi/$1.u.yaml`
2. **Read test data catalog**: Use `Grep` to search `vignettes/test-data-complete-catalog.Rmd` for `$1_*.omv` files
3. **Select 1-2 most relevant datasets**: Choose clinically meaningful examples (e.g., prefer `_immuno.omv` over `_test.omv`)
4. **Extract clinical value**: Identify what clinical problem the function solves
5. **Generate promotional variants**: Create all three versions with hashtags
6. **Add dataset section**: Include direct GitHub download links with clinical context

Begin by checking if random selection is needed (Step 0), then analyzing the function files and looking up relevant test datasets.
