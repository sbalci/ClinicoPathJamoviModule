# Waterfall Analysis Explanations Feature

## Overview

Added a "Show Analysis Explanations" checkbox that displays comprehensive explanations and a natural language summary at the bottom of the waterfall analysis results.

---

## Feature Components

### 1. UI Checkbox

**Location**: Top of analysis panel (after "Enable Guided Analysis Mode")

**Name**: "Show Analysis Explanations"

**Default**: Unchecked (hidden by default)

---

### 2. Output Sections (When Enabled)

#### Section 1: Treatment Response Summary
**Color**: Blue (#007bff)
**Content**: Data-driven natural language summary

```
Treatment Response Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Analysis Overview:
Response analysis of 20 patients using RECIST v1.1 criteria.

Key Findings:

Objective Response Rate (ORR):
35% (7 patients achieved complete or partial response)

Disease Control Rate (DCR):
85% (17 patients achieved response or stable disease)

Response Distribution:
• Complete Response: 1 patients (5%)
• Partial Response: 6 patients (30%)
• Stable Disease: 10 patients (50%)
• Progressive Disease: 3 patients (15%)

Clinical Interpretation:
Promising activity for single-agent therapy (general benchmark;
verify against tumor-specific thresholds)
```

---

#### Section 2: Analysis Guide

##### 2.1 What This Analysis Does
**Color**: Teal (#17a2b8)

```
What This Analysis Does
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

The Treatment Response Analysis creates waterfall and spider plots
to visualize tumor response data according to RECIST v1.1 criteria.

Visualization Types:
• Waterfall Plot: Shows best response for each patient as vertical
  bars, ideal for single timepoint or best response data.

• Spider Plot: Shows response trajectories over time as connected
  lines, requires time variable for longitudinal data.
```

##### 2.2 When to Use This Analysis
**Color**: Yellow (#ffc107)

```
When to Use This Analysis:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

• Oncology clinical trials and treatment response studies
• Drug efficacy evaluation
• Tumor response monitoring
• Biomarker correlation studies
```

##### 2.3 Data Requirements
**Color**: Cyan (#0c5460)

```
Data Requirements:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

• Patient ID: Unique identifier for each patient
• Response Data: Either percentage changes from baseline or raw
  tumor measurements
• Time Variable: Required for spider plots (e.g., months from baseline)
```

##### 2.4 Key Assumptions & Limitations
**Color**: Red (#dc3545)

```
Key Assumptions & Limitations:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

• RECIST v1.1 thresholds: CR ≤-100%, PR ≤-30%, PD >+20%
• For raw measurements, baseline assumed at time = 0
• Waterfall plot shows best (most negative) response per patient
• Missing values are excluded from analysis

Tip: Start with percentage data if available, or use raw measurements
with proper time variables for automatic calculation.
```

---

## Implementation Details

### Files Modified

| File | Lines | Purpose |
|------|-------|---------|
| `jamovi/waterfall.a.yaml` | 404-418 | Added `showExplanations` option |
| `jamovi/waterfall.r.yaml` | 303-320 | Added HTML output items |
| `jamovi/waterfall.u.yaml` | 12-13 | Added UI checkbox |
| `R/waterfall.b.R` | 2041-2134 | Implemented `.generateExplanations()` function |

### Function Signature

```r
.generateExplanations = function(processed_data, metrics)
```

**Input Parameters:**
- `processed_data`: Data frame with waterfall plot data
- `metrics`: List containing ORR, DCR, n_cr, n_pr, n_sd, n_pd

**Outputs:**
- `self$results$naturalLanguageSummary`: HTML content with data summary
- `self$results$explanations`: HTML content with analysis guide

---

## Usage Instructions

### For Users (jamovi GUI)

1. **Load Data**
   ```
   File → Open → waterfall_percentage_basic.omv
   ```

2. **Run Analysis**
   ```
   Analyses → OncoPathT → Patient Follow-Up Plots → Treatment Response Analysis
   ```

3. **Configure Variables**
   - Patient ID: `PatientID`
   - Response Value: `Response`
   - Input Type: `Percentage Changes`

4. **Enable Explanations**
   - ✅ Check "Show Analysis Explanations"

5. **View Results**
   - Scroll to bottom of results
   - See "Treatment Response Summary" (blue box)
   - See "Analysis Guide" (multiple colored boxes)

---

### For Developers (R)

```r
library(ClinicoPath)

# Load test data
data("waterfall_percentage_basic")

# Run analysis with explanations
result <- waterfall(
  data = waterfall_percentage_basic,
  patientID = "PatientID",
  responseVar = "Response",
  inputType = "percentage",
  showExplanations = TRUE  # Enable explanations
)

# Explanations are automatically populated in:
# - result$naturalLanguageSummary
# - result$explanations
```

---

## Benefits

### 1. Educational Value
- Helps new users understand what the analysis does
- Explains when to use waterfall vs spider plots
- Clarifies data requirements

### 2. Documentation
- Provides copy-ready text for methods sections
- Shows RECIST criteria and thresholds
- Explains assumptions and limitations

### 3. Interpretation
- Natural language summary of key findings
- Clinical interpretation of ORR/DCR
- Automatic calculation of response distribution

### 4. Transparency
- Makes analysis methodology explicit
- Documents limitations upfront
- Provides context for results

---

## Styling Guidelines

### Color Scheme
All sections use Bootstrap-inspired colors with semantic meaning:

| Color | Hex | Usage |
|-------|-----|-------|
| Blue | #007bff | Summary/Results |
| Teal | #17a2b8 | Information/Guide |
| Yellow | #ffc107 | Usage/Applications |
| Cyan | #0c5460 | Requirements/Input |
| Red | #dc3545 | Warnings/Limitations |

### Layout Pattern
```html
<div style="padding: 15px;
            background-color: #...;
            border-left: 4px solid #...;
            margin: 20px 0;">
  <h3 style="color: #...; margin-top: 0;">Section Title</h3>
  <p>Content...</p>
  <ul>
    <li>Bullet points...</li>
  </ul>
</div>
```

---

## Testing Checklist

### ✅ Pre-Testing (Completed)
- [x] YAML syntax validation (all 3 files)
- [x] R syntax validation (waterfall.b.R)
- [x] Function implementation (.generateExplanations)
- [x] Option added to .a.yaml
- [x] UI checkbox added to .u.yaml
- [x] HTML items added to .r.yaml

### 🧪 Testing Needed
- [ ] Checkbox appears in jamovi UI
- [ ] Checkbox toggles explanations visibility
- [ ] Natural language summary displays with correct data
- [ ] Analysis guide displays with all 4 sections
- [ ] HTML formatting renders correctly
- [ ] Colors display as intended
- [ ] Works with different datasets (small n, large n)
- [ ] ORR/DCR calculations are accurate
- [ ] Clinical interpretation appears correctly
- [ ] No console errors or warnings

### 📊 Test Datasets
Use these datasets to verify functionality:

| Dataset | Patients | Expected ORR | Expected DCR | Notes |
|---------|:--------:|:------------:|:------------:|-------|
| `waterfall_percentage_basic` | 20 | 35% | 85% | Standard test |
| `waterfall_oncology_trial` | 50 | 40% | 70% | Larger sample |
| `waterfall_single_patient` | 1 | varies | varies | Edge case |
| `waterfall_edge_cases` | 10 | varies | varies | Boundary values |

---

## Maintenance Notes

### Updating Content
To modify explanation text, edit `.generateExplanations()` in `R/waterfall.b.R`:

**Lines 2054-2080**: Natural Language Summary HTML
**Lines 2086-2131**: Analysis Guide HTML

### Adding Sections
To add a new section:
1. Add HTML to `.generateExplanations()`
2. Use consistent styling (padding, border-left, colors)
3. Follow semantic color scheme
4. Test rendering in jamovi

### Internationalization
All text uses `_(...)` function for translation support:
```r
.("Text to translate")
sprintf(.("Format %d patients"), n)
```

---

## Related Documentation

- **Main Guide**: [tests/WATERFALL_TEST_DATA_GUIDE.md](WATERFALL_TEST_DATA_GUIDE.md)
- **Quick Reference**: [tests/WATERFALL_QUICK_TEST_GUIDE.md](WATERFALL_QUICK_TEST_GUIDE.md)
- **Test Summary**: [tests/WATERFALL_TEST_SUMMARY.md](WATERFALL_TEST_SUMMARY.md)

---

**Feature Status**: ✅ **Implementation Complete**

**Next Step**: Install module in jamovi and test with sample data

**Created**: 2025-12-28
**Version**: Implements showExplanations option
