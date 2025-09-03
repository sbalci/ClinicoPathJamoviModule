# Riverplot Test Datasets

This folder contains test datasets specifically designed for testing and demonstrating the riverplot function in ClinicoPath.

## Datasets

### 1. riverplot_patient_journey.csv (100 patients, Wide Format)
**Purpose**: Comprehensive patient journey analysis from diagnosis to 12-month follow-up
**Variables**:
- patient_id: Unique patient identifier (P001-P100)
- baseline_stage: Initial disease stage (Stage_I, Stage_II, Stage_III, Stage_IV)
- treatment_type: Primary treatment modality (Surgery, Chemotherapy, Radiation, Immunotherapy, Combination)
- response_6m: 6-month treatment response (Complete_Response, Partial_Response, Stable_Disease, Progressive_Disease)
- followup_12m: 12-month follow-up status (No_Evidence_Disease, Local_Recurrence, Distant_Metastasis, Death)

**Riverplot Configuration**:
```
strata: c("baseline_stage", "treatment_type", "response_6m", "followup_12m")
id: "patient_id"
dataFormat: "wide"
plotType: "alluvial" (recommended)
clinicalPreset: "patient_journey"
```

### 2. riverplot_simple_clinical.csv (60 cases, Wide Format)
**Purpose**: Simple 3-stage clinical decision pathway for beginners
**Variables**:
- case_id: Case identifier (C01-C60)
- initial_diagnosis: Diagnostic category (Benign, DCIS, Invasive)
- treatment: Treatment approach (Surgery_Only, Surgery_Chemo, Surgery_Radio, Surgery_Both)
- outcome: Clinical outcome (Excellent, Good, Fair, Poor)

**Riverplot Configuration**:
```
strata: c("initial_diagnosis", "treatment", "outcome")
id: "case_id"
dataFormat: "wide"
plotType: "alluvial"
clinicalPreset: "treatment_response" (recommended)
```

### 3. riverplot_treatment_long.csv (150 observations, Long Format)
**Purpose**: Longitudinal treatment pathway analysis with time-based progression
**Variables**:
- patient_id: Patient identifier (PT01-PT50, each with 3 time points)
- time_point: Time stage (Baseline, Month_6, Month_12)
- status: Patient status at each time point (Responding, Stable, Progressive, Complete_Response)
- weight: Treatment intensity weight (0.5-2.0)

**Riverplot Configuration**:
```
strata: c("status")
time: "time_point"
id: "patient_id"
weight: "weight"
dataFormat: "long"
plotType: "sankey" (recommended)
```

## Clinical Interpretation Examples

### Patient Journey Analysis
"This riverplot analysis tracks 100 patients through their treatment journey from initial staging to 12-month follow-up. Flow width represents patient numbers, with thicker flows indicating more common pathways. Early-stage patients (Stage I-II) show predominantly favorable outcomes, while advanced-stage patients exhibit more diverse response patterns."

### Simple Clinical Decision
"This simplified pathway analysis demonstrates the relationship between initial diagnosis, treatment selection, and clinical outcomes. Clear flow patterns emerge showing treatment preferences for different diagnostic categories and their associated outcome distributions."

### Treatment Pathways (Long Format)
"The longitudinal analysis reveals treatment pathway evolution over 12 months. Weighted flows (treatment intensity) demonstrate how individual patients transition through treatment phases from baseline to 12-month follow-up."

## Usage Notes

1. **Data Quality**: All datasets include realistic clinical patterns and distributions
2. **Sample Sizes**: Varied sample sizes (50-100) appropriate for different analysis scenarios  
3. **Complexity Levels**: From simple 3-stage to complex 4-stage pathways
4. **Format Testing**: Both wide and long format examples for comprehensive testing
5. **Clinical Relevance**: Realistic medical scenarios with appropriate outcome distributions

## Testing Scenarios

- ✓ Wide format data processing
- ✓ Long format data processing  
- ✓ Individual patient tracking (ID variable)
- ✓ Weighted flows (weight variable)
- ✓ Clinical preset configurations
- ✓ Different numbers of categories (3-5 per stage)
- ✓ Realistic clinical distributions and patterns

## Quick Start

1. Load any dataset into jamovi
2. Navigate to ClinicoPath → River Plots & Alluvial Diagrams
3. Configure variables according to the recommendations above
4. Apply clinical presets for optimal visualization
5. Generate copy-ready clinical reports

Created: 2024-09-03  
Purpose: Riverplot function testing and validation  
Status: Ready for use