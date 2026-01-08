# StreamGraph Test Datasets

This folder contains test datasets for demonstrating the enhanced jjstreamgraph function.

## Datasets

### 1. streamgraph_patient_biomarkers.csv (60 observations)
**Purpose**: Patient biomarker evolution over 12 months
**Variables**:
- patient_group: Treatment/patient groups (Group_A through Group_E)
- month: Time points (1-12 months)
- biomarker_level: Numeric biomarker values with realistic trends

**StreamGraph Configuration**:
```
timeVar: "month"
valueVar: "biomarker_level" 
groupVar: "patient_group"
clinicalPreset: "biomarker_evolution"
```

### 2. streamgraph_treatment_response.csv (64 observations)
**Purpose**: Treatment response monitoring over 16 weeks
**Variables**:
- treatment_arm: Treatment groups (Placebo, Low_Dose, High_Dose, Combination)
- week: Time points (0-15 weeks)
- response_rate: Response rates (0-100%) with realistic medical patterns

**StreamGraph Configuration**:
```
timeVar: "week"
valueVar: "response_rate"
groupVar: "treatment_arm" 
clinicalPreset: "treatment_response"
offset: "expand"  # Shows proportional response
```

### 3. streamgraph_population_health.csv (120 observations)
**Purpose**: Population health surveillance over 5 years
**Variables**:
- disease_category: Health conditions (Diabetes, Hypertension, Heart_Disease, Cancer, Respiratory, Mental_Health)
- quarter: Quarterly timepoints (2019.25 - 2024.0)
- case_count: Number of cases per 10,000 population

**StreamGraph Configuration**:
```
timeVar: "quarter"
valueVar: "case_count"
groupVar: "disease_category"
clinicalPreset: "population_health"
interpolate: "step"  # Clear quarterly transitions
```

## Clinical Features Demonstrated

1. **Clinical Presets**: Pre-configured settings for common medical scenarios
2. **Interactive Legends**: Group selection and highlighting
3. **Analysis Summary**: Automated clinical interpretation
4. **Copy-Ready Reports**: Clinical report sentences
5. **Custom Colors**: Medical-appropriate color schemes
6. **Enhanced Error Handling**: Clinical context in validation messages

## Usage Examples

### Biomarker Trends
Shows individual biomarker evolution across patient groups with smooth interpolation, ideal for tracking treatment effects or disease progression.

### Treatment Response
Demonstrates proportional streamgraph (expand offset) to show relative treatment efficacy across multiple arms over time.

### Population Health
Uses step interpolation to show discrete quarterly health surveillance data with seasonal patterns and trend analysis.

Created: 2024-09-03
Purpose: jjstreamgraph function testing and validation
Status: Ready for clinical demonstration

