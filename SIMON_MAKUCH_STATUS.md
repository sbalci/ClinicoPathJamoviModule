# Simon-Makuch Time-Dependent Survival Analysis - Implementation Status

## Executive Summary

✅ **ALREADY IMPLEMENTED AND AVAILABLE**

The Simon-Makuch time-dependent survival analysis module is **fully implemented** in the ClinicoPath jamovi module. This comprehensive analysis tool addresses the critical challenge of immortal time bias when studying treatments, biomarkers, or exposures that change during patient follow-up.

## Current Implementation Status

### Module Location
- **Menu**: SurvivalD > ClinicoPath Survival
- **Analysis Name**: `simonmakuch`
- **Function**: Simon-Makuch Time-Dependent Survival Analysis
- **Subtitle**: Time-Dependent Variables, Simon-Makuch Plots, Landmark Analysis

### Files Available
- ✅ `R/simonmakuch.b.R` - Full backend implementation (1,116 lines)
- ✅ `jamovi/simonmakuch.a.yaml` - Analysis options (488 lines)
- ✅ `jamovi/simonmakuch.r.yaml` - Results definitions (530 lines)
- ✅ `jamovi/simonmakuch.u.yaml` - User interface definition
- ✅ `R/simonmakuch.h.R` - Auto-generated header
- ✅ `data-raw/simon_makuch_test_data.R` - Test data generation
- ✅ `man/simonmakuch.Rd` - Documentation

## Comprehensive Features

### Core Analysis Capabilities

#### 1. Simon-Makuch Methodology
- **Modified Kaplan-Meier curves** for time-dependent exposures
- **Counting process format** for proper handling of exposure timing
- **Immortal time bias correction** built into the methodology
- **Time-dependent Cox regression** with proper hazard ratio estimation

#### 2. Statistical Methods
- ✅ **Simon-Makuch Plots** - Visual representation of survival by time-varying exposure
- ✅ **Time-Dependent Cox Regression** - Hazard ratios accounting for exposure timing
- ✅ **Landmark Analysis** - Conditional survival from fixed time points
- ✅ **Log-Rank Test** - Properly adapted for time-dependent data
- ✅ **Mantel-Byar Test** - Specialized for time-dependent comparisons
- ✅ **Time-Varying Effects Testing** - Tests if exposure effects change over time

#### 3. Immortal Time Bias Assessment
- ✅ Comparison of naive vs. proper time-dependent analysis
- ✅ Quantification of bias magnitude
- ✅ Demonstration of bias impact on hazard ratios
- ✅ Educational explanations of immortal time bias

#### 4. Advanced Features
- ✅ **Landmark Analysis** with user-specified time points
- ✅ **Bootstrap Validation** for stability assessment
- ✅ **Sensitivity Analysis** with different assumptions
- ✅ **Model Diagnostics** - Proportional hazards testing, residual analysis
- ✅ **Robust Variance Estimation** for clustered data
- ✅ **Multiple Covariates** support in time-dependent models

### Required Variables

The analysis requires five key variables:

1. **Survival Time** - Time to event or censoring (e.g., `follow_up_months`)
2. **Event Indicator** - Binary or factor indicating event occurrence (e.g., `death_status`)
3. **Time-Dependent Variable** - The exposure that changes over time (e.g., `treatment_exposure`)
4. **Time of Change** - When the exposure status changes (e.g., `treatment_start_time`)
5. **Time-Dependent Status** - Current exposure status (e.g., `baseline_treatment_status`)

### Visualization Capabilities

#### Available Plots
- ✅ **Simon-Makuch Survival Curves** - Main survival plot with time-dependent exposure
- ✅ **Landmark Survival Plots** - Survival from each landmark time
- ✅ **Cumulative Incidence Plots** - Visual exposure pattern evolution
- ✅ **Exposure Status Over Time** - Timeline of exposure changes
- ✅ **Model Diagnostic Plots** - Residual and assumption checking plots

#### Plot Options
- Confidence intervals (adjustable)
- Risk tables below survival curves
- Customizable time ranges
- Publication-ready formatting
- Integration with jamovi themes

### Output Tables

#### 1. Survival Estimates Table
- Survival probabilities at key time points
- Number at risk and events by exposure status
- Standard errors and confidence intervals

#### 2. Time-Dependent Cox Regression
- Hazard ratios with 95% confidence intervals
- Z-statistics and p-values
- Support for multiple covariates
- Clinical interpretation included

#### 3. Landmark Analysis Results
- Results at each specified landmark time
- Sample sizes at each landmark
- Hazard ratios from landmark times forward
- Statistical significance testing

#### 4. Immortal Time Bias Assessment
- Comparison of naive vs. proper methods
- Bias magnitude quantification
- Direction of bias indication

#### 5. Statistical Tests
- Log-rank test results
- Mantel-Byar test statistics
- Time-varying effects tests
- Model diagnostic statistics

### Educational Features

#### Comprehensive Explanations
- ✅ **Simon-Makuch Methodology** - Detailed explanation of the method
- ✅ **Landmark Analysis Guide** - When and how to use landmark analysis
- ✅ **Immortal Time Bias** - What it is and why it matters
- ✅ **Time-Dependent Cox** - Proper interpretation of results
- ✅ **Clinical Interpretation Guidance** - Translating statistics to practice
- ✅ **Methodology Notes** - Advanced statistical details

#### Clinical Applications Highlighted
- Treatment effectiveness with varying initiation times
- Biomarker evolution studies
- Disease progression impact analysis
- Healthcare intervention timing effects
- Transplantation outcome studies

### Analysis Types Supported

#### 1. Basic Simon-Makuch Analysis
- Core survival curves
- Basic time-dependent comparison
- Essential statistical tests

#### 2. Comprehensive Time-Dependent Analysis
- All statistical methods
- Complete diagnostic suite
- Multiple comparison approaches

#### 3. Landmark Analysis Focus
- Multiple landmark time points
- Conditional survival estimates
- Landmark-specific hazard ratios

#### 4. Publication-Ready Analysis
- All methods included
- Bootstrap validation
- Comprehensive documentation
- Publication-quality outputs

## Clinical Applications

### Oncology
- **Treatment Timing**: Analyzing survival benefit of therapies started after initial failure
- **Biomarker Changes**: Impact of mutation status changes during disease course
- **Progression Effects**: Survival impact when metastasis develops during follow-up

### Cardiology
- **Intervention Timing**: Effects of procedures initiated after symptom onset
- **Risk Factor Evolution**: Impact of biomarkers that change over time
- **Treatment Crossover**: Handling patients who switch treatments

### Transplantation
- **Organ Availability**: Survival analysis when transplant occurs during waitlist period
- **Graft Function**: Impact of graft failure developing over time
- **Rejection Episodes**: Time-varying effect of rejection events

### General Medicine
- **Medication Adherence**: Impact of treatment discontinuation timing
- **Comorbidity Development**: Effects of new conditions arising during follow-up
- **Healthcare Utilization**: Impact of interventions received at various timepoints

## Example Usage

### Basic Example
```r
simonmakuch(
    data = simon_makuch_simple,
    survivalTime = "follow_up_months",
    event = "death_status",
    eventLevel = 1,
    timeDepVariable = "treatment_exposure",
    timeDepTime = "treatment_start_time_clean",
    timeDepStatus = "baseline_treatment_status",
    exposedLevel = "On-treatment",
    showSimonMakuchPlot = TRUE,
    showExplanations = TRUE
)
```

### Comprehensive Analysis
```r
simonmakuch(
    data = simon_makuch_simple,
    survivalTime = "follow_up_months",
    event = "death_status_factor",
    eventLevel = "Dead",
    timeDepVariable = "treatment_exposure",
    timeDepTime = "treatment_start_time_clean",
    timeDepStatus = "baseline_treatment_status",
    exposedLevel = "Eventually",
    analysisType = "comprehensive",
    performLandmarkAnalysis = TRUE,
    landmarkTimes = "6, 12, 24",
    performTimeDependentCox = TRUE,
    assessImmortalTimeBias = TRUE,
    showLandmarkPlots = TRUE,
    includeClinicalGuidance = TRUE
)
```

### Publication-Ready Analysis
```r
simonmakuch(
    data = simon_makuch_simple,
    survivalTime = "follow_up_months",
    event = "death_status",
    eventLevel = 1,
    timeDepVariable = "treatment_received",
    timeDepTime = "treatment_start_time_clean",
    timeDepStatus = "baseline_treatment_status",
    exposedLevel = "Yes",
    analysisType = "publication",
    showSimonMakuchPlot = TRUE,
    showConfidenceIntervals = TRUE,
    showRiskTables = TRUE,
    performLogRankTest = TRUE,
    performMantelByarTest = TRUE,
    showSurvivalEstimates = TRUE,
    showHazardRatios = TRUE,
    performBootstrapValidation = TRUE,
    showMethodologyNotes = TRUE
)
```

## Test Data Available

### Dataset: `simon_makuch_simple`
Example dataset with:
- 200 patients
- Time-dependent treatment variable
- Realistic survival times
- Multiple baseline covariates (age, sex, stage)
- Proper time-dependent data structure

### Variables Included
- `patient_id` - Patient identifier
- `age` - Age at baseline
- `sex` - Patient sex
- `stage` - Disease stage (I-IV)
- `follow_up_months` - Total follow-up time
- `death_status` - Event indicator (0/1)
- `death_status_factor` - Event as factor (Alive/Dead)
- `treatment_start_time` - When treatment started
- `treatment_start_time_clean` - Cleaned version for analysis
- `baseline_treatment_status` - Status at baseline
- `treatment_exposure` - Exposure classification (Never/Eventually)
- `treatment_received` - Whether treatment was received (Yes/No)

## Technical Implementation Details

### Data Transformation
The analysis automatically:
1. Converts data to counting process format
2. Creates (tstart, tstop] intervals for each patient
3. Handles patients who never receive exposure
4. Properly attributes pre-exposure time to unexposed group
5. Manages exposure status changes during follow-up

### Statistical Methods Used
- **survival** package for core Cox regression
- **survminer** package for survival curve visualization
- **ggplot2** for custom plotting
- Counting process format via `Surv(tstart, tstop, event)`
- Time-dependent covariates in Cox models

### Model Diagnostics
- Proportional hazards testing (cox.zph)
- Schoenfeld residuals
- Martingale residuals
- Deviance residuals
- Influential observation detection

## What's Already Working

✅ **Complete Implementation**
- All core functionality implemented
- Comprehensive error handling
- Detailed explanatory text
- Multiple analysis types
- Full suite of plots and tables
- Bootstrap validation
- Sensitivity analysis
- Model diagnostics

✅ **User Interface**
- Well-organized options
- Clear variable selection
- Analysis type presets
- Customizable outputs
- Educational guidance

✅ **Documentation**
- Inline help text
- Methodology explanations
- Clinical interpretation guides
- Example usage
- Test datasets

## Recommendations for Users

### 1. Getting Started
- Use the "Basic Simon-Makuch Analysis" type first
- Review the explanatory sections to understand the methodology
- Start with the provided test data (`simon_makuch_simple`)

### 2. Data Preparation
- Ensure time variables are in consistent units (months recommended)
- Code event variable as 1 = event, 0 = censored
- Time of change should be 0 for baseline exposure, actual time for changes
- Verify exposure timing is accurately recorded

### 3. Analysis Selection
- **Basic**: Quick overview of time-dependent survival
- **Comprehensive**: Full analysis with all methods
- **Landmark**: Focus on specific time points
- **Publication**: All methods with validation

### 4. Interpretation
- Always review immortal time bias assessment
- Compare landmark results at different time points
- Check proportional hazards assumption
- Consider sensitivity analyses

### 5. Clinical Use
- Focus on clinical significance, not just statistical significance
- Consider magnitude of hazard ratios
- Evaluate confidence interval widths
- Assess generalizability to your population

## Future Enhancements (Optional)

While the current implementation is comprehensive, potential future additions could include:

1. **Competing Risks Extension**
   - Fine-Gray subdistribution hazard models
   - Cumulative incidence functions
   - Cause-specific hazard models

2. **Multiple Time-Dependent Variables**
   - Joint modeling of multiple exposures
   - Interaction between time-dependent variables
   - Complex exposure patterns

3. **Marginal Structural Models**
   - Causal inference framework
   - Inverse probability weighting
   - G-estimation methods

4. **Recurrent Events**
   - Time-dependent exposures with recurrent outcomes
   - Frailty models for within-subject correlation
   - Gap time analysis

5. **Enhanced Visualizations**
   - Swimmer plots for individual trajectories
   - State transition diagrams
   - Dynamic exposure status visualization

## References

The implementation is based on:

- **Simon R, Makuch RW** (1984). A non-parametric graphical representation of the relationship between survival and the occurrence of an event: Application to responder versus non-responder bias. *Statistics in Medicine*, 3(1):35-44.

- **Makuch RW** (1982). Adjusted survival curve estimation using covariates. *Journal of Chronic Diseases*, 35(6):437-443.

- **Therneau TM, Grambsch PM** (2000). *Modeling Survival Data: Extending the Cox Model*. Springer, New York.

- **Suissa S** (2008). Immortal time bias in pharmaco-epidemiology. *American Journal of Epidemiology*, 167(4):492-499.

## Conclusion

✅ **The Simon-Makuch time-dependent survival analysis is FULLY FUNCTIONAL and AVAILABLE NOW in ClinicoPath.**

The module provides:
- ✅ Comprehensive time-dependent survival analysis
- ✅ Simon-Makuch plots and methodology
- ✅ Landmark analysis capabilities
- ✅ Immortal time bias assessment
- ✅ Time-dependent Cox regression
- ✅ Multiple statistical tests
- ✅ Extensive educational content
- ✅ Clinical interpretation guidance
- ✅ Publication-ready outputs

**Users can start using this feature immediately** through the jamovi interface under:
**SurvivalD > ClinicoPath Survival > Simon-Makuch Time-Dependent Survival Analysis**

---

*Document prepared: 2025-11-03*
*Module Status: Fully Implemented and Production-Ready*
*Location: ClinicoPath jamovi Module*
