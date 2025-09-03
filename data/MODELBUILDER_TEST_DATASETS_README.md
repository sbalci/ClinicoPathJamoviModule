
# Model Builder Test Datasets

This file describes the comprehensive test datasets created for the modelbuilder function.

## Dataset 1: Cardiac Risk Prediction (cardiac_data)
- **Sample Size**: 800 patients
- **Outcome**: cardiovascular_event (15% event rate)
- **Use Cases**: Tests all model types, standard workflows
- **Predictors**: 
  - Basic: age, sex, diabetes, hypertension, smoking, family_history
  - Enhanced: bmi, blood_pressure, exercise, alcohol  
  - Biomarkers: cholesterol panel, CRP, troponin, HbA1c
  - Custom: stress_score, medication_count, income_bracket
- **Missing Data**: 5-8% in select biomarkers
- **Special Features**: Realistic clinical correlations, interaction potential

## Dataset 2: Cancer Prognosis (cancer_data) 
- **Sample Size**: 200 patients
- **Outcome**: death_within_2years (30% event rate)
- **Use Cases**: Tests smaller samples, higher event rates
- **Predictors**: 
  - Basic: stage, grade, tumor_size
  - Enhanced: lymph_nodes, performance_status, comorbidities
  - Biomarkers: CEA, CA19-9, hemoglobin, albumin
  - Custom: treatment_response, mutation_status
- **Missing Data**: 8-12% in biomarkers
- **Special Features**: Ordinal predictors, treatment interactions

## Dataset 3: Diabetes Complications (diabetes_data)
- **Sample Size**: 400 patients  
- **Outcome**: nephropathy_progression (20% event rate)
- **Use Cases**: Tests penalized regression, high-dimensional data
- **Predictors**: 25+ variables including demographics, lab values, medications, lifestyle
- **Missing Data**: 5-15% across multiple variables
- **Special Features**: Many correlated predictors, ideal for LASSO/Ridge

## Dataset 4: Minimal Sample (minimal_data)
- **Sample Size**: 60 patients
- **Outcome**: outcome (25% event rate, exactly 15 events)
- **Use Cases**: Tests minimum sample requirements, edge cases
- **Predictors**: 6 basic predictors
- **Missing Data**: None
- **Special Features**: Tests EPV ratios, small sample stability

## Dataset 5: Separation Challenge (separation_data)
- **Sample Size**: 150 patients
- **Outcome**: outcome (variable event rate)
- **Use Cases**: Tests separation detection and handling
- **Predictors**: Includes perfect predictor (rare_condition)
- **Missing Data**: None  
- **Special Features**: Perfect separation scenario, convergence challenges

## Testing Recommendations

1. **Basic Functionality**: Use cardiac_data with basic predictors
2. **All Model Types**: Use cardiac_data with appropriate predictor sets
3. **Penalized Regression**: Use diabetes_data with many predictors
4. **Small Samples**: Use minimal_data 
5. **Edge Cases**: Use separation_data
6. **Missing Data**: Use cancer_data or diabetes_data
7. **Cross-validation**: Any dataset >100 samples
8. **Bootstrap**: Any dataset >50 samples
9. **Model Comparison**: Use cardiac_data with multiple model types

## Expected Performance Metrics

- **Cardiac Data**: AUC 0.75-0.85, good calibration
- **Cancer Data**: AUC 0.70-0.80, moderate calibration  
- **Diabetes Data**: Variable AUC depending on penalty, excellent for variable selection
- **Minimal Data**: Lower AUC (0.60-0.75), wider confidence intervals
- **Separation Data**: Convergence warnings, extreme coefficients

