# Test Data for Medical Decision Analysis

Test datasets for evaluating diagnostic test performance using the
decision function. These datasets cover various clinical scenarios
including screening tests, diagnostic tests, biomarkers, imaging, and
pathology evaluations with different prevalence rates and test
characteristics.

## Usage

``` r
decision_test

decision_screening

decision_diagnostic

decision_biomarker

decision_imaging

decision_infectious

decision_small

decision_large

decision_perfect

decision_poor

decision_rare

decision_common

decision_missing

decision_multilevel

decision_pathology

decision_pointofcare
```

## Format

Data frames with variables for gold standard reference and test results:

- patient_id:

  Patient identifier

- GoldStandard/Reference:

  True disease status (gold standard)

- NewTest/Test:

  Test result being evaluated

- Additional clinical variables:

  Age, symptoms, risk factors, etc.

decision_test: Basic diagnostic test evaluation (n=200, 30% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive, Sens=0.85, Spec=0.90)

- age:

  Patient age in years

- sex:

  Patient sex (Female/Male)

decision_screening: Cancer screening test with low prevalence (n=250, 5%
prevalence)

- patient_id:

  Patient identifier

- Biopsy:

  Gold standard biopsy result (Benign/Malignant)

- ScreeningTest:

  Screening test result (Negative/Positive, Sens=0.92, Spec=0.88)

- age:

  Patient age in years

- risk_factor:

  Cancer risk factor (Low/Medium/High)

decision_diagnostic: Clinical diagnostic test with high prevalence
(n=180, 60% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Absent/Present)

- ClinicalTest:

  Clinical test result (Negative/Positive, Sens=0.88, Spec=0.85)

- symptom_severity:

  Symptom severity score (1-10)

- duration_days:

  Symptom duration in days

decision_biomarker: Cardiac biomarker test evaluation (n=220, 35%
prevalence)

- patient_id:

  Patient identifier

- Angiography:

  Gold standard angiography (No_MI/MI)

- Troponin:

  Troponin test result (Normal/Elevated, Sens=0.95, Spec=0.92)

- chest_pain:

  Chest pain severity (None/Mild/Severe)

- ecg_changes:

  ECG findings (Normal/Abnormal)

decision_imaging: Medical imaging test performance (n=160, 40%
prevalence)

- patient_id:

  Patient identifier

- Pathology:

  Pathology diagnosis (Benign/Malignant)

- CT_Scan:

  CT scan result (Normal/Abnormal, Sens=0.87, Spec=0.83)

- tumor_size_cm:

  Tumor size in centimeters

- location:

  Tumor location (Lung/Liver/Kidney/Pancreas)

decision_infectious: Infectious disease rapid test (n=200, 25%
prevalence)

- patient_id:

  Patient identifier

- Culture:

  Gold standard culture result (Negative/Positive)

- RapidTest:

  Rapid test result (Negative/Positive, Sens=0.82, Spec=0.95)

- fever:

  Fever present (No/Yes)

- symptom_onset_days:

  Days since symptom onset

decision_small: Small sample dataset (n=30, 40% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive, Sens=0.85, Spec=0.85)

decision_large: Large sample dataset (n=500, 30% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive, Sens=0.88, Spec=0.90)

- age:

  Patient age in years

- comorbidities:

  Number of comorbidities

decision_perfect: Perfect test performance (n=150, 35% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- PerfectTest:

  Perfect test result (Sens=1.00, Spec=1.00)

- age:

  Patient age in years

decision_poor: Poor test performance (n=140, 30% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- PoorTest:

  Poor performing test (Sens=0.60, Spec=0.65)

decision_rare: Rare disease scenario (n=300, 2% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive, Sens=0.90, Spec=0.95)

- genetic_marker:

  Genetic marker status (Absent/Present)

decision_common: Common disease scenario (n=180, 75% prevalence)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive, Sens=0.85, Spec=0.88)

decision_missing: Dataset with missing values (n=150, ~5% missing)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status with missing values

- NewTest:

  Test result with missing values

- age:

  Patient age in years

decision_multilevel: Three-level variables with indeterminate results
(n=180)

- patient_id:

  Patient identifier

- GoldStandard:

  True disease status (Negative/Positive)

- NewTest:

  Test result (Negative/Positive/Indeterminate, ~10% indeterminate)

decision_pathology: Pathology frozen section evaluation (n=190, 45%
prevalence)

- patient_id:

  Patient identifier

- Biopsy:

  Permanent section diagnosis (Benign/Malignant)

- FrozenSection:

  Frozen section diagnosis (Benign/Malignant, Sens=0.93, Spec=0.96)

- specimen_type:

  Specimen type (Core_Biopsy/Excision/FNA)

- tumor_grade:

  Tumor grade (Low/Intermediate/High)

decision_pointofcare: Point-of-care test evaluation (n=210, 28%
prevalence)

- patient_id:

  Patient identifier

- LabTest:

  Laboratory test result (Negative/Positive)

- PointOfCare:

  Point-of-care test result (Negative/Positive, Sens=0.78, Spec=0.92)

- setting:

  Testing setting (Emergency/Clinic/Home)

- urgency:

  Clinical urgency (Routine/Urgent/Critical)

## Details

All datasets use realistic clinical scenarios with appropriate
sensitivity, specificity, and disease prevalence values. Datasets
include edge cases (small/large samples, perfect/poor performance,
rare/common diseases) for comprehensive testing.

## Examples

``` r
# Load basic test dataset
data(decision_test)
head(decision_test)
#> # A tibble: 6 × 5
#>   patient_id GoldStandard NewTest    age sex   
#>   <chr>      <fct>        <fct>    <dbl> <fct> 
#> 1 PT001      Positive     Negative    31 Male  
#> 2 PT002      Positive     Positive    59 Male  
#> 3 PT003      Negative     Negative    69 Male  
#> 4 PT004      Positive     Negative    80 Female
#> 5 PT005      Negative     Negative    38 Male  
#> 6 PT006      Negative     Negative    41 Male  

if (FALSE) { # \dontrun{
# Evaluate diagnostic test
decision(
  data = decision_test,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = "Negative",
  newtest = "NewTest",
  testPositive = "Positive",
  testNegative = "Negative"
)
} # }
```
