# Test Dataset for Chi-Square Post-Hoc Analysis

A comprehensive test dataset specifically designed for testing the
chisqposttest function. Contains multiple categorical variables with
known associations of different strengths, edge cases, and missing data
patterns.

## Usage

``` r
data("chisqposttest_test_data")
```

## Format

A data frame with 300 observations and 14 variables:

- PatientID:

  Patient identifier (1-300)

- Treatment:

  Treatment group: "Standard", "Experimental"

- Response:

  Treatment response: "No Response", "Response" (strongly associated
  with Treatment)

- Sex:

  Patient sex: "Male", "Female" (balanced)

- TumorGrade:

  Tumor grade: "Grade 1", "Grade 2", "Grade 3"

- TumorStage:

  Tumor stage: "Stage I", "Stage II", "Stage III" (moderately associated
  with TumorGrade)

- Institution:

  Hospital: "Hospital A", "Hospital B", "Hospital C", "Hospital D"

- QualityScore:

  Quality rating: "High", "Low" (weakly associated with Institution)

- RandomVar1:

  Random variable: "Group A", "Group B", "Group C" (no associations)

- RandomVar2:

  Random variable: "Type X", "Type Y" (no associations)

- RareCategory:

  Frequency category: "Common", "Uncommon", "Rare" (unbalanced)

- BinaryOutcome:

  Binary outcome: "Negative", "Positive" (associated with RareCategory)

- AgeGroup:

  Age category: "Young", "Middle", "Elderly"

- BiomarkerStatus:

  Biomarker status: "Negative", "Positive" (moderately associated with
  AgeGroup)

## Details

This dataset contains several types of associations designed to test
different aspects of chi-square post-hoc analysis:

**Strong Associations:**

- Treatment -\> Response: Clear treatment effect with odds ratio ~5

**Moderate Associations:**

- TumorGrade -\> TumorStage: Higher grades associated with advanced
  stages

- AgeGroup -\> BiomarkerStatus: Age-related biomarker expression pattern

**Weak Associations:**

- Institution -\> QualityScore: Institutional quality differences

- RareCategory -\> BinaryOutcome: Effect in rare category with small
  cell counts

**No Associations:**

- RandomVar1 ⊥ RandomVar2: Independent random variables for null
  hypothesis testing

The dataset includes approximately 5% missing data in Treatment, Sex,
and TumorGrade variables to test missing data handling options.

## Source

Simulated data created for testing purposes. Associations are based on
realistic clinical scenarios but data is artificially generated.

## See also

[`chisqposttest`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/chisqposttest.md)

## Examples

``` r
# Load the dataset
data(chisqposttest_test_data)

# Examine structure
str(chisqposttest_test_data)
#> 'data.frame':    300 obs. of  14 variables:
#>  $ PatientID      : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Treatment      : Factor w/ 2 levels "Standard","Experimental": 1 1 2 1 1 2 1 2 1 1 ...
#>  $ Response       : Factor w/ 2 levels "No Response",..: 1 1 1 1 1 2 1 2 1 1 ...
#>  $ Sex            : Factor w/ 2 levels "Male","Female": 2 2 2 2 1 1 2 2 2 2 ...
#>  $ TumorGrade     : Factor w/ 3 levels "Grade 1","Grade 2",..: 3 2 1 2 3 3 1 2 NA 3 ...
#>  $ TumorStage     : Factor w/ 3 levels "Stage I","Stage II",..: 3 3 1 2 2 3 2 2 3 3 ...
#>  $ Institution    : Factor w/ 4 levels "Hospital A","Hospital B",..: 1 1 3 1 3 4 1 1 3 1 ...
#>  $ QualityScore   : Factor w/ 2 levels "High","Low": 1 2 2 1 1 1 1 1 2 1 ...
#>  $ RandomVar1     : Factor w/ 3 levels "Group A","Group B",..: 2 2 2 2 1 2 1 1 1 2 ...
#>  $ RandomVar2     : Factor w/ 2 levels "Type X","Type Y": 1 2 2 1 2 2 1 2 1 1 ...
#>  $ RareCategory   : Factor w/ 3 levels "Common","Uncommon",..: 2 2 1 1 1 1 2 1 1 1 ...
#>  $ BinaryOutcome  : Factor w/ 2 levels "Negative","Positive": 1 2 1 1 2 1 2 1 1 1 ...
#>  $ AgeGroup       : Factor w/ 3 levels "Young","Middle",..: 3 2 1 1 3 2 2 2 1 2 ...
#>  $ BiomarkerStatus: Factor w/ 2 levels "Negative","Positive": 2 1 2 1 2 2 2 1 1 1 ...
#>  - attr(*, "description")= chr "Test dataset for chisqposttest function with known associations"
#>  - attr(*, "associations")=List of 4
#>   ..$ strong  : chr "Treatment -> Response"
#>   ..$ moderate: chr [1:2] "TumorGrade -> TumorStage" "AgeGroup -> BiomarkerStatus"
#>   ..$ weak    : chr [1:2] "Institution -> QualityScore" "RareCategory -> BinaryOutcome"
#>   ..$ none    : chr "RandomVar1 ⊥ RandomVar2"
#>  - attr(*, "created")= Date[1:1], format: "2025-07-02"
#>  - attr(*, "sample_size")= num 300

# Example 1: Strong association (should be highly significant)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "Treatment",
  cols = "Response",
  posthoc = "bonferroni"
)
#> 
#>  CHI-SQUARE POST-HOC TESTS
#> 
#>  <div style='padding: 15px; background-color: #fff3cd; border: 1px
#>  solid #ffc107; color: #856404;'>Warning:33% of cells have expected
#>  counts < 5. Chi-square test assumptions violated. Use Fisher's exact
#>  test for more reliable results.
#> 
#>  Chi-Square Test Results                        
#>  ────────────────────────────────────────────── 
#>    Statistic     Value       df    p-value      
#>  ────────────────────────────────────────────── 
#>    Chi-Square    66.63312     2    < .0000001   
#>  ────────────────────────────────────────────── 
#> 
#> 
#>  <table style="border-collapse: collapse; width: 100%; margin: 15px 0;
#>  font-family: &#39;Segoe UI&#39;, system-ui, sans-serif; font-size:
#>  13px; background-color: white; box-shadow: 0 1px 3px
#>  rgba(0,0,0,0.1);">
#> 
#>  <tr style="background-color: #e3f2fd;">
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #f8f9fa;">
#>  <div style="font-weight: bold; color: #495057;">
#>  Response →
#>  <br/>
#>  Treatment ↓
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Response
#>  <div style="font-size: 13px; color: #212529;">No Response
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Response
#>  <div style="font-size: 13px; color: #212529;">Response
#> 
#> 
#> 
#> 
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Treatment
#>  <div style="font-size: 13px; color: #212529;">Standard
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">88
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">28
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Treatment
#>  <div style="font-size: 13px; color: #212529;">Experimental
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">49
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">130
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Treatment
#>  <div style="font-size: 13px; color: #212529;">NA
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">2
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">3
#> 
#> 
#> 
#>  <div style="padding: 10px; background-color: #e3f2fd; border-left: 4px
#>  solid #1976d2; margin: 8px 0;">
#>  Method notice: Pairwise comparisons with expected cell counts below 5
#>  are automatically analysed with Fisher's exact test; the reported
#>  p-values use that exact method.
#> 
#>  Pairwise Comparison Results                                                                                                                      
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison                  Test Method       Chi-Square    p-value       Adj. p-value    Effect Size (Phi/V)    95% CI (Phi)    Significant   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Standard vs Experimental    Chi-square        66.5303419    < .0000001    < .0000001             0.47500000                    Yes           
#>    Standard vs NA              Fisher's exact     3.2350414     0.1056590       0.4226362             0.16400000                    No            
#>    Experimental vs NA          Fisher's exact     0.3870168     0.6181533       1.0000000             0.04600000                    No            
#>    No Response vs Response     Fisher's exact    66.6331188    < .0000001    < .0000001             0.47100000                    Yes           
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

# Example 2: Moderate association (should be significant with post-hoc differences)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "TumorGrade", 
  cols = "TumorStage",
  posthoc = "fdr"
)
#> 
#>  CHI-SQUARE POST-HOC TESTS
#> 
#>  <div style='padding: 15px; background-color: #fff3cd; border: 1px
#>  solid #ffc107; color: #856404;'>Warning:25% of cells have expected
#>  counts < 5. Chi-square test assumptions violated. Use Fisher's exact
#>  test for more reliable results.
#> 
#>  Chi-Square Test Results                        
#>  ────────────────────────────────────────────── 
#>    Statistic     Value       df    p-value      
#>  ────────────────────────────────────────────── 
#>    Chi-Square    109.4691     6    < .0000001   
#>  ────────────────────────────────────────────── 
#> 
#> 
#>  <table style="border-collapse: collapse; width: 100%; margin: 15px 0;
#>  font-family: &#39;Segoe UI&#39;, system-ui, sans-serif; font-size:
#>  13px; background-color: white; box-shadow: 0 1px 3px
#>  rgba(0,0,0,0.1);">
#> 
#>  <tr style="background-color: #e3f2fd;">
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #f8f9fa;">
#>  <div style="font-weight: bold; color: #495057;">
#>  TumorStage →
#>  <br/>
#>  TumorGrade ↓
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorStage
#>  <div style="font-size: 13px; color: #212529;">Stage I
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorStage
#>  <div style="font-size: 13px; color: #212529;">Stage II
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorStage
#>  <div style="font-size: 13px; color: #212529;">Stage III
#> 
#> 
#> 
#> 
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorGrade
#>  <div style="font-size: 13px; color: #212529;">Grade 1
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">56
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">24
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">7
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorGrade
#>  <div style="font-size: 13px; color: #212529;">Grade 2
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">28
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">71
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">22
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorGrade
#>  <div style="font-size: 13px; color: #212529;">Grade 3
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">6
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">32
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">49
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">TumorGrade
#>  <div style="font-size: 13px; color: #212529;">NA
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">0
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">3
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">2
#> 
#> 
#> 
#>  <div style="padding: 10px; background-color: #e3f2fd; border-left: 4px
#>  solid #1976d2; margin: 8px 0;">
#>  Method notice: Pairwise comparisons with expected cell counts below 5
#>  are automatically analysed with Fisher's exact test; the reported
#>  p-values use that exact method.
#> 
#>  Pairwise Comparison Results                                                                                                                   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison               Test Method       Chi-Square    p-value       Adj. p-value    Effect Size (Phi/V)    95% CI (Phi)    Significant   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Grade 1 vs Grade 2       Chi-square         35.741905    < .0000001    < .0000001              0.4150000                    Yes           
#>    Grade 1 vs Grade 3       Chi-square         72.965438    < .0000001    < .0000001              0.6480000                    Yes           
#>    Grade 1 vs NA            Fisher's exact      9.846232     0.0045976       0.0059112              0.3270000                    Yes           
#>    Grade 2 vs Grade 3       Chi-square         34.637706    < .0000001    < .0000001              0.4080000                    Yes           
#>    Grade 2 vs NA            Fisher's exact      2.358454     0.2905058       0.3268190              0.1370000                    No            
#>    Grade 3 vs NA            Fisher's exact      1.242070     0.5701787       0.5701787              0.1160000                    No            
#>    Stage I vs Stage II      Fisher's exact     46.531754    < .0000001    < .0000001              0.4600000                    Yes           
#>    Stage I vs Stage III     Fisher's exact     74.117520    < .0000001    < .0000001              0.6600000                    Yes           
#>    Stage II vs Stage III    Fisher's exact     28.625696     0.0000014       0.0000021              0.3690000                    Yes           
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

# Example 3: No association (should be non-significant)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "RandomVar1",
  cols = "RandomVar2",
  posthoc = "bonferroni"
)
#> 
#>  CHI-SQUARE POST-HOC TESTS
#> 
#> character(0)
#> 
#>  Chi-Square Test Results                       
#>  ───────────────────────────────────────────── 
#>    Statistic     Value       df    p-value     
#>  ───────────────────────────────────────────── 
#>    Chi-Square    6.359561     2    0.0415948   
#>  ───────────────────────────────────────────── 
#> 
#> 
#>  <table style="border-collapse: collapse; width: 100%; margin: 15px 0;
#>  font-family: &#39;Segoe UI&#39;, system-ui, sans-serif; font-size:
#>  13px; background-color: white; box-shadow: 0 1px 3px
#>  rgba(0,0,0,0.1);">
#> 
#>  <tr style="background-color: #e3f2fd;">
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #f8f9fa;">
#>  <div style="font-weight: bold; color: #495057;">
#>  RandomVar2 →
#>  <br/>
#>  RandomVar1 ↓
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RandomVar2
#>  <div style="font-size: 13px; color: #212529;">Type X
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RandomVar2
#>  <div style="font-size: 13px; color: #212529;">Type Y
#> 
#> 
#> 
#> 
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RandomVar1
#>  <div style="font-size: 13px; color: #212529;">Group A
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">47
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">49
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RandomVar1
#>  <div style="font-size: 13px; color: #212529;">Group B
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">51
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">57
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RandomVar1
#>  <div style="font-size: 13px; color: #212529;">Group C
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">61
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">35
#> 
#> 
#> 
#> character(0)
#> 
#>  Pairwise Comparison Results                                                                                                            
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison            Test Method    Chi-Square    p-value      Adj. p-value    Effect Size (Phi/V)    95% CI (Phi)    Significant   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Group A vs Group B    Chi-square     0.06136889    0.8043453       1.0000000             0.01700000                    No            
#>    Group A vs Group C    Chi-square     4.14814815    0.0416801       0.1667203             0.14700000                    No            
#>    Group B vs Group C    Chi-square     5.46676048    0.0193815       0.0775261             0.16400000                    No            
#>    Type X vs Type Y      Chi-square     6.35956109    0.0415948       0.1663791             0.14600000                    No            
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

# Example 4: Edge case with rare categories
chisqposttest(
  data = chisqposttest_test_data,
  rows = "RareCategory",
  cols = "BinaryOutcome",
  posthoc = "fdr"
)
#> 
#>  CHI-SQUARE POST-HOC TESTS
#> 
#> character(0)
#> 
#>  Chi-Square Test Results                       
#>  ───────────────────────────────────────────── 
#>    Statistic     Value       df    p-value     
#>  ───────────────────────────────────────────── 
#>    Chi-Square    24.74713     2    0.0000042   
#>  ───────────────────────────────────────────── 
#> 
#> 
#>  <table style="border-collapse: collapse; width: 100%; margin: 15px 0;
#>  font-family: &#39;Segoe UI&#39;, system-ui, sans-serif; font-size:
#>  13px; background-color: white; box-shadow: 0 1px 3px
#>  rgba(0,0,0,0.1);">
#> 
#>  <tr style="background-color: #e3f2fd;">
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #f8f9fa;">
#>  <div style="font-weight: bold; color: #495057;">
#>  BinaryOutcome →
#>  <br/>
#>  RareCategory ↓
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">BinaryOutcome
#>  <div style="font-size: 13px; color: #212529;">Negative
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">BinaryOutcome
#>  <div style="font-size: 13px; color: #212529;">Positive
#> 
#> 
#> 
#> 
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RareCategory
#>  <div style="font-size: 13px; color: #212529;">Common
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">144
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">56
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RareCategory
#>  <div style="font-size: 13px; color: #212529;">Uncommon
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">53
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">28
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">RareCategory
#>  <div style="font-size: 13px; color: #212529;">Rare
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">3
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">16
#> 
#> 
#> 
#> character(0)
#> 
#>  Pairwise Comparison Results                                                                                                              
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison              Test Method    Chi-Square    p-value      Adj. p-value    Effect Size (Phi/V)    95% CI (Phi)    Significant   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Common vs Uncommon      Chi-square       1.186668    0.2760028       0.2760028             0.06500000                    No            
#>    Common vs Rare          Chi-square      24.843523    0.0000006       0.0000025             0.33700000                    Yes           
#>    Uncommon vs Rare        Chi-square      15.392437    0.0000873       0.0001164             0.39200000                    Yes           
#>    Negative vs Positive    Chi-square      24.747135    0.0000042       0.0000085             0.28700000                    Yes           
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

# Example 5: Missing data handling
chisqposttest(
  data = chisqposttest_test_data,
  rows = "Treatment",
  cols = "Sex",
  excl = TRUE  # Exclude missing values
)
#> 
#>  CHI-SQUARE POST-HOC TESTS
#> 
#> character(0)
#> 
#>  Chi-Square Test Results                         
#>  ─────────────────────────────────────────────── 
#>    Statistic     Value         df    p-value     
#>  ─────────────────────────────────────────────── 
#>    Chi-Square    0.02066086     1    0.8857067   
#>  ─────────────────────────────────────────────── 
#> 
#> 
#>  <table style="border-collapse: collapse; width: 100%; margin: 15px 0;
#>  font-family: &#39;Segoe UI&#39;, system-ui, sans-serif; font-size:
#>  13px; background-color: white; box-shadow: 0 1px 3px
#>  rgba(0,0,0,0.1);">
#> 
#>  <tr style="background-color: #e3f2fd;">
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #f8f9fa;">
#>  <div style="font-weight: bold; color: #495057;">
#>  Sex →
#>  <br/>
#>  Treatment ↓
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom: 2px;">Sex
#>  <div style="font-size: 13px; color: #212529;">Male
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center; background-color: #e3f2fd;">
#>  <div style="font-weight: bold;">
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom: 2px;">Sex
#>  <div style="font-size: 13px; color: #212529;">Female
#> 
#> 
#> 
#> 
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Treatment
#>  <div style="font-size: 13px; color: #212529;">Standard
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">50
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">62
#> 
#> 
#>  <th style="border: 1px solid #e1e5e9; padding: 8px; background-color:
#>  #e3f2fd; font-weight: bold;">
#> 
#>  <div style="font-size: 11px; color: #6c757d; margin-bottom:
#>  2px;">Treatment
#>  <div style="font-size: 13px; color: #212529;">Experimental
#> 
#> 
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">81
#>  <td style="border: 1px solid #e1e5e9; padding: 8px; text-align:
#>  center;">97
#> 
#> 
#> 
#>  <div style='padding: 15px; background-color: #fff3cd; border: 1px
#>  solid #ffc107;'>Post-hoc Testing Not Performed: Overall chi-square
#>  test is not significant (p = 0.886 ≥ 0.05). Post-hoc pairwise
#>  comparisons are only valid when the overall test is significant.
#>  Running pairwise tests after a non-significant omnibus test increases
#>  Type I error (false positives) and constitutes data dredging.
```
