
# TNM Stage Migration Analysis Test Datasets

This directory contains comprehensive test datasets for the Advanced TNM Stage Migration Analysis function.

## Datasets

### 1. stagemigration_lung_data.csv (N=500)
- **Purpose**: Primary test dataset for lung cancer staging migration
- **Migration Pattern**: Typical 7th to 8th edition TNM changes
- **Follow-up**: 60 months
- **Event Rate**: 68.2%
- **Migration Rate**: 70.8%

### 2. stagemigration_breast_data.csv (N=350)
- **Purpose**: Breast cancer with major staging revision
- **Migration Pattern**: Major revision scenario
- **Follow-up**: 120 months
- **Event Rate**: 56%
- **Migration Rate**: 90.9%

### 3. stagemigration_colon_data.csv (N=300)
- **Purpose**: Colon cancer with minor staging updates
- **Migration Pattern**: Minor update scenario
- **Follow-up**: 80 months
- **Event Rate**: 56.7%
- **Migration Rate**: 14%

### 4. stagemigration_small_test.csv (N=100)
- **Purpose**: Small dataset for basic functionality testing
- **Migration Pattern**: Typical
- **Follow-up**: 48 months
- **Event Rate**: 64%
- **Migration Rate**: 77%

### 5. stagemigration_large_test.csv (N=1000)
- **Purpose**: Large dataset for performance testing
- **Migration Pattern**: Major revision
- **Follow-up**: 60 months
- **Event Rate**: 69.4%
- **Migration Rate**: 74.3%

### 6. stagemigration_comprehensive_data.csv (N=1150)
- **Purpose**: Combined dataset with all cancer types
- **Migration Pattern**: Mixed scenarios
- **Event Rate**: 61.5%
- **Migration Rate**: 62.1%

## Variables

### Core Variables (Required)
- **PatientID**: Unique patient identifier
- **OldTNMStage**: Original TNM staging system
- **NewTNMStage**: Revised TNM staging system
- **SurvivalMonths**: Survival time in months
- **DeathEvent**: Death event indicator (1 = death, 0 = censored)

### Clinical Covariates (Optional)
- **Age**: Patient age at diagnosis
- **Gender**: Male/Female
- **SmokingStatus**: Never/Former/Current
- **PerformanceStatus**: ECOG performance status (0-2)
- **ComorbidityScore**: Comorbidity burden score

### Derived Variables
- **CancerType**: Cancer type (lung/breast/colon)
- **Dataset**: Dataset identifier
- **AgeBinary**: Age dichotomized at 65
- **HighRisk**: High-risk patient indicator

## Usage Examples

### Basic Migration Analysis
```r
# Load lung cancer data
data <- read.csv('stagemigration_lung_data.csv')

# Run basic analysis
ClinicoPath::stagemigration(
    data = data,
    oldStage = 'OldTNMStage',
    newStage = 'NewTNMStage',
    survivalTime = 'SurvivalMonths',
    event = 'DeathEvent'
)
```

### Advanced Multifactorial Analysis
```r
# Load comprehensive data
data <- read.csv('stagemigration_comprehensive_data.csv')

# Run advanced analysis with covariates
ClinicoPath::stagemigration(
    data = data,
    oldStage = 'OldTNMStage',
    newStage = 'NewTNMStage',
    survivalTime = 'SurvivalMonths',
    event = 'DeathEvent',
    continuousCovariates = c('Age'),
    categoricalCovariates = c('Gender', 'SmokingStatus'),
    performBootstrap = TRUE,
    calculateNRI = TRUE,
    calculateIDI = TRUE
)
```

## Data Generation

All datasets were generated using realistic clinical parameters:
- Age-appropriate survival distributions by cancer type
- Clinically realistic TNM stage migration patterns
- Representative covariate distributions
- Appropriate censoring rates (~15%)
- Missing data patterns for robustness testing

Generated on: 2025-09-24
Generator version: 1.0

