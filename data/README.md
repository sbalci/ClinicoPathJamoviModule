# Swimmer Plot Test Data

This folder contains realistic test datasets for the swimmer plot analysis function.

## Available Datasets

### 1. `swimmerplot_test_data.csv`
**Description**: Date-based clinical trial dataset with comprehensive milestone and event data.

**Variables**:
- `PatientID`: Unique patient identifier (PT001-PT050)
- `StartDate`: Treatment start date (2022-2023 range)
- `EndDate`: Treatment end date
- `TreatmentStart`: Always 0 (treatment start reference)
- `TreatmentEnd`: Treatment duration in months
- `ResponseCategory`: RECIST response (CR, PR, SD, PD)
- `Surgery`: Surgery timing (months from start)
- `ProgAssessment`: Progression assessment timing (months)
- `Progression`: Progression event timing (months)
- `Death`: Death event timing (months)
- `ToxicityEvent`: Adverse event description
- `ToxicityTime`: Adverse event timing (months)
- `EventType`: Event category (Toxicity/Response)
- `EventTime`: Event timing (months)

**Use Case**: Test date/time processing, milestone tracking, and event markers.

### 2. `swimmerplot_numeric_data.csv`
**Description**: Numeric time-based dataset with simplified structure.

**Variables**:
- `PatientID`: Unique patient identifier (PT001-PT030)
- `StartTime`: Always 0 (baseline)
- `EndTime`: Follow-up duration in days
- `ResponseCategory`: Treatment response (CR, PR, SD, PD)
- `Surgery`: Surgery timing (days from start)
- `FirstAssessment`: First response assessment (usually day 90)
- `Progression`: Disease progression timing (days)
- `Death`: Death event timing (days)
- `AdverseEvent`: Adverse event description
- `EventTime`: Adverse event timing (days)

**Use Case**: Test numeric time processing and basic functionality.

## How to Test the Interface

### Basic Setup
1. Load either dataset into jamovi
2. Run the Swimmer Plot analysis from OncoPathT > Patient Follow-Up Plots

### Core Variables
- **Patient ID**: Select `PatientID`
- **Start Time**: Select `StartDate` (dates) or `StartTime` (numeric)
- **End Time**: Select `EndDate` (dates) or `EndTime` (numeric)
- **Response Variable**: Select `ResponseCategory`

### Important Notes
- **Date Auto-Detection**: The function automatically detects date strings in your data and will stop the analysis with clear guidance when dates are found but Time Type is set to "Raw Values"
- **Manual Configuration Required**: When using date data like `swimmerplot_test_data.csv`, you must manually configure the Time Input Type to "Date/Time" and select the appropriate date format
- **User Control**: This approach ensures you have full control over date parsing and timeline display options
- **Time Units**: The default time unit is "months" which works well with the test data

### Date Detection Behavior

When you load date data but have Time Type set to "Raw Values", the function will:
1. **Detect** common date formats (YYYY-MM-DD, MM/DD/YYYY, DD/MM/YYYY, etc.)
2. **Stop analysis** immediately with a helpful guidance message
3. **Show examples** of detected date formats from your data
4. **Provide step-by-step instructions** to configure the Time Input Type properly

**Example Detection Message:**
```
🔍 DATE FORMAT DETECTED:
Found date format (YYYY-MM-DD) in your time variables.
Examples: 2023-01-15, 2023-01-20

📋 REQUIRED ACTION:
1. Go to 'Time & Date Settings' section
2. Change 'Time Input Type' from 'Raw Values' to 'Date/Time'
3. Select 'Date Format': YYYY-MM-DD
4. Choose your preferred 'Time Display' mode
5. Re-run the analysis
```

### Testing Different Features

#### Time Processing
- **Date Format**: Use `swimmerplot_test_data.csv` with Time Type = "Date/Time"
- **Numeric Format**: Use `swimmerplot_numeric_data.csv` with Time Type = "Raw Values"
- **Time Units**: Test Days/Weeks/Months/Years
- **Display Mode**: Try both Relative and Absolute

#### Milestones
- **Surgery**: Use `Surgery` variable
- **Assessment**: Use `ProgAssessment` or `FirstAssessment`
- **Progression**: Use `Progression` variable
- **Death**: Use `Death` variable

#### Event Markers
- **Event Type**: Use `EventType` or `AdverseEvent`
- **Event Time**: Use `EventTime` or `ToxicityTime`

#### Clinical Enhancements
- Enable "Show Clinical Glossary" for medical term definitions
- Enable "Show Copy-Ready Manuscript Text" for publication-ready text
- Enable "Show About This Analysis" for usage guidance

### Expected Results

#### With Response Data
- ORR (Objective Response Rate): ~40-50%
- DCR (Disease Control Rate): ~70-80%
- Median follow-up: 6-8 months
- Color-coded swim lanes by response

#### Visual Elements
- Horizontal swim lanes for each patient
- Milestone markers at clinical events
- Event markers for adverse events
- Reference lines (optional)
- Professional clinical styling

### Testing Scenarios

1. **Basic Analysis**: Core variables only with numeric data
2. **Date Detection Test**: Load date data with Time Type = "Raw Values" to see detection message
3. **Proper Date Configuration**: Configure Time Type = "Date/Time" with correct date format
4. **Full Featured**: All milestones + event markers + clinical options
5. **Large Dataset**: Test performance with all 50 patients
6. **Missing Data**: Test robustness with partial data

## Notes

- All test data is synthetic and created for testing purposes only
- Response rates and timelines are representative of typical oncology trials
- Adverse events include realistic clinical terminology
- Datasets are designed to test all major functionality