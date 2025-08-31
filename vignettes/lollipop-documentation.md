# Lollipop Function Documentation

## Overview

The lollipop function creates comprehensive lollipop charts for categorical data visualization with emphasis on clinical applications. Lollipop charts are particularly effective for displaying categorical data with focus on individual values, making them ideal for patient timelines, treatment outcomes, biomarker levels, and comparative clinical assessments.

## Architecture Components

### 1. Analysis Definition (.a.yaml)
- **File**: `jamovi/lollipop.a.yaml`
- **Purpose**: Defines all available options, parameters, defaults, and constraints
- **Key sections**: Data types, validation rules, clinical examples, usage patterns

### 2. User Interface (.u.yaml)
- **File**: `jamovi/lollipop.u.yaml`
- **Purpose**: Defines the UI layout, controls, and user interaction patterns
- **Key sections**: Variable selection, collapsible option groups, conditional enabling

### 3. Backend Implementation (.b.R)
- **File**: `R/lollipop.b.R`
- **Purpose**: Core R6 class with analysis logic and ggplot2 visualization
- **Key sections**: Data validation, statistical calculations, plot generation

### 4. Results Definition (.r.yaml)
- **File**: `jamovi/lollipop.r.yaml`
- **Purpose**: Defines output structure, tables, plots, and references
- **Key sections**: Instructions display, data summary table, plot output

## UI Controls to Options Mapping

### Variable Selection Section
```yaml
UI Control                     → Option Name    → Type        → Description
─────────────────────────────────────────────────────────────────────────
VariablesListBox (dep)        → dep            → Variable    → Numeric dependent variable
VariablesListBox (group)      → group          → Variable    → Categorical grouping variable
CheckBox "Use Highlighting"   → useHighlight   → Bool        → Enable/disable highlighting
LevelSelector "Highlight"     → highlight      → Level       → Specific level to highlight
```

### Chart Configuration Section
```yaml
UI Control                     → Option Name    → Type        → Default      → Description
────────────────────────────────────────────────────────────────────────────────────────
ComboBox "Sort Order"         → sortBy         → List        → original     → Data ordering method
ComboBox "Chart Orientation"  → orientation    → List        → vertical     → Plot orientation
CheckBox "Show Value Labels"  → showValues     → Bool        → false        → Display value labels
CheckBox "Show Mean Line"     → showMean       → Bool        → false        → Display mean reference
```

### Visual Appearance Section
```yaml
UI Control                     → Option Name    → Type        → Default      → Range       → Description
──────────────────────────────────────────────────────────────────────────────────────────────────────
ComboBox "Color Scheme"       → colorScheme    → List        → default      → N/A         → Color palette
ComboBox "Plot Theme"         → theme          → List        → default      → N/A         → Overall appearance
TextBox "Point Size"          → pointSize      → Number      → 3            → 1-10        → Lollipop point size
TextBox "Line Width"          → lineWidth      → Number      → 1            → 0.5-5       → Stem line width
ComboBox "Line Type"          → lineType       → List        → solid        → N/A         → Stem line style
TextBox "Baseline Value"      → baseline       → Number      → 0            → Any         → Starting point for stems
```

### Advanced Coloring Section
```yaml
UI Control                     → Option Name        → Type     → Default → Description
──────────────────────────────────────────────────────────────────────────────────
CheckBox "Conditional Color"  → conditionalColor   → Bool     → false   → Enable threshold coloring
TextBox "Color Threshold"     → colorThreshold     → Number   → 0       → Threshold for color change
```

### Labels & Titles Section
```yaml
UI Control                     → Option Name    → Type        → Description
─────────────────────────────────────────────────────────────────────────
TextBox "Chart Title"         → title          → String      → Custom plot title
TextBox "X-axis Label"        → xlabel         → String      → Custom x-axis label
TextBox "Y-axis Label"        → ylabel         → String      → Custom y-axis label
```

### Plot Size Section
```yaml
UI Control                     → Option Name    → Type        → Default → Range      → Description
─────────────────────────────────────────────────────────────────────────────────────────────
TextBox "Width (pixels)"      → width          → Integer     → 800     → 300-1200  → Plot width
TextBox "Height (pixels)"     → height         → Integer     → 600     → 300-1000  → Plot height
```

## Options Reference

### Core Options
- **data**: Data frame containing the variables
- **dep**: Numeric dependent variable (biomarker levels, scores, measurements)
- **group**: Categorical grouping variable (patient IDs, treatments, conditions)

### Highlighting Options
- **useHighlight**: Boolean to enable/disable highlighting functionality
- **highlight**: Specific level from grouping variable to highlight with different color/style

### Layout Options
- **sortBy**: Data ordering method
  - `original`: Keep original data order
  - `value_asc`: Sort by value (ascending)
  - `value_desc`: Sort by value (descending)
  - `group_alpha`: Sort by group name (alphabetical)
- **orientation**: Plot orientation (`vertical` or `horizontal`)

### Display Options
- **showValues**: Display numeric values on lollipop points
- **showMean**: Add horizontal/vertical reference line at mean value

### Styling Options
- **colorScheme**: Color palette selection
  - `default`: Standard ggplot2 colors
  - `clinical`: Clinical research appropriate colors
  - `viridis`: Colorblind-friendly viridis palette
  - `colorblind`: Colorblind-safe palette
- **theme**: Overall plot appearance
  - `default`: Standard ggplot2 theme
  - `minimal`: Clean, minimal appearance
  - `classic`: Traditional statistical plot style
  - `publication`: Publication-ready formatting

### Advanced Visual Options
- **pointSize**: Size of lollipop points (1-10)
- **lineWidth**: Width of lollipop stems (0.5-5)
- **lineType**: Style of stems (`solid`, `dashed`, `dotted`, `dotdash`)
- **baseline**: Starting value for lollipop stems (default: 0)

### Conditional Coloring
- **conditionalColor**: Enable color coding based on threshold
- **colorThreshold**: Numeric threshold for color change

### Customization
- **title**: Custom plot title
- **xlabel**: Custom x-axis label
- **ylabel**: Custom y-axis label
- **width**: Plot width in pixels (300-1200)
- **height**: Plot height in pixels (300-1000)

## Backend Implementation Patterns

### R6 Class Structure
```r
lollipopClass <- R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,  # Auto-generated from YAML files
    private = list(
        .init = function() { ... },      # Initialization and validation
        .run = function() { ... },       # Main analysis pipeline
        .cleanData = function() { ... }, # Data preparation and validation
        .plot = function(...) { ... }    # Plot generation
    )
)
```

### Key Backend Methods

#### .init() - Initialization
```r
- Package dependency checking (ggplot2, dplyr)
- Welcome message display for new users
- Result visibility management
- Initial validation setup
```

#### .run() - Main Analysis Pipeline
```r
1. Early validation (data existence, variable selection)
2. Data cleaning and preparation (.cleanData())
3. Statistical summary calculation (.calculateSummary())
4. Clinical summary generation (.generateClinicalSummary())
5. Warning and misuse detection (.checkForMisuseAndWarnings())
6. Plot data preparation (.savePlotData())
7. Comprehensive error handling with user-friendly messages
```

#### .cleanData() - Data Validation
```r
- Variable existence verification
- Numeric conversion for dependent variable
- Factor conversion for grouping variable
- Missing data handling with reporting
- Minimum data requirements checking
- Highlight level validation
- Data sorting application
```

#### .plot() - Visualization Generation
```r
- ggplot2 lollipop chart construction using geom_segment() and geom_point()
- Conditional aesthetics for highlighting
- Color scheme application
- Theme and styling
- Orientation handling (vertical/horizontal)
- Value label positioning
- Mean line addition
- Custom baseline implementation
```

### Data Flow Architecture

```
Input Data
    ↓
[Variable Validation]
    ↓
[Data Cleaning & Conversion]
    ↓
[Missing Data Handling]
    ↓
[Statistical Calculations]
    ↓
[Plot Data Preparation]
    ↓
[ggplot2 Visualization]
    ↓
Output (Plot + Summary)
```

## Results Structure

### Output Components

#### 1. Instructions (todo)
- **Type**: Html
- **Purpose**: Welcome message, clinical summary, warnings, error messages
- **Visibility**: Dynamic based on analysis state
- **Content**: Internationalized clinical guidance and statistical interpretation

#### 2. Data Summary (summary) 
- **Type**: Table
- **Columns**: 
  - `statistic`: Statistical measure name
  - `value`: Calculated value
- **Purpose**: Key descriptive statistics for clinical interpretation
- **Content**: Sample size, means, ranges, group information

#### 3. Plot Output (plot)
- **Type**: Image
- **Dimensions**: 700x500 (default, customizable via options)
- **Function**: `.plot` method
- **Requirements**: `requiresData: true`
- **Title**: Dynamic based on selected variables

### References
- ClinicoPathJamoviModule
- RGraphGalleryLollipop  
- ggplot2

## Data Flow Diagrams

### Main Analysis Flow
```
User Input → Variable Selection → Data Validation → Statistical Analysis → Visualization
     ↓              ↓                    ↓                   ↓                ↓
Options Panel → UI Controls → .cleanData() → .calculateSummary() → .plot()
     ↓              ↓                    ↓                   ↓                ↓
 Parameters → Validation → Clean Dataset → Statistics Table → Final Chart
```

### Error Handling Flow  
```
Error Detected → Error Classification → User Message Generation → UI Display
      ↓                    ↓                      ↓                  ↓
Missing Packages → Dependencies → Installation Guide → Alert Box
Missing Data → Data Issues → Validation Message → Warning Panel  
Invalid Options → Option Problems → Correction Guide → Error Display
```

### Conditional UI Flow
```
useHighlight = FALSE → highlight control disabled
useHighlight = TRUE → highlight control enabled → level selection available

conditionalColor = FALSE → colorThreshold disabled
conditionalColor = TRUE → colorThreshold enabled → threshold input available
```

## Execution Sequences

### 1. Initialization Sequence
```
1. R6 class instantiation
2. Package dependency verification
3. UI state initialization
4. Welcome message display
5. Result panel visibility setup
```

### 2. Analysis Execution Sequence
```
1. User variable selection triggers .run()
2. Input validation (data, variables)
3. Data cleaning pipeline (.cleanData())
4. Statistical calculation (.calculateSummary()) 
5. Clinical summary generation
6. Warning system activation
7. Plot data preparation
8. Visualization rendering (.plot())
9. Results display update
```

### 3. Plot Generation Sequence
```
1. Data preprocessing (sorting, filtering)
2. ggplot2 base layer creation
3. Lollipop geometry addition (geom_segment + geom_point)
4. Conditional aesthetics application
5. Color scheme and theme styling
6. Orientation transformation (if horizontal)
7. Label and title addition
8. Final plot rendering and display
```

## Clinical Applications

### 1. Treatment Comparison
```r
# Compare biomarker levels across treatment groups
lollipop(
    data = clinical_data,
    dep = "hemoglobin", 
    group = "treatment_group",
    sortBy = "value_desc",
    showMean = TRUE,
    title = "Hemoglobin Response by Treatment"
)
```

### 2. Patient Timeline Visualization
```r  
# Visualize patient progression over time
lollipop(
    data = patient_timeline,
    dep = "days_to_event",
    group = "patient_id", 
    useHighlight = TRUE,
    highlight = "high_risk_patient",
    orientation = "horizontal"
)
```

### 3. Biomarker Threshold Analysis
```r
# Identify patients above/below clinical thresholds
lollipop(
    data = lab_results,
    dep = "creatinine",
    group = "patient_id",
    conditionalColor = TRUE, 
    colorThreshold = 1.2,  # Clinical threshold
    baseline = 0.8         # Normal baseline
)
```

## Change Impact Guide

### When Modifying UI (.u.yaml)
- **Adding new controls**: Update corresponding options in .a.yaml
- **Changing control names**: Update option mappings in .b.R
- **Modifying layout**: Consider user workflow and clinical needs
- **Adding conditional logic**: Test all enable/disable scenarios

### When Modifying Options (.a.yaml)
- **Adding new options**: Add corresponding UI controls in .u.yaml
- **Changing defaults**: Consider clinical best practices
- **Updating validation**: Synchronize with .cleanData() method
- **Modifying examples**: Update with realistic clinical data

### When Modifying Backend (.b.R)
- **Changing validation logic**: Update error messages for clarity
- **Adding new features**: Consider performance impact with .checkpoint()
- **Modifying plot generation**: Test all option combinations
- **Updating statistics**: Ensure clinical relevance and accuracy

### When Modifying Results (.r.yaml)
- **Adding output items**: Update corresponding backend methods
- **Changing table structure**: Update population methods in .b.R
- **Modifying titles**: Ensure clinical appropriateness
- **Adding references**: Include relevant documentation

## Testing Scenarios

### Basic Functionality
1. Load simple clinical data
2. Select numeric dependent variable (biomarker level)
3. Select categorical grouping variable (treatment group)  
4. Verify plot generation and summary statistics

### Advanced Features
1. Test highlighting with clinical risk categories
2. Verify conditional coloring with clinical thresholds
3. Test orientation changes for patient timeline data
4. Validate sorting options for outcome ranking

### Error Handling
1. Test missing data scenarios
2. Verify invalid variable type handling
3. Test empty dataset conditions
4. Validate highlight level mismatch scenarios

### Clinical Workflow
1. Import real clinical dataset
2. Apply clinical thresholds and baselines
3. Generate publication-ready outputs
4. Verify statistical accuracy and clinical relevance

## Best Practices

### For Developers
- Always use .checkpoint() before expensive operations
- Implement comprehensive error handling with clinical context
- Use internationalization .() for all user messages
- Follow jamovi 4-file architecture consistently
- Test with realistic clinical datasets

### For Clinical Users  
- Choose appropriate color schemes for publication
- Use highlighting to emphasize clinical significance
- Apply relevant clinical thresholds and baselines
- Consider orientation based on data type (timeline vs. comparison)
- Validate statistical summaries against clinical expectations

### Performance Considerations
- Use strategic checkpoints for large datasets
- Implement early validation to avoid unnecessary computation
- Consider data size limits for visualization clarity
- Optimize plot rendering for publication quality
