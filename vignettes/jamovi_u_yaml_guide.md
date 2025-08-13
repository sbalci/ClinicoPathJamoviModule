# The Complete Guide to Writing `.u.yaml` Files for jamovi Development

This is the definitive, comprehensive guide to writing `.u.yaml` files for developing jamovi modules. These files define the user interface layout and visual organization of your analysis, transforming the parameter definitions from `.a.yaml` into an intuitive, user-friendly interface.

## Table of Contents

1. [Introduction: The Role of `.u.yaml`](#1-introduction-the-role-of-uyaml)
2. [File Structure and Core Properties](#2-file-structure-and-core-properties)
3. [Essential Top-Level Properties](#3-essential-top-level-properties)
4. [UI Component Architecture](#4-ui-component-architecture)
5. [Core UI Components: Complete Reference](#5-core-ui-components-complete-reference)
6. [Layout and Organization Patterns](#6-layout-and-organization-patterns)
7. [Dynamic and Conditional Interfaces](#7-dynamic-and-conditional-interfaces)
8. [Advanced UI Features and Customization](#8-advanced-ui-features-and-customization)
9. [Clinical and Scientific Interface Design](#9-clinical-and-scientific-interface-design)
10. [Best Practices and Design Principles](#10-best-practices-and-design-principles)
11. [Complete Examples](#11-complete-examples)

---

## 1. Introduction: The Role of `.u.yaml`

The `.u.yaml` file serves as the **user interface definition** for a jamovi analysis, acting as the bridge between parameter specifications and user experience. It transforms the options defined in `.a.yaml` into a structured, intuitive interface.

### Key Responsibilities

- **Visual Layout**: How UI elements are arranged and grouped
- **User Experience**: Progressive disclosure, logical flow, accessibility
- **Dynamic Behavior**: Conditional visibility, enabling/disabling elements
- **Information Architecture**: Organizing complex interfaces into manageable sections

### The jamovi Module Integration

The `.u.yaml` file works in coordination with:

- **`.a.yaml`** - References option definitions by name
- **`.r.yaml`** - Interface affects result visibility through option values
- **`.b.R`** - UI selections control R code execution paths

**Compilation Process**: The `.u.yaml` file is compiled into interface code that creates the actual UI elements users interact with in jamovi.

---

## 2. File Structure and Core Properties

Every `.u.yaml` file follows a hierarchical tree structure:

```yaml
title: Analysis Title
name: analysisname  
jus: '3.0'
stage: 0
compilerMode: tame
children:
    - type: ComponentType1
      property1: value1
      # ... component-specific properties
      children:
          - type: NestedComponent
            # ... nested properties
    - type: ComponentType2
      property2: value2
      # ... more components
```

---

## 3. Essential Top-Level Properties

### Core Identification Properties

#### `title` (Required)
- **Type**: String
- **Purpose**: Display title shown in jamovi analysis selection
- **Guidelines**: Should match or complement the `.a.yaml` title
- **Example**: `title: Advanced Survival Analysis`

#### `name` (Required)
- **Type**: String
- **Purpose**: Must exactly match corresponding `.a.yaml`, `.r.yaml`, and `.b.R` files
- **Rules**: Single word, no spaces, valid identifier
- **Example**: `name: advancedsurvival`

#### `jus` (Required)
- **Type**: String (quoted)
- **Purpose**: jamovi UI Specification version
- **Current Standard**: `'3.0'`
- **Example**: `jus: '3.0'`

### Development and Compilation Properties

#### `stage` (Required)
- **Type**: Integer
- **Purpose**: Development stage indicator
- **Common Values**: 
  - `0` - Development/testing
  - `1` - Beta release
  - `2` - Production release
- **Example**: `stage: 0`

#### `compilerMode` (Required)
- **Type**: String
- **Purpose**: UI compilation optimization level
- **Options**:
  - `tame` - Conservative compilation (recommended)
  - `aggressive` - Advanced optimizations
- **Example**: `compilerMode: tame`

---

## 4. UI Component Architecture

### Component Hierarchy

jamovi UI components follow a tree structure where each component can contain child components:

```yaml
children:
    - type: LayoutBox           # Container component
      children:
          - type: ComboBox      # Input component
          - type: CheckBox      # Input component
    - type: CollapseBox         # Grouping component
      children:
          - type: VariableSupplier  # Complex container
            children:
                - type: TargetLayoutBox
                  children:
                      - type: VariablesListBox
```

### Component Categories

#### 1. Container Components
- **Purpose**: Organize and group other components
- **Examples**: `LayoutBox`, `CollapseBox`, `VariableSupplier`

#### 2. Input Components
- **Purpose**: Allow user input and selection
- **Examples**: `ComboBox`, `CheckBox`, `TextBox`, `VariablesListBox`

#### 3. Display Components
- **Purpose**: Provide information and labels
- **Examples**: `Label`, `Separator`

#### 4. Specialized Components
- **Purpose**: Complex, domain-specific functionality
- **Examples**: `LevelSelector`, `TargetLayoutBox`

---

## 5. Core UI Components: Complete Reference

### Container Components

#### `LayoutBox`
General-purpose container for organizing elements.

```yaml
- type: LayoutBox
  margin: large
  stretchFactor: 1
  children:
      - type: ComboBox
        name: analysisMethod
      - type: CheckBox
        name: includeConfidenceIntervals
```

**Properties**:
- `margin`: Spacing around the box (`none`, `small`, `normal`, `large`)
- `stretchFactor`: Proportional sizing weight
- `children`: List of child components

**Use Cases**:
- Grouping related controls
- Creating vertical or horizontal layouts
- Managing spacing and alignment

#### `CollapseBox`
Collapsible container for organizing advanced options.

```yaml
- type: CollapseBox
  label: Advanced Options
  collapsed: true
  stretchFactor: 1
  children:
      - type: CheckBox
        name: performBootstrap
      - type: TextBox
        name: bootstrapReps
        enable: performBootstrap
```

**Properties**:
- `label`: Header text displayed on the collapse bar
- `collapsed`: Initial state (`true` = collapsed, `false` = expanded)
- `stretchFactor`: Sizing behavior
- `children`: Components shown when expanded

**Use Cases**:
- Progressive disclosure of advanced features
- Reducing interface complexity
- Grouping expert-level options

#### `VariableSupplier`
Sophisticated container for variable selection interface.

```yaml
- type: VariableSupplier
  name: variableSupplier
  suggested: [continuous, nominal]
  permitted: [numeric, factor]
  populate: manual
  stretchFactor: 1
  children:
      - type: TargetLayoutBox
        label: Outcome Variable
        children:
            - type: VariablesListBox
              name: outcome
              maxItemCount: 1
              isTarget: true
```

**Properties**:
- `suggested`: Recommended variable types (highlights in source list)
- `permitted`: Allowed variable types (filters source list)
- `populate`: Population method (`manual`, `auto`)
- `children`: Target areas for variable selection

### Input Components

#### `ComboBox`
Dropdown selection for `List` options from `.a.yaml`.

```yaml
- type: ComboBox
  name: analysisMethod
  enable: performAnalysis
```

**Properties**:
- `name`: Must match a `List` option in `.a.yaml`
- `enable`: Conditional enabling expression

**R Integration**: Links to `List` options defined in `.a.yaml`

#### `CheckBox`
Checkbox for `Bool` options from `.a.yaml`.

```yaml
- type: CheckBox
  name: includeConfidenceIntervals
  label: Include 95% Confidence Intervals
  enable: showAdvancedOptions
```

**Properties**:
- `name`: Must match a `Bool` option in `.a.yaml`
- `label`: Override label from `.a.yaml` (optional)
- `enable`: Conditional enabling

#### `TextBox`
Text input for `String`, `Number`, and `Integer` options.

```yaml
- type: TextBox
  name: confidenceLevel
  suffix: "%"
  inputPattern: "[0-9]+\\.?[0-9]*"
  enable: includeConfidenceIntervals
```

**Properties**:
- `name`: Must match option in `.a.yaml`
- `suffix`: Text appended after input (e.g., units)
- `inputPattern`: Regular expression validation
- `enable`: Conditional enabling

#### `VariablesListBox`
Variable selection list for `Variable` and `Variables` options.

```yaml
- type: VariablesListBox
  name: covariates
  maxItemCount: 10
  isTarget: true
  itemDropBehaviour: insert
  enable: performMultivariateAnalysis
```

**Properties**:
- `name`: Must match `Variable` or `Variables` option in `.a.yaml`
- `maxItemCount`: Maximum variables allowed
- `isTarget`: Whether this is a drop target (usually `true`)
- `itemDropBehaviour`: How items are added (`insert`, `overwrite`)

#### `LevelSelector`
Level selection for `Level` options from `.a.yaml`.

```yaml
- type: LevelSelector
  name: eventLevel
  enable: (outcome && !performCompetingRisks)
```

**Properties**:
- `name`: Must match a `Level` option in `.a.yaml`
- `enable`: Usually conditional on variable selection

### Display Components

#### `Label`
Text labels for organizing and explaining interface sections.

```yaml
- type: Label
  label: Analysis Options
  children:
      - type: ComboBox
        name: method
```

**Properties**:
- `label`: Text to display
- `children`: Components grouped under this label

**Use Cases**:
- Section headers
- Explanatory text
- Organizing related controls

#### `Separator`
Visual separator line for organizing interface sections.

```yaml
- type: Separator
```

**Use Cases**:
- Dividing interface sections
- Visual organization
- Creating clear boundaries

### Specialized Components

#### `TargetLayoutBox`
Container for variable drop targets with labels.

```yaml
- type: TargetLayoutBox
  label: Primary Outcome
  children:
      - type: VariablesListBox
        name: outcome
        maxItemCount: 1
        isTarget: true
      - type: LevelSelector
        name: outcomeLevel
        enable: outcome
```

**Properties**:
- `label`: Descriptive text for the target area
- `children`: Usually contains `VariablesListBox` and related selectors

---

## 6. Layout and Organization Patterns

### Hierarchical Organization Strategy

#### 1. Variable Selection Area (Top)
```yaml
- type: VariableSupplier
  stretchFactor: 1
  children:
      # Primary variables
      - type: TargetLayoutBox
        label: Outcome Variable
        children:
            - type: VariablesListBox
              name: outcome
              maxItemCount: 1
              isTarget: true
              
      # Secondary variables
      - type: TargetLayoutBox
        label: Explanatory Variables
        children:
            - type: VariablesListBox
              name: covariates
              maxItemCount: 10
              isTarget: true
```

#### 2. Main Analysis Options (Middle)
```yaml
- type: LayoutBox
  margin: large
  children:
      - type: Label
        label: Analysis Method
      - type: ComboBox
        name: method
        
      - type: CheckBox
        name: includeConfidenceIntervals
```

#### 3. Advanced Options (Bottom)
```yaml
- type: CollapseBox
  label: Advanced Options
  collapsed: true
  children:
      - type: LayoutBox
        margin: normal
        children:
            - type: CheckBox
              name: performBootstrap
            - type: TextBox
              name: bootstrapReps
              enable: performBootstrap
```

### Layout Patterns

#### Vertical Stacking (Default)
```yaml
- type: LayoutBox
  margin: large
  children:
      - type: ComboBox
        name: method
      - type: CheckBox
        name: option1
      - type: CheckBox
        name: option2
```

#### Horizontal Grouping
```yaml
- type: LayoutBox
  margin: large
  children:
      - type: LayoutBox
        margin: small
        children:
            - type: CheckBox
              name: leftOption
      - type: LayoutBox
        margin: small
        children:
            - type: CheckBox
              name: rightOption
```

#### Nested Organization
```yaml
- type: CollapseBox
  label: Statistical Options
  children:
      - type: LayoutBox
        margin: normal
        children:
            - type: Label
              label: Hypothesis Testing
              children:
                  - type: ComboBox
                    name: testType
                  - type: TextBox
                    name: significanceLevel
```

---

## 7. Dynamic and Conditional Interfaces

### Enable Conditions

#### Simple Boolean Conditions
```yaml
# Enable when checkbox is checked
- type: TextBox
  name: bootstrapReps
  enable: performBootstrap

# Enable when checkbox is NOT checked  
- type: ComboBox
  name: alternativeMethod
  enable: (!useStandardMethod)
```

#### Variable-Based Conditions
```yaml
# Enable when variable is selected
- type: LevelSelector
  name: eventLevel
  enable: outcome

# Enable when specific variable type is selected
- type: CheckBox
  name: calculateMedian
  enable: (outcome && outcome.measureType == 'continuous')
```

#### Complex Logical Conditions
```yaml
# Multiple AND conditions
- type: LayoutBox
  enable: (outcome && covariates && performAdvanced)

# Multiple OR conditions
- type: ComboBox
  name: postHocTest
  enable: (method:anova || method:kruskal_wallis)

# Nested conditions
- type: TextBox
  name: customThreshold
  enable: (method:custom && (dataType:continuous || dataType:ordinal))
```

### Conditional Visibility Patterns

#### Progressive Disclosure
```yaml
# Basic options always visible
- type: ComboBox
  name: analysisType

# Intermediate options conditionally visible
- type: LayoutBox
  enable: (analysisType:advanced)
  children:
      - type: CheckBox
        name: performDiagnostics

# Expert options deeply nested
- type: CollapseBox
  label: Expert Options
  collapsed: true
  enable: (analysisType:advanced && performDiagnostics)
  children:
      - type: TextBox
        name: customParameters
```

#### Method-Specific Interfaces
```yaml
# Survival analysis specific
- type: LayoutBox
  enable: (method:survival)
  children:
      - type: VariablesListBox
        name: timeVar
      - type: VariablesListBox
        name: eventVar

# Regression analysis specific
- type: LayoutBox
  enable: (method:regression)
  children:
      - type: VariablesListBox
        name: predictors
      - type: CheckBox
        name: includeInteractions
```

### Dynamic Labels and Context

#### Context-Sensitive Labels
```yaml
- type: TargetLayoutBox
  label: "Time Variable (${timeUnit})"
  children:
      - type: VariablesListBox
        name: timeVar
```

#### Conditional Component Types
```yaml
# Different components based on data type
- type: VariablesListBox
  name: groupingVar
  enable: (analysisType:grouped)
  
- type: LevelSelector
  name: groupingLevel  
  enable: (groupingVar && groupingVar.measureType == 'nominal')
```

---

## 8. Advanced UI Features and Customization

### Visual Customization

#### Spacing and Layout Control
```yaml
- type: LayoutBox
  margin: large           # large, normal, small, none
  stretchFactor: 2        # Proportional sizing
  fitToGrid: true         # Grid alignment
  children:
      - type: ComboBox
        name: method
        stretchFactor: 1
```

#### Component Sizing
```yaml
- type: VariablesListBox
  name: variables
  maxItemCount: 5         # Limit selections
  minItemCount: 1         # Require selections
  isTarget: true
  fitToGrid: true
  stretchFactor: 1
```

### Advanced Variable Handling

#### Variable Type Filtering
```yaml
- type: VariableSupplier
  name: variableSupplier
  suggested: [continuous, ordinal]    # Highlighted in source
  permitted: [numeric]                # Allowed in targets
  populate: manual                    # Population method
```

#### Drop Behavior Customization
```yaml
- type: VariablesListBox
  name: covariates
  itemDropBehaviour: insert    # insert, overwrite, insertAfter
  maxItemCount: 10
  isTarget: true
```

### Interaction Patterns

#### Cascading Selections
```yaml
# Primary selection affects secondary options
- type: VariablesListBox
  name: primaryVar
  maxItemCount: 1
  isTarget: true

- type: LevelSelector
  name: primaryLevel
  enable: primaryVar

- type: ComboBox
  name: analysisMethod
  enable: (primaryVar && primaryLevel)
```

#### Mutual Exclusivity
```yaml
# Only one analysis type can be selected
- type: CheckBox
  name: performSurvival
  enable: (!performRegression && !performClassification)

- type: CheckBox
  name: performRegression  
  enable: (!performSurvival && !performClassification)

- type: CheckBox
  name: performClassification
  enable: (!performSurvival && !performRegression)
```

---

## 9. Clinical and Scientific Interface Design

### Clinical Workflow Patterns

#### Patient Outcome Analysis Interface
```yaml
children:
    # Patient/Study Variables
    - type: VariableSupplier
      stretchFactor: 1
      children:
          - type: TargetLayoutBox
            label: Patient ID
            children:
                - type: VariablesListBox
                  name: patientID
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Primary Outcome
            children:
                - type: VariablesListBox
                  name: primaryOutcome
                  maxItemCount: 1
                  isTarget: true
                - type: LevelSelector
                  name: outcomeLevel
                  enable: primaryOutcome
                  
          - type: TargetLayoutBox
            label: Time Variables
            children:
                - type: VariablesListBox
                  name: timeVariables
                  maxItemCount: 2
                  isTarget: true
                  
    # Analysis Configuration
    - type: Label
      label: Analysis Configuration
    - type: LayoutBox
      margin: large
      children:
          - type: ComboBox
            name: analysisType
          - type: TextBox
            name: followUpPeriod
            suffix: "months"
            enable: (analysisType:survival)
```

#### Diagnostic Test Interface
```yaml
children:
    - type: VariableSupplier
      children:
          - type: TargetLayoutBox
            label: Test Result
            children:
                - type: VariablesListBox
                  name: testResult
                  maxItemCount: 1
                  isTarget: true
                - type: LevelSelector
                  name: positiveLevel
                  enable: testResult
                  
          - type: TargetLayoutBox
            label: Gold Standard (Reference)
            children:
                - type: VariablesListBox
                  name: goldStandard
                  maxItemCount: 1
                  isTarget: true
                - type: LevelSelector
                  name: diseaseLevel
                  enable: goldStandard
                  
    - type: CollapseBox
      label: Diagnostic Metrics
      collapsed: false
      children:
          - type: CheckBox
            name: calculateSensitivity
          - type: CheckBox
            name: calculateSpecificity
          - type: CheckBox
            name: calculatePredictiveValues
          - type: CheckBox
            name: calculateLikelihoodRatios
```

#### Survival Analysis Interface
```yaml
children:
    - type: VariableSupplier
      children:
          - type: TargetLayoutBox
            label: Survival Time
            children:
                - type: VariablesListBox
                  name: timeVar
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Event Indicator
            children:
                - type: VariablesListBox
                  name: eventVar
                  maxItemCount: 1
                  isTarget: true
                - type: LevelSelector
                  name: eventLevel
                  enable: (eventVar && !competingRisks)
                  
          - type: TargetLayoutBox
            label: Grouping Variable
            children:
                - type: VariablesListBox
                  name: grouping
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Covariates
            children:
                - type: VariablesListBox
                  name: covariates
                  maxItemCount: 10
                  isTarget: true
                  
    - type: CollapseBox
      label: Analysis Options
      collapsed: false
      children:
          - type: CheckBox
            name: performKaplanMeier
          - type: CheckBox
            name: performCoxRegression
            enable: covariates
          - type: CheckBox
            name: competingRisks
          - type: CheckBox
            name: performLogRank
            enable: grouping
```

### Research-Focused Interfaces

#### Meta-Analysis Interface
```yaml
children:
    - type: Label
      label: Study Data Requirements
    - type: VariableSupplier
      children:
          - type: TargetLayoutBox
            label: Effect Sizes
            children:
                - type: VariablesListBox
                  name: effectSizes
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Standard Errors
            children:
                - type: VariablesListBox
                  name: standardErrors
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Study Identifiers
            children:
                - type: VariablesListBox
                  name: studyIDs
                  maxItemCount: 1
                  isTarget: true
                  
    - type: CollapseBox
      label: Meta-Analysis Options
      children:
          - type: ComboBox
            name: analysisModel
          - type: CheckBox
            name: assessHeterogeneity
          - type: CheckBox
            name: performSensitivityAnalysis
          - type: CheckBox
            name: assessPublicationBias
```

---

## 10. Best Practices and Design Principles

### User Experience Design

#### 1. Progressive Disclosure
Structure interfaces from basic to advanced:

```yaml
# Always visible - essential controls
- type: LayoutBox
  children:
      - type: ComboBox
        name: analysisType
      - type: VariablesListBox
        name: primaryVariable

# Conditionally visible - intermediate options
- type: LayoutBox
  enable: (analysisType:advanced)
  children:
      - type: CheckBox
        name: performAdvancedAnalysis

# Collapsed by default - expert options  
- type: CollapseBox
  label: Expert Options
  collapsed: true
  enable: performAdvancedAnalysis
  children:
      - type: TextBox
        name: customParameters
```

#### 2. Logical Organization
Group related elements and maintain consistent patterns:

```yaml
# Data input section
- type: VariableSupplier
  # ... variable selections

# Analysis configuration section  
- type: Label
  label: Analysis Configuration
- type: LayoutBox
  # ... analysis options

# Advanced options section
- type: CollapseBox
  label: Advanced Options
  # ... expert controls
```

#### 3. Clear Labeling and Context
Provide descriptive labels that guide users:

```yaml
- type: TargetLayoutBox
  label: Primary Outcome (Required)
  children:
      - type: VariablesListBox
        name: outcome
        
- type: TargetLayoutBox
  label: Covariates (Optional - for adjusted analysis)"
  children:
      - type: VariablesListBox
        name: covariates
```

### Technical Best Practices

#### 4. Consistent Conditional Logic
Use clear, readable enable conditions:

```yaml
# Simple conditions
enable: performAdvanced

# Variable-dependent conditions
enable: (outcome && grouping)

# Method-specific conditions
enable: (method:regression && includeInteractions)

# Complex but readable conditions
enable: (analysisType:survival && (performKM || performCox))
```

#### 5. Appropriate Component Selection
Choose UI components that match data types and user expectations:

```yaml
# List options -> ComboBox
- type: ComboBox
  name: analysisMethod

# Boolean options -> CheckBox  
- type: CheckBox
  name: includeConfidenceIntervals

# Numeric input -> TextBox with validation
- type: TextBox
  name: significanceLevel
  inputPattern: "0\\.[0-9]+"

# Variable selection -> VariablesListBox
- type: VariablesListBox
  name: covariates
```

#### 6. Performance and Responsiveness
Structure for efficient rendering and updates:

```yaml
# Group expensive operations
- type: CollapseBox
  label: Computationally Intensive Options
  collapsed: true
  children:
      - type: CheckBox
        name: performBootstrap
      - type: TextBox
        name: bootstrapReps
        enable: performBootstrap
```

### Accessibility and Usability

#### 7. Visual Hierarchy
Use spacing and organization to create clear visual structure:

```yaml
- type: LayoutBox
  margin: large              # Create breathing room
  children:
      - type: Label
        label: Section Title  # Clear section headers
      - type: LayoutBox
        margin: normal        # Sub-section spacing
        children:
            # Related controls grouped together
```

#### 8. Error Prevention
Design interfaces that prevent invalid selections:

```yaml
# Require essential variables before enabling analysis
- type: ComboBox
  name: analysisType
  enable: (outcome && (timeVar || grouping))

# Prevent conflicting selections
- type: CheckBox
  name: competingRisks
  enable: (!coxRegression || allowCompetingWithCox)
```

---

## 11. Complete Examples

### Example 1: Simple Analysis Interface

```yaml
title: Two-Sample t-Test
name: simplettest
jus: '3.0'
stage: 0
compilerMode: tame
children:
    # Variable Selection
    - type: VariableSupplier
      stretchFactor: 1
      children:
          - type: TargetLayoutBox
            label: Dependent Variable
            children:
                - type: VariablesListBox
                  name: outcome
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Grouping Variable
            children:
                - type: VariablesListBox
                  name: grouping
                  maxItemCount: 1
                  isTarget: true
                  
    # Analysis Options
    - type: Label
      label: Test Options
    - type: LayoutBox
      margin: large
      children:
          - type: CheckBox
            name: equalVariances
            label: Assume Equal Variances
          - type: ComboBox
            name: alternative
          - type: TextBox
            name: confidenceLevel
            suffix: "%"
            
    # Advanced Options
    - type: CollapseBox
      label: Advanced Options
      collapsed: true
      children:
          - type: CheckBox
            name: showDescriptives
          - type: CheckBox
            name: performNormalityTest
```

### Example 2: Complex Clinical Interface

```yaml
title: Comprehensive Survival Analysis
name: comprehensivesurvival
jus: '3.0'
stage: 0
compilerMode: tame
children:
    # Primary Variables
    - type: VariableSupplier
      name: primaryVariables
      stretchFactor: 1
      children:
          - type: TargetLayoutBox
            label: Time to Event
            children:
                - type: VariablesListBox
                  name: timeVar
                  maxItemCount: 1
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Event Indicator
            children:
                - type: VariablesListBox
                  name: eventVar
                  maxItemCount: 1
                  isTarget: true
                - type: LevelSelector
                  name: eventLevel
                  enable: (eventVar && !competingRisks)
                  
          - type: TargetLayoutBox
            label: Grouping Variable (Optional)
            children:
                - type: VariablesListBox
                  name: grouping
                  maxItemCount: 1
                  isTarget: true
                  
    # Analysis Methods
    - type: Label
      label: Analysis Methods
    - type: LayoutBox
      margin: large
      children:
          - type: CheckBox
            name: performKM
            label: Kaplan-Meier Analysis
          - type: CheckBox
            name: performCox
            label: Cox Regression
          - type: CheckBox
            name: competingRisks
            label: Competing Risks Analysis
            
    # Cox Regression Options
    - type: CollapseBox
      label: Cox Regression Options
      collapsed: false
      enable: performCox
      children:
          - type: VariableSupplier
            name: coxVariables
            children:
                - type: TargetLayoutBox
                  label: Covariates
                  children:
                      - type: VariablesListBox
                        name: covariates
                        maxItemCount: 20
                        isTarget: true
                        
          - type: LayoutBox
            margin: normal
            children:
                - type: CheckBox
                  name: performStepwise
                  label: Stepwise Variable Selection
                - type: ComboBox
                  name: selectionMethod
                  enable: performStepwise
                - type: CheckBox
                  name: showHazardRatios
                  label: Display Hazard Ratios
                  
    # Model Diagnostics
    - type: CollapseBox
      label: Model Diagnostics
      collapsed: true
      enable: performCox
      children:
          - type: CheckBox
            name: checkProportionalHazards
            label: Test Proportional Hazards Assumption
          - type: CheckBox
            name: showResidualPlots
            label: Residual Plots
          - type: CheckBox
            name: performInfluenceAnalysis
            label: Influence Analysis
            
    # Plot Options
    - type: CollapseBox
      label: Plot Options
      collapsed: false
      children:
          - type: CheckBox
            name: showSurvivalCurves
            label: Show Survival Curves
            enable: (performKM && grouping)
          - type: CheckBox
            name: showRiskTable
            label: Show Risk Table
            enable: showSurvivalCurves
          - type: CheckBox
            name: showConfidenceBands
            label: Show Confidence Bands
            enable: showSurvivalCurves
          - type: TextBox
            name: plotTimeLimit
            label: Plot Time Limit
            suffix: "units"
            enable: showSurvivalCurves
            
    # Advanced Statistical Options
    - type: CollapseBox
      label: Advanced Statistical Options
      collapsed: true
      children:
          - type: TextBox
            name: confidenceLevel
            label: Confidence Level
            suffix: "%"
          - type: CheckBox
            name: performBootstrap
            label: Bootstrap Validation
          - type: TextBox
            name: bootstrapReps
            label: Bootstrap Replications
            enable: performBootstrap
          - type: CheckBox
            name: robustVariance
            label: Robust Variance Estimation
            enable: performCox
```

### Example 3: Specialized Decision Analysis Interface

```yaml
title: Clinical Decision Analysis
name: clinicaldecision
jus: '3.0'
stage: 0
compilerMode: tame
children:
    # Tree Structure Variables
    - type: VariableSupplier
      name: treeVariables
      stretchFactor: 1
      children:
          - type: TargetLayoutBox
            label: Decision Variables
            children:
                - type: VariablesListBox
                  name: decisions
                  maxItemCount: 5
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Probability Variables
            children:
                - type: VariablesListBox
                  name: probabilities
                  maxItemCount: 20
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Cost Variables
            children:
                - type: VariablesListBox
                  name: costs
                  maxItemCount: 20
                  isTarget: true
                  
          - type: TargetLayoutBox
            label: Utility Variables
            children:
                - type: VariablesListBox
                  name: utilities
                  maxItemCount: 20
                  isTarget: true
                  
    # Tree Configuration
    - type: Label
      label: Tree Configuration
    - type: LayoutBox
      margin: large
      children:
          - type: ComboBox
            name: treeType
          - type: ComboBox
            name: layout
            
    # Economic Analysis
    - type: CollapseBox
      label: Economic Analysis
      collapsed: false
      children:
          - type: CheckBox
            name: performCEA
            label: Cost-Effectiveness Analysis
          - type: TextBox
            name: willingnessToPay
            label: Willingness to Pay Threshold
            suffix: "per QALY"
            enable: performCEA
          - type: TextBox
            name: discountRate
            label: Discount Rate
            suffix: "%"
            enable: performCEA
          - type: TextBox
            name: timeHorizon
            label: Time Horizon
            suffix: "years"
            enable: performCEA
            
    # Sensitivity Analysis
    - type: CollapseBox
      label: Sensitivity Analysis
      collapsed: true
      children:
          - type: CheckBox
            name: performSensitivity
            label: One-Way Sensitivity Analysis
          - type: CheckBox
            name: probabilisticAnalysis
            label: Probabilistic Sensitivity Analysis
          - type: TextBox
            name: numSimulations
            label: Number of Simulations
            enable: probabilisticAnalysis
          - type: CheckBox
            name: createCEAC
            label: Cost-Effectiveness Acceptability Curve
            enable: probabilisticAnalysis
            
    # Visualization Options
    - type: CollapseBox
      label: Visualization Options
      collapsed: false
      children:
          - type: CheckBox
            name: showTreeDiagram
            label: Decision Tree Diagram
          - type: CheckBox
            name: showTornadoDiagram
            label: Tornado Diagram
            enable: performSensitivity
          - type: CheckBox
            name: showScatterPlot
            label: Cost-Effectiveness Scatter Plot
            enable: probabilisticAnalysis
          - type: ComboBox
            name: colorScheme
            label: Color Scheme
```

---

## Conclusion

This comprehensive guide provides everything needed to create professional, user-friendly `.u.yaml` files for jamovi development. Key principles:

1. **User-Centered Design**: Organize interfaces to match user workflows and mental models
2. **Progressive Disclosure**: Show basic options first, advanced options conditionally
3. **Logical Organization**: Group related controls and maintain consistent patterns
4. **Dynamic Behavior**: Use conditional logic to create responsive, intelligent interfaces
5. **Clinical Focus**: Design interfaces that support clinical and research workflows

### Additional Resources

- [Official jamovi UI Definition Documentation](https://dev.jamovi.org/api_ui-definition.html)
- [jamovi Basic UI Design](https://dev.jamovi.org/ui-basic-design.html)
- [jamovi Advanced UI Design](https://dev.jamovi.org/ui-advanced-design.html)
- [jamovi Advanced UI Customization](https://dev.jamovi.org/ui-advanced-customisation.html)
- [ClinicoPath Module Examples](https://github.com/sbalci/ClinicoPathJamoviModule/tree/master/jamovi)

### Next Steps

After mastering `.u.yaml` files, explore:
- Integration patterns with `.a.yaml` and `.r.yaml` files
- `.b.R` files for implementing the logic behind UI interactions
- Advanced UI customization and theming
- Accessibility and internationalization considerations

This guide establishes the foundation for creating intuitive, professional interfaces that make complex statistical analyses accessible to clinical and research users.