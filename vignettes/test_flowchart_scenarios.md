# Flowchart Module Test Scenarios

This document provides comprehensive test scenarios for validating all flowchart features using the provided test datasets.

## Test Datasets Overview

### 1. Node-Count Mode (`test_data_node_count.csv`)
- **Purpose**: Traditional flowchart with participant counts at each stage
- **Features**: 14 nodes across screening, enrollment, treatment, and analysis phases
- **Key Variables**: `node_label`, `participant_count`, `node_type`, `study_phase`

### 2. Edge-List Mode (`test_data_edge_list.csv`)
- **Purpose**: Network-based flowchart showing connections between processes
- **Features**: 19 connections with different transition types
- **Key Variables**: `from_node`, `to_node`, `connection_type`, `process_group`, `transition_count`

### 3. Clinical Trial Flow (`test_data_clinical_flow.csv`)
- **Purpose**: Individual participant tracking through trial stages
- **Features**: 30 participants with complete follow-up data
- **Key Variables**: `participant_id`, `randomized`, `treatment_group`, `primary_outcome`, `exclusion_reason`

### 4. CONSORT Compliance (`test_data_consort_complete.csv`)
- **Purpose**: Full CONSORT 2010 standard compliance testing
- **Features**: 50 participants with comprehensive trial tracking
- **Key Variables**: All CONSORT-required fields including allocation, intervention, follow-up, and analysis populations

## Comprehensive Test Scenarios

### Scenario 1: Basic Node-Count Flowchart
**Dataset**: `test_data_node_count.csv`
**Configuration**:
- Data Mode: Node-count
- Engine: DiagrammeR
- Layout: Hierarchical
- Show Counts: Yes
- Show Percentages: Yes

**Expected Output**: Traditional flowchart showing participant flow from screening (485) to final analysis (318)

**Validation Points**:
- All 14 nodes display correctly
- Percentages calculated relative to initial screening
- Node colors differentiate by `node_type`
- Study phases group logically

### Scenario 2: Network Edge-List Visualization
**Dataset**: `test_data_edge_list.csv`
**Configuration**:
- Data Mode: Edge-list
- Engine: ggflowchart
- Layout: Tree
- Connection Style: Curved
- Show Transition Counts: Yes

**Expected Output**: Network diagram showing process connections with transition counts

**Validation Points**:
- 19 connections render correctly
- Different connection types (sequential, decision, split) styled differently
- Transition counts displayed on edges
- Process groups color-coded

### Scenario 3: Clinical Trial Flow Analysis
**Dataset**: `test_data_clinical_flow.csv`
**Configuration**:
- Data Mode: Clinical trial flow
- Engine: flowchart package
- Show Percentages: Yes
- Group by Treatment: Yes

**Expected Output**: Clinical trial participant flow with treatment group analysis

**Validation Points**:
- Automatic flow generation from individual participant data
- Treatment groups (Drug_A vs Drug_B) properly separated
- Exclusion reasons categorized and counted
- Completion rates calculated correctly

### Scenario 4: CONSORT Compliance Testing (Separate Module)
**Dataset**: `test_data_consort_complete.csv`
**Module**: **CONSORT Flowchart** (separate from Flowchart module)
**Configuration**:
- Data-Driven Mode: Enabled
- Validation: CONSORT 2010 compliance
- All clinical trial variables assigned

**Expected Output**: CONSORT 2010 compliant flowchart with validation report

**Validation Points**:
- All CONSORT elements present (Enrollment, Allocation, Follow-up, Analysis)
- Randomization properly displayed with arm allocation
- Intention-to-treat vs per-protocol populations shown
- Safety population tracking
- Structured validation report with compliance status

**Note**: For comprehensive CONSORT testing, use the dedicated CONSORT Flowchart module.

### Scenario 5: Advanced Styling and Layout
**Dataset**: `test_data_node_count.csv`
**Configuration**:
- Engine: ggflowchart
- Layout: Nicely (igraph)
- Node Style: Custom
- Color Scheme: Viridis
- Node Size: Proportional to count
- Custom Labels: Enabled

**Expected Output**: Stylized flowchart with advanced visual features

**Validation Points**:
- igraph layout algorithm applied successfully
- Node sizes proportional to participant counts
- Custom color scheme applied
- Professional styling maintained

### Scenario 6: Error Handling and Edge Cases
**Dataset**: Modified versions of test data
**Test Cases**:
1. Missing required columns
2. Invalid data types
3. Negative participant counts
4. Circular references in edge-list
5. Incomplete CONSORT data

**Expected Behavior**:
- Clear error messages without "Error:" prefixes
- Graceful fallback to alternative layouts
- Validation warnings for incomplete data
- Suggestions for data correction

### Scenario 7: Multi-Engine Comparison
**Dataset**: `test_data_clinical_flow.csv`
**Test**: Same data with different engines
- Engine 1: DiagrammeR (traditional)
- Engine 2: ggflowchart (modern)
- Engine 3: flowchart package (clinical)

**Validation Points**:
- Consistent data interpretation across engines
- Engine-appropriate visual styles
- Performance comparison
- Feature availability per engine

### Scenario 8: Large Dataset Performance
**Dataset**: `test_data_consort_complete.csv` (50 participants)
**Configuration**:
- All features enabled
- Maximum detail level
- Complex grouping and filtering

**Validation Points**:
- Rendering performance acceptable
- Memory usage reasonable
- Visual clarity maintained with large datasets
- Interactive elements responsive

## Quality Assurance Checklist

### Data Processing
- [ ] All data modes process correctly
- [ ] Column mappings work as expected
- [ ] Data validation catches common errors
- [ ] Percentages calculate accurately

### Visual Output
- [ ] All layout algorithms functional
- [ ] Colors and styling apply consistently
- [ ] Text labels readable and positioned correctly
- [ ] Proportional sizing works properly

### CONSORT Compliance
- [ ] All required elements present
- [ ] Validation logic accurate
- [ ] HTML output formatted correctly
- [ ] Compliance report comprehensive

### Error Handling
- [ ] Clear error messages
- [ ] Graceful degradation
- [ ] Helpful user guidance
- [ ] No system crashes

### Performance
- [ ] Reasonable rendering speed
- [ ] Memory usage within limits
- [ ] Large datasets handled gracefully
- [ ] Interactive features responsive

## Step-by-Step Variable Assignment Guide

### Test 1: Basic Node-Count Flowchart (`test_data_node_count.csv`)

**jamovi Interface Setup:**
1. Load `test_data_node_count.csv`
2. **Data Mode**: Select "Node-count"
3. **Variable Assignments**:
   - **Node Data** → `node_label` (stage descriptions like "Patients screened for eligibility")
   - **Node Counts** → `participant_count` (numbers: 485, 412, 389, etc.)
   - **Node Type** (optional) → `study_phase` (for color coding by phase)
4. **Engine Settings**: Try both DiagrammeR (traditional) and ggplot2 (modern)
5. **Display Options**: 
   - ✅ Show Percentages
   - ✅ Show Counts
   - ✅ Proportional Node Sizes

**Expected Result**: 14-node flowchart showing clinical trial progression from 485 screened to 318 analyzed

---

### Test 2: Network Edge-List Visualization (`test_data_edge_list.csv`)

**jamovi Interface Setup:**
1. Load `test_data_edge_list.csv`
2. **Data Mode**: Select "Edge-list"
3. **Variable Assignments**:
   - **From Variable** → `from_node` (starting process nodes)
   - **To Variable** → `to_node` (ending process nodes)
   - **Connection Type** (optional) → `connection_type` (sequential, decision, split)
   - **Transition Counts** (optional) → `transition_count` (edge weights)
4. **Engine Settings**: ggflowchart recommended for network layouts
5. **Layout Options**: Tree, Nicely, or Circle layout
6. **Display Options**:
   - ✅ Show Transition Counts
   - ✅ Curved Connections
   - ✅ Color by Connection Type

**Expected Result**: Network diagram with 19 connections showing process flow relationships

---

### Test 3: Clinical Trial Flow Analysis (`test_data_clinical_flow.csv`)

**jamovi Interface Setup:**
1. Load `test_data_clinical_flow.csv`
2. **Data Mode**: Select "Clinical trial flow"
3. **Variable Assignments**:
   - **Participant Variable** → `participant_id` (individual participant IDs)
   - **Trial Group Variable** → `treatment_group` (Drug_A vs Drug_B)
   - **Exclusion Variable** → `exclusion_reason` (dropout reasons)
   - **Randomization Variable** → `randomized` (1/0 randomization status)
   - **Completion Variable** → `study_complete` (completion status)
   - **Outcome Variable** → `primary_outcome` (treatment response)
4. **Engine Settings**: flowchart package (specialized for clinical flows)
5. **Clinical Options**:
   - ✅ Show Percentages in Flow
   - ✅ Group by Treatment
   - ✅ Show Exclusion Details

**Expected Result**: Participant flow diagram with treatment arm separation and dropout analysis

---

### Test 4: Full CONSORT Compliance (`test_data_consort_complete.csv`)

**⚠️ CONSORT Features Moved to Dedicated Module**

**Important**: CONSORT compliance features are now available in the dedicated **CONSORT Flowchart** module for specialized clinical trial reporting.

**For CONSORT-compliant diagrams:**
1. Navigate to **Clinico-Path Analyses → CONSORT Flowchart** (separate module)
2. Load `test_data_consort_complete.csv`
3. **Data-Driven Mode Available**: Use participant-level data for automatic CONSORT generation
4. **Variable Assignments**:
   - **Participant Variable** → `participant_id`
   - **Randomization Variable** → `randomized`
   - **Treatment Group Variable** → `treatment_arm`
   - **Completion Variable** → `completed_followup`
   - **Outcome Variable** → `primary_endpoint`
   - **Exclusion Variable** → `exclusion_reason`
5. **CONSORT Validation**: Built-in CONSORT 2010 compliance checking

**Expected Result**: Professional CONSORT 2010 compliant flowchart with validation report

**Alternative for Flowchart Module**: Use Test 3 (Clinical Trial Flow) for general participant tracking without CONSORT-specific validation.

---

### Test 5: Advanced Styling Options (Any Dataset)

**Visual Enhancement Settings:**
1. **Engine**: ggflowchart (for advanced styling)
2. **Layout Algorithm**: 
   - Tree (hierarchical)
   - Nicely (force-directed)
   - Circle (circular arrangement)
   - Star (radial from center)
3. **Color Schemes**:
   - Viridis (colorblind-friendly)
   - Set1 (distinct colors)
   - Custom (user-defined)
4. **Node Styling**:
   - ✅ Proportional Node Sizes
   - ✅ Content-Aware Sizing
   - ✅ Custom Node Colors
5. **Typography**:
   - Font size adjustment
   - Label positioning
   - Text wrapping options

---

## Quick Start Testing Sequence

### Immediate Test (Current Setup)
**Current Interface Shows**: Node-Count Mode selected
**Next Steps**:
1. Assign **Node Data** → `node_label`
2. Assign **Node Counts** → `participant_count` 
3. ✅ Enable "Show Percentages"
4. Try switching between DiagrammeR and ggplot2 engines
5. Add **Node Type** → `study_phase` for color coding

### Progressive Testing
1. **Start Simple**: Node-count with basic settings
2. **Add Complexity**: Enable styling and percentages  
3. **Switch Modes**: Try edge-list with network layout
4. **Clinical Focus**: Test clinical trial flow features
5. **Full CONSORT**: Complete compliance validation

## Usage Instructions

1. Load the ClinicoPathJamoviModule in jamovi
2. Navigate to Clinico-Path Analyses → Flowchart
3. Import one of the test datasets from the `data/` folder
4. Follow the variable assignment guide above for your chosen test
5. Configure display and engine options
6. Generate flowchart and validate against expected output
7. Check quality assurance points
8. Document any issues or unexpected behavior

## Troubleshooting Common Issues

### Layout Problems
- Try different layout algorithms if nodes overlap
- Adjust node spacing parameters
- Consider engine alternatives

### CONSORT Validation Issues
**Note**: CONSORT validation is now handled by the dedicated CONSORT Flowchart module.
- Use the CONSORT Flowchart module for compliance validation
- Check required column presence in CONSORT module
- Verify data types match CONSORT requirements

### Performance Issues
- Reduce dataset size for testing
- Disable optional features temporarily
- Try simpler layout algorithms

### Visual Quality Issues
- Adjust color schemes for accessibility
- Modify text size for readability
- Check node size proportionality settings

This comprehensive test suite ensures all flowchart features are validated with realistic clinical trial data.