# Q-TWiST Implementation Plan for ClinicoPath

## Current Status: ❌ NOT IMPLEMENTED

Q-TWiST (Quality-adjusted Time Without Symptoms or Toxicity) analysis is **not currently available** in the ClinicoPath module, but implementation is **highly feasible** given existing infrastructure.

## Executive Summary

Q-TWiST is a sophisticated survival analysis method that partitions survival time into health states with different quality-of-life weights. It's particularly valuable in oncology trials where treatments may extend survival but with significant toxicity or side effects.

### What Q-TWiST Provides

- **Partitioned Survival**: Splits survival time into TOX, TWiST, and REL states
- **Quality-Adjusted Outcomes**: Weights each state by utility values (0-1)
- **Comprehensive Comparison**: Compares treatments on quality-adjusted survival, not just quantity
- **Clinical Interpretability**: Provides intuitive metrics for clinical decision-making

## Existing Building Blocks in ClinicoPath

### ✅ Already Available

1. **RMST Analysis** (`R/rmst.b.R`)
   - Restricted Mean Survival Time calculation
   - Essential for calculating time in each Q-TWiST state
   - Group comparisons with confidence intervals
   - Tau selection and sensitivity analysis

2. **Multi-State Survival Models** (`R/flexmultistate.b.R`)
   - Transition probability estimation
   - State occupancy probability calculation
   - Flexible parametric modeling
   - Complex state structure handling

3. **Quality of Life Analysis** (`R/qualityoflife.b.R`)
   - Health utility calculation framework
   - QoL domain scoring
   - Clinical interpretation tools
   - Group comparison methods

4. **Comprehensive Survival Infrastructure**
   - `R/survival.b.R` - Core survival analysis
   - `R/survivalcont.b.R` - Continuous survival
   - `R/pseudosurvival.b.R` - Pseudo-value regression
   - `R/progressionsurvival.b.R` - Progression-free survival
   - `R/simonmakuch.b.R` - Time-dependent analysis

5. **Advanced Plotting Capabilities**
   - State transition diagrams
   - Partitioned survival plots
   - Custom ggplot2 themes
   - Interactive visualization options

### What's Missing for Q-TWiST

- Dedicated Q-TWiST calculation functions
- State partitioning algorithms (TOX, TWiST, REL)
- Utility weight application framework
- Sensitivity analysis for varying utilities
- Q-TWiST-specific visualizations
- Bootstrap confidence intervals for Q-TWiST differences

## Q-TWiST Methodology Overview

### The Three Health States

1. **TOX (Toxicity)**: Time with significant treatment-related toxicity
   - Typically first X months after treatment
   - Grade 3-4 adverse events
   - Utility weight typically 0-0.5

2. **TWiST (Time Without Symptoms or Toxicity)**: Good quality time
   - Time before disease progression/relapse
   - Without significant toxicity
   - Utility weight = 1.0 (reference)

3. **REL (Relapse/Progression)**: Time after disease progression
   - After relapse or progression
   - Symptomatic disease period
   - Utility weight typically 0-0.5

### Q-TWiST Calculation Formula

```
Q-TWiST = μ_TOX × E[TOX] + μ_TWiST × E[TWiST] + μ_REL × E[REL]

Where:
- μ = utility weight (0-1)
- E[·] = expected time in state (calculated from RMST)
```

### Key Mathematical Relationship

```
E[TOX] + E[TWiST] + E[REL] = RMST(τ)

Where τ = analysis time horizon (e.g., 24 months)
```

## Proposed Implementation

### Module Name: `qtwist`

### Required Data Structure

Users need to provide:

1. **Overall Survival Data**
   - `time_os`: Time to death or censoring
   - `event_os`: Death indicator (1 = death, 0 = censored)

2. **Progression-Free Survival Data**
   - `time_pfs`: Time to progression/relapse or death
   - `event_pfs`: Progression/death indicator

3. **Toxicity Data**
   - `tox_days`: Days with grade 3-4 toxicity (or toxicity window end time)
   - OR `tox_window`: Fixed toxicity assessment period (e.g., 90 days)

4. **Treatment Arm**
   - `treatment`: Treatment group indicator (A vs B)

### File Structure

```
ClinicoPathJamoviModule/
├── R/
│   └── qtwist.b.R               # Main implementation
├── jamovi/
│   ├── qtwist.a.yaml            # Analysis options
│   ├── qtwist.u.yaml            # User interface
│   └── qtwist.r.yaml            # Results definitions
├── data-raw/
│   └── qtwist_test_data.R       # Example datasets
└── vignettes/
    └── qtwist_guide.md          # User documentation
```

## Detailed Implementation Specifications

### 1. Analysis Options (qtwist.a.yaml)

```yaml
name: qtwist
title: Q-TWiST Analysis
menuGroup: SurvivalD
menuSubgroup: Quality-Adjusted Survival
menuSubtitle: Quality-adjusted Time Without Symptoms or Toxicity

options:
  # Survival Time Variables
  - name: time_os
    title: Overall Survival Time
    type: Variable
    suggested: [continuous]

  - name: event_os
    title: Overall Survival Event
    type: Variable
    suggested: [ordinal, nominal]

  - name: time_pfs
    title: Progression-Free Survival Time
    type: Variable
    suggested: [continuous]

  - name: event_pfs
    title: PFS Event
    type: Variable
    suggested: [ordinal, nominal]

  # Toxicity Definition
  - name: toxicity_definition
    title: Toxicity Definition Method
    type: List
    options:
      - title: Fixed Window (first X months)
        name: fixed_window
      - title: Individual Toxicity Durations
        name: individual_duration
    default: fixed_window

  - name: toxicity_window
    title: Toxicity Window (months)
    type: Number
    default: 3
    min: 0
    max: 24

  - name: toxicity_duration
    title: Toxicity Duration Variable
    type: Variable
    suggested: [continuous]

  # Treatment Comparison
  - name: treatment
    title: Treatment Variable
    type: Variable
    suggested: [ordinal, nominal]

  # Analysis Parameters
  - name: tau
    title: Time Horizon (τ)
    type: Number
    default: 24
    min: 1
    max: 120
    description: Restricted time horizon in months

  # Utility Weights
  - name: utility_tox
    title: Utility Weight for TOX
    type: Number
    default: 0.5
    min: 0
    max: 1

  - name: utility_twist
    title: Utility Weight for TWiST
    type: Number
    default: 1.0
    min: 0
    max: 1

  - name: utility_rel
    title: Utility Weight for REL
    type: Number
    default: 0.5
    min: 0
    max: 1

  # Sensitivity Analysis
  - name: sensitivity_analysis
    title: Sensitivity Analysis
    type: Bool
    default: true

  - name: utility_range
    title: Utility Range for Sensitivity
    type: String
    default: "0, 0.25, 0.5, 0.75, 1.0"

  # Inference
  - name: confidence_level
    title: Confidence Level
    type: Number
    default: 0.95
    min: 0.80
    max: 0.99

  - name: bootstrap_samples
    title: Bootstrap Samples
    type: Number
    default: 1000
    min: 100
    max: 10000

  # Output Options
  - name: show_state_partitions
    title: Show State Partition Details
    type: Bool
    default: true

  - name: show_qtwist_scores
    title: Show Q-TWiST Scores
    type: Bool
    default: true

  - name: show_treatment_difference
    title: Show Treatment Difference
    type: Bool
    default: true

  - name: show_sensitivity_table
    title: Show Sensitivity Analysis Table
    type: Bool
    default: true

  # Plots
  - name: plot_partitioned_survival
    title: Partitioned Survival Plot
    type: Bool
    default: true

  - name: plot_sensitivity_contour
    title: Sensitivity Contour Plot
    type: Bool
    default: true

  - name: plot_qtwist_comparison
    title: Q-TWiST Comparison Plot
    type: Bool
    default: true
```

### 2. Core Implementation Functions (qtwist.b.R)

```R
#' @title Q-TWiST Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @export

qtwistClass <- R6::R6Class(
    "qtwistClass",
    inherit = qtwistBase,
    private = list(

        # State calculation
        .calculateStateTime = function(data, tau) {
            # Calculate expected time in each state
            # Using RMST methodology

            # 1. Overall Survival RMST
            os_rmst <- private$.calculateRMST(
                data$time_os,
                data$event_os,
                tau
            )

            # 2. PFS RMST (time without progression)
            pfs_rmst <- private$.calculateRMST(
                data$time_pfs,
                data$event_pfs,
                tau
            )

            # 3. Toxicity time
            if (self$options$toxicity_definition == "fixed_window") {
                # Fixed window approach
                tox_window <- self$options$toxicity_window

                # Survival probability in toxicity window
                tox_surv <- private$.calculateRMST(
                    pmin(data$time_os, tox_window),
                    data$event_os,
                    tox_window
                )

                E_TOX <- tox_surv
            } else {
                # Individual toxicity durations
                E_TOX <- mean(pmin(data[[self$options$toxicity_duration]], tau))
            }

            # 4. REL time = OS_RMST - PFS_RMST
            E_REL <- max(0, os_rmst - pfs_rmst)

            # 5. TWiST time = PFS_RMST - TOX
            E_TWIST <- max(0, pfs_rmst - E_TOX)

            # Verify partition
            total <- E_TOX + E_TWIST + E_REL
            if (abs(total - os_rmst) > 0.01) {
                warning("State partition doesn't sum to OS RMST")
            }

            return(list(
                TOX = E_TOX,
                TWIST = E_TWIST,
                REL = E_REL,
                OS_RMST = os_rmst,
                PFS_RMST = pfs_rmst
            ))
        },

        # Calculate RMST
        .calculateRMST = function(time, event, tau) {
            # Fit Kaplan-Meier
            fit <- survival::survfit(survival::Surv(time, event) ~ 1)

            # Restrict to tau
            times <- c(0, fit$time[fit$time <= tau], tau)
            surv <- c(1, fit$surv[fit$time <= tau], tail(fit$surv[fit$time <= tau], 1))

            # Calculate area under curve (RMST)
            rmst <- sum(diff(times) * head(surv, -1))

            return(rmst)
        },

        # Calculate Q-TWiST
        .calculateQTWIST = function(states, utilities) {
            qtwist <- (utilities$tox * states$TOX +
                      utilities$twist * states$TWIST +
                      utilities$rel * states$REL)

            return(qtwist)
        },

        # Treatment comparison
        .compareArms = function(data, tau) {
            # Split by treatment
            arms <- unique(data[[self$options$treatment]])

            if (length(arms) != 2) {
                stop("Q-TWiST comparison requires exactly 2 treatment arms")
            }

            results <- list()

            for (arm in arms) {
                arm_data <- data[data[[self$options$treatment]] == arm, ]

                # Calculate states
                states <- private$.calculateStateTime(arm_data, tau)

                # Calculate Q-TWiST
                utilities <- list(
                    tox = self$options$utility_tox,
                    twist = self$options$utility_twist,
                    rel = self$options$utility_rel
                )

                qtwist <- private$.calculateQTWIST(states, utilities)

                results[[as.character(arm)]] <- list(
                    arm = arm,
                    states = states,
                    qtwist = qtwist,
                    n = nrow(arm_data)
                )
            }

            # Calculate difference
            arms_list <- as.list(arms)
            difference <- results[[as.character(arms_list[[1]])]]$qtwist -
                         results[[as.character(arms_list[[2]])]]$qtwist

            results$difference <- difference
            results$arms <- arms

            return(results)
        },

        # Bootstrap confidence intervals
        .bootstrapQTWIST = function(data, tau, n_boot = 1000) {
            set.seed(123)

            boot_diffs <- numeric(n_boot)

            for (b in 1:n_boot) {
                # Sample with replacement
                boot_indices <- sample(nrow(data), replace = TRUE)
                boot_data <- data[boot_indices, ]

                # Calculate Q-TWiST for bootstrap sample
                boot_results <- private$.compareArms(boot_data, tau)
                boot_diffs[b] <- boot_results$difference
            }

            # Calculate confidence intervals
            ci_level <- self$options$confidence_level
            ci_lower <- quantile(boot_diffs, (1 - ci_level) / 2)
            ci_upper <- quantile(boot_diffs, 1 - (1 - ci_level) / 2)

            return(list(
                mean_diff = mean(boot_diffs),
                se = sd(boot_diffs),
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                boot_diffs = boot_diffs
            ))
        },

        # Sensitivity analysis
        .sensitivityAnalysis = function(data, tau) {
            # Parse utility range
            util_values <- as.numeric(unlist(strsplit(
                self$options$utility_range,
                ","
            )))

            results <- expand.grid(
                utility_tox = util_values,
                utility_rel = util_values
            )

            results$qtwist_diff <- NA

            # Calculate Q-TWiST for each utility combination
            for (i in 1:nrow(results)) {
                # Temporarily override utilities
                temp_util_tox <- self$options$utility_tox
                temp_util_rel <- self$options$utility_rel

                self$options$utility_tox <- results$utility_tox[i]
                self$options$utility_rel <- results$utility_rel[i]

                # Calculate difference
                comp_results <- private$.compareArms(data, tau)
                results$qtwist_diff[i] <- comp_results$difference

                # Restore original utilities
                self$options$utility_tox <- temp_util_tox
                self$options$utility_rel <- temp_util_rel
            }

            return(results)
        },

        # Main run function
        .run = function() {
            # Validate inputs
            if (is.null(self$options$time_os) ||
                is.null(self$options$event_os) ||
                is.null(self$options$time_pfs) ||
                is.null(self$options$event_pfs) ||
                is.null(self$options$treatment)) {
                return()
            }

            # Get data
            data <- self$data

            # Get tau
            tau <- self$options$tau

            # Calculate states and Q-TWiST
            comparison_results <- private$.compareArms(data, tau)

            # Populate state partition table
            if (self$options$show_state_partitions) {
                private$.populateStatePartitionTable(comparison_results)
            }

            # Populate Q-TWiST scores table
            if (self$options$show_qtwist_scores) {
                private$.populateQTWISTScoresTable(comparison_results)
            }

            # Calculate and populate treatment difference
            if (self$options$show_treatment_difference) {
                boot_results <- private$.bootstrapQTWIST(
                    data,
                    tau,
                    self$options$bootstrap_samples
                )
                private$.populateTreatmentDifferenceTable(
                    comparison_results,
                    boot_results
                )
            }

            # Sensitivity analysis
            if (self$options$sensitivity_analysis &&
                self$options$show_sensitivity_table) {
                sens_results <- private$.sensitivityAnalysis(data, tau)
                private$.populateSensitivityTable(sens_results)
            }
        },

        # Plot functions
        .plotPartitionedSurvival = function(image, ggtheme, theme, ...) {
            # Create stacked area plot showing TOX, TWiST, REL over time
            # Implementation similar to economic evaluation plots
        },

        .plotSensitivityContour = function(image, ggtheme, theme, ...) {
            # Create 2D contour plot of Q-TWiST difference
            # across utility weight combinations
        },

        .plotQTWISTComparison = function(image, ggtheme, theme, ...) {
            # Create bar plot comparing Q-TWiST components
            # between treatment arms
        }
    )
)
```

### 3. Results Definitions (qtwist.r.yaml)

```yaml
name: qtwist
title: Q-TWiST Analysis
jrs: '1.1'

items:
  # Welcome/Instructions
  - name: welcomeMessage
    title: Q-TWiST Analysis Overview
    type: Html

  # State Partition Table
  - name: statePartition
    title: Health State Partition (months)
    type: Table
    visible: (show_state_partitions)
    columns:
      - name: Treatment
        title: Treatment
        type: text
      - name: N
        title: N
        type: integer
      - name: TOX
        title: TOX (Toxicity)
        type: number
        format: zto
      - name: TWIST
        title: TWiST
        type: number
        format: zto
      - name: REL
        title: REL (Relapse)
        type: number
        format: zto
      - name: Total
        title: Total RMST
        type: number
        format: zto

  # Q-TWiST Scores Table
  - name: qtwistScores
    title: Quality-Adjusted Survival (Q-TWiST)
    type: Table
    visible: (show_qtwist_scores)
    columns:
      - name: Treatment
        title: Treatment
        type: text
      - name: QTWIST
        title: Q-TWiST (months)
        type: number
        format: zto
      - name: Utility_TOX
        title: μ_TOX
        type: number
        format: zto
      - name: Utility_TWIST
        title: μ_TWiST
        type: number
        format: zto
      - name: Utility_REL
        title: μ_REL
        type: number
        format: zto

  # Treatment Difference
  - name: treatmentDifference
    title: Q-TWiST Treatment Difference
    type: Table
    visible: (show_treatment_difference)
    columns:
      - name: Comparison
        title: Comparison
        type: text
      - name: Difference
        title: Δ Q-TWiST (months)
        type: number
        format: zto
      - name: SE
        title: SE
        type: number
        format: zto
      - name: CI_Lower
        title: 95% CI Lower
        type: number
        format: zto
      - name: CI_Upper
        title: 95% CI Upper
        type: number
        format: zto
      - name: Interpretation
        title: Interpretation
        type: text

  # Sensitivity Analysis
  - name: sensitivityTable
    title: Sensitivity Analysis - Varying Utilities
    type: Table
    visible: (show_sensitivity_table && sensitivity_analysis)
    columns:
      - name: Utility_TOX
        title: μ_TOX
        type: number
        format: zto
      - name: Utility_REL
        title: μ_REL
        type: number
        format: zto
      - name: QTWIST_Diff
        title: Δ Q-TWiST
        type: number
        format: zto
      - name: Favors
        title: Favors
        type: text

  # Plots
  - name: partitionedSurvivalPlot
    title: Partitioned Survival by Health State
    type: Image
    width: 800
    height: 600
    renderFun: .plotPartitionedSurvival
    visible: (plot_partitioned_survival)

  - name: sensitivityContourPlot
    title: Sensitivity Analysis - Utility Contour
    type: Image
    width: 700
    height: 600
    renderFun: .plotSensitivityContour
    visible: (plot_sensitivity_contour && sensitivity_analysis)

  - name: qtwistComparisonPlot
    title: Q-TWiST Component Comparison
    type: Image
    width: 700
    height: 500
    renderFun: .plotQTWISTComparison
    visible: (plot_qtwist_comparison)

  # Methodology Explanation
  - name: methodologyExplanation
    title: Understanding Q-TWiST Methodology
    type: Html
    visible: (showExplanations)

refs:
  - ClinicoPathJamoviModule
  - survival
  - gelber1995
  - glasziou1990
  - qtwist2024
```

## Example Test Dataset

### Generate Q-TWiST Test Data

```R
# data-raw/qtwist_test_data.R

library(dplyr)
library(tibble)

set.seed(42)
n <- 300

qtwist_oncology <- tibble(
  patient_id = 1:n,
  treatment = factor(rep(c("Chemotherapy", "Targeted Therapy"), each = n/2)),
  age = round(rnorm(n, 60, 10)),
  stage = factor(sample(c("II", "III", "IV"), n, replace = TRUE,
                        prob = c(0.3, 0.4, 0.3))),

  # Overall survival (months)
  time_os = pmax(1, rnorm(n,
                         ifelse(treatment == "Targeted Therapy", 28, 22),
                         8)),
  event_os = rbinom(n, 1, 0.7),

  # Progression-free survival (months)
  time_pfs = pmax(1, rnorm(n,
                          ifelse(treatment == "Targeted Therapy", 16, 12),
                          5)),
  event_pfs = rbinom(n, 1, 0.85),

  # Toxicity duration (days with grade 3-4 toxicity in first 3 months)
  toxicity_days = pmax(0, rnorm(n,
                                ifelse(treatment == "Targeted Therapy", 15, 30),
                                10))
) %>%
  # Ensure logical ordering
  mutate(
    time_pfs = pmin(time_pfs, time_os),
    toxicity_months = toxicity_days / 30,

    # Ensure PFS event if OS event
    event_pfs = ifelse(event_os == 1, 1, event_pfs)
  )

# Save dataset
usethis::use_data(qtwist_oncology, overwrite = TRUE)
```

## Implementation Timeline

### Phase 1: Core Functionality (2-3 weeks)
- ✅ State partition calculation (TOX, TWiST, REL)
- ✅ Q-TWiST score calculation
- ✅ Treatment arm comparison
- ✅ Basic tables and output

### Phase 2: Statistical Inference (1-2 weeks)
- ✅ Bootstrap confidence intervals
- ✅ Standard error estimation
- ✅ p-value calculation
- ✅ Treatment difference testing

### Phase 3: Sensitivity Analysis (1 week)
- ✅ Utility weight variation
- ✅ Threshold analysis
- ✅ Sensitivity tables
- ✅ Tornado diagrams

### Phase 4: Visualization (1-2 weeks)
- ✅ Partitioned survival plots (stacked areas)
- ✅ Q-TWiST comparison plots (bar charts)
- ✅ Sensitivity contour plots (2D heatmaps)
- ✅ State transition diagrams

### Phase 5: Documentation & Testing (1 week)
- ✅ User guide and vignettes
- ✅ Example datasets
- ✅ Unit tests
- ✅ Integration tests
- ✅ Clinical interpretation guidance

**Total Estimated Time: 6-9 weeks**

## Clinical Use Cases

### Oncology Trials
**Scenario**: Comparing standard chemotherapy vs. targeted therapy

**Q-TWiST Advantage**: Accounts for:
- Higher toxicity in chemotherapy arm (more TOX time)
- Longer progression-free survival in targeted therapy (more TWiST)
- Similar overall survival but different quality

### Adjuvant Therapy
**Scenario**: Evaluating additional treatment after surgery

**Q-TWiST Advantage**:
- Captures toxicity burden of adjuvant treatment
- Balances survival extension against quality reduction
- Informs shared decision-making

### Immunotherapy Evaluation
**Scenario**: Checkpoint inhibitors with delayed response

**Q-TWiST Advantage**:
- Accounts for immune-related adverse events (TOX)
- Captures durable responses (extended TWiST)
- Handles non-proportional hazards naturally

## Integration with Existing ClinicoPath Modules

### Synergies

1. **Survival Analysis** → Provides OS and PFS curves
2. **RMST Analysis** → Core calculation engine for state times
3. **Multi-State Models** → Alternative approach for complex transitions
4. **Quality of Life** → Utility weight elicitation
5. **Decision Analysis** → Cost-effectiveness integration

### Workflow Example

```
1. Standard Survival Analysis (survival.b.R)
   ↓
2. RMST Calculation (rmst.b.R)
   ↓
3. Q-TWiST Partitioning (qtwist.b.R) ← NEW
   ↓
4. Cost-Effectiveness (decisiongraph.b.R)
```

## R Package Dependencies

### Required Packages
- `survival` - Already used ✅
- `ggplot2` - Already used ✅
- `dplyr` - Already used ✅

### Optional Enhancement Packages
- `survRM2` - Advanced RMST calculations
- `boot` - Non-parametric bootstrap
- `gridExtra` - Multi-panel plots
- `patchwork` - Plot composition

### All packages already in DESCRIPTION ✅

## Alternative: Use survRM2 Package

If full implementation is too resource-intensive, could wrap `survRM2` package:

```R
library(survRM2)

# survRM2 has some Q-TWiST-like functionality
# but not full Q-TWiST implementation
# We would still need custom partition logic
```

**Recommendation**: Build custom implementation for full control and jamovi integration.

## Expected Benefits

### For Users
1. **Comprehensive Treatment Evaluation**: Beyond just survival time
2. **Clinical Decision Support**: Quality-adjusted outcomes
3. **Regulatory Submission**: Increasingly requested by agencies
4. **Publication-Ready**: Standard in oncology trials

### For ClinicoPath
1. **Unique Feature**: Few jamovi modules offer Q-TWiST
2. **Academic Impact**: High citation potential
3. **Clinical Relevance**: Direct patient care impact
4. **Market Position**: Competitive advantage in oncology statistics

## Risk Assessment

### Technical Risks
- **Low**: Core methodology well-established
- **Low**: Required packages already integrated
- **Low**: Building blocks already exist (RMST, multi-state)

### Complexity Risks
- **Medium**: User interface must be intuitive
- **Medium**: Proper handling of edge cases (negative times, etc.)
- **Low**: Documentation needs to be comprehensive

### Maintenance Risks
- **Low**: Stable methodology (published 1980s-1990s)
- **Low**: No dependency on removed CRAN packages
- **Low**: Active use in clinical trials

## Success Criteria

### Functionality
- ✅ Accurate state partition calculation
- ✅ Correct Q-TWiST formula implementation
- ✅ Valid bootstrap confidence intervals
- ✅ Comprehensive sensitivity analysis

### Usability
- ✅ Intuitive variable selection
- ✅ Clear result interpretation
- ✅ Educational explanations
- ✅ Clinical examples

### Quality
- ✅ Match published trial results
- ✅ Validation against R packages
- ✅ Edge case handling
- ✅ Error messages helpful

## Recommendation

### ✅ PROCEED WITH IMPLEMENTATION

**Rationale:**
1. **High Clinical Value**: Addresses important unmet need in oncology trials
2. **Feasible Implementation**: Building blocks already exist
3. **Low Technical Risk**: Well-established methodology
4. **Competitive Advantage**: Unique feature in jamovi ecosystem
5. **Growing Demand**: Increasingly required for trial reporting

**Priority Level**: **HIGH**

This should be implemented as a dedicated module given:
- Complexity warrants standalone analysis
- Natural fit with existing survival infrastructure
- Strong synergies with RMST and multi-state modules
- Clear clinical user base

## References

### Foundational Papers
1. **Gelber RD, Goldhirsch A** (1986). A new endpoint for the assessment of adjuvant therapy in postmenopausal women with operable breast cancer. *Journal of Clinical Oncology*, 4(12):1772-1779.

2. **Gelber RD, Cole BF, Goldhirsch A et al.** (1995). Adjuvant chemotherapy plus tamoxifen compared with tamoxifen alone for postmenopausal breast cancer: meta-analysis of quality-adjusted survival. *Lancet*, 346(8979):1014-1021.

3. **Glasziou PP, Simes RJ, Gelber RD** (1990). Quality adjusted survival analysis. *Statistics in Medicine*, 9(11):1259-1276.

4. **Cole BF, Gelber RD, Goldhirsch A** (2004). Cox regression models for quality adjusted survival analysis. *Statistics in Medicine*, 23(21):3319-3337.

### Modern Applications
5. **Revicki DA, Feeny D, Hunt TL, Cole BF** (2006). Analyzing oncology clinical trial data using the Q-TWiST method: clinical importance and sources of information. *Quality of Life Research*, 15(3):411-423.

6. **Hwang JS, Wang JD** (1999). Integrating health profile with survival for quality of life assessment. *Quality of Life Research*, 8(1-2):1-7.

---

## Appendix: Code Snippets

### Basic Q-TWiST Calculation (Standalone R)

```R
# Simple Q-TWiST calculation function
calculate_qtwist_simple <- function(time_os, event_os,
                                   time_pfs, event_pfs,
                                   tox_window = 3,
                                   tau = 24,
                                   u_tox = 0.5, u_twist = 1.0, u_rel = 0.5) {

  # Calculate RMST for OS
  fit_os <- survfit(Surv(time_os, event_os) ~ 1)
  rmst_os <- sum(diff(c(0, fit_os$time[fit_os$time <= tau], tau)) *
                c(1, fit_os$surv[fit_os$time <= tau]))

  # Calculate RMST for PFS
  fit_pfs <- survfit(Surv(time_pfs, event_pfs) ~ 1)
  rmst_pfs <- sum(diff(c(0, fit_pfs$time[fit_pfs$time <= tau], tau)) *
                 c(1, fit_pfs$surv[fit_pfs$time <= tau]))

  # Calculate TOX time (simplified: fixed window)
  fit_tox <- survfit(Surv(pmin(time_os, tox_window), event_os) ~ 1)
  rmst_tox <- sum(diff(c(0, fit_tox$time, tox_window)) *
                 c(1, fit_tox$surv))

  # Partition states
  E_TOX <- rmst_tox
  E_REL <- rmst_os - rmst_pfs
  E_TWIST <- rmst_pfs - E_TOX

  # Calculate Q-TWiST
  qtwist <- u_tox * E_TOX + u_twist * E_TWIST + u_rel * E_REL

  return(list(
    TOX = E_TOX,
    TWIST = E_TWIST,
    REL = E_REL,
    QTWIST = qtwist
  ))
}
```

---

*Document prepared: 2025-11-03*
*Status: Implementation Plan - Ready for Development*
*Priority: HIGH - High Clinical Value, Low Technical Risk*
