# ===================================================================
# TNM STAGE MIGRATION TEST DATA GENERATOR
# ===================================================================
#
# Generates comprehensive realistic datasets for testing the advanced
# TNM stage migration analysis function. Creates multiple scenarios
# representing different cancer types and staging transitions.
#
# Author: Advanced TNM Stage Migration Analysis Function
# Purpose: Testing stagemigration function with realistic clinical data
# ===================================================================

library(survival)
set.seed(42)  # Reproducible results

# ===================================================================
# HELPER FUNCTIONS
# ===================================================================

#' Generate TNM stages with realistic distribution
#' @param n Number of patients
#' @param stage_type Type of staging system ("7th", "8th", "custom")
#' @param cancer_type Cancer type affecting stage distribution
generate_tnm_stages <- function(n, stage_type = "7th", cancer_type = "lung") {

    # Define stage probabilities by cancer type and staging system
    stage_probs <- list(
        "7th" = list(
            "lung" = c("I" = 0.15, "II" = 0.20, "III" = 0.35, "IV" = 0.30),
            "breast" = c("I" = 0.25, "II" = 0.40, "III" = 0.25, "IV" = 0.10),
            "colon" = c("I" = 0.20, "II" = 0.30, "III" = 0.35, "IV" = 0.15)
        ),
        "8th" = list(
            "lung" = c("IA" = 0.08, "IB" = 0.12, "IIA" = 0.10, "IIB" = 0.15,
                      "IIIA" = 0.20, "IIIB" = 0.10, "IIIC" = 0.05, "IV" = 0.20),
            "breast" = c("IA" = 0.15, "IB" = 0.15, "IIA" = 0.20, "IIB" = 0.20,
                        "IIIA" = 0.15, "IIIB" = 0.05, "IIIC" = 0.05, "IV" = 0.05),
            "colon" = c("I" = 0.20, "IIA" = 0.15, "IIB" = 0.15, "IIC" = 0.05,
                       "IIIA" = 0.15, "IIIB" = 0.15, "IIIC" = 0.05, "IV" = 0.10)
        )
    )

    probs <- stage_probs[[stage_type]][[cancer_type]]
    if (is.null(probs)) probs <- stage_probs[["7th"]][["lung"]]

    sample(names(probs), n, replace = TRUE, prob = probs)
}

#' Generate realistic survival times based on stage
#' @param stages Vector of TNM stages
#' @param cancer_type Cancer type affecting survival
generate_survival_times <- function(stages, cancer_type = "lung") {

    # Median survival by stage (months)
    median_survival <- list(
        "lung" = list("I" = 60, "IA" = 70, "IB" = 55, "II" = 36, "IIA" = 40, "IIB" = 32,
                     "III" = 18, "IIIA" = 20, "IIIB" = 16, "IIIC" = 14, "IV" = 8),
        "breast" = list("I" = 120, "IA" = 130, "IB" = 120, "II" = 96, "IIA" = 100, "IIB" = 90,
                       "III" = 60, "IIIA" = 65, "IIIB" = 55, "IIIC" = 45, "IV" = 24),
        "colon" = list("I" = 100, "II" = 80, "IIA" = 85, "IIB" = 75, "IIC" = 70,
                      "III" = 48, "IIIA" = 55, "IIIB" = 45, "IIIC" = 40, "IV" = 20)
    )

    surv_params <- median_survival[[cancer_type]]
    if (is.null(surv_params)) surv_params <- median_survival[["lung"]]

    # Generate survival times using exponential distribution
    survival_times <- numeric(length(stages))
    for (i in seq_along(stages)) {
        stage <- stages[i]
        median_time <- surv_params[[stage]]
        if (is.null(median_time)) median_time <- 24  # Default

        # Convert median to rate parameter for exponential distribution
        rate <- log(2) / median_time
        survival_times[i] <- rexp(1, rate)
    }

    # Add some random variation
    survival_times * rnorm(length(stages), mean = 1, sd = 0.2)
}

#' Generate event indicators based on survival time
#' @param survival_times Vector of survival times
#' @param followup_time Maximum follow-up time
generate_events <- function(survival_times, followup_time = 60) {
    # Event occurs if survival time is less than follow-up time
    events <- ifelse(survival_times <= followup_time, 1, 0)

    # Censor some events randomly (lost to follow-up)
    censoring_rate <- 0.15
    censor_indices <- sample(seq_along(events),
                           size = floor(length(events) * censoring_rate))
    events[censor_indices] <- 0

    # Adjust survival times for censored cases
    survival_times[events == 0] <- pmin(survival_times[events == 0],
                                       runif(sum(events == 0),
                                            min = 6, max = followup_time))

    list(times = pmax(survival_times, 0.5), events = events)
}

#' Create realistic stage migration patterns
#' @param old_stages Old staging system stages
#' @param cancer_type Cancer type
create_stage_migration <- function(old_stages, cancer_type = "lung") {

    # Define migration patterns (old -> new probabilities)
    migration_patterns <- list(
        "lung" = list(
            "I" = list("IA" = 0.60, "IB" = 0.35, "IIA" = 0.05),
            "II" = list("IB" = 0.10, "IIA" = 0.40, "IIB" = 0.45, "IIIA" = 0.05),
            "III" = list("IIB" = 0.05, "IIIA" = 0.50, "IIIB" = 0.35, "IIIC" = 0.10),
            "IV" = list("IIIC" = 0.05, "IV" = 0.95)
        ),
        "breast" = list(
            "I" = list("IA" = 0.55, "IB" = 0.40, "IIA" = 0.05),
            "II" = list("IB" = 0.10, "IIA" = 0.45, "IIB" = 0.40, "IIIA" = 0.05),
            "III" = list("IIB" = 0.05, "IIIA" = 0.60, "IIIB" = 0.25, "IIIC" = 0.10),
            "IV" = list("IIIC" = 0.10, "IV" = 0.90)
        ),
        "colon" = list(
            "I" = list("I" = 0.95, "IIA" = 0.05),
            "II" = list("I" = 0.05, "IIA" = 0.50, "IIB" = 0.40, "IIC" = 0.05),
            "III" = list("IIB" = 0.05, "IIIA" = 0.45, "IIIB" = 0.40, "IIIC" = 0.10),
            "IV" = list("IIIC" = 0.05, "IV" = 0.95)
        )
    )

    patterns <- migration_patterns[[cancer_type]]
    if (is.null(patterns)) patterns <- migration_patterns[["lung"]]

    new_stages <- character(length(old_stages))

    for (i in seq_along(old_stages)) {
        old_stage <- old_stages[i]
        migration_probs <- patterns[[old_stage]]

        if (is.null(migration_probs)) {
            # Default: no migration
            new_stages[i] <- old_stage
        } else {
            new_stages[i] <- sample(names(migration_probs), 1, prob = unlist(migration_probs))
        }
    }

    new_stages
}

#' Generate clinical covariates
#' @param n Number of patients
#' @param cancer_type Cancer type
generate_covariates <- function(n, cancer_type = "lung") {

    # Age distribution by cancer type
    age_params <- list(
        "lung" = list(mean = 65, sd = 12),
        "breast" = list(mean = 58, sd = 14),
        "colon" = list(mean = 62, sd = 16)
    )

    age_param <- age_params[[cancer_type]]
    if (is.null(age_param)) age_param <- age_params[["lung"]]

    data.frame(
        age = pmax(pmin(rnorm(n, age_param$mean, age_param$sd), 90), 18),
        gender = sample(c("Male", "Female"), n, replace = TRUE,
                       prob = if(cancer_type == "breast") c(0.01, 0.99) else c(0.55, 0.45)),
        smoking_status = if(cancer_type == "lung") {
            sample(c("Never", "Former", "Current"), n, replace = TRUE,
                  prob = c(0.20, 0.45, 0.35))
        } else {
            sample(c("Never", "Former", "Current"), n, replace = TRUE,
                  prob = c(0.60, 0.30, 0.10))
        },
        performance_status = sample(0:2, n, replace = TRUE, prob = c(0.60, 0.30, 0.10)),
        comorbidity_score = rpois(n, lambda = if(cancer_type == "lung") 1.5 else 1.0)
    )
}

# ===================================================================
# MAIN DATA GENERATION FUNCTIONS
# ===================================================================

#' Generate comprehensive TNM stage migration dataset
#' @param n Number of patients
#' @param cancer_type Cancer type ("lung", "breast", "colon")
#' @param followup_months Maximum follow-up time in months
#' @param migration_scenario Migration scenario ("typical", "major_revision", "minor_update")
generate_stagemigration_data <- function(n = 500,
                                        cancer_type = "lung",
                                        followup_months = 60,
                                        migration_scenario = "typical") {

    message(paste("Generating", n, "patient", cancer_type, "cancer staging migration dataset..."))

    # Generate patient IDs
    patient_ids <- sprintf("PT_%s_%04d", toupper(substr(cancer_type, 1, 1)), seq_len(n))

    # Generate old staging system (7th edition)
    old_stages <- generate_tnm_stages(n, "7th", cancer_type)

    # Create new stages based on migration scenario
    if (migration_scenario == "major_revision") {
        # More complex migration patterns
        new_stages <- create_stage_migration(old_stages, cancer_type)
    } else if (migration_scenario == "minor_update") {
        # Limited migration (mostly same stages)
        migration_rate <- 0.20
        change_indices <- sample(seq_len(n), floor(n * migration_rate))
        new_stages <- old_stages
        new_stages[change_indices] <- create_stage_migration(old_stages[change_indices], cancer_type)
    } else {
        # Typical migration patterns
        new_stages <- create_stage_migration(old_stages, cancer_type)
    }

    # Generate survival data based on new staging (more prognostic)
    survival_data <- generate_events(
        generate_survival_times(new_stages, cancer_type),
        followup_months
    )

    # Generate clinical covariates
    covariates <- generate_covariates(n, cancer_type)

    # Combine all data
    dataset <- data.frame(
        PatientID = patient_ids,
        OldTNMStage = old_stages,
        NewTNMStage = new_stages,
        SurvivalMonths = round(survival_data$times, 1),
        DeathEvent = survival_data$events,
        Age = round(covariates$age),
        Gender = covariates$gender,
        SmokingStatus = covariates$smoking_status,
        PerformanceStatus = covariates$performance_status,
        ComorbidityScore = covariates$comorbidity_score,
        CancerType = cancer_type,
        Dataset = paste0(cancer_type, "_", migration_scenario),
        stringsAsFactors = FALSE
    )

    # Add some derived variables for testing advanced features
    dataset$AgeBinary <- ifelse(dataset$Age >= 65, "≥65", "<65")
    dataset$HighRisk <- as.numeric((dataset$PerformanceStatus >= 1) |
                                  (dataset$ComorbidityScore >= 2) |
                                  (dataset$Age >= 70))

    # Add some missingness for robustness testing
    missing_rate <- 0.05
    for (col in c("SmokingStatus", "PerformanceStatus")) {
        missing_indices <- sample(seq_len(n), floor(n * missing_rate))
        dataset[[col]][missing_indices] <- NA
    }

    message(paste("Generated dataset with", nrow(dataset), "patients"))
    message(paste("Event rate:", round(mean(dataset$DeathEvent) * 100, 1), "%"))
    message(paste("Migration rate:", round(mean(dataset$OldTNMStage != dataset$NewTNMStage) * 100, 1), "%"))

    return(dataset)
}

# ===================================================================
# GENERATE MULTIPLE TEST DATASETS
# ===================================================================

message("=== Generating TNM Stage Migration Test Datasets ===")

# 1. Primary lung cancer dataset (comprehensive)
lung_data <- generate_stagemigration_data(
    n = 500,
    cancer_type = "lung",
    followup_months = 60,
    migration_scenario = "typical"
)

# 2. Breast cancer dataset (major revision scenario)
breast_data <- generate_stagemigration_data(
    n = 350,
    cancer_type = "breast",
    followup_months = 120,
    migration_scenario = "major_revision"
)

# 3. Colon cancer dataset (minor update scenario)
colon_data <- generate_stagemigration_data(
    n = 300,
    cancer_type = "colon",
    followup_months = 80,
    migration_scenario = "minor_update"
)

# 4. Small dataset for basic testing
small_data <- generate_stagemigration_data(
    n = 100,
    cancer_type = "lung",
    followup_months = 48,
    migration_scenario = "typical"
)

# 5. Large dataset for performance testing
large_data <- generate_stagemigration_data(
    n = 1000,
    cancer_type = "lung",
    followup_months = 60,
    migration_scenario = "major_revision"
)

# ===================================================================
# SAVE DATASETS
# ===================================================================

# Save individual datasets
write.csv(lung_data, "data/stagemigration_lung_data.csv", row.names = FALSE)
write.csv(breast_data, "data/stagemigration_breast_data.csv", row.names = FALSE)
write.csv(colon_data, "data/stagemigration_colon_data.csv", row.names = FALSE)
write.csv(small_data, "data/stagemigration_small_test.csv", row.names = FALSE)
write.csv(large_data, "data/stagemigration_large_test.csv", row.names = FALSE)

# Create comprehensive combined dataset
combined_data <- rbind(
    lung_data,
    breast_data,
    colon_data
)

write.csv(combined_data, "data/stagemigration_comprehensive_data.csv", row.names = FALSE)

# ===================================================================
# GENERATE DATASET DOCUMENTATION
# ===================================================================

documentation <- paste0("
# TNM Stage Migration Analysis Test Datasets

This directory contains comprehensive test datasets for the Advanced TNM Stage Migration Analysis function.

## Datasets

### 1. stagemigration_lung_data.csv (N=500)
- **Purpose**: Primary test dataset for lung cancer staging migration
- **Migration Pattern**: Typical 7th to 8th edition TNM changes
- **Follow-up**: 60 months
- **Event Rate**: ", round(mean(lung_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(lung_data$OldTNMStage != lung_data$NewTNMStage) * 100, 1), "%

### 2. stagemigration_breast_data.csv (N=350)
- **Purpose**: Breast cancer with major staging revision
- **Migration Pattern**: Major revision scenario
- **Follow-up**: 120 months
- **Event Rate**: ", round(mean(breast_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(breast_data$OldTNMStage != breast_data$NewTNMStage) * 100, 1), "%

### 3. stagemigration_colon_data.csv (N=300)
- **Purpose**: Colon cancer with minor staging updates
- **Migration Pattern**: Minor update scenario
- **Follow-up**: 80 months
- **Event Rate**: ", round(mean(colon_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(colon_data$OldTNMStage != colon_data$NewTNMStage) * 100, 1), "%

### 4. stagemigration_small_test.csv (N=100)
- **Purpose**: Small dataset for basic functionality testing
- **Migration Pattern**: Typical
- **Follow-up**: 48 months
- **Event Rate**: ", round(mean(small_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(small_data$OldTNMStage != small_data$NewTNMStage) * 100, 1), "%

### 5. stagemigration_large_test.csv (N=1000)
- **Purpose**: Large dataset for performance testing
- **Migration Pattern**: Major revision
- **Follow-up**: 60 months
- **Event Rate**: ", round(mean(large_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(large_data$OldTNMStage != large_data$NewTNMStage) * 100, 1), "%

### 6. stagemigration_comprehensive_data.csv (N=", nrow(combined_data), ")
- **Purpose**: Combined dataset with all cancer types
- **Migration Pattern**: Mixed scenarios
- **Event Rate**: ", round(mean(combined_data$DeathEvent) * 100, 1), "%
- **Migration Rate**: ", round(mean(combined_data$OldTNMStage != combined_data$NewTNMStage) * 100, 1), "%

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

Generated on: ", Sys.Date(), "
Generator version: 1.0
")

writeLines(documentation, "data/README_STAGEMIGRATION.md")

# ===================================================================
# SUMMARY REPORT
# ===================================================================

cat("\n=== TNM Stage Migration Test Data Generation Complete ===\n")
cat("Generated", length(list.files("data", pattern = "stagemigration.*\\.csv")), "CSV datasets\n")
cat("Total patients:", nrow(combined_data), "\n")
cat("Cancer types:", length(unique(combined_data$CancerType)), "\n")
cat("Staging scenarios:", length(unique(combined_data$Dataset)), "\n")
cat("Documentation saved to: data/README_STAGEMIGRATION.md\n")
cat("=========================================================\n")