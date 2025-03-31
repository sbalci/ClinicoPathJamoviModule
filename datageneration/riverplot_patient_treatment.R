# Create a sample dataset for river plot testing
# This represents patient treatment pathways over time

# Set seed for reproducibility
set.seed(123)

# Number of patients
n_patients <- 500

# Create patient IDs
patient_ids <- paste0("P", 1:n_patients)

# Create timepoints
timepoints <- c("Initial", "Month3", "Month6", "Month12")

# Create diagnoses categories
diagnoses <- c("Mild", "Moderate", "Severe")

# Create treatment categories
treatments <- c("Medication A", "Medication B", "Combination", "Surgery", "Watchful Waiting")

# Create outcome categories
outcomes <- c("Improved", "Stable", "Worsened", "Discontinued")

# Function to generate treatment pathways with some realistic patterns
generate_pathway <- function(initial_diagnosis) {
    # Initial treatment depends on diagnosis severity
    if (initial_diagnosis == "Mild") {
        initial_treatment <- sample(c("Medication A", "Watchful Waiting"), 1,
                                    prob = c(0.7, 0.3))
    } else if (initial_diagnosis == "Moderate") {
        initial_treatment <- sample(c("Medication A", "Medication B", "Combination"), 1,
                                    prob = c(0.4, 0.4, 0.2))
    } else { # Severe
        initial_treatment <- sample(c("Medication B", "Combination", "Surgery"), 1,
                                    prob = c(0.3, 0.4, 0.3))
    }

    # Month 3 outcome depends on initial diagnosis and treatment
    if (initial_diagnosis == "Mild" && initial_treatment == "Watchful Waiting") {
        month3_outcome <- sample(c("Improved", "Stable", "Worsened"), 1,
                                 prob = c(0.2, 0.6, 0.2))
    } else if (initial_diagnosis == "Severe" && initial_treatment == "Surgery") {
        month3_outcome <- sample(c("Improved", "Stable", "Worsened", "Discontinued"), 1,
                                 prob = c(0.5, 0.3, 0.1, 0.1))
    } else {
        month3_outcome <- sample(c("Improved", "Stable", "Worsened", "Discontinued"), 1,
                                 prob = c(0.3, 0.4, 0.2, 0.1))
    }

    # Month 6 outcome shows some progression from month 3
    if (month3_outcome == "Improved") {
        month6_outcome <- sample(c("Improved", "Stable"), 1, prob = c(0.8, 0.2))
    } else if (month3_outcome == "Stable") {
        month6_outcome <- sample(c("Improved", "Stable", "Worsened"), 1, prob = c(0.3, 0.5, 0.2))
    } else if (month3_outcome == "Worsened") {
        month6_outcome <- sample(c("Improved", "Stable", "Worsened", "Discontinued"), 1,
                                 prob = c(0.1, 0.3, 0.4, 0.2))
    } else { # Discontinued
        month6_outcome <- "Discontinued"
    }

    # Month 12 is the final outcome
    if (month6_outcome == "Improved") {
        month12_outcome <- sample(c("Improved", "Stable"), 1, prob = c(0.9, 0.1))
    } else if (month6_outcome == "Stable") {
        month12_outcome <- sample(c("Improved", "Stable", "Worsened"), 1, prob = c(0.3, 0.6, 0.1))
    } else if (month6_outcome == "Worsened") {
        month12_outcome <- sample(c("Improved", "Stable", "Worsened", "Discontinued"), 1,
                                  prob = c(0.1, 0.2, 0.6, 0.1))
    } else { # Discontinued
        month12_outcome <- "Discontinued"
    }

    return(c(initial_treatment, month3_outcome, month6_outcome, month12_outcome))
}

# Generate data
patient_data <- data.frame(
    PatientID = rep(patient_ids, each = 4),
    TimePoint = rep(timepoints, times = n_patients),
    Age = rep(sample(30:80, n_patients, replace = TRUE), each = 4),
    Gender = rep(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.48, 0.52)), each = 4)
)

# Add diagnosis - stays constant per patient
patient_data$Diagnosis <- rep(
    sample(diagnoses, n_patients, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    each = 4
)

# Generate treatment pathways
pathways <- list()
for (i in 1:n_patients) {
    pathways[[i]] <- generate_pathway(patient_data$Diagnosis[(i-1)*4 + 1])
}

# Add treatment status
patient_data$TreatmentStatus <- unlist(pathways)

# Add a cost variable (numeric) that can be used as a weight
# Cost depends on diagnosis severity and treatment
patient_data$Cost <- 0
for (i in 1:nrow(patient_data)) {
    base_cost <- switch(patient_data$Diagnosis[i],
                        "Mild" = 100,
                        "Moderate" = 300,
                        "Severe" = 700)

    treatment_multiplier <- switch(patient_data$TreatmentStatus[i],
                                   "Medication A" = 1.2,
                                   "Medication B" = 1.5,
                                   "Combination" = 2.0,
                                   "Surgery" = 5.0,
                                   "Watchful Waiting" = 0.5,
                                   "Improved" = 0.8,
                                   "Stable" = 1.0,
                                   "Worsened" = 1.5,
                                   "Discontinued" = 0.3)

    patient_data$Cost[i] <- base_cost * treatment_multiplier * (1 + runif(1, -0.1, 0.1))
}

# Create alternate format dataset (wide format) for multi-strata visualization
patient_wide <- data.frame(
    PatientID = patient_ids,
    Age = patient_data$Age[seq(1, nrow(patient_data), by=4)],
    Gender = patient_data$Gender[seq(1, nrow(patient_data), by=4)],
    Diagnosis = patient_data$Diagnosis[seq(1, nrow(patient_data), by=4)],
    Initial = sapply(pathways, function(x) x[1]),
    Month3 = sapply(pathways, function(x) x[2]),
    Month6 = sapply(pathways, function(x) x[3]),
    Month12 = sapply(pathways, function(x) x[4]),
    TotalCost = sapply(1:n_patients, function(i) sum(patient_data$Cost[((i-1)*4+1):(i*4)]))
)

# Write both datasets to CSV files
write.csv(patient_data, "./data/patient_treatment_long.csv", row.names = FALSE)
write.csv(patient_wide, "./data/patient_treatment_wide.csv", row.names = FALSE)
