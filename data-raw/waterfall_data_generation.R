# Load necessary packages
library(dplyr)

# -----------------------------
# Test Dataset 1: Percentage Data
# -----------------------------
# This dataset uses pre-calculated percentage changes for each patient.
data_percentage <- data.frame(
    PatientID = paste0("PT", 1:10),
    Response = c(-100, -45, -30, -20, -10, 0, 10, 20, 30, 40)
)
cat("=== Percentage Data Test ===\n")
print(data_percentage)

write.csv(data_percentage, "./data/data_percentage.csv", row.names = FALSE)

# Example usage with the waterfall function:
# ClinicoPath::waterfall(
#     data = data_percentage,
#     patientID = "PatientID",
#     responseVar = "Response",
#     inputType = "percentage"
# )

# -----------------------------
# Test Dataset 2: Raw Measurements Data
# -----------------------------
# This dataset contains raw tumor measurements with a time variable.
# The waterfall function will compute the percentage change from the baseline (time = 0).
data_raw <- data.frame(
    PatientID = rep(paste0("PT", 1:5), each = 3),
    Time = rep(c(0, 2, 4), times = 5),
    Measurement = c(
        50, 40, 30,   # PT1: Baseline = 50
        60, 45, 35,   # PT2: Baseline = 60
        55, 50, 45,   # PT3: Baseline = 55
        70, 60, 55,   # PT4: Baseline = 70
        65, 55, 50    # PT5: Baseline = 65
    )
)
cat("\n=== Raw Measurements Data Test ===\n")
print(data_raw)

write.csv(data_raw, "./data/data_raw.csv", row.names = FALSE)


# Example usage with the waterfall function:
# ClinicoPath::waterfall(
#     data = data_raw,
#     patientID = "PatientID",
#     responseVar = "Measurement",
#     timeVar = "Time",
#     inputType = "raw"
# )

# -----------------------------
# Test Dataset 3: Data with Subgroup Variable
# -----------------------------
# This dataset includes a subgroup (e.g., treatment group) for subgroup analysis.
data_subgroup <- data.frame(
    PatientID = paste0("PT", 1:12),
    Response = c(-100, -60, -35, -20, -15, 0, 5, 10, 25, 40, 50, 60),
    Group = rep(c("A", "B", "C"), each = 4)
)
cat("\n=== Subgroup Analysis Data Test ===\n")
print(data_subgroup)

write.csv(data_subgroup, "./data/data_subgroup.csv", row.names = FALSE)


# Example usage with subgroup analysis:
# ClinicoPath::waterfall(
#     data = data_subgroup,
#     patientID = "PatientID",
#     responseVar = "Response",
#     inputType = "percentage",
#     subgroupVar = "Group"
# )

# -----------------------------
# Test Dataset 4: Simulated Longitudinal Data
# -----------------------------
# This dataset simulates longitudinal raw measurements for multiple patients.
# It can be used to test additional features like duration of response and time to response.
set.seed(123)
patientIDs <- paste0("PT", 1:20)
data_longitudinal <- do.call(rbind, lapply(patientIDs, function(id) {
    times <- seq(0, 12, by = 2)
    # Simulate a baseline measurement and subsequent values with random fluctuation
    baseline <- sample(40:80, 1)
    measurements <- baseline * runif(length(times), 0.5, 1.2)
    data.frame(
        PatientID = id,
        Time = times,
        Measurement = round(measurements, 1)
    )
}))
cat("\n=== Simulated Longitudinal Data Test ===\n")
print(head(data_longitudinal, 10))  # Display first 10 rows for brevity

write.csv(data_longitudinal, "./data/data_longitudinal.csv", row.names = FALSE)


# Example usage with additional metrics enabled:
# ClinicoPath::waterfall(
#     data = data_longitudinal,
#     patientID = "PatientID",
#     responseVar = "Measurement",
#     timeVar = "Time",
#     inputType = "raw",
#     calculateDuration = TRUE,
#     calculateTimeToResponse = TRUE
# )
