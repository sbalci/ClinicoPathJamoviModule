# Generate medical research test data for groupsummary function
# This creates a clinical study dataset with patient data over time

set.seed(456)  # For reproducibility

# Number of patients
n_patients <- 200
# Number of visits per patient (varies)
visits_per_patient <- sample(3:6, n_patients, replace = TRUE)
n_total <- sum(visits_per_patient)

# Generate patient IDs
patient_ids <- rep(paste0("PT", sprintf("%03d", 1:n_patients)), times = visits_per_patient)

# Create base medical dataset
medical_data <- data.frame(
    # Patient identifiers
    PatientID = patient_ids,

    # Visit information
    VisitNumber = unlist(lapply(visits_per_patient, function(x) 1:x)),

    # Dates - visits spread over 2 years
    VisitDate = as.Date("2022-01-01") +
        unlist(lapply(visits_per_patient, function(x) cumsum(c(0, sample(30:120, x-1))))),

    # Study center (multi-center trial)
    StudyCenter = factor(rep(
        sample(c("Boston Medical Center", "Johns Hopkins", "Mayo Clinic",
                 "Cleveland Clinic", "UCLA Medical"), n_patients, replace = TRUE),
        times = visits_per_patient
    )),

    # Demographics (constant per patient)
    AgeGroup = factor(rep(
        sample(c("18-30", "31-45", "46-60", "61-75", ">75"), n_patients,
               replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.10)),
        times = visits_per_patient
    ), levels = c("18-30", "31-45", "46-60", "61-75", ">75"), ordered = TRUE),

    Gender = factor(rep(
        sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.48, 0.52)),
        times = visits_per_patient
    )),

    # Clinical grouping variables
    TreatmentGroup = factor(rep(
        sample(c("Control", "Treatment A", "Treatment B", "Treatment A+B"), n_patients,
               replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
        times = visits_per_patient
    )),

    DiagnosisPrimary = factor(rep(
        sample(c("Hypertension", "Diabetes Type 2", "Coronary Artery Disease",
                 "Chronic Kidney Disease", "Heart Failure"), n_patients,
               replace = TRUE, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
        times = visits_per_patient
    )),

    DiseaseStage = factor(rep(
        sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n_patients,
               replace = TRUE, prob = c(0.20, 0.35, 0.30, 0.15)),
        times = visits_per_patient
    ), levels = c("Stage I", "Stage II", "Stage III", "Stage IV"), ordered = TRUE),

    # Comorbidities
    ComorbidityCount = rep(
        sample(0:5, n_patients, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.15, 0.05)),
        times = visits_per_patient
    ),

    # BMI category
    BMICategory = factor(rep(
        sample(c("Underweight", "Normal", "Overweight", "Obese"), n_patients,
               replace = TRUE, prob = c(0.05, 0.35, 0.35, 0.25)),
        times = visits_per_patient
    ), levels = c("Underweight", "Normal", "Overweight", "Obese"), ordered = TRUE),

    stringsAsFactors = FALSE
)

# Add numeric clinical measurements (these vary by visit)
n_rows <- nrow(medical_data)

# Vital signs
medical_data$SystolicBP <- round(rnorm(n_rows,
                                       mean = 130 + (medical_data$DiseaseStage == "Stage IV") * 15,
                                       sd = 15), 0)
medical_data$DiastolicBP <- round(medical_data$SystolicBP * 0.6 + rnorm(n_rows, 20, 5), 0)
medical_data$HeartRate <- round(rnorm(n_rows, mean = 75, sd = 12), 0)
medical_data$Temperature <- round(rnorm(n_rows, mean = 37.0, sd = 0.5), 1)
medical_data$RespiratoryRate <- round(rnorm(n_rows, mean = 16, sd = 3), 0)
medical_data$OxygenSaturation <- pmin(100, round(rnorm(n_rows, mean = 97, sd = 2), 0))

# Laboratory values
medical_data$HbA1c <- round(rnorm(n_rows,
                                  mean = 6.5 + (medical_data$DiagnosisPrimary == "Diabetes Type 2") * 1.5,
                                  sd = 1.2), 1)
medical_data$Creatinine <- round(rnorm(n_rows,
                                       mean = 1.0 + (medical_data$DiagnosisPrimary == "Chronic Kidney Disease") * 0.8,
                                       sd = 0.3), 2)
medical_data$eGFR <- round(pmax(15, 120 - medical_data$Creatinine * 30 + rnorm(n_rows, 0, 10)), 0)
medical_data$Hemoglobin <- round(rnorm(n_rows, mean = 13.5, sd = 1.5), 1)
medical_data$WBC <- round(rnorm(n_rows, mean = 7.5, sd = 2.0), 1)
medical_data$Platelets <- round(rnorm(n_rows, mean = 250, sd = 50), 0)

# Cholesterol panel
medical_data$TotalCholesterol <- round(rnorm(n_rows, mean = 200, sd = 40), 0)
medical_data$LDLCholesterol <- round(medical_data$TotalCholesterol * 0.6 + rnorm(n_rows, 0, 20), 0)
medical_data$HDLCholesterol <- round(rnorm(n_rows, mean = 50, sd = 15), 0)
medical_data$Triglycerides <- round(rnorm(n_rows, mean = 150, sd = 50), 0)

# Patient reported outcomes
medical_data$PainScore <- pmin(10, pmax(0, round(rnorm(n_rows,
                                                       mean = 4 + (medical_data$DiseaseStage == "Stage IV") * 2,
                                                       sd = 2), 0)))
medical_data$QualityOfLife <- pmin(100, pmax(0, round(rnorm(n_rows, mean = 70, sd = 15), 0)))
medical_data$FunctionalStatus <- pmin(100, pmax(0, round(rnorm(n_rows, mean = 75, sd = 20), 0)))

# Medication adherence
medical_data$MedicationAdherence <- pmin(100, pmax(0, round(rnorm(n_rows, mean = 85, sd = 15), 0)))

# Treatment response (improves over visits for treatment groups)
treatment_idx <- which(medical_data$TreatmentGroup != "Control" & !is.na(medical_data$TreatmentGroup))
treatment_effect <- numeric(n_rows)
treatment_effect[treatment_idx] <- (medical_data$VisitNumber[treatment_idx] - 1) * 5
medical_data$ClinicalScore <- pmin(100, pmax(0,
                                             round(50 + treatment_effect + rnorm(n_rows, 0, 10), 0)))

# Add treatment-specific improvements (handling NA values)
treatment_bp_idx <- which(medical_data$TreatmentGroup != "Control" & !is.na(medical_data$TreatmentGroup))
medical_data$SystolicBP[treatment_bp_idx] <-
    medical_data$SystolicBP[treatment_bp_idx] -
    (medical_data$VisitNumber[treatment_bp_idx] - 1) * 2

# Add some missing values (realistic pattern)
# More missing in later visits
missing_prob <- 0.05 + (medical_data$VisitNumber - 1) * 0.02

# Lab values have more missing data
lab_vars <- c("HbA1c", "Creatinine", "eGFR", "Hemoglobin", "WBC", "Platelets",
              "TotalCholesterol", "LDLCholesterol", "HDLCholesterol", "Triglycerides")
for (var in lab_vars) {
    missing_idx <- runif(n_rows) < (missing_prob + 0.05)
    medical_data[[var]][missing_idx] <- NA
}

# Some missing in patient reported outcomes
pro_vars <- c("PainScore", "QualityOfLife", "FunctionalStatus")
for (var in pro_vars) {
    missing_idx <- runif(n_rows) < missing_prob
    medical_data[[var]][missing_idx] <- NA
}

# Create admission date/time data for emergency subset
emergency_subset <- medical_data[medical_data$VisitNumber == 1, ]
emergency_subset <- emergency_subset[1:min(50, nrow(emergency_subset)), ]  # First 50 patients or less

# Create hourly admission data
n_emergency_patients <- nrow(emergency_subset)
admission_data <- data.frame(
    PatientID = rep(emergency_subset$PatientID, each = 24),
    AdmissionDate = rep(emergency_subset$VisitDate, each = 24),
    Hour = rep(0:23, times = n_emergency_patients),
    stringsAsFactors = FALSE
)

# Create admission timestamp
admission_data$AdmissionTime <- paste(
    admission_data$AdmissionDate,
    sprintf("%02d:00:00", admission_data$Hour)
)

# Add emergency department data
admission_data$Department = sample(c("Emergency", "ICU", "General Ward"),
                                   nrow(admission_data),
                                   replace = TRUE,
                                   prob = c(0.5, 0.3, 0.2))

# Vital signs monitored hourly
admission_data$HeartRate <- round(rnorm(nrow(admission_data), mean = 80, sd = 15), 0)
admission_data$SystolicBP <- round(rnorm(nrow(admission_data), mean = 125, sd = 20), 0)
admission_data$OxygenSaturation <- pmin(100, round(rnorm(nrow(admission_data), mean = 96, sd = 3), 0))
admission_data$PainScore <- pmin(10, pmax(0, round(rnorm(nrow(admission_data), mean = 5, sd = 2), 0)))

# Reorder columns
medical_data <- medical_data[, c(
    "PatientID", "VisitNumber", "VisitDate", "StudyCenter",
    "AgeGroup", "Gender", "TreatmentGroup", "DiagnosisPrimary",
    "DiseaseStage", "ComorbidityCount", "BMICategory",
    "SystolicBP", "DiastolicBP", "HeartRate", "Temperature",
    "RespiratoryRate", "OxygenSaturation",
    "HbA1c", "Creatinine", "eGFR", "Hemoglobin", "WBC", "Platelets",
    "TotalCholesterol", "LDLCholesterol", "HDLCholesterol", "Triglycerides",
    "PainScore", "QualityOfLife", "FunctionalStatus",
    "MedicationAdherence", "ClinicalScore"
)]

# Save datasets
write.csv(medical_data, "./data/medical_research_data.csv", row.names = FALSE)
write.csv(admission_data, "./data/hospital_admission_hourly.csv", row.names = FALSE)

# Display summary
cat("=== MEDICAL RESEARCH TEST DATA CREATED ===\n\n")
cat("Main dataset: medical_research_data.csv\n")
cat("- Patients:", n_patients, "\n")
cat("- Total visits:", n_rows, "\n")
cat("- Variables:", ncol(medical_data), "\n\n")

cat("Categorical variables for grouping:\n")
cat("- StudyCenter: Multi-center clinical trial sites\n")
cat("- AgeGroup: Ordered age categories\n")
cat("- Gender: Male/Female\n")
cat("- TreatmentGroup: Control, Treatment A, B, A+B\n")
cat("- DiagnosisPrimary: Main diagnosis\n")
cat("- DiseaseStage: Ordered stages I-IV\n")
cat("- BMICategory: Ordered BMI categories\n\n")

cat("Numeric variables to summarize:\n")
cat("Vital Signs: SystolicBP, DiastolicBP, HeartRate, Temperature, RespiratoryRate, OxygenSaturation\n")
cat("Lab Values: HbA1c, Creatinine, eGFR, Hemoglobin, WBC, Platelets\n")
cat("Lipids: TotalCholesterol, LDLCholesterol, HDLCholesterol, Triglycerides\n")
cat("Patient Outcomes: PainScore, QualityOfLife, FunctionalStatus, MedicationAdherence\n")
cat("Clinical: ClinicalScore (treatment response)\n\n")

cat("Hourly dataset: hospital_admission_hourly.csv\n")
cat("- ", n_emergency_patients, " patients, 24 hours each\n", sep="")
cat("- For testing hourly aggregation\n\n")

# Testing scenarios
cat("=== SUGGESTED TESTING SCENARIOS ===\n\n")

cat("1. Treatment Group Comparison:\n")
cat("   Group by: TreatmentGroup\n")
cat("   Summarize: ClinicalScore, SystolicBP\n")
cat("   Statistics: mean, median, n\n")
cat("   Expected: Treatment groups show improvement\n\n")

cat("2. Multi-Center Analysis:\n")
cat("   Group by: StudyCenter, TreatmentGroup\n")
cat("   Summarize: ClinicalScore, MedicationAdherence\n")
cat("   Statistics: mean, n\n")
cat("   Check: Center variations\n\n")

cat("3. Disease Stage Analysis:\n")
cat("   Group by: DiseaseStage\n")
cat("   Summarize: PainScore, QualityOfLife, SystolicBP\n")
cat("   Statistics: mean, median\n")
cat("   Expected: Worse outcomes in higher stages\n\n")

cat("4. Longitudinal Analysis (by Visit):\n")
cat("   Group by: VisitNumber, TreatmentGroup\n")
cat("   Summarize: ClinicalScore, SystolicBP\n")
cat("   Statistics: mean, n\n")
cat("   Expected: Improvement trend in treatment groups\n\n")

cat("5. Monthly Patient Flow:\n")
cat("   Group by: VisitDate\n")
cat("   Date variable: VisitDate\n")
cat("   Date format: ymd\n")
cat("   Time aggregation: month\n")
cat("   Summarize: PatientID (will show visit counts)\n")
cat("   Statistics: n\n\n")

cat("6. Lab Value Analysis by Diagnosis:\n")
cat("   Group by: DiagnosisPrimary\n")
cat("   Summarize: HbA1c, Creatinine, eGFR\n")
cat("   Statistics: mean, median, n\n")
cat("   Expected: Diabetics have higher HbA1c, CKD patients have higher creatinine\n\n")

cat("7. Hourly Vital Signs (use hospital_admission_hourly.csv):\n")
cat("   Group by: AdmissionTime, Department\n")
cat("   Date variable: AdmissionTime\n")
cat("   Date format: ymd_hms\n")
cat("   Time aggregation: hour\n")
cat("   Summarize: HeartRate, SystolicBP, OxygenSaturation\n")
cat("   Statistics: mean, n\n\n")

cat("8. Gender and Age Analysis:\n")
cat("   Group by: Gender, AgeGroup\n")
cat("   Summarize: TotalCholesterol, HDLCholesterol, BMI-related measures\n")
cat("   Statistics: mean, median\n")
cat("   Sort by: First Summary Variable (Descending)\n\n")

# Display first few rows
cat("\nFirst 10 rows of medical data:\n")
print(head(medical_data, 10))

cat("\n\nMissing data summary:\n")
missing_summary <- colSums(is.na(medical_data))
print(missing_summary[missing_summary > 0])
