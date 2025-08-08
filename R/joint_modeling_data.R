#' PSA Trajectory and Prostate Cancer Survival Data
#'
#' A longitudinal dataset containing PSA measurements and survival outcomes for 200 prostate cancer patients.
#' This dataset is designed for demonstrating joint longitudinal-survival modeling techniques.
#'
#' @format A data frame with 1369 observations and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PSA_001 to PSA_200)}
#'   \item{age}{Numeric. Patient age at baseline (years)}
#'   \item{stage}{Factor. Tumor stage (T1, T2, T3, T4)}
#'   \item{gleason_score}{Numeric. Gleason score (6-10)}
#'   \item{visit_time}{Numeric. Time of PSA measurement (months from baseline)}
#'   \item{psa_level}{Numeric. PSA level (ng/mL)}
#'   \item{survival_time}{Numeric. Time to death or last follow-up (months)}
#'   \item{death_status}{Numeric. Event indicator (0 = censored, 1 = death)}
#' }
#' 
#' @details 
#' The dataset simulates realistic PSA trajectories where:
#' \itemize{
#'   \item PSA levels generally increase over time
#'   \item Higher tumor stage and Gleason score are associated with higher PSA levels
#'   \item Current PSA level influences survival hazard
#'   \item Visit intervals are irregular, mimicking real clinical practice
#'   \item 14.5% event rate with median follow-up of 60 months
#' }
#' 
#' @examples
#' data(psa_joint_data)
#' 
#' # Basic data exploration
#' head(psa_joint_data)
#' 
#' # Number of patients and visits
#' length(unique(psa_joint_data$patient_id))  # 200 patients
#' table(table(psa_joint_data$patient_id))    # Visit distribution
#' 
#' # Plot individual PSA trajectories for first 10 patients
#' library(ggplot2)
#' first_10 <- subset(psa_joint_data, patient_id %in% unique(patient_id)[1:10])
#' ggplot(first_10, aes(x = visit_time, y = psa_level, color = patient_id)) +
#'   geom_line() + geom_point() +
#'   labs(title = "PSA Trajectories", x = "Time (months)", y = "PSA (ng/mL)")
#' 
#' @source Simulated data based on typical prostate cancer cohort characteristics
"psa_joint_data"

#' CD4 Count and AIDS Progression Data
#'
#' A longitudinal dataset containing CD4 count measurements and AIDS progression outcomes for 180 HIV patients.
#' 
#' @format A data frame with 1341 observations and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (HIV_001 to HIV_180)}
#'   \item{age}{Numeric. Patient age at baseline (years)}
#'   \item{baseline_viral_load}{Numeric. Baseline viral load (copies/mL)}
#'   \item{art_adherence}{Factor. Antiretroviral therapy adherence (Poor, Good)}
#'   \item{visit_time}{Numeric. Time of CD4 measurement (months from baseline)}
#'   \item{cd4_count}{Numeric. CD4+ T cell count (cells/μL)}
#'   \item{survival_time}{Numeric. Time to AIDS/death or last follow-up (months)}
#'   \item{aids_death_status}{Numeric. Event indicator (0 = censored, 1 = AIDS/death)}
#' }
#' 
#' @details
#' The dataset simulates HIV disease progression where:
#' \itemize{
#'   \item CD4 counts generally increase with good ART adherence
#'   \item Poor adherence leads to CD4 decline
#'   \item Lower CD4 counts increase hazard of AIDS/death
#'   \item Regular monitoring every ~6 months
#'   \item Low event rate (1.1%) reflecting modern HIV care
#' }
#' 
#' @examples
#' data(cd4_joint_data)
#' 
#' # Compare CD4 trajectories by adherence
#' library(ggplot2)
#' ggplot(cd4_joint_data, aes(x = visit_time, y = cd4_count, color = art_adherence)) +
#'   geom_smooth(method = "loess") +
#'   labs(title = "CD4 Trajectories by ART Adherence", 
#'        x = "Time (months)", y = "CD4 Count (cells/μL)")
#' 
#' @source Simulated data based on HIV cohort studies
"cd4_joint_data"

#' Kidney Function Decline and ESRD/Death Data
#'
#' A longitudinal dataset containing eGFR measurements and end-stage renal disease (ESRD) or death outcomes
#' for 150 chronic kidney disease patients.
#' 
#' @format A data frame with 1573 observations and 9 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CKD_001 to CKD_150)}
#'   \item{age}{Numeric. Patient age at baseline (years)}
#'   \item{diabetes}{Factor. Diabetes status (No, Yes)}
#'   \item{hypertension}{Factor. Hypertension status (No, Yes)}
#'   \item{baseline_proteinuria}{Numeric. Baseline proteinuria (g/day)}
#'   \item{visit_time}{Numeric. Time of eGFR measurement (months from baseline)}
#'   \item{egfr}{Numeric. Estimated glomerular filtration rate (mL/min/1.73m²)}
#'   \item{survival_time}{Numeric. Time to ESRD/death or last follow-up (months)}
#'   \item{esrd_death_status}{Numeric. Event indicator (0 = censored, 1 = ESRD, 2 = death)}
#' }
#' 
#' @details
#' The dataset simulates chronic kidney disease progression where:
#' \itemize{
#'   \item eGFR generally declines over time
#'   \item Diabetes and hypertension accelerate decline
#'   \item Lower eGFR increases hazard of ESRD or death
#'   \item Competing risks: ESRD vs death
#'   \item 14.7% event rate over 72 months follow-up
#' }
#' 
#' @examples
#' data(kidney_joint_data)
#' 
#' # eGFR trajectories by diabetes status
#' library(ggplot2)
#' ggplot(kidney_joint_data, aes(x = visit_time, y = egfr, color = diabetes)) +
#'   geom_smooth(method = "loess") +
#'   labs(title = "eGFR Decline by Diabetes Status", 
#'        x = "Time (months)", y = "eGFR (mL/min/1.73m²)")
#' 
#' @source Simulated data based on chronic kidney disease cohort studies
"kidney_joint_data"

#' Cardiac Biomarkers and Heart Failure Events Data
#'
#' A longitudinal dataset containing NT-proBNP measurements and heart failure events for 120 cardiac patients.
#' 
#' @format A data frame with 964 observations and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (HF_001 to HF_120)}
#'   \item{age}{Numeric. Patient age at baseline (years)}
#'   \item{nyha_class}{Factor. NYHA functional class (Class 1, Class 2, Class 3, Class 4)}
#'   \item{baseline_ef}{Numeric. Baseline ejection fraction (%)}
#'   \item{visit_time}{Numeric. Time of NT-proBNP measurement (months from baseline)}
#'   \item{nt_probnp}{Numeric. NT-proBNP level (pg/mL)}
#'   \item{survival_time}{Numeric. Time to heart failure event or last follow-up (months)}
#'   \item{hf_event_status}{Numeric. Event indicator (0 = censored, 1 = heart failure hospitalization/death)}
#' }
#' 
#' @details
#' The dataset simulates heart failure progression where:
#' \itemize{
#'   \item NT-proBNP levels generally increase over time
#'   \item Higher NYHA class associated with higher NT-proBNP
#'   \item Higher NT-proBNP increases hazard of heart failure events
#'   \item Regular monitoring every ~4 months
#'   \item 20% event rate over 36 months follow-up
#' }
#' 
#' @examples
#' data(cardiac_joint_data)
#' 
#' # NT-proBNP trajectories by NYHA class
#' library(ggplot2)
#' ggplot(cardiac_joint_data, aes(x = visit_time, y = log(nt_probnp), color = nyha_class)) +
#'   geom_smooth(method = "loess") +
#'   labs(title = "NT-proBNP Trajectories by NYHA Class", 
#'        x = "Time (months)", y = "log(NT-proBNP)")
#' 
#' @source Simulated data based on heart failure cohort studies
"cardiac_joint_data"

#' Simple Tumor Marker and Cancer Progression Data
#'
#' A simplified longitudinal dataset containing tumor marker measurements and cancer progression 
#' for 100 cancer patients. Ideal for teaching and initial exploration of joint models.
#' 
#' @format A data frame with 588 observations and 6 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CA_001 to CA_100)}
#'   \item{age}{Numeric. Patient age at baseline (years)}
#'   \item{treatment}{Factor. Treatment group (Standard, Experimental)}
#'   \item{visit_time}{Numeric. Time of tumor marker measurement (months from baseline)}
#'   \item{tumor_marker}{Numeric. Tumor marker level (units/mL)}
#'   \item{survival_time}{Numeric. Time to progression/death or last follow-up (months)}
#'   \item{progression_status}{Numeric. Event indicator (0 = censored, 1 = progression/death)}
#' }
#' 
#' @details
#' This simplified dataset is perfect for:
#' \itemize{
#'   \item Learning joint modeling concepts
#'   \item Quick algorithm testing
#'   \item Demonstrating treatment effects
#'   \item Smaller sample size for faster computation
#'   \item Clear biomarker-survival relationship
#' }
#' 
#' Features:
#' \itemize{
#'   \item Tumor marker levels generally increase over time
#'   \item Experimental treatment slows marker increase
#'   \item Higher marker levels increase progression hazard
#'   \item 10% event rate over 24 months follow-up
#' }
#' 
#' @examples
#' data(simple_cancer_data)
#' 
#' # Basic joint modeling analysis
#' library(ggplot2)
#' 
#' # Marker trajectories by treatment
#' ggplot(simple_cancer_data, aes(x = visit_time, y = tumor_marker, color = treatment)) +
#'   geom_smooth(method = "loess") +
#'   labs(title = "Tumor Marker by Treatment", 
#'        x = "Time (months)", y = "Tumor Marker")
#' 
#' # Individual trajectories for first 20 patients
#' first_20 <- subset(simple_cancer_data, patient_id %in% unique(patient_id)[1:20])
#' ggplot(first_20, aes(x = visit_time, y = tumor_marker, group = patient_id)) +
#'   geom_line(alpha = 0.7) + geom_point(alpha = 0.5) +
#'   facet_wrap(~treatment) +
#'   labs(title = "Individual Tumor Marker Trajectories")
#' 
#' @source Simulated data for educational purposes
"simple_cancer_data"