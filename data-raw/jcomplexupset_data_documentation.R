# Documentation for jcomplexupset test datasets

#' jcomplexupset Test Data
#'
#' A comprehensive dataset for testing Complex UpSet plot visualizations.
#' Contains binary variables representing treatment modalities, biomarkers,
#' complications, and response indicators for 200 cancer patients.
#'
#' @format A data frame with 200 rows and 21 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier (1-200)}
#'   \item{Age}{Patient age in years (25-85)}
#'   \item{Sex}{Patient sex: "Male", "Female"}
#'   \item{Surgery}{Logical: received surgical treatment}
#'   \item{Chemotherapy}{Logical: received chemotherapy}
#'   \item{Radiotherapy}{Logical: received radiotherapy}
#'   \item{Immunotherapy}{Logical: received immunotherapy}
#'   \item{TargetedTherapy}{Logical: received targeted therapy}
#'   \item{HER2_Positive}{Logical: HER2 positive status}
#'   \item{ER_Positive}{Logical: Estrogen receptor positive status}
#'   \item{PR_Positive}{Logical: Progesterone receptor positive status}
#'   \item{PDL1_Positive}{Logical: PD-L1 positive status}
#'   \item{Infection}{Logical: developed infection complication}
#'   \item{Bleeding}{Logical: developed bleeding complication}
#'   \item{Nausea}{Logical: experienced nausea}
#'   \item{Fatigue}{Logical: experienced fatigue}
#'   \item{Complete_Response}{Logical: achieved complete response}
#'   \item{Partial_Response}{Logical: achieved partial response}
#'   \item{Stable_Disease}{Logical: had stable disease}
#'   \item{Progressive_Disease}{Logical: had progressive disease}
#'   \item{TumorSize}{Numeric: tumor size in centimeters}
#'   \item{SurvivalMonths}{Numeric: survival time in months}
#'   \item{TreatmentCost}{Numeric: treatment cost in dollars}
#' }
#' @source Simulated data for testing jcomplexupset function
#' @examples
#' data(jcomplexupset_test_data)
#' # Analyze treatment combinations
#' treatment_vars <- c("Surgery", "Chemotherapy", "Radiotherapy", "Immunotherapy")
#' summary(jcomplexupset_test_data[, treatment_vars])
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"jcomplexupset_test_data"

#' Molecular Subtype Data
#'
#' A dataset containing molecular markers and pathway alterations for 150 
#' cancer samples, suitable for analyzing mutation patterns and pathway 
#' disruptions using UpSet plots.
#'
#' @format A data frame with 150 rows and 15 variables:
#' \describe{
#'   \item{SampleID}{Sample identifier (1-150)}
#'   \item{BRCA1_Mutation}{Logical: BRCA1 mutation present}
#'   \item{BRCA2_Mutation}{Logical: BRCA2 mutation present}
#'   \item{TP53_Mutation}{Logical: TP53 mutation present}
#'   \item{PIK3CA_Mutation}{Logical: PIK3CA mutation present}
#'   \item{KRAS_Mutation}{Logical: KRAS mutation present}
#'   \item{EGFR_Mutation}{Logical: EGFR mutation present}
#'   \item{PI3K_Pathway}{Logical: PI3K pathway altered}
#'   \item{WNT_Pathway}{Logical: WNT pathway altered}
#'   \item{RB_Pathway}{Logical: RB pathway altered}
#'   \item{DNA_Repair}{Logical: DNA repair pathway altered}
#'   \item{Grade}{Numeric: tumor grade (1-3)}
#'   \item{Stage}{Numeric: tumor stage (1-4)}
#'   \item{Age_Group}{Character: "Young", "Middle", "Elderly"}
#' }
#' @source Simulated molecular profiling data
#' @examples
#' data(molecular_subtype_data)
#' # Analyze mutation patterns
#' mutation_vars <- c("BRCA1_Mutation", "BRCA2_Mutation", "TP53_Mutation", "PIK3CA_Mutation")
#' table(rowSums(molecular_subtype_data[, mutation_vars]))
"molecular_subtype_data"

#' Diagnostic Test Data
#'
#' A dataset representing diagnostic test utilization patterns for 180 
#' clinical cases, including imaging, laboratory tests, and specialist 
#' consultations.
#'
#' @format A data frame with 180 rows and 15 variables:
#' \describe{
#'   \item{CaseID}{Case identifier (1-180)}
#'   \item{CT_Scan}{Logical: CT scan performed}
#'   \item{MRI}{Logical: MRI performed}
#'   \item{PET_Scan}{Logical: PET scan performed}
#'   \item{Ultrasound}{Logical: Ultrasound performed}
#'   \item{Biopsy}{Logical: Biopsy performed}
#'   \item{CBC}{Logical: Complete blood count performed}
#'   \item{Chemistry_Panel}{Logical: Chemistry panel performed}
#'   \item{Tumor_Markers}{Logical: Tumor markers tested}
#'   \item{Genetic_Testing}{Logical: Genetic testing performed}
#'   \item{Oncology}{Logical: Oncology consultation}
#'   \item{Surgery}{Logical: Surgery consultation}
#'   \item{Radiology}{Logical: Radiology consultation}
#'   \item{Pathology}{Logical: Pathology consultation}
#'   \item{Diagnosis_Confirmed}{Logical: Diagnosis confirmed}
#'   \item{Time_to_Diagnosis}{Numeric: Time to diagnosis in days}
#' }
#' @source Simulated diagnostic workflow data
#' @examples
#' data(diagnostic_test_data)
#' # Analyze imaging combinations
#' imaging_vars <- c("CT_Scan", "MRI", "PET_Scan", "Ultrasound")
#' colSums(diagnostic_test_data[, imaging_vars])
"diagnostic_test_data"
