#' Clinical Trial CONSORT Flow Data
#'
#' @description
#' Realistic simulated dataset for a Phase III randomized controlled trial (RCT)
#' with multiple exclusion stages. Designed for testing the \code{consortdiagram}
#' function and demonstrating CONSORT 2010 compliant flow diagrams.
#'
#' @format A data frame with 500 rows and 17 columns:
#' \describe{
#'   \item{participant_id}{Unique participant identifier (PT-0001 to PT-0500)}
#'   \item{age}{Age in years (mean=62, sd=12)}
#'   \item{sex}{Sex (Male/Female)}
#'   \item{age_exclusion}{Screening exclusion reason: age criteria violations}
#'   \item{performance_exclusion}{Screening exclusion reason: ECOG performance status or organ function issues}
#'   \item{comorbidity_exclusion}{Screening exclusion reason: severe comorbidity or prior malignancy}
#'   \item{consent_exclusion}{Enrollment exclusion reason: consent-related issues}
#'   \item{eligibility_exclusion}{Enrollment exclusion reason: eligibility re-assessment failures}
#'   \item{treatment_arm}{Randomized treatment assignment (Treatment A/Treatment B)}
#'   \item{allocation_exclusion}{Post-randomization exclusion: intervention not received}
#'   \item{followup_loss}{Follow-up exclusion reason: lost to follow-up or moved}
#'   \item{followup_death}{Follow-up exclusion reason: death or withdrawal}
#'   \item{analysis_exclusion}{Analysis exclusion reason: missing data or protocol violations}
#'   \item{outcome_response}{Treatment outcome (Complete/Partial Response, Stable/Progressive Disease)}
#' }
#'
#' @details
#' This dataset simulates a realistic Phase III clinical trial with:
#' \itemize{
#'   \item 500 participants initially assessed for eligibility
#'   \item 20\% screening exclusion rate (age, performance, comorbidity)
#'   \item 10\% enrollment exclusion rate (consent, eligibility)
#'   \item 1:1 randomization to Treatment A vs Treatment B
#'   \item 5\% allocation exclusion rate (intervention not received)
#'   \item 12\% follow-up loss rate (lost to follow-up, death, withdrawal)
#'   \item 3\% analysis exclusion rate (missing data, protocol violations)
#'   \item Final retention: 58.4\% (292 participants analyzed)
#' }
#'
#' Exclusion variables use NA for participants who continued to the next stage.
#' Non-NA values indicate the specific exclusion reason at that stage.
#'
#' @section Usage:
#' This dataset demonstrates all features of the \code{consortdiagram} function:
#' \itemize{
#'   \item Multiple exclusion stages (screening, enrollment, allocation, follow-up, analysis)
#'   \item Multiple exclusion reasons per stage
#'   \item Randomized treatment arms
#'   \item Realistic retention rates
#'   \item Detailed exclusion reasons for CONSORT compliance
#' }
#'
#' @seealso
#' \code{\link{observational_study_flow_data}}, \code{\link{multicenter_trial_data}}
#'
#' @source
#' Generated using \code{data-raw/create_clinical_trial_flow_data.R} (seed: 20251005)
#'
#' @references
#' Schulz KF, Altman DG, Moher D. (2010). CONSORT 2010 Statement: updated
#' guidelines for reporting parallel group randomised trials. BMJ, 340:c332.
#'
#' Ots R. (2024). CONSORT diagrams in R using patient-level data.
#' \url{https://www.riinu.me/2024/02/consort/}
#'
#' @examples
#' \dontrun{
#' # Load data
#' data(clinical_trial_consort_data)
#'
#' # View structure
#' str(clinical_trial_consort_data)
#'
#' # Summary statistics
#' summary(clinical_trial_consort_data)
#'
#' # Count participants at each stage
#' n_assessed <- nrow(clinical_trial_consort_data)
#' n_screening_excl <- sum(!is.na(clinical_trial_consort_data$age_exclusion) |
#'                         !is.na(clinical_trial_consort_data$performance_exclusion) |
#'                         !is.na(clinical_trial_consort_data$comorbidity_exclusion))
#' n_randomized <- sum(!is.na(clinical_trial_consort_data$treatment_arm))
#' n_analyzed <- sum(!is.na(clinical_trial_consort_data$outcome_response))
#'
#' cat("Assessed:", n_assessed, "\n")
#' cat("Screening excluded:", n_screening_excl, "\n")
#' cat("Randomized:", n_randomized, "\n")
#' cat("Analyzed:", n_analyzed, "\n")
#' }
#'
"clinical_trial_consort_data"


#' Observational Study Flow Data
#'
#' @description
#' Simulated dataset for a prospective cohort study (non-randomized observational design)
#' with multiple exclusion stages. Designed for testing the \code{studydiagram} function
#' and demonstrating participant flow in observational research.
#'
#' @format A data frame with 350 rows and 8 columns:
#' \describe{
#'   \item{participant_id}{Unique participant identifier (OBS-0001 to OBS-0350)}
#'   \item{enrollment_date}{Date of enrollment (2023-01-01 onwards)}
#'   \item{age}{Age in years (mean=58, sd=15)}
#'   \item{diagnosis}{Disease stage (Stage I/II/III/IV)}
#'   \item{initial_exclusion}{Initial screening exclusion reasons}
#'   \item{consent_status}{Consent/enrollment exclusion reasons}
#'   \item{followup_status}{Follow-up completion status and reasons for loss}
#'   \item{in_final_analysis}{Logical flag indicating inclusion in final analysis}
#'   \item{study_group}{Observational cohort assignment (Surgery/Radiation/Combination)}
#' }
#'
#' @details
#' This dataset simulates a realistic observational cohort study with:
#' \itemize{
#'   \item 350 participants initially enrolled
#'   \item 15\% initial exclusion rate (incomplete records, duplicates)
#'   \item 8\% consent exclusion rate (declined, unable to contact)
#'   \item 10\% follow-up loss rate (lost, moved, death, withdrew)
#'   \item Final retention: 70.6\% (247 participants in final analysis)
#' }
#'
#' Unlike the RCT dataset (\code{clinical_trial_consort_data}), this represents
#' observational allocation to treatment groups (not randomized).
#'
#' @section Usage:
#' This dataset demonstrates:
#' \itemize{
#'   \item Non-randomized study flow visualization
#'   \item Observational cohort allocation
#'   \item Temporal enrollment patterns (enrollment_date)
#'   \item Multiple exclusion pathways without randomization
#' }
#'
#' @seealso
#' \code{\link{clinical_trial_consort_data}}, \code{\link{multicenter_trial_data}}
#'
#' @source
#' Generated using \code{data-raw/create_clinical_trial_flow_data.R} (seed: 20251005)
#'
#' @examples
#' \dontrun{
#' # Load data
#' data(observational_study_flow_data)
#'
#' # View structure
#' str(observational_study_flow_data)
#'
#' # Enrollment over time
#' table(format(observational_study_flow_data$enrollment_date, "%Y-%m"))
#'
#' # Final analysis by study group
#' table(observational_study_flow_data$study_group,
#'       observational_study_flow_data$in_final_analysis)
#' }
#'
"observational_study_flow_data"


#' Multi-center Clinical Trial Data
#'
#' @description
#' Simulated dataset for a multi-center randomized controlled trial with differential
#' retention across sites. Designed for testing complex flow scenarios with site-level
#' variability in the \code{consortdiagram} function.
#'
#' @format A data frame with 600 rows and 10 columns:
#' \describe{
#'   \item{participant_id}{Unique participant identifier (MC-00001 to MC-00600)}
#'   \item{site}{Study site (Site A/B/C/D)}
#'   \item{age}{Age in years (mean=65, sd=10)}
#'   \item{sex}{Sex (Male/Female)}
#'   \item{screening_failure}{Screening exclusion reasons (inclusion/exclusion criteria, lab values)}
#'   \item{enrollment_issue}{Enrollment exclusion reasons (consent, travel distance)}
#'   \item{arm}{Randomized treatment arm (Experimental/Control)}
#'   \item{not_received}{Allocation exclusion reasons (intervention unavailable, deterioration)}
#'   \item{followup_loss_reason}{Follow-up loss reasons (lost, withdrew, site closure)}
#'   \item{analysis_issue}{Analysis exclusion reasons (missing endpoint)}
#' }
#'
#' @details
#' This dataset simulates a realistic multi-center trial with:
#' \itemize{
#'   \item 600 participants assessed across 4 sites
#'   \item 25\% screening failure rate (higher than single-center for realism)
#'   \item 5\% enrollment exclusion rate
#'   \item 1:1 randomization to Experimental vs Control
#'   \item 3\% allocation exclusion rate
#'   \item 15\% follow-up loss rate (varies by site)
#'   \item 2\% analysis exclusion rate
#'   \item Final retention: 57.7\% (346 participants analyzed)
#'   \item Site-specific retention rates: 52.5\% to 60.1\%
#' }
#'
#' The dataset demonstrates realistic site variability in retention rates, which is
#' common in multi-center trials due to differences in site infrastructure, patient
#' populations, and study management.
#'
#' @section Usage:
#' This dataset demonstrates:
#' \itemize{
#'   \item Multi-site trial flow visualization
#'   \item Site-level retention variability
#'   \item Complex exclusion patterns
#'   \item Higher attrition rates typical of multi-center studies
#' }
#'
#' @seealso
#' \code{\link{clinical_trial_consort_data}}, \code{\link{observational_study_flow_data}}
#'
#' @source
#' Generated using \code{data-raw/create_clinical_trial_flow_data.R} (seed: 20251005)
#'
#' @examples
#' \dontrun{
#' # Load data
#' data(multicenter_trial_data)
#'
#' # Site-specific retention
#' library(dplyr)
#' multicenter_trial_data %>%
#'   group_by(site) %>%
#'   summarise(
#'     total = n(),
#'     analyzed = sum(is.na(screening_failure) &
#'                    is.na(enrollment_issue) &
#'                    is.na(not_received) &
#'                    is.na(followup_loss_reason) &
#'                    is.na(analysis_issue)),
#'     retention = analyzed / total * 100
#'   )
#' }
#'
"multicenter_trial_data"
