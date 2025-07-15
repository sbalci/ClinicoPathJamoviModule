#' Enhanced Treatment Toxicity Profile Test Datasets
#'
#' Comprehensive collection of test datasets designed for validating enhanced treatment toxicity
#' profile analysis functions. Each dataset represents different clinical trial scenarios, 
#' therapeutic modalities, and safety monitoring challenges commonly encountered in 
#' pharmaceutical development, regulatory submissions, and clinical practice.
#'
#' @name toxicityprofile_datasets
#' @docType data
#' @usage data(toxicityprofile_oncology_trial)
#' @usage data(toxicityprofile_immunotherapy)
#' @usage data(toxicityprofile_targeted_therapy)
#' @usage data(toxicityprofile_dose_escalation)
#' @usage data(toxicityprofile_pediatric)
#' @usage data(toxicityprofile_small_sample)
NULL

#' Oncology Clinical Trial Safety Dataset
#'
#' Simulated comprehensive oncology clinical trial adverse event dataset with three treatment
#' arms, realistic CTCAE grading, and time-to-event information. Designed to test standard
#' oncology safety analysis, treatment comparisons, regulatory reporting, and comprehensive
#' toxicity profiling typical in phase II/III oncology trials.
#'
#' @format A data frame with 2,061 adverse events from 250 patients and 9 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PT_001 to PT_250)}
#'   \item{treatment_group}{Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")}
#'   \item{adverse_event}{Factor. CTCAE adverse event term (22 unique oncology-specific events)}
#'   \item{toxicity_grade}{Integer. CTCAE toxicity grade (1-5 scale)}
#'   \item{system_organ_class}{Factor. MedDRA System Organ Class categorization}
#'   \item{time_to_event}{Integer. Days from treatment initiation to AE onset (1-365 days)}
#'   \item{event_date}{Date. Calculated date of adverse event occurrence}
#'   \item{patient_age}{Integer. Patient age at enrollment (range varies by realistic distribution)}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This dataset simulates a comprehensive oncology clinical trial with realistic adverse event
#' patterns, CTCAE grading distributions, and treatment-specific toxicity profiles. It includes
#' common oncology adverse events such as hematologic toxicities, gastrointestinal effects,
#' constitutional symptoms, and treatment-specific events.
#'
#' **Clinical Context:**
#' - Multi-arm oncology trial with control and two experimental treatments
#' - 250 patients with realistic demographic distribution
#' - 22 common oncology adverse events with expected frequency patterns
#' - CTCAE v5.0 compatible grading system
#' - Realistic time-to-event distributions for safety monitoring
#'
#' **Adverse Events Included:**
#' - **Hematologic**: Anemia, Neutropenia, Thrombocytopenia
#' - **Gastrointestinal**: Nausea, Vomiting, Diarrhea, Constipation, Mucositis
#' - **Constitutional**: Fatigue, Fever, Decreased appetite
#' - **Neurologic**: Peripheral neuropathy
#' - **Dermatologic**: Alopecia, Rash
#' - **Laboratory**: Elevated ALT/AST, Hyponatremia
#' - **Serious Events**: Pneumonitis, Infection, Dehydration
#' - **Cardiovascular**: Hypertension, Proteinuria
#'
#' **Statistical Features:**
#' - Treatment-specific incidence patterns with realistic effect sizes
#' - Grade distributions appropriate for each adverse event type
#' - Time-to-event patterns reflecting clinical experience
#' - Balanced patient demographics across treatment arms
#'
#' **Recommended Analysis Scenarios:**
#' - Standard safety tabulations and listings
#' - Treatment group comparisons with statistical testing
#' - Time-to-event analysis and cumulative incidence
#' - System organ class summaries for regulatory reporting
#' - High-grade (≥3) adverse event analysis
#' - Dose-limiting toxicity assessment
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_oncology_trial)
#' 
#' # Basic toxicity profile analysis
#' result <- toxicityprofile(
#'   data = toxicityprofile_oncology_trial,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "stacked_bar",
#'   showHighGradeOnly = FALSE
#' )
#' 
#' # Treatment comparison analysis
#' result_comparison <- toxicityprofile(
#'   data = toxicityprofile_oncology_trial,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event", 
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   systemOrganClass = "system_organ_class",
#'   groupComparison = TRUE,
#'   showConfidenceIntervals = TRUE
#' )
#' 
#' # Time-to-event analysis
#' result_time <- toxicityprofile(
#'   data = toxicityprofile_oncology_trial,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade", 
#'   timeToEvent = "time_to_event",
#'   plotType = "time_to_event",
#'   cumulativeIncidence = TRUE
#' )
#' }
"toxicityprofile_oncology_trial"

#' Immunotherapy Safety Profile Dataset
#'
#' Simulated immunotherapy clinical trial adverse event dataset featuring immune-related
#' adverse events (irAEs), monotherapy vs combination therapy comparison, and characteristic
#' delayed-onset toxicity patterns. Designed to test immunotherapy-specific safety analysis,
#' immune-related AE monitoring, and regulatory reporting for checkpoint inhibitors.
#'
#' @format A data frame with 541 adverse events from 171 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (IMM_001 to IMM_180)}
#'   \item{treatment_group}{Factor. Treatment regimen ("Monotherapy", "Combination")}
#'   \item{adverse_event}{Factor. Immune-related adverse events (20 unique irAEs)}
#'   \item{toxicity_grade}{Integer. CTCAE toxicity grade with irAE-specific distributions}
#'   \item{system_organ_class}{Factor. MedDRA SOC with emphasis on immune-related systems}
#'   \item{time_to_event}{Integer. Days to irAE onset (typically delayed, 7-365 days)}
#'   \item{patient_age}{Integer. Patient age at enrollment}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This dataset simulates immunotherapy safety data with characteristic immune-related
#' adverse events, delayed onset patterns, and combination vs monotherapy toxicity profiles.
#' It reflects real-world immunotherapy safety experience including checkpoint inhibitor
#' toxicities and immune system activation effects.
#'
#' **Clinical Context:**
#' - Immunotherapy trial comparing monotherapy vs combination treatment
#' - 180 patients with immune-related adverse event focus
#' - 20 specific immune-related adverse events
#' - Delayed onset patterns characteristic of immunotherapy
#' - Grade distributions reflecting irAE severity patterns
#'
#' **Immune-Related Adverse Events:**
#' - **Gastrointestinal**: Diarrhea, Colitis
#' - **Pulmonary**: Pneumonitis
#' - **Hepatic**: Hepatitis
#' - **Endocrine**: Thyroiditis, Adrenal insufficiency, Diabetes mellitus, Hypophysitis
#' - **Dermatologic**: Rash, Pruritus, Vitiligo, Severe skin reactions
#' - **Musculoskeletal**: Arthralgia, Myalgia
#' - **Renal**: Nephritis
#' - **Neurologic**: Neurological toxicity
#' - **Cardiac**: Myocarditis (rare but serious)
#' - **Ocular**: Uveitis
#'
#' **Key Features:**
#' - Delayed onset patterns typical of immune activation
#' - Higher incidence in combination therapy vs monotherapy
#' - Appropriate grade distributions for each irAE type
#' - Serious events (myocarditis, severe colitis) at realistic low frequencies
#' - System organ class categorization for regulatory reporting
#'
#' **Recommended Analysis Scenarios:**
#' - Immune-related adverse event profiling
#' - Monotherapy vs combination safety comparison
#' - Time-to-onset analysis for delayed toxicities
#' - Endocrine toxicity monitoring
#' - Serious irAE identification and reporting
#' - Safety stopping rule evaluation
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_immunotherapy)
#' 
#' # Immune-related AE analysis
#' result <- toxicityprofile(
#'   data = toxicityprofile_immunotherapy,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   systemOrganClass = "system_organ_class",
#'   plotType = "heatmap"
#' )
#' 
#' # High-grade irAE analysis
#' result_serious <- toxicityprofile(
#'   data = toxicityprofile_immunotherapy,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   showHighGradeOnly = TRUE,
#'   minIncidence = 1
#' )
#' 
#' # Time-to-onset analysis
#' result_timing <- toxicityprofile(
#'   data = toxicityprofile_immunotherapy,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   timeToEvent = "time_to_event",
#'   plotType = "time_to_event"
#' )
#' }
"toxicityprofile_immunotherapy"

#' Targeted Therapy Safety Profile Dataset
#'
#' Simulated targeted therapy clinical trial adverse event dataset featuring characteristic
#' targeted therapy toxicities, early onset patterns, and class-specific effects. Designed
#' to test targeted therapy safety analysis, mechanism-based toxicity profiling, and
#' kinase inhibitor safety monitoring typical in precision oncology.
#'
#' @format A data frame with 1,142 adverse events from 200 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (TRG_001 to TRG_200)}
#'   \item{treatment_group}{Factor. Treatment regimen ("Monotherapy", "Combination")}
#'   \item{adverse_event}{Factor. Targeted therapy-specific adverse events (24 unique events)}
#'   \item{toxicity_grade}{Integer. CTCAE grade with targeted therapy-specific patterns}
#'   \item{system_organ_class}{Factor. MedDRA SOC emphasizing targeted therapy effects}
#'   \item{time_to_event}{Integer. Days to AE onset (typically early, 1-365 days)}
#'   \item{patient_age}{Integer. Patient age at enrollment}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This dataset simulates targeted therapy safety data with characteristic class effects,
#' early onset patterns, and mechanism-based toxicity profiles. It reflects real-world
#' targeted therapy safety experience including kinase inhibitor effects, EGFR inhibitor
#' toxicities, and angiogenesis inhibitor-related events.
#'
#' **Clinical Context:**
#' - Targeted therapy trial comparing monotherapy vs combination
#' - 200 patients with targeted therapy-specific toxicity focus
#' - 24 mechanism-based adverse events
#' - Early onset patterns typical of targeted agents
#' - Class-specific toxicity distributions
#'
#' **Targeted Therapy-Specific Adverse Events:**
#' - **Gastrointestinal**: Diarrhea, Mucositis, Nausea, Vomiting
#' - **Dermatologic**: Rash, Acneiform rash, Paronychia, Dry skin, Hand-foot syndrome
#' - **Constitutional**: Fatigue, Decreased appetite
#' - **Cardiovascular**: Hypertension, QT prolongation
#' - **Vascular**: Bleeding, Thrombosis, Proteinuria
#' - **Hepatic**: Elevated transaminases, Hyperbilirubinemia
#' - **Pulmonary**: Pneumonitis, Interstitial lung disease
#' - **Other**: Peripheral edema, Pleural effusion, Muscle spasms, Arthralgia
#'
#' **Key Features:**
#' - Early onset patterns characteristic of targeted agents
#' - Higher frequency of dermatologic and GI toxicities
#' - Cardiovascular effects typical of angiogenesis inhibitors
#' - Appropriate grade distributions for each toxicity class
#' - Realistic incidence rates based on clinical experience
#'
#' **Recommended Analysis Scenarios:**
#' - Targeted therapy toxicity profiling
#' - Class effect identification and analysis
#' - Early vs late onset toxicity patterns
#' - Dermatologic toxicity monitoring
#' - Cardiovascular safety assessment
#' - Dose modification analysis
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_targeted_therapy)
#' 
#' # Targeted therapy toxicity profile
#' result <- toxicityprofile(
#'   data = toxicityprofile_targeted_therapy,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "stacked_bar",
#'   sortBy = "frequency"
#' )
#' 
#' # Dermatologic toxicity focus
#' dermatologic_data <- subset(toxicityprofile_targeted_therapy, 
#'                            system_organ_class == "Skin and subcutaneous tissue disorders")
#' result_derm <- toxicityprofile(
#'   data = dermatologic_data,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   plotType = "dot_plot",
#'   showConfidenceIntervals = TRUE
#' )
#' 
#' # Early onset pattern analysis
#' result_timing <- toxicityprofile(
#'   data = toxicityprofile_targeted_therapy,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   timeToEvent = "time_to_event",
#'   plotType = "time_to_event"
#' )
#' }
"toxicityprofile_targeted_therapy"

#' Dose Escalation Study Safety Dataset
#'
#' Simulated dose escalation study adverse event dataset featuring dose-dependent toxicity
#' patterns, escalating severity with increasing dose levels, and dose-limiting toxicity
#' assessment. Designed to test dose-response safety analysis, maximum tolerated dose
#' determination, and phase I trial safety monitoring.
#'
#' @format A data frame with 603 adverse events from 148 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (DSE_001 to DSE_150)}
#'   \item{treatment_group}{Factor. Dose level ("Dose Level 1" to "Dose Level 5")}
#'   \item{adverse_event}{Factor. General adverse events (20 events across all systems)}
#'   \item{toxicity_grade}{Integer. CTCAE grade with dose-dependent severity escalation}
#'   \item{system_organ_class}{Factor. Simplified SOC categorization}
#'   \item{time_to_event}{Integer. Days to AE onset (typically early in dose escalation)}
#'   \item{patient_age}{Integer. Patient age at enrollment}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This dataset simulates a dose escalation study with realistic dose-dependent toxicity
#' patterns, increasing incidence and severity at higher dose levels, and appropriate
#' distributions for dose-limiting toxicity assessment. It reflects typical phase I
#' dose escalation trial safety patterns.
#'
#' **Clinical Context:**
#' - Five-level dose escalation study (3+3 or similar design)
#' - 150 patients distributed across dose levels
#' - Dose-dependent incidence and severity patterns
#' - Early toxicity assessment (first cycle focus)
#' - DLT evaluation timeframe (typically 28 days)
#'
#' **Dose-Response Characteristics:**
#' - Increasing incidence rates with dose escalation
#' - Higher grade toxicities at elevated dose levels
#' - Realistic dose level patient distributions
#' - Early onset appropriate for DLT assessment
#' - General toxicity profile applicable across therapeutic areas
#'
#' **Adverse Events Pattern:**
#' - **Constitutional**: Fatigue, Nausea, Vomiting, Decreased appetite
#' - **Gastrointestinal**: Diarrhea, Constipation, Abdominal pain, Dyspepsia
#' - **Neurologic**: Headache, Dizziness, Peripheral neuropathy, Muscle weakness
#' - **Psychiatric**: Insomnia, Anxiety
#' - **Dermatologic**: Rash, Pruritus, Dry mouth
#' - **Musculoskeletal**: Arthralgia, Back pain
#' - **Cardiovascular**: Hypertension
#'
#' **Key Features:**
#' - Clear dose-response relationship in toxicity
#' - Escalating severity with increasing dose
#' - Appropriate for DLT assessment
#' - Realistic phase I trial patient numbers
#' - Early onset patterns for cycle 1 evaluation
#'
#' **Recommended Analysis Scenarios:**
#' - Dose-response toxicity analysis
#' - Maximum tolerated dose determination
#' - Dose-limiting toxicity identification
#' - Safety run-in analysis
#' - Dose modification recommendations
#' - Phase I safety summary
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_dose_escalation)
#' 
#' # Dose-response analysis
#' result <- toxicityprofile(
#'   data = toxicityprofile_dose_escalation,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "stacked_bar",
#'   sortBy = "high_grade"
#' )
#' 
#' # High-grade toxicity by dose level
#' result_dlts <- toxicityprofile(
#'   data = toxicityprofile_dose_escalation,
#'   patientID = "patient_id", 
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   showHighGradeOnly = TRUE,
#'   groupComparison = FALSE
#' )
#' 
#' # Dose level comparison
#' result_comparison <- toxicityprofile(
#'   data = toxicityprofile_dose_escalation,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event", 
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "heatmap"
#' )
#' }
"toxicityprofile_dose_escalation"

#' Pediatric Safety Study Dataset
#'
#' Simulated pediatric clinical trial adverse event dataset featuring age-appropriate
#' adverse events, milder severity profiles, and pediatric-specific safety considerations.
#' Designed to test pediatric safety analysis, age-specific toxicity profiling, and
#' regulatory pediatric safety reporting requirements.
#'
#' @format A data frame with 387 adverse events from 114 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PED_001 to PED_120)}
#'   \item{treatment_group}{Factor. Treatment assignment ("Active", "Placebo")}
#'   \item{adverse_event}{Factor. Pediatric-appropriate adverse events (18 unique events)}
#'   \item{toxicity_grade}{Integer. CTCAE grade with pediatric-appropriate severity (1-3 focus)}
#'   \item{system_organ_class}{Factor. Simplified SOC for pediatric context}
#'   \item{time_to_event}{Integer. Days to AE onset (shorter timeframe, 1-90 days)}
#'   \item{patient_age}{Integer. Pediatric age range (6-17 years)}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This dataset simulates pediatric clinical trial safety data with age-appropriate
#' adverse events, typically milder severity profiles, and pediatric-specific considerations
#' such as growth, development, and behavioral effects. It reflects pediatric safety
#' monitoring requirements and regulatory considerations.
#'
#' **Clinical Context:**
#' - Pediatric clinical trial with active treatment vs placebo
#' - 120 patients aged 6-17 years
#' - Age-appropriate adverse event profile
#' - Milder severity patterns typical in pediatric populations
#' - Shorter assessment timeframe appropriate for pediatric studies
#'
#' **Pediatric-Specific Adverse Events:**
#' - **Constitutional**: Fatigue, Fever, Decreased appetite, Weight loss
#' - **Gastrointestinal**: Nausea, Vomiting, Diarrhea, Abdominal pain
#' - **Neurologic**: Headache
#' - **Behavioral**: Irritability, Sleep disturbance, Mood changes, Attention difficulties, Hyperactivity
#' - **Respiratory**: Upper respiratory infection, Cough
#' - **Dermatologic**: Rash
#' - **Local**: Injection site reaction
#'
#' **Key Features:**
#' - Age-appropriate adverse event selection
#' - Predominantly grade 1-2 events (milder severity)
#' - Behavioral and developmental considerations
#' - Realistic pediatric study size and design
#' - Appropriate timeframe for pediatric assessment
#' - Placebo-controlled design typical in pediatric trials
#'
#' **Recommended Analysis Scenarios:**
#' - Pediatric safety profiling
#' - Active vs placebo safety comparison
#' - Age-stratified safety analysis
#' - Behavioral toxicity assessment
#' - Growth and development monitoring
#' - Pediatric regulatory safety reporting
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_pediatric)
#' 
#' # Pediatric safety profile
#' result <- toxicityprofile(
#'   data = toxicityprofile_pediatric,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "dot_plot",
#'   showConfidenceIntervals = TRUE
#' )
#' 
#' # Behavioral toxicity focus
#' behavioral_events <- c("Irritability", "Sleep disturbance", "Mood changes", 
#'                       "Attention difficulties", "Hyperactivity")
#' behavioral_data <- subset(toxicityprofile_pediatric, 
#'                          adverse_event %in% behavioral_events)
#' result_behavioral <- toxicityprofile(
#'   data = behavioral_data,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   groupComparison = TRUE
#' )
#' 
#' # Age-stratified analysis
#' adolescent_data <- subset(toxicityprofile_pediatric, patient_age >= 13)
#' result_adolescent <- toxicityprofile(
#'   data = adolescent_data,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   plotType = "stacked_bar"
#' )
#' }
"toxicityprofile_pediatric"

#' Small Sample Edge Cases Dataset
#'
#' Minimal adverse event dataset with very small sample size designed for edge case testing,
#' validation of statistical methods with limited data, and assessment of function robustness
#' with minimal observations. Essential for testing graceful degradation and error handling.
#'
#' @format A data frame with 15 adverse events from 15 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Simple patient identifier (SML_01 to SML_15)}
#'   \item{treatment_group}{Factor. Binary treatment assignment ("A", "B")}
#'   \item{adverse_event}{Factor. Limited adverse events (3 types: "Fatigue", "Nausea", "Rash")}
#'   \item{toxicity_grade}{Integer. CTCAE grade (1-3 range for testing)}
#'   \item{system_organ_class}{Factor. Basic SOC categories}
#'   \item{time_to_event}{Integer. Days to AE onset (1-30 days range)}
#'   \item{patient_age}{Integer. Adult age range (25-70 years)}
#'   \item{patient_sex}{Factor. Patient sex ("Male", "Female")}
#' }
#'
#' @details
#' This minimal dataset tests the robustness of toxicity profile analysis with very small
#' sample sizes, which can reveal edge cases in statistical calculations, visualization
#' algorithms, and summary statistics. It validates graceful handling of insufficient
#' data scenarios common in early-phase trials or rare disease studies.
#'
#' **Clinical Context:**
#' - Minimal sample size scenario (N=15)
#' - Simple two-arm comparison
#' - Limited adverse event types
#' - Basic severity grading
#' - Short assessment timeframe
#'
#' **Key Characteristics:**
#' - Only 15 total adverse events
#' - Three basic adverse event types
#' - Simple binary treatment comparison
#' - Small cell counts for statistical testing
#' - Minimal data for visualization algorithms
#'
#' **Testing Scenarios:**
#' - Statistical method robustness with small samples
#' - Visualization algorithm edge cases
#' - Confidence interval calculation with limited data
#' - Group comparison with small cell counts
#' - Error handling and graceful degradation
#' - Minimum viable analysis requirements
#'
#' **Expected Behaviors:**
#' - Appropriate handling of small sample statistics
#' - Clear visualization despite limited data
#' - Robust confidence interval calculations
#' - Appropriate warnings for limited statistical power
#' - Graceful handling of empty cells in cross-tabulations
#'
#' **Recommended Analysis Scenarios:**
#' - Small sample robustness testing
#' - Edge case validation
#' - Statistical method verification
#' - Minimum sample size assessment
#' - Error handling validation
#' - Algorithm stability testing
#'
#' @source Simulated data generated using create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile}}, \code{\link{toxicityprofile_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toxicityprofile_small_sample)
#' 
#' # Minimal sample analysis
#' result <- toxicityprofile(
#'   data = toxicityprofile_small_sample,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   plotType = "stacked_bar"
#' )
#' 
#' # Edge case testing with group comparison
#' result_comparison <- toxicityprofile(
#'   data = toxicityprofile_small_sample,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   treatment = "treatment_group",
#'   groupComparison = TRUE,
#'   showConfidenceIntervals = TRUE
#' )
#' 
#' # Minimal visualization testing
#' result_minimal <- toxicityprofile(
#'   data = toxicityprofile_small_sample,
#'   patientID = "patient_id",
#'   adverseEvent = "adverse_event",
#'   grade = "toxicity_grade",
#'   plotType = "dot_plot",
#'   minIncidence = 1
#' )
#' }
"toxicityprofile_small_sample"

#' Treatment Toxicity Profile Dataset Collection Summary
#'
#' Summary table providing comprehensive overview of all enhanced toxicityprofile test 
#' datasets including adverse event counts, patient numbers, unique events, clinical
#' contexts, key features, and recommended usage scenarios for comprehensive safety
#' analysis validation.
#'
#' @format A data frame with 6 observations and 7 variables:
#' \describe{
#'   \item{Dataset}{Character. Dataset name}
#'   \item{Events}{Integer. Total number of adverse events}
#'   \item{Patients}{Integer. Number of unique patients}
#'   \item{Unique_AEs}{Integer. Number of unique adverse events}
#'   \item{Description}{Character. Clinical context and study type}
#'   \item{Key_Features}{Character. Main characteristics and toxicity patterns}
#'   \item{Primary_Use_Case}{Character. Primary testing and analysis scenarios}
#' }
#'
#' @details
#' This summary table provides a comprehensive reference for all enhanced toxicityprofile
#' test datasets, helping users select appropriate datasets for their testing needs and
#' understand the clinical contexts and analytical capabilities of each dataset.
#'
#' **Total Collection Statistics:**
#' - 6 comprehensive test datasets
#' - 4,749 total adverse events across all datasets
#' - 898 total patients with realistic distributions
#' - 107 unique adverse events covering all major therapeutic areas
#' - Complete coverage of oncology, immunotherapy, targeted therapy, dose escalation, and pediatric scenarios
#'
#' **Clinical Scenarios Covered:**
#' - Standard oncology clinical trials with multi-arm comparisons
#' - Immunotherapy trials with immune-related adverse events
#' - Targeted therapy trials with mechanism-based toxicities
#' - Dose escalation studies with dose-response relationships
#' - Pediatric trials with age-appropriate safety considerations
#' - Small sample scenarios for edge case testing
#'
#' @source Generated by create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile_datasets}}, \code{\link{toxicityprofile}}
"toxicityprofile_datasets_summary"

#' Treatment Toxicity Profile Test Scenarios Documentation
#'
#' Comprehensive testing scenarios designed to validate different aspects of enhanced
#' treatment toxicity profile functionality using the toxicityprofile test datasets.
#' Each scenario targets specific analysis capabilities, visualization types, and
#' statistical methods for comprehensive validation.
#'
#' @format A data frame with 12 observations and 5 variables:
#' \describe{
#'   \item{Scenario}{Character. Name of the testing scenario}
#'   \item{Dataset}{Character. Recommended dataset for this scenario}
#'   \item{Analysis_Type}{Character. Type of analysis being validated}
#'   \item{Variables}{Character. Key variables for the analysis}
#'   \item{Expected_Result}{Character. Expected outcome and validation criteria}
#' }
#'
#' @details
#' This documentation provides systematic testing scenarios covering all major
#' toxicity profile analysis capabilities and validation requirements:
#'
#' **Core Analysis Types Tested:**
#' - **Basic Toxicity Profiling**: Frequency distributions and grade analysis
#' - **Treatment Comparisons**: Statistical testing and risk assessment
#' - **High-Grade Event Analysis**: Focus on serious adverse events (Grade ≥3)
#' - **System Organ Class Analysis**: Regulatory reporting by organ system
#' - **Time-to-Event Analysis**: Cumulative incidence and timing patterns
#' - **Dose-Response Analysis**: Dose-dependent toxicity relationships
#' - **Immunotherapy-Specific**: Immune-related adverse event patterns
#' - **Pediatric Safety**: Age-appropriate toxicity profiling
#' - **Statistical Testing**: Comprehensive validation of statistical methods
#' - **Confidence Intervals**: Exact binomial and bootstrap methods
#' - **Edge Case Robustness**: Small sample and extreme scenario testing
#' - **Visualization Testing**: Multiple plot types and formatting options
#'
#' **Statistical Methods Validated:**
#' - Fisher's exact test for treatment comparisons
#' - Chi-square test for grade distribution differences
#' - Exact binomial confidence intervals for incidence rates
#' - Risk ratio calculations with confidence intervals
#' - Time-to-event analysis and cumulative incidence
#' - Dose-response trend testing
#'
#' **Visualization Types Tested:**
#' - Stacked bar charts with grade distributions
#' - Dot plots with confidence intervals
#' - Heatmaps for comprehensive overviews
#' - Time-to-event curves and cumulative incidence plots
#' - System organ class summaries
#'
#' Each scenario includes the recommended dataset, analysis type, key variables,
#' and expected results for comprehensive validation of enhanced toxicity profile
#' analysis capabilities.
#'
#' @source Generated by create_toxicityprofile_test_data.R
#' @seealso \code{\link{toxicityprofile_datasets}}, \code{\link{toxicityprofile}}
"toxicityprofile_test_scenarios"