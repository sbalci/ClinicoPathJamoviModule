#' Enhanced Medical Decision Tree Test Datasets
#'
#' Comprehensive collection of test datasets designed for validating enhanced medical decision tree
#' functionality. Each dataset represents different clinical scenarios, patient populations, and
#' analytical challenges commonly encountered in medical research, diagnosis, and treatment
#' decision-making.
#'
#' @name tree_datasets
#' @docType data
#' @usage data(cancer_biomarkers)
#' @usage data(cardiovascular_risk)
#' @usage data(pathology_diagnosis)
#' @usage data(drug_response)
#' @usage data(pediatric_growth)
#' @usage data(small_sample_tree)
NULL

#' Cancer Biomarker Diagnosis Dataset
#'
#' Simulated prostate cancer diagnosis dataset with comprehensive biomarker panels, patient
#' demographics, and clinical variables. Designed to test biomarker-based decision tree
#' classification, feature importance analysis, and clinical interpretation in oncology
#' diagnostics.
#'
#' @format A data frame with 500 patients and 11 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PAT_0001 to PAT_0500)}
#'   \item{PSA}{Numeric. Prostate-specific antigen level (ng/mL)}
#'   \item{age}{Integer. Patient age at diagnosis (years)}
#'   \item{tumor_size}{Numeric. Tumor size measurement (cm)}
#'   \item{grade}{Factor. Tumor grade classification ("Low", "Intermediate", "High")}
#'   \item{stage}{Factor. Cancer stage (I, II, III, IV)}
#'   \item{diagnosis}{Factor. Primary outcome - disease classification ("benign", "cancer")}
#'   \item{cohort}{Factor. Study cohort for train/test split ("discovery", "validation")}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{biopsy_gleason}{Integer. Gleason score from biopsy (6-10, cancer cases only)}
#'   \item{x_coord, y_coord}{Numeric. Spatial coordinates for autocart spatial analysis}
#' }
#'
#' @details
#' This dataset simulates a comprehensive prostate cancer biomarker study with realistic
#' distributions of PSA levels, patient demographics, and clinical characteristics. The
#' dataset includes both benign and malignant cases with appropriate biomarker patterns
#' for testing medical decision tree algorithms.
#'
#' **Clinical Context:**
#' - Prostate cancer screening and diagnosis
#' - Biomarker-based clinical decision support
#' - Multi-modal diagnostic approach combining clinical and laboratory data
#' - Risk stratification for treatment planning
#'
#' **Key Features:**
#' - Realistic PSA distributions (normal, elevated, very high)
#' - Age-appropriate patient demographics
#' - Tumor grading and staging information
#' - Discovery/validation cohort split for model testing
#' - Spatial coordinates for autocart spatial analysis
#' - Realistic missing data patterns (5-15% across variables)
#'
#' **Recommended Analysis Scenarios:**
#' - Basic decision tree classification for diagnosis
#' - Feature importance analysis for biomarker selection
#' - Cross-validation performance assessment
#' - Clinical interpretation of decision rules
#' - Risk stratification based on biomarker combinations
#' - Spatial analysis using autocart methodology
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(cancer_biomarkers)
#' 
#' # Basic tree analysis
#' result <- tree(
#'   data = cancer_biomarkers,
#'   vars = c("PSA", "age", "tumor_size"),
#'   facs = c("grade", "stage"),
#'   target = "diagnosis",
#'   targetLevel = "cancer",
#'   train = "cohort",
#'   trainLevel = "discovery",
#'   clinicalMetrics = TRUE,
#'   featureImportance = TRUE,
#'   showInterpretation = TRUE
#' )
#' 
#' # Advanced analysis with spatial coordinates
#' result_spatial <- tree(
#'   data = cancer_biomarkers,
#'   vars = c("PSA", "age"),
#'   facs = c("grade", "stage"),
#'   target = "diagnosis",
#'   targetLevel = "cancer",
#'   spatialCoords = c("x_coord", "y_coord"),
#'   useAutocart = TRUE,
#'   compareModels = TRUE
#' )
#' }
"cancer_biomarkers"

#' Cardiovascular Risk Assessment Dataset
#'
#' Simulated cardiovascular risk assessment dataset with traditional risk factors,
#' patient demographics, and 5-year cardiovascular event outcomes. Designed to test
#' risk prediction models, clinical decision support, and cardiovascular screening
#' decision trees.
#'
#' @format A data frame with 400 patients and 13 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CVD_0001 to CVD_0400)}
#'   \item{systolic_bp}{Numeric. Systolic blood pressure (mmHg)}
#'   \item{diastolic_bp}{Numeric. Diastolic blood pressure (mmHg)}
#'   \item{cholesterol}{Numeric. Total cholesterol level (mg/dL)}
#'   \item{hdl}{Numeric. HDL cholesterol level (mg/dL)}
#'   \item{ldl}{Numeric. LDL cholesterol level (mg/dL)}
#'   \item{triglycerides}{Numeric. Triglyceride level (mg/dL)}
#'   \item{bmi}{Numeric. Body mass index (kg/m²)}
#'   \item{age}{Integer. Patient age (years)}
#'   \item{smoking}{Factor. Smoking status ("Never", "Former", "Current")}
#'   \item{diabetes}{Factor. Diabetes status ("No", "Yes")}
#'   \item{family_history}{Factor. Family history of cardiovascular disease ("No", "Yes")}
#'   \item{cv_event}{Factor. Primary outcome - cardiovascular event within 5 years ("No", "Yes")}
#'   \item{study_cohort}{Factor. Study cohort ("training", "testing")}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{ethnicity}{Factor. Patient ethnicity ("White", "Black", "Hispanic", "Asian", "Other")}
#'   \item{x_coord, y_coord}{Numeric. Spatial coordinates for geographic analysis}
#' }
#'
#' @details
#' This dataset simulates a comprehensive cardiovascular risk assessment study with
#' traditional risk factors and demographic variables. The dataset follows established
#' epidemiological patterns for cardiovascular disease risk factors and outcomes.
#'
#' **Clinical Context:**
#' - Cardiovascular disease risk prediction
#' - Primary prevention screening
#' - Clinical decision support for risk stratification
#' - Population health management
#'
#' **Key Features:**
#' - Traditional Framingham Risk Score variables
#' - Realistic distributions of cardiovascular risk factors
#' - Diverse patient demographics and ethnicities
#' - Geographic variation modeling with spatial coordinates
#' - Appropriate missing data patterns (6-12% across variables)
#'
#' **Recommended Analysis Scenarios:**
#' - Risk prediction model development
#' - Clinical decision thresholds optimization
#' - Population-based risk stratification
#' - Geographic variation analysis
#' - Multi-ethnic risk assessment
#' - Bootstrap validation for confidence intervals
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(cardiovascular_risk)
#' 
#' # Risk stratification analysis
#' result <- tree(
#'   data = cardiovascular_risk,
#'   vars = c("systolic_bp", "cholesterol", "hdl", "bmi", "age"),
#'   facs = c("smoking", "diabetes", "family_history"),
#'   target = "cv_event",
#'   targetLevel = "Yes",
#'   train = "study_cohort",
#'   trainLevel = "training",
#'   clinicalContext = "screening",
#'   riskStratification = TRUE,
#'   showInterpretation = TRUE,
#'   crossValidation = TRUE
#' )
#' }
"cardiovascular_risk"

#' Pathology Diagnosis Dataset
#'
#' Simulated breast pathology diagnosis dataset with histological measurements,
#' immunohistochemistry markers, and tissue characteristics. Designed to test
#' pathology-based decision tree classification, biomarker integration, and
#' diagnostic accuracy assessment.
#'
#' @format A data frame with 300 cases and 12 variables:
#' \describe{
#'   \item{case_id}{Character. Unique case identifier (PATH_0001 to PATH_0300)}
#'   \item{cell_size}{Numeric. Cell size measurement (μm)}
#'   \item{nuclear_area}{Numeric. Nuclear area measurement (μm²)}
#'   \item{mitotic_count}{Integer. Mitotic count per high-power field}
#'   \item{pleomorphism_score}{Integer. Nuclear pleomorphism score (1-3)}
#'   \item{ki67_percentage}{Numeric. Ki-67 proliferation index (%)}
#'   \item{p53_positive}{Factor. p53 immunostaining ("Negative", "Positive")}
#'   \item{her2_status}{Factor. HER2 status ("Negative", "Positive")}
#'   \item{tumor_type}{Factor. Tumor type ("Ductal", "Lobular", "Mixed")}
#'   \item{differentiation}{Factor. Tumor differentiation ("Well", "Moderate", "Poor")}
#'   \item{malignancy}{Factor. Primary outcome - malignancy status ("Benign", "Malignant")}
#'   \item{validation_set}{Factor. Validation set ("internal", "external")}
#'   \item{patient_age}{Integer. Patient age (years)}
#'   \item{menopausal_status}{Factor. Menopausal status ("Pre", "Post")}
#'   \item{x_coord, y_coord}{Numeric. Tissue microarray coordinates}
#' }
#'
#' @details
#' This dataset simulates a comprehensive breast pathology study combining traditional
#' histological measurements with modern immunohistochemistry markers. The dataset
#' reflects realistic patterns of pathological findings in breast tissue evaluation.
#'
#' **Clinical Context:**
#' - Breast cancer pathology diagnosis
#' - Histological pattern recognition
#' - Biomarker-guided diagnosis
#' - Tissue microarray analysis
#'
#' **Key Features:**
#' - Quantitative histological measurements
#' - Immunohistochemistry marker integration
#' - Tissue microarray spatial coordinates
#' - Realistic pathological distributions
#' - Internal/external validation structure
#' - Age and menopausal status considerations
#'
#' **Recommended Analysis Scenarios:**
#' - Pathology-based diagnostic classification
#' - Biomarker importance ranking
#' - Spatial tissue analysis with autocart
#' - Internal vs external validation
#' - Multi-modal diagnostic integration
#' - Clinical threshold optimization
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(pathology_diagnosis)
#' 
#' # Pathology classification analysis
#' result <- tree(
#'   data = pathology_diagnosis,
#'   vars = c("cell_size", "nuclear_area", "mitotic_count", "ki67_percentage"),
#'   facs = c("pleomorphism_score", "p53_positive", "her2_status", "tumor_type"),
#'   target = "malignancy",
#'   targetLevel = "Malignant",
#'   train = "validation_set",
#'   trainLevel = "internal",
#'   clinicalContext = "diagnosis",
#'   featureImportance = TRUE,
#'   showInterpretation = TRUE
#' )
#' 
#' # Spatial tissue analysis
#' result_spatial <- tree(
#'   data = pathology_diagnosis,
#'   vars = c("cell_size", "nuclear_area"),
#'   facs = c("tumor_type", "differentiation"),
#'   target = "malignancy",
#'   targetLevel = "Malignant",
#'   spatialCoords = c("x_coord", "y_coord"),
#'   useAutocart = TRUE,
#'   showPartitionPlot = TRUE
#' )
#' }
"pathology_diagnosis"

#' Drug Response Prediction Dataset
#'
#' Simulated pharmacogenomics dataset with genomic biomarkers, protein levels, and
#' clinical characteristics for drug response prediction. Designed to test precision
#' medicine decision trees, biomarker integration, and treatment response modeling.
#'
#' @format A data frame with 350 patients and 13 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (DRG_0001 to DRG_0350)}
#'   \item{gene_expression_1, gene_expression_2, gene_expression_3}{Numeric. Gene expression levels}
#'   \item{protein_level_a, protein_level_b}{Numeric. Protein concentration levels}
#'   \item{mutation_status}{Factor. Mutation status ("Wild-type", "Mutant")}
#'   \item{age}{Integer. Patient age (years)}
#'   \item{performance_status}{Integer. ECOG performance status (0-2)}
#'   \item{prior_treatments}{Integer. Number of prior treatment regimens (0-3)}
#'   \item{tumor_stage}{Factor. Tumor stage (II, III, IV)}
#'   \item{histology}{Factor. Tumor histology ("Adenocarcinoma", "Squamous", "Other")}
#'   \item{drug_response}{Factor. Primary outcome - response to treatment ("Non-responder", "Responder")}
#'   \item{study_phase}{Factor. Study phase ("phase1", "phase2")}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{x_coord, y_coord}{Numeric. Spatial coordinates for multi-center analysis}
#' }
#'
#' @details
#' This dataset simulates a comprehensive pharmacogenomics study combining genomic
#' biomarkers, protein levels, and clinical variables for drug response prediction.
#' The dataset reflects realistic patterns of biomarker-response relationships in
#' precision medicine.
#'
#' **Clinical Context:**
#' - Precision medicine and personalized treatment
#' - Pharmacogenomics-guided therapy selection
#' - Biomarker-based treatment decisions
#' - Clinical trial design and analysis
#'
#' **Key Features:**
#' - Multi-omic biomarker integration
#' - Realistic genomic-clinical associations
#' - Treatment response endpoints
#' - Multi-phase study design
#' - Patient performance status considerations
#' - Geographic distribution modeling
#'
#' **Recommended Analysis Scenarios:**
#' - Biomarker-based response prediction
#' - Precision medicine decision trees
#' - Multi-omic data integration
#' - Treatment selection optimization
#' - Clinical trial endpoint analysis
#' - Bootstrap validation for biomarker stability
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(drug_response)
#' 
#' # Precision medicine analysis
#' result <- tree(
#'   data = drug_response,
#'   vars = c("gene_expression_1", "gene_expression_2", "protein_level_a", "age"),
#'   facs = c("mutation_status", "tumor_stage", "histology"),
#'   target = "drug_response",
#'   targetLevel = "Responder",
#'   train = "study_phase",
#'   trainLevel = "phase1",
#'   clinicalContext = "treatment",
#'   featureImportance = TRUE,
#'   bootstrapValidation = TRUE,
#'   showInterpretation = TRUE
#' )
#' }
"drug_response"

#' Pediatric Growth Assessment Dataset
#'
#' Simulated pediatric growth and development assessment dataset with growth
#' measurements, developmental scores, and demographic factors. Designed to test
#' pediatric-specific decision trees, growth delay prediction, and developmental
#' assessment models.
#'
#' @format A data frame with 200 children and 12 variables:
#' \describe{
#'   \item{child_id}{Character. Unique child identifier (PED_0001 to PED_0200)}
#'   \item{height_cm}{Numeric. Height measurement (cm)}
#'   \item{weight_kg}{Numeric. Weight measurement (kg)}
#'   \item{head_circumference}{Numeric. Head circumference (cm)}
#'   \item{age_months}{Integer. Age in months}
#'   \item{motor_score}{Integer. Motor development score}
#'   \item{cognitive_score}{Integer. Cognitive development score}
#'   \item{sex}{Factor. Child sex ("Male", "Female")}
#'   \item{birth_weight}{Factor. Birth weight category ("Normal", "Low", "Very Low")}
#'   \item{gestational_age}{Factor. Gestational age ("Term", "Preterm")}
#'   \item{growth_delay}{Factor. Primary outcome - growth delay status ("Normal", "Delayed")}
#'   \item{study_site}{Factor. Study site ("site_A", "site_B")}
#'   \item{maternal_age}{Integer. Maternal age at birth (years)}
#'   \item{socioeconomic_status}{Factor. Socioeconomic status ("Low", "Medium", "High")}
#'   \item{x_coord, y_coord}{Numeric. Geographic coordinates for population analysis}
#' }
#'
#' @details
#' This dataset simulates a comprehensive pediatric growth and development study
#' with anthropometric measurements, developmental assessments, and demographic
#' factors. The dataset reflects realistic patterns of pediatric growth and
#' development outcomes.
#'
#' **Clinical Context:**
#' - Pediatric growth monitoring
#' - Developmental delay screening
#' - Early childhood assessment
#' - Population health surveillance
#'
#' **Key Features:**
#' - Age-appropriate growth measurements
#' - Developmental assessment scores
#' - Birth history and demographic factors
#' - Socioeconomic considerations
#' - Multi-site study design
#' - Geographic variation modeling
#'
#' **Recommended Analysis Scenarios:**
#' - Growth delay prediction models
#' - Developmental screening tools
#' - Multi-site validation studies
#' - Socioeconomic impact analysis
#' - Age-stratified decision trees
#' - Missing data handling in pediatric studies
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(pediatric_growth)
#' 
#' # Growth delay prediction
#' result <- tree(
#'   data = pediatric_growth,
#'   vars = c("height_cm", "weight_kg", "head_circumference", "age_months"),
#'   facs = c("sex", "birth_weight", "gestational_age"),
#'   target = "growth_delay",
#'   targetLevel = "Delayed",
#'   train = "study_site",
#'   trainLevel = "site_A",
#'   clinicalContext = "screening",
#'   imputeMissing = TRUE,
#'   showInterpretation = TRUE
#' )
#' }
"pediatric_growth"

#' Small Sample Edge Case Dataset
#'
#' Minimal dataset with very small sample size designed for edge case testing,
#' validation of statistical methods with limited data, and assessment of function
#' robustness with minimal observations. Essential for testing graceful degradation
#' and error handling in medical decision tree analysis.
#'
#' @format A data frame with 25 patients and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Simple patient identifier (SM_01 to SM_25)}
#'   \item{biomarker_1, biomarker_2}{Numeric. Simple biomarker measurements}
#'   \item{age}{Integer. Patient age (years)}
#'   \item{treatment}{Factor. Treatment assignment ("A", "B")}
#'   \item{stage}{Factor. Disease stage ("Early", "Advanced")}
#'   \item{outcome}{Factor. Primary outcome ("No", "Yes")}
#'   \item{cohort}{Factor. Study cohort ("train", "test")}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{x_coord, y_coord}{Numeric. Spatial coordinates for testing}
#' }
#'
#' @details
#' This minimal dataset tests the robustness of medical decision tree analysis with
#' very small sample sizes, which can reveal edge cases in statistical calculations,
#' visualization algorithms, and clinical interpretation algorithms.
#'
#' **Clinical Context:**
#' - Rare disease studies
#' - Pilot studies and proof-of-concept
#' - Method validation with limited data
#' - Edge case testing and quality assurance
#'
#' **Key Features:**
#' - Minimal sample size (N=25)
#' - Simple variable structure
#' - Basic categorical and continuous variables
#' - Limited treatment groups
#' - Small cohort sizes for testing
#'
#' **Testing Scenarios:**
#' - Statistical method robustness with small samples
#' - Visualization algorithm edge cases
#' - Clinical interpretation with limited data
#' - Error handling and graceful degradation
#' - Minimum sample size requirements
#' - Algorithm stability testing
#'
#' **Expected Behaviors:**
#' - Appropriate handling of small sample statistics
#' - Clear visualization despite limited data
#' - Robust clinical interpretation
#' - Appropriate warnings for limited statistical power
#' - Graceful handling of edge cases
#'
#' @source Simulated data generated using create_tree_test_data.R
#' @seealso \code{\link{tree}}, \code{\link{tree_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(small_sample_tree)
#' 
#' # Edge case testing
#' result <- tree(
#'   data = small_sample_tree,
#'   vars = c("biomarker_1", "biomarker_2", "age"),
#'   facs = c("treatment", "stage"),
#'   target = "outcome",
#'   targetLevel = "Yes",
#'   train = "cohort",
#'   trainLevel = "train",
#'   clinicalMetrics = TRUE,
#'   showInterpretation = TRUE
#' )
#' }
"small_sample_tree"

#' Tree Datasets Summary
#'
#' Summary table providing comprehensive overview of all enhanced tree test datasets
#' including patient counts, variable numbers, clinical contexts, and primary usage
#' scenarios for comprehensive medical decision tree analysis validation.
#'
#' @format A data frame with 6 observations and 7 variables:
#' \describe{
#'   \item{Dataset}{Character. Dataset name}
#'   \item{N_Patients}{Integer. Number of patients/cases}
#'   \item{N_Variables}{Integer. Number of variables}
#'   \item{Description}{Character. Brief dataset description}
#'   \item{Clinical_Context}{Character. Primary clinical application area}
#'   \item{Target_Variable}{Character. Primary outcome variable name}
#'   \item{Spatial_Analysis}{Character. Spatial analysis capability}
#' }
#'
#' @details
#' This summary table provides a comprehensive reference for all enhanced tree test
#' datasets, helping users select appropriate datasets for their testing needs and
#' understand the clinical contexts and analytical capabilities of each dataset.
#'
#' **Total Collection Statistics:**
#' - 6 comprehensive test datasets
#' - 1,775 total patients across all datasets
#' - Multiple clinical contexts covered
#' - Complete coverage of medical decision tree functionality
#'
#' **Clinical Scenarios Covered:**
#' - Oncology diagnosis and biomarker selection
#' - Cardiovascular risk assessment and screening
#' - Pathology diagnosis and tissue analysis
#' - Pharmacogenomics and precision medicine
#' - Pediatric growth and development assessment
#' - Small sample and edge case scenarios
#'
#' @source Generated by create_tree_test_data.R
#' @seealso \code{\link{tree_datasets}}, \code{\link{tree}}
"tree_datasets_summary"

#' Tree Test Scenarios Documentation
#'
#' Comprehensive testing scenarios designed to validate different aspects of enhanced
#' medical decision tree functionality using the tree test datasets. Each scenario
#' targets specific analysis capabilities, visualization types, and statistical
#' methods for comprehensive validation.
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
#' medical decision tree analysis capabilities and validation requirements:
#'
#' **Core Analysis Types Tested:**
#' - **Basic Classification**: Standard decision tree diagnosis
#' - **Biomarker Selection**: Feature importance and ranking
#' - **Risk Stratification**: Clinical risk group assignment
#' - **Spatial Analysis**: Autocart spatial decision trees
#' - **Cross-Validation**: k-fold performance assessment
#' - **Bootstrap Validation**: Confidence interval estimation
#' - **Model Comparison**: Algorithm performance comparison
#' - **Clinical Interpretation**: Medical guideline generation
#' - **Missing Data Handling**: Imputation and robustness
#' - **Class Imbalance**: Rare disease and outcome handling
#' - **Feature Scaling**: Multi-scale biomarker integration
#' - **Edge Case Testing**: Small sample robustness
#'
#' **Statistical Methods Validated:**
#' - FFTrees fast-and-frugal trees
#' - Spatial autocorrelation analysis
#' - Bootstrap confidence intervals
#' - Cross-validation performance metrics
#' - Clinical performance assessment
#' - Risk stratification analysis
#'
#' **Clinical Applications Tested:**
#' - Oncology diagnosis and staging
#' - Cardiovascular risk assessment
#' - Pathology pattern recognition
#' - Pharmacogenomics treatment selection
#' - Pediatric developmental screening
#' - Edge case handling and robustness
#'
#' Each scenario includes the recommended dataset, analysis type, key variables,
#' and expected results for comprehensive validation of enhanced medical decision
#' tree analysis capabilities.
#'
#' @source Generated by create_tree_test_data.R
#' @seealso \code{\link{tree_datasets}}, \code{\link{tree}}
"tree_test_scenarios"