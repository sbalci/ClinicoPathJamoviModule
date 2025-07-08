# Test Data Generation Script for kappasizeci Function
# Creates realistic datasets for comprehensive testing of kappa sample size calculations
# Each dataset represents different research scenarios requiring interobserver agreement studies

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(2024)

# Helper function to validate proportions
validate_proportions <- function(props) {
  all(props > 0) && all(props < 1) && abs(sum(props) - 1) < 0.01
}

# Helper function to create realistic kappa ranges
create_kappa_range <- function(base_kappa, precision_width = 0.20) {
  half_width <- precision_width / 2
  kappaL <- max(0.01, base_kappa - half_width)
  kappaU <- min(0.99, base_kappa + half_width)
  
  # Ensure kappa0 is within bounds
  kappa0 <- max(kappaL, min(kappaU, base_kappa))
  
  return(list(kappa0 = kappa0, kappaL = kappaL, kappaU = kappaU))
}

# 1. Medical Diagnosis Studies - Binary Outcomes
create_medical_diagnosis_data <- function() {
  scenarios <- list(
    # Radiology screening studies
    mammography_screening = list(
      study_type = "Mammography Screening",
      description = "Breast cancer screening agreement between radiologists",
      outcome = "2",
      raters = "2",
      kappa_params = create_kappa_range(0.75),  # High agreement expected
      props = c(0.15, 0.85),  # ~15% positive findings
      alpha = 0.05,
      clinical_context = "Two radiologists independently review mammograms for suspicious findings",
      expected_agreement = "High (>0.70) due to standardized BI-RADS criteria"
    ),
    
    chest_xray_pneumonia = list(
      study_type = "Chest X-ray Pneumonia Detection",
      description = "Emergency department physicians detecting pneumonia",
      outcome = "2", 
      raters = "3",
      kappa_params = create_kappa_range(0.65),
      props = c(0.25, 0.75),  # 25% pneumonia prevalence
      alpha = 0.05,
      clinical_context = "Three emergency physicians review chest X-rays for pneumonia",
      expected_agreement = "Good (0.60-0.80) with some variability in borderline cases"
    ),
    
    dermatology_melanoma = list(
      study_type = "Dermatological Melanoma Assessment",
      description = "Dermatologists assessing melanoma risk",
      outcome = "2",
      raters = "4",
      kappa_params = create_kappa_range(0.70),
      props = c(0.08, 0.92),  # Low melanoma prevalence
      alpha = 0.05,
      clinical_context = "Four dermatologists evaluate skin lesions for melanoma risk",
      expected_agreement = "High agreement needed for critical diagnosis"
    ),
    
    # Cardiac assessments
    ecg_interpretation = list(
      study_type = "ECG Interpretation",
      description = "Cardiologists interpreting ECG abnormalities", 
      outcome = "2",
      raters = "2",
      kappa_params = create_kappa_range(0.80),
      props = c(0.35, 0.65),  # 35% abnormal ECGs
      alpha = 0.05,
      clinical_context = "Two cardiologists independently interpret ECGs for abnormalities",
      expected_agreement = "Very high agreement expected for clear abnormalities"
    ),
    
    # Psychiatric assessments
    depression_screening = list(
      study_type = "Depression Screening",
      description = "Mental health professionals screening for major depression",
      outcome = "2",
      raters = "2", 
      kappa_params = create_kappa_range(0.60, 0.25),  # More variable
      props = c(0.30, 0.70),  # 30% depression prevalence
      alpha = 0.05,
      clinical_context = "Two mental health professionals using structured interviews",
      expected_agreement = "Moderate agreement due to symptom subjectivity"
    )
  )
  
  return(scenarios)
}

# 2. Multi-Category Medical Assessments (3 categories)
create_medical_severity_data <- function() {
  scenarios <- list(
    # Pathology grading
    cancer_grading = list(
      study_type = "Cancer Grading",
      description = "Pathologists grading tumor differentiation",
      outcome = "3",
      raters = "3",
      kappa_params = create_kappa_range(0.70),
      props = c(0.20, 0.50, 0.30),  # Well, Moderate, Poor differentiation
      alpha = 0.05,
      clinical_context = "Three pathologists grade tumor specimens: Well/Moderate/Poor differentiation",
      expected_agreement = "Good agreement with standardized criteria"
    ),
    
    # Respiratory assessments
    asthma_severity = list(
      study_type = "Asthma Severity Assessment",
      description = "Pulmonologists assessing asthma severity",
      outcome = "3",
      raters = "2",
      kappa_params = create_kappa_range(0.65),
      props = c(0.40, 0.45, 0.15),  # Mild, Moderate, Severe
      alpha = 0.05,
      clinical_context = "Two pulmonologists assess asthma severity using clinical guidelines",
      expected_agreement = "Good agreement for clear cases, some variability for borderline"
    ),
    
    # Orthopedic assessments
    joint_degeneration = list(
      study_type = "Joint Degeneration Assessment",
      description = "Orthopedic surgeons grading osteoarthritis",
      outcome = "3",
      raters = "4",
      kappa_params = create_kappa_range(0.75),
      props = c(0.35, 0.40, 0.25),  # Mild, Moderate, Severe OA
      alpha = 0.05,
      clinical_context = "Four orthopedic surgeons grade knee osteoarthritis severity",
      expected_agreement = "High agreement with radiographic scoring systems"
    ),
    
    # Wound assessment
    wound_healing = list(
      study_type = "Wound Healing Assessment", 
      description = "Nurses assessing wound healing stages",
      outcome = "3",
      raters = "3",
      kappa_params = create_kappa_range(0.60),
      props = c(0.30, 0.45, 0.25),  # Early, Progressing, Advanced healing
      alpha = 0.05,
      clinical_context = "Three wound care nurses assess healing progression",
      expected_agreement = "Moderate agreement with some subjective elements"
    ),
    
    # Neurological assessments
    stroke_severity = list(
      study_type = "Stroke Severity Assessment",
      description = "Neurologists using NIHSS scale categories",
      outcome = "3", 
      raters = "2",
      kappa_params = create_kappa_range(0.80),
      props = c(0.25, 0.50, 0.25),  # Mild, Moderate, Severe stroke
      alpha = 0.05,
      clinical_context = "Two neurologists assess stroke severity using NIHSS categories",
      expected_agreement = "High agreement with standardized neurological scale"
    )
  )
  
  return(scenarios)
}

# 3. Radiological Assessments (4 categories)
create_radiological_assessment_data <- function() {
  scenarios <- list(
    # Breast imaging
    birads_assessment = list(
      study_type = "BI-RADS Assessment",
      description = "Radiologists using BI-RADS classification",
      outcome = "4",
      raters = "3",
      kappa_params = create_kappa_range(0.75),
      props = c(0.40, 0.30, 0.20, 0.10),  # BI-RADS 1,2,3,4/5
      alpha = 0.05,
      clinical_context = "Three radiologists classify mammograms using BI-RADS categories",
      expected_agreement = "High agreement with standardized BI-RADS system"
    ),
    
    # Lung imaging
    lung_nodule_assessment = list(
      study_type = "Lung Nodule Assessment",
      description = "Radiologists assessing lung nodule characteristics",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.65),
      props = c(0.35, 0.30, 0.20, 0.15),  # Benign, Probably benign, Suspicious, Malignant
      alpha = 0.05,
      clinical_context = "Two thoracic radiologists evaluate pulmonary nodules",
      expected_agreement = "Good agreement for clear cases, variability for indeterminate"
    ),
    
    # Abdominal imaging
    liver_lesion_assessment = list(
      study_type = "Liver Lesion Assessment",
      description = "Radiologists characterizing liver lesions",
      outcome = "4",
      raters = "4",
      kappa_params = create_kappa_range(0.70),
      props = c(0.25, 0.35, 0.25, 0.15),  # Cyst, Hemangioma, Focal nodular hyperplasia, Malignant
      alpha = 0.05,
      clinical_context = "Four abdominal radiologists characterize liver lesions on MRI",
      expected_agreement = "Good agreement with multiparametric imaging"
    ),
    
    # Musculoskeletal imaging
    meniscal_tear_assessment = list(
      study_type = "Meniscal Tear Assessment",
      description = "Radiologists grading meniscal tears",
      outcome = "4",
      raters = "3",
      kappa_params = create_kappa_range(0.80),
      props = c(0.20, 0.30, 0.30, 0.20),  # None, Grade 1, Grade 2, Grade 3
      alpha = 0.05,
      clinical_context = "Three musculoskeletal radiologists grade meniscal tears on MRI",
      expected_agreement = "High agreement with established grading criteria"
    ),
    
    # Cardiac imaging
    wall_motion_assessment = list(
      study_type = "Cardiac Wall Motion Assessment",
      description = "Cardiologists assessing wall motion abnormalities",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.75),
      props = c(0.40, 0.25, 0.20, 0.15),  # Normal, Hypokinetic, Akinetic, Dyskinetic
      alpha = 0.05,
      clinical_context = "Two cardiologists assess regional wall motion on echocardiography",
      expected_agreement = "High agreement for experienced readers"
    )
  )
  
  return(scenarios)
}

# 4. Psychological and Behavioral Assessments (5 categories)
create_psychological_assessment_data <- function() {
  scenarios <- list(
    # Clinical psychology
    anxiety_severity = list(
      study_type = "Anxiety Severity Rating",
      description = "Psychologists rating anxiety severity using clinical interviews",
      outcome = "5",
      raters = "2",
      kappa_params = create_kappa_range(0.60, 0.30),
      props = c(0.15, 0.20, 0.30, 0.25, 0.10),  # None, Mild, Moderate, Severe, Extreme
      alpha = 0.05,
      clinical_context = "Two clinical psychologists rate anxiety severity",
      expected_agreement = "Moderate agreement due to subjective nature of assessment"
    ),
    
    # Child psychology
    autism_severity = list(
      study_type = "Autism Severity Assessment",
      description = "Psychologists using ADOS severity ratings",
      outcome = "5",
      raters = "3",
      kappa_params = create_kappa_range(0.75),
      props = c(0.20, 0.15, 0.25, 0.25, 0.15),  # No evidence, Minimal, Mild, Moderate, Severe
      alpha = 0.05,
      clinical_context = "Three child psychologists use ADOS for autism severity rating",
      expected_agreement = "Good agreement with standardized assessment tool"
    ),
    
    # Behavioral assessment
    aggression_rating = list(
      study_type = "Aggression Behavior Rating",
      description = "Observers rating aggressive behavior in children",
      outcome = "5",
      raters = "4",
      kappa_params = create_kappa_range(0.65),
      props = c(0.25, 0.25, 0.25, 0.15, 0.10),  # None, Minimal, Mild, Moderate, Severe
      alpha = 0.05,
      clinical_context = "Four trained observers rate aggressive behavior during play sessions",
      expected_agreement = "Good inter-rater reliability with training"
    ),
    
    # Cognitive assessment
    dementia_staging = list(
      study_type = "Dementia Staging",
      description = "Neuropsychologists staging dementia severity",
      outcome = "5",
      raters = "2",
      kappa_params = create_kappa_range(0.70),
      props = c(0.10, 0.20, 0.30, 0.25, 0.15),  # Normal, MCI, Mild, Moderate, Severe dementia
      alpha = 0.05,
      clinical_context = "Two neuropsychologists stage dementia using clinical criteria",
      expected_agreement = "Good agreement for established staging criteria"
    ),
    
    # Pain assessment
    pain_intensity = list(
      study_type = "Pain Intensity Assessment",
      description = "Nurses rating patient pain using behavioral indicators",
      outcome = "5",
      raters = "3",
      kappa_params = create_kappa_range(0.55, 0.35),
      props = c(0.20, 0.25, 0.25, 0.20, 0.10),  # No pain, Mild, Moderate, Severe, Extreme
      alpha = 0.05,
      clinical_context = "Three nurses assess pain in non-verbal patients using behavioral scales",
      expected_agreement = "Moderate agreement due to subjective nature of pain assessment"
    )
  )
  
  return(scenarios)
}

# 5. Quality Control and Performance Assessment Data
create_quality_control_data <- function() {
  scenarios <- list(
    # Manufacturing quality
    product_quality = list(
      study_type = "Product Quality Assessment",
      description = "Quality inspectors evaluating product defects",
      outcome = "3",
      raters = "4",
      kappa_params = create_kappa_range(0.85),
      props = c(0.70, 0.25, 0.05),  # Acceptable, Minor defect, Major defect
      alpha = 0.05,
      clinical_context = "Four quality inspectors assess manufactured products",
      expected_agreement = "Very high agreement expected for objective criteria"
    ),
    
    # Food safety
    food_safety_rating = list(
      study_type = "Food Safety Assessment",
      description = "Health inspectors rating restaurant compliance",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.80),
      props = c(0.15, 0.25, 0.35, 0.25),  # Poor, Fair, Good, Excellent
      alpha = 0.05,
      clinical_context = "Two health inspectors assess restaurant food safety compliance",
      expected_agreement = "High agreement with standardized inspection criteria"
    ),
    
    # Educational assessment
    teaching_effectiveness = list(
      study_type = "Teaching Effectiveness Rating",
      description = "Administrators rating teacher performance",
      outcome = "5",
      raters = "3",
      kappa_params = create_kappa_range(0.65),
      props = c(0.05, 0.15, 0.35, 0.35, 0.10),  # Unsatisfactory, Developing, Proficient, Accomplished, Distinguished
      alpha = 0.05,
      clinical_context = "Three administrators rate teacher effectiveness using rubrics",
      expected_agreement = "Good agreement with detailed evaluation rubrics"
    ),
    
    # Customer service
    service_quality = list(
      study_type = "Customer Service Quality",
      description = "Supervisors rating customer service interactions",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.70),
      props = c(0.10, 0.20, 0.45, 0.25),  # Poor, Fair, Good, Excellent
      alpha = 0.05,
      clinical_context = "Two supervisors rate customer service call quality",
      expected_agreement = "Good agreement with structured evaluation criteria"
    ),
    
    # Research quality
    research_validity = list(
      study_type = "Research Methodology Assessment",
      description = "Reviewers assessing research study quality",
      outcome = "3",
      raters = "5",
      kappa_params = create_kappa_range(0.60),
      props = c(0.20, 0.50, 0.30),  # Low, Moderate, High quality
      alpha = 0.05,
      clinical_context = "Five experts assess research methodology quality",
      expected_agreement = "Moderate agreement due to subjective evaluation elements"
    )
  )
  
  return(scenarios)
}

# 6. Content Analysis and Communication Studies
create_content_analysis_data <- function() {
  scenarios <- list(
    # Media content analysis
    news_sentiment = list(
      study_type = "News Sentiment Analysis",
      description = "Researchers coding sentiment in news articles",
      outcome = "3",
      raters = "3",
      kappa_params = create_kappa_range(0.70),
      props = c(0.20, 0.50, 0.30),  # Negative, Neutral, Positive
      alpha = 0.05,
      clinical_context = "Three researchers code sentiment in political news coverage",
      expected_agreement = "Good agreement with clear coding guidelines"
    ),
    
    # Social media analysis
    social_media_emotion = list(
      study_type = "Social Media Emotion Classification",
      description = "Analysts classifying emotional content in posts",
      outcome = "5",
      raters = "2",
      kappa_params = create_kappa_range(0.55),
      props = c(0.15, 0.25, 0.30, 0.20, 0.10),  # Anger, Sadness, Neutral, Joy, Surprise
      alpha = 0.05,
      clinical_context = "Two analysts classify emotions in social media posts",
      expected_agreement = "Moderate agreement due to contextual interpretation"
    ),
    
    # Educational content
    learning_objective_coding = list(
      study_type = "Learning Objective Classification",
      description = "Educators coding cognitive complexity of learning objectives",
      outcome = "4",
      raters = "4",
      kappa_params = create_kappa_range(0.75),
      props = c(0.30, 0.35, 0.25, 0.10),  # Remember, Understand, Apply, Analyze
      alpha = 0.05,
      clinical_context = "Four educators classify learning objectives using Bloom's taxonomy",
      expected_agreement = "High agreement with established taxonomic framework"
    ),
    
    # Interview analysis
    interview_theme_coding = list(
      study_type = "Qualitative Interview Coding",
      description = "Researchers coding themes in interview transcripts",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.60, 0.30),
      props = c(0.25, 0.30, 0.25, 0.20),  # Theme A, B, C, D
      alpha = 0.05,
      clinical_context = "Two researchers independently code interview themes",
      expected_agreement = "Moderate agreement typical for qualitative coding"
    ),
    
    # Document classification
    legal_document_classification = list(
      study_type = "Legal Document Classification",
      description = "Legal clerks classifying document types",
      outcome = "5",
      raters = "3",
      kappa_params = create_kappa_range(0.80),
      props = c(0.25, 0.20, 0.20, 0.20, 0.15),  # Contract, Motion, Brief, Order, Other
      alpha = 0.05,
      clinical_context = "Three legal clerks classify document types for case management",
      expected_agreement = "High agreement with clear document type definitions"
    )
  )
  
  return(scenarios)
}

# 7. Precision Medicine and Biomarker Studies  
create_precision_medicine_data <- function() {
  scenarios <- list(
    # Genomic classification
    mutation_classification = list(
      study_type = "Genetic Mutation Classification",
      description = "Geneticists classifying mutation pathogenicity",
      outcome = "5",
      raters = "3",
      kappa_params = create_kappa_range(0.70),
      props = c(0.20, 0.25, 0.30, 0.15, 0.10),  # Benign, Likely benign, VUS, Likely pathogenic, Pathogenic
      alpha = 0.05,
      clinical_context = "Three clinical geneticists classify variants using ACMG guidelines",
      expected_agreement = "Good agreement with standardized classification criteria"
    ),
    
    # Immunohistochemistry
    ihc_scoring = list(
      study_type = "Immunohistochemistry Scoring",
      description = "Pathologists scoring protein expression",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.75),
      props = c(0.25, 0.30, 0.25, 0.20),  # 0, 1+, 2+, 3+ expression
      alpha = 0.05,
      clinical_context = "Two pathologists score HER2 expression in breast cancer",
      expected_agreement = "High agreement with standardized scoring systems"
    ),
    
    # Flow cytometry
    cell_population_identification = list(
      study_type = "Flow Cytometry Analysis",
      description = "Laboratory technologists identifying cell populations",
      outcome = "3",
      raters = "4",
      kappa_params = create_kappa_range(0.80),
      props = c(0.15, 0.35, 0.50),  # Negative, Low positive, High positive
      alpha = 0.05,
      clinical_context = "Four technologists analyze flow cytometry data for cell markers",
      expected_agreement = "High agreement with objective gating criteria"
    ),
    
    # Radiogenomics
    imaging_biomarker = list(
      study_type = "Imaging Biomarker Assessment",
      description = "Radiologists assessing imaging biomarkers",
      outcome = "3",
      raters = "3",
      kappa_params = create_kappa_range(0.65),
      props = c(0.40, 0.35, 0.25),  # Low, Intermediate, High biomarker expression
      alpha = 0.05,
      clinical_context = "Three radiologists assess imaging-based biomarker signatures",
      expected_agreement = "Good agreement with quantitative imaging metrics"
    ),
    
    # Pharmacogenomics
    drug_response_prediction = list(
      study_type = "Drug Response Classification",
      description = "Pharmacologists predicting drug response from genomic data",
      outcome = "4",
      raters = "2",
      kappa_params = create_kappa_range(0.70),
      props = c(0.20, 0.30, 0.30, 0.20),  # Poor, Intermediate, Normal, Rapid metabolizer
      alpha = 0.05,
      clinical_context = "Two pharmacologists predict drug metabolism based on genotype",
      expected_agreement = "Good agreement with established pharmacogenomic guidelines"
    )
  )
  
  return(scenarios)
}

# Function to consolidate all scenarios into structured datasets
consolidate_all_scenarios <- function() {
  
  all_scenarios <- list(
    medical_diagnosis = create_medical_diagnosis_data(),
    medical_severity = create_medical_severity_data(),
    radiological_assessment = create_radiological_assessment_data(),
    psychological_assessment = create_psychological_assessment_data(),
    quality_control = create_quality_control_data(),
    content_analysis = create_content_analysis_data(),
    precision_medicine = create_precision_medicine_data()
  )
  
  # Convert to structured data frames
  consolidated_data <- list()
  
  for (domain in names(all_scenarios)) {
    domain_scenarios <- all_scenarios[[domain]]
    
    for (scenario_name in names(domain_scenarios)) {
      scenario <- domain_scenarios[[scenario_name]]
      
      # Create data frame row
      scenario_df <- tibble(
        domain = domain,
        scenario_id = scenario_name,
        study_type = scenario$study_type,
        description = scenario$description,
        outcome_categories = as.numeric(scenario$outcome),
        raters = as.numeric(scenario$raters),
        kappa0 = scenario$kappa_params$kappa0,
        kappaL = scenario$kappa_params$kappaL,
        kappaU = scenario$kappa_params$kappaU,
        precision_width = scenario$kappa_params$kappaU - scenario$kappa_params$kappaL,
        proportions = paste(scenario$props, collapse = ", "),
        alpha = scenario$alpha,
        clinical_context = scenario$clinical_context,
        expected_agreement = scenario$expected_agreement
      )
      
      consolidated_data[[paste(domain, scenario_name, sep = "_")]] <- scenario_df
    }
  }
  
  # Combine all scenarios into single data frame
  final_dataset <- do.call(rbind, consolidated_data)
  
  return(final_dataset)
}

# Function to create parameter validation test cases
create_validation_test_cases <- function() {
  
  validation_cases <- tibble(
    test_case = c(
      "valid_binary", "valid_three_cat", "valid_four_cat", "valid_five_cat",
      "invalid_kappa0_high", "invalid_kappa0_low", "invalid_kappaL_high", 
      "invalid_kappaU_low", "invalid_bounds", "invalid_props_count",
      "invalid_props_sum", "invalid_alpha_high", "invalid_alpha_low",
      "invalid_outcome", "invalid_raters", "edge_case_minimal", 
      "edge_case_maximal", "precision_high", "precision_low"
    ),
    
    outcome = c(
      "2", "3", "4", "5",
      "2", "2", "2", "2", "2", "3",
      "2", "2", "2", "6", "2", "2",
      "2", "2", "2"
    ),
    
    kappa0 = c(
      0.60, 0.50, 0.65, 0.55,
      1.5, -0.1, 0.60, 0.60, 0.60, 0.50,
      0.60, 0.60, 0.60, 0.60, 0.60, 0.01,
      0.99, 0.60, 0.60
    ),
    
    kappaL = c(
      0.40, 0.30, 0.45, 0.35,
      0.40, 0.40, 1.2, 0.40, 0.80, 0.30,
      0.40, 0.40, 0.40, 0.40, 0.40, 0.005,
      0.95, 0.58, 0.20
    ),
    
    kappaU = c(
      0.80, 0.70, 0.85, 0.75,
      0.80, 0.80, 0.80, -0.2, 0.70, 0.70,
      0.80, 0.80, 0.80, 0.80, 0.80, 0.015,
      0.995, 0.62, 1.00
    ),
    
    props = c(
      "0.30, 0.70", "0.25, 0.35, 0.40", "0.20, 0.25, 0.30, 0.25", "0.15, 0.20, 0.25, 0.25, 0.15",
      "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.25, 0.75",
      "0.30, 0.80", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.50, 0.50",
      "0.05, 0.95", "0.45, 0.55", "0.10, 0.90"
    ),
    
    raters = c(
      "2", "3", "3", "4",
      "2", "2", "2", "2", "2", "3",
      "2", "2", "2", "2", "7", "2",
      "5", "2", "2"
    ),
    
    alpha = c(
      0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
      0.05, 1.5, -0.01, 0.05, 0.05, 0.05,
      0.01, 0.05, 0.05
    ),
    
    expected_result = c(
      "valid", "valid", "valid", "valid",
      "error", "error", "error", "error", "error", "error",
      "error", "error", "error", "error", "error", "valid",
      "valid", "valid", "valid"
    ),
    
    error_type = c(
      NA, NA, NA, NA,
      "kappa0_out_of_range", "kappa0_out_of_range", "kappaL_out_of_range", 
      "kappaU_out_of_range", "invalid_confidence_bounds", "props_count_mismatch",
      "props_sum_invalid", "alpha_out_of_range", "alpha_out_of_range",
      "invalid_outcome_categories", "invalid_rater_count", NA,
      NA, NA, NA
    )
  )
  
  return(validation_cases)
}

# Generate all datasets
message("Generating kappasizeci test datasets...")

# Create comprehensive scenario dataset
kappa_scenarios_comprehensive <- consolidate_all_scenarios()

# Create validation test cases
kappa_validation_cases <- create_validation_test_cases()

# Create statistical accuracy test cases
kappa_statistical_cases <- tibble(
  test_name = c(
    "high_precision_small_sample", "low_precision_large_sample",
    "many_raters_efficiency", "few_raters_larger_sample",
    "balanced_proportions", "unbalanced_proportions",
    "high_kappa_estimate", "low_kappa_estimate",
    "strict_alpha", "lenient_alpha"
  ),
  
  description = c(
    "High precision requirement should increase sample size",
    "Low precision requirement should reduce sample size",
    "More raters should reduce required sample size",
    "Fewer raters should increase required sample size", 
    "Balanced proportions baseline scenario",
    "Unbalanced proportions may affect sample size",
    "High kappa values with narrow confidence intervals",
    "Low kappa values with appropriate intervals",
    "Strict significance level increases sample size",
    "Lenient significance level reduces sample size"
  ),
  
  outcome = c("2", "2", "2", "2", "3", "3", "2", "2", "2", "2"),
  kappa0 = c(0.70, 0.70, 0.60, 0.60, 0.60, 0.60, 0.85, 0.25, 0.60, 0.60),
  kappaL = c(0.68, 0.50, 0.50, 0.50, 0.50, 0.50, 0.80, 0.15, 0.50, 0.50),
  kappaU = c(0.72, 0.90, 0.70, 0.70, 0.70, 0.70, 0.90, 0.35, 0.70, 0.70),
  props = c(
    "0.50, 0.50", "0.50, 0.50", "0.50, 0.50", "0.50, 0.50",
    "0.33, 0.33, 0.34", "0.10, 0.20, 0.70", "0.20, 0.80", "0.50, 0.50",
    "0.50, 0.50", "0.50, 0.50"
  ),
  raters = c("2", "2", "5", "2", "3", "3", "2", "2", "2", "2"),
  alpha = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.01, 0.20),
  expected_relationship = c(
    "smaller_sample_than_low_precision", "larger_sample_than_high_precision",
    "smaller_sample_than_few_raters", "larger_sample_than_many_raters",
    "baseline_comparison", "different_from_balanced",
    "specific_sample_size", "specific_sample_size",
    "larger_sample_than_lenient", "smaller_sample_than_strict"
  )
)

# Save datasets to package data directory (if it exists)
if (dir.exists("data")) {
  save(kappa_scenarios_comprehensive, file = "data/kappasizeci_scenarios_comprehensive.rda")
  save(kappa_validation_cases, file = "data/kappasizeci_validation_cases.rda")
  save(kappa_statistical_cases, file = "data/kappasizeci_statistical_cases.rda")
  
  message("✓ All kappa test datasets saved to data/ directory")
} else {
  message("Note: data/ directory not found. Datasets created in memory only.")
}

# Print dataset summaries
message("\n=== Dataset Summaries ===")

message("\n1. Comprehensive Scenarios Dataset:")
message(sprintf("   - Total scenarios: %d", nrow(kappa_scenarios_comprehensive)))
message(sprintf("   - Research domains: %d", length(unique(kappa_scenarios_comprehensive$domain))))
message(sprintf("   - Outcome categories covered: %s", paste(sort(unique(kappa_scenarios_comprehensive$outcome_categories)), collapse = ", ")))
message(sprintf("   - Rater combinations: %s", paste(sort(unique(kappa_scenarios_comprehensive$raters)), collapse = ", ")))

# Domain breakdown
domain_summary <- kappa_scenarios_comprehensive %>%
  group_by(domain) %>%
  summarise(
    scenarios = n(),
    avg_kappa0 = round(mean(kappa0), 3),
    avg_precision = round(mean(precision_width), 3),
    .groups = "drop"
  )

message("\nDomain breakdown:")
for (i in 1:nrow(domain_summary)) {
  message(sprintf("   %s: %d scenarios (avg κ₀=%.3f, avg precision=%.3f)", 
                 domain_summary$domain[i], 
                 domain_summary$scenarios[i],
                 domain_summary$avg_kappa0[i], 
                 domain_summary$avg_precision[i]))
}

message("\n2. Validation Test Cases:")
message(sprintf("   - Total test cases: %d", nrow(kappa_validation_cases)))
valid_cases <- sum(kappa_validation_cases$expected_result == "valid")
error_cases <- sum(kappa_validation_cases$expected_result == "error")
message(sprintf("   - Valid parameter combinations: %d", valid_cases))
message(sprintf("   - Invalid parameter combinations: %d", error_cases))

message("\n3. Statistical Accuracy Cases:")
message(sprintf("   - Relationship test scenarios: %d", nrow(kappa_statistical_cases)))
message(sprintf("   - Parameter combinations tested: %s", paste(unique(kappa_statistical_cases$expected_relationship), collapse = ", ")))

message("\n=== Usage Examples ===")
message("# Basic kappa sample size calculation")
message("kappaSizeCI(outcome='2', kappa0=0.60, kappaL=0.40, kappaU=0.80, props='0.30, 0.70', raters='2', alpha=0.05)")
message("")
message("# Medical diagnosis study")
message("kappaSizeCI(outcome='2', kappa0=0.75, kappaL=0.65, kappaU=0.85, props='0.15, 0.85', raters='2', alpha=0.05)")
message("")
message("# Multi-category assessment")
message("kappaSizeCI(outcome='4', kappa0=0.70, kappaL=0.55, kappaU=0.85, props='0.25, 0.30, 0.25, 0.20', raters='3', alpha=0.05)")
message("")
message("# High precision study")
message("kappaSizeCI(outcome='3', kappa0=0.65, kappaL=0.62, kappaU=0.68, props='0.33, 0.33, 0.34', raters='4', alpha=0.01)")

message("\n✓ kappasizeci test data generation completed successfully!")