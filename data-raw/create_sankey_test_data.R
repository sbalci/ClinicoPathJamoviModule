# Create test data for jjsankeyfier function
# This script generates comprehensive datasets for testing sankey and alluvial diagrams

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(123)

# Load required libraries
library(dplyr)

# 1. Simple Source-Target Flow Data
sankey_simple_data <- data.frame(
  source = rep(c("A", "B", "C", "D"), each = 4),
  target = rep(c("X", "Y", "Z", "W"), 4),
  flow_value = c(45, 30, 15, 10, 25, 40, 20, 15, 35, 25, 30, 10, 20, 15, 25, 40),
  group = rep(c("Group1", "Group2"), each = 8),
  time_period = rep(c("Q1", "Q2", "Q3", "Q4"), 4)
)

# 2. Multi-Node Alluvial Data (Treatment Pathways)
treatment_pathways_data <- data.frame(
  patient_id = 1:200,
  initial_diagnosis = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 200, 
                           replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  first_treatment = sample(c("Surgery", "Chemotherapy", "Radiation", "Immunotherapy"), 200, 
                         replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 200,
                   replace = TRUE, prob = c(0.3, 0.35, 0.2, 0.15)),
  final_outcome = sample(c("Remission", "Relapse", "Stable", "Death"), 200,
                        replace = TRUE, prob = c(0.4, 0.25, 0.25, 0.1)),
  patient_count = 1,  # Each patient counts as 1 for flow calculation
  age_group = sample(c("18-40", "41-65", "65+"), 200, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  time_to_outcome = sample(1:24, 200, replace = TRUE)  # months
)

# 3. Educational Pathway Data (Academic Progress)
education_pathways_data <- data.frame(
  student_id = 1:150,
  high_school_type = sample(c("Public", "Private", "Charter"), 150, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
  college_admission = sample(c("State University", "Private College", "Community College", "No College"), 150,
                           replace = TRUE, prob = c(0.35, 0.2, 0.3, 0.15)),
  major_category = sample(c("STEM", "Liberal Arts", "Business", "Other", "N/A"), 150,
                        replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.15, 0.1)),
  graduation_status = sample(c("Graduated", "Dropped Out", "Transferred", "Still Enrolled"), 150,
                           replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.1)),
  student_count = 1,
  socioeconomic_status = sample(c("Low", "Middle", "High"), 150, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  year_cohort = sample(2015:2020, 150, replace = TRUE)
)

# 4. Business Process Flow Data
business_process_data <- data.frame(
  process_id = 1:300,
  department_start = sample(c("Sales", "Marketing", "Operations", "Support"), 300, replace = TRUE),
  process_step_1 = sample(c("Initial Contact", "Proposal", "Demo", "Follow-up"), 300, replace = TRUE),
  process_step_2 = sample(c("Negotiation", "Technical Review", "Legal Review", "Approval"), 300, replace = TRUE),
  final_outcome = sample(c("Closed Won", "Closed Lost", "On Hold", "Cancelled"), 300,
                        replace = TRUE, prob = c(0.35, 0.4, 0.15, 0.1)),
  process_value = round(runif(300, 1000, 50000), 0),  # Dollar amounts
  region = sample(c("North", "South", "East", "West"), 300, replace = TRUE),
  quarter = sample(c("Q1", "Q2", "Q3", "Q4"), 300, replace = TRUE)
)

# 5. Technology Migration Data
tech_migration_data <- data.frame(
  system_id = 1:100,
  legacy_system = sample(c("System A", "System B", "System C", "Manual Process"), 100, replace = TRUE),
  migration_phase_1 = sample(c("Assessment", "Planning", "Pilot", "Skip"), 100, replace = TRUE),
  migration_phase_2 = sample(c("Development", "Testing", "Training", "Hold"), 100, replace = TRUE),
  implementation = sample(c("Full Deployment", "Partial Deployment", "Rollback", "Cancelled"), 100,
                        replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1)),
  migration_cost = round(runif(100, 5000, 100000), 0),
  business_unit = sample(c("Finance", "HR", "Operations", "IT"), 100, replace = TRUE),
  migration_year = sample(2020:2024, 100, replace = TRUE)
)

# 6. Simple source-target for basic testing
simple_flow_data <- data.frame(
  from_node = c("A", "A", "B", "B", "C", "C", "D", "D"),
  to_node = c("X", "Y", "X", "Z", "Y", "Z", "X", "Y"),
  flow_amount = c(100, 50, 75, 25, 40, 60, 30, 45),
  category = c("Type1", "Type2", "Type1", "Type2", "Type1", "Type2", "Type1", "Type2")
)

# 7. Complex multi-level flow data
complex_alluvial_data <- data.frame(
  record_id = 1:500,
  level_1 = sample(c("Region_A", "Region_B", "Region_C"), 500, replace = TRUE),
  level_2 = sample(c("Product_X", "Product_Y", "Product_Z"), 500, replace = TRUE),
  level_3 = sample(c("Channel_Online", "Channel_Retail", "Channel_Direct"), 500, replace = TRUE),
  level_4 = sample(c("Customer_New", "Customer_Existing", "Customer_Returning"), 500, replace = TRUE),
  revenue = round(runif(500, 100, 10000), 2),
  customer_segment = sample(c("Enterprise", "SMB", "Individual"), 500, replace = TRUE),
  time_quarter = sample(c("2023Q1", "2023Q2", "2023Q3", "2023Q4"), 500, replace = TRUE)
)

# Save all datasets
save(sankey_simple_data, file = "data/sankey_simple_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(sankey_simple_data, "data/sankey_simple_data.omv")
  message("✓ Created sankey_simple_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(sankey_simple_data, "data/sankey_simple_data.omv")
  message("✓ Created sankey_simple_data.omv")
}
save(treatment_pathways_data, file = "data/treatment_pathways_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatment_pathways_data, "data/treatment_pathways_data.omv")
  message("✓ Created treatment_pathways_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatment_pathways_data, "data/treatment_pathways_data.omv")
  message("✓ Created treatment_pathways_data.omv")
}
save(education_pathways_data, file = "data/education_pathways_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(education_pathways_data, "data/education_pathways_data.omv")
  message("✓ Created education_pathways_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(education_pathways_data, "data/education_pathways_data.omv")
  message("✓ Created education_pathways_data.omv")
}
save(business_process_data, file = "data/business_process_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(business_process_data, "data/business_process_data.omv")
  message("✓ Created business_process_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(business_process_data, "data/business_process_data.omv")
  message("✓ Created business_process_data.omv")
}
save(tech_migration_data, file = "data/tech_migration_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(tech_migration_data, "data/tech_migration_data.omv")
  message("✓ Created tech_migration_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(tech_migration_data, "data/tech_migration_data.omv")
  message("✓ Created tech_migration_data.omv")
}
save(simple_flow_data, file = "data/simple_flow_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(simple_flow_data, "data/simple_flow_data.omv")
  message("✓ Created simple_flow_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(simple_flow_data, "data/simple_flow_data.omv")
  message("✓ Created simple_flow_data.omv")
}
save(complex_alluvial_data, file = "data/complex_alluvial_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(complex_alluvial_data, "data/complex_alluvial_data.omv")
  message("✓ Created complex_alluvial_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(complex_alluvial_data, "data/complex_alluvial_data.omv")
  message("✓ Created complex_alluvial_data.omv")
}

# Create combined dataset for comprehensive testing
sankey_comprehensive_data <- list(
  simple = sankey_simple_data,
  treatment = treatment_pathways_data,
  education = education_pathways_data,
  business = business_process_data,
  technology = tech_migration_data,
  basic_flow = simple_flow_data,
  complex_alluvial = complex_alluvial_data
)

save(sankey_comprehensive_data, file = "data/sankey_comprehensive_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(sankey_comprehensive_data, "data/sankey_comprehensive_data.omv")
  message("✓ Created sankey_comprehensive_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(sankey_comprehensive_data, "data/sankey_comprehensive_data.omv")
  message("✓ Created sankey_comprehensive_data.omv")
}

message("Successfully created comprehensive test datasets for jjsankeyfier function:")
message("- sankey_simple_data.rda")
message("- treatment_pathways_data.rda") 
message("- education_pathways_data.rda")
message("- business_process_data.rda")
message("- tech_migration_data.rda")
message("- simple_flow_data.rda")
message("- complex_alluvial_data.rda")
message("- sankey_comprehensive_data.rda")
