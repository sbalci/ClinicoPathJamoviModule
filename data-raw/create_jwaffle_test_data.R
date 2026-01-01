# Test Data Generation Script for jwaffle Function
# Creates diverse, realistic datasets for comprehensive testing of waffle chart functionality
# Each dataset represents different real-world scenarios and use cases

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(42)

# Function to create balanced categorical data
create_balanced_categories <- function(categories, n, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, length(categories))
  }
  sample(categories, n, replace = TRUE, prob = weights)
}

# 1. Market Research Data - Product Market Share Analysis
create_market_research_data <- function() {
  n_responses <- 1500
  
  market_data <- tibble(
    # Core variables
    preferred_brand = factor(create_balanced_categories(
      c("Brand_A", "Brand_B", "Brand_C", "Brand_D", "Brand_E"),
      n_responses,
      c(0.35, 0.25, 0.20, 0.15, 0.05)
    )),
    
    purchase_count = sample(1:12, n_responses, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.1, 0.05, 0.05, rep(0.01, 6))),
    
    # Demographic segmentation
    age_group = factor(create_balanced_categories(
      c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"),
      n_responses,
      c(0.15, 0.25, 0.25, 0.20, 0.10, 0.05)
    )),
    
    income_bracket = factor(create_balanced_categories(
      c("Under_30K", "30K-50K", "50K-75K", "75K-100K", "Over_100K"),
      n_responses,
      c(0.15, 0.25, 0.30, 0.20, 0.10)
    )),
    
    # Geographic variables
    region = factor(create_balanced_categories(
      c("North_America", "Europe", "Asia_Pacific", "Latin_America", "Middle_East_Africa"),
      n_responses,
      c(0.35, 0.30, 0.20, 0.10, 0.05)
    )),
    
    urban_rural = factor(create_balanced_categories(
      c("Urban", "Suburban", "Rural"),
      n_responses,
      c(0.55, 0.35, 0.10)
    )),
    
    # Purchase behavior
    channel_preference = factor(create_balanced_categories(
      c("Online", "In_Store", "Mobile_App", "Phone"),
      n_responses,
      c(0.45, 0.35, 0.15, 0.05)
    )),
    
    loyalty_status = factor(create_balanced_categories(
      c("New_Customer", "Returning", "Loyal", "Premium"),
      n_responses,
      c(0.25, 0.35, 0.30, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic brand-age interactions
      preferred_brand = case_when(
        age_group %in% c("18-25", "26-35") & runif(n()) < 0.3 ~ "Brand_D",
        age_group %in% c("56-65", "65+") & runif(n()) < 0.4 ~ "Brand_A",
        TRUE ~ preferred_brand
      ),
      
      # Adjust purchase counts based on income
      purchase_count = case_when(
        income_bracket == "Over_100K" ~ pmax(purchase_count, sample(3:12, n(), replace = TRUE)),
        income_bracket == "Under_30K" ~ pmin(purchase_count, sample(1:6, n(), replace = TRUE)),
        TRUE ~ purchase_count
      )
    )
  
  return(market_data)
}

# 2. Survey Response Data - Employee Satisfaction Study
create_survey_response_data <- function() {
  n_employees <- 800
  
  survey_data <- tibble(
    # Satisfaction ratings
    job_satisfaction = factor(create_balanced_categories(
      c("Very_Satisfied", "Satisfied", "Neutral", "Dissatisfied", "Very_Dissatisfied"),
      n_employees,
      c(0.20, 0.35, 0.25, 0.15, 0.05)
    ), levels = c("Very_Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very_Satisfied")),
    
    work_life_balance = factor(create_balanced_categories(
      c("Excellent", "Good", "Fair", "Poor"),
      n_employees,
      c(0.15, 0.45, 0.30, 0.10)
    ), levels = c("Poor", "Fair", "Good", "Excellent")),
    
    # Employee demographics
    department = factor(create_balanced_categories(
      c("Engineering", "Sales", "Marketing", "HR", "Finance", "Operations", "Customer_Service"),
      n_employees,
      c(0.25, 0.20, 0.15, 0.08, 0.10, 0.12, 0.10)
    )),
    
    tenure_years = sample(0:15, n_employees, replace = TRUE, prob = c(0.2, 0.15, 0.12, 0.1, rep(0.05, 8), rep(0.02, 4))),
    
    employment_type = factor(create_balanced_categories(
      c("Full_Time", "Part_Time", "Contract", "Intern"),
      n_employees,
      c(0.75, 0.15, 0.08, 0.02)
    )),
    
    # Performance metrics
    performance_rating = factor(create_balanced_categories(
      c("Exceeds", "Meets", "Below"),
      n_employees,
      c(0.25, 0.65, 0.10)
    )),
    
    training_completed = sample(0:8, n_employees, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.03, 0.02)),
    
    # Workplace factors
    remote_work = factor(create_balanced_categories(
      c("Fully_Remote", "Hybrid", "On_Site"),
      n_employees,
      c(0.30, 0.45, 0.25)
    )),
    
    office_location = factor(create_balanced_categories(
      c("Headquarters", "Regional_Office", "Satellite", "Home_Office"),
      n_employees,
      c(0.40, 0.30, 0.20, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic satisfaction-tenure relationships
      job_satisfaction = case_when(
        tenure_years < 1 & runif(n()) < 0.3 ~ sample(c("Neutral", "Dissatisfied"), n(), replace = TRUE),
        tenure_years > 10 & performance_rating == "Exceeds" & runif(n()) < 0.6 ~ "Very_Satisfied",
        TRUE ~ job_satisfaction
      ),
      
      # Remote work affects work-life balance
      work_life_balance = case_when(
        remote_work == "Fully_Remote" & runif(n()) < 0.4 ~ sample(c("Excellent", "Good"), n(), replace = TRUE),
        remote_work == "On_Site" & runif(n()) < 0.2 ~ sample(c("Fair", "Poor"), n(), replace = TRUE),
        TRUE ~ work_life_balance
      )
    )
  
  return(survey_data)
}

# 3. Social Media Engagement Data - Content Performance Analysis
create_social_media_data <- function() {
  n_posts <- 600
  
  social_data <- tibble(
    # Content performance
    engagement_level = factor(create_balanced_categories(
      c("High", "Medium", "Low", "Very_Low"),
      n_posts,
      c(0.15, 0.35, 0.35, 0.15)
    ), levels = c("Very_Low", "Low", "Medium", "High")),
    
    engagement_count = case_when(
      engagement_level == "High" ~ sample(500:2000, n_posts, replace = TRUE),
      engagement_level == "Medium" ~ sample(100:500, n_posts, replace = TRUE),
      engagement_level == "Low" ~ sample(20:100, n_posts, replace = TRUE),
      engagement_level == "Very_Low" ~ sample(0:20, n_posts, replace = TRUE)
    ),
    
    # Content characteristics
    content_type = factor(create_balanced_categories(
      c("Video", "Image", "Text", "Link", "Poll", "Story"),
      n_posts,
      c(0.30, 0.25, 0.20, 0.10, 0.10, 0.05)
    )),
    
    post_timing = factor(create_balanced_categories(
      c("Morning", "Afternoon", "Evening", "Night"),
      n_posts,
      c(0.20, 0.30, 0.35, 0.15)
    )),
    
    # Platform and audience
    platform = factor(create_balanced_categories(
      c("Instagram", "Facebook", "Twitter", "LinkedIn", "TikTok", "YouTube"),
      n_posts,
      c(0.25, 0.20, 0.15, 0.15, 0.15, 0.10)
    )),
    
    target_audience = factor(create_balanced_categories(
      c("Gen_Z", "Millennials", "Gen_X", "Boomers", "All_Ages"),
      n_posts,
      c(0.25, 0.30, 0.20, 0.15, 0.10)
    )),
    
    # Campaign data
    campaign_type = factor(create_balanced_categories(
      c("Organic", "Paid_Promotion", "Influencer", "Contest", "Educational"),
      n_posts,
      c(0.40, 0.25, 0.15, 0.10, 0.10)
    )),
    
    hashtag_count = sample(0:15, n_posts, replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.2, 0.15, 0.1, rep(0.025, 10))),
    
    # Performance metrics
    shares_count = pmax(0, round(engagement_count * runif(n_posts, 0.05, 0.20))),
    comments_count = pmax(0, round(engagement_count * runif(n_posts, 0.10, 0.30)))
  ) %>%
    mutate(
      # Platform-specific adjustments
      engagement_count = case_when(
        platform == "TikTok" & content_type == "Video" ~ round(engagement_count * 1.5),
        platform == "LinkedIn" & content_type == "Text" ~ round(engagement_count * 1.2),
        platform == "Instagram" & content_type == "Image" ~ round(engagement_count * 1.3),
        TRUE ~ engagement_count
      )
    )
  
  return(social_data)
}

# 4. Healthcare Outcome Data - Treatment Response Analysis
create_healthcare_data <- function() {
  n_patients <- 450
  
  healthcare_data <- tibble(
    # Treatment outcomes
    treatment_response = factor(create_balanced_categories(
      c("Complete_Response", "Partial_Response", "Stable_Disease", "Progressive_Disease"),
      n_patients,
      c(0.25, 0.35, 0.25, 0.15)
    )),
    
    # Patient demographics
    age_category = factor(create_balanced_categories(
      c("Under_40", "40-60", "60-75", "Over_75"),
      n_patients,
      c(0.15, 0.35, 0.35, 0.15)
    )),
    
    gender = factor(create_balanced_categories(
      c("Male", "Female", "Other"),
      n_patients,
      c(0.48, 0.50, 0.02)
    )),
    
    # Clinical characteristics
    disease_stage = factor(create_balanced_categories(
      c("Early", "Intermediate", "Advanced"),
      n_patients,
      c(0.30, 0.45, 0.25)
    )),
    
    treatment_type = factor(create_balanced_categories(
      c("Surgery", "Chemotherapy", "Radiation", "Immunotherapy", "Combination"),
      n_patients,
      c(0.20, 0.25, 0.20, 0.15, 0.20)
    )),
    
    # Risk factors
    comorbidities = factor(create_balanced_categories(
      c("None", "One", "Multiple"),
      n_patients,
      c(0.40, 0.35, 0.25)
    )),
    
    smoking_status = factor(create_balanced_categories(
      c("Never", "Former", "Current"),
      n_patients,
      c(0.50, 0.35, 0.15)
    )),
    
    # Healthcare system
    hospital_type = factor(create_balanced_categories(
      c("Academic_Medical_Center", "Community_Hospital", "Cancer_Center", "Private_Practice"),
      n_patients,
      c(0.35, 0.30, 0.25, 0.10)
    )),
    
    insurance_coverage = factor(create_balanced_categories(
      c("Private", "Medicare", "Medicaid", "Uninsured"),
      n_patients,
      c(0.55, 0.30, 0.12, 0.03)
    ))
  ) %>%
    mutate(
      # Create realistic clinical relationships
      treatment_response = case_when(
        disease_stage == "Early" & runif(n()) < 0.4 ~ "Complete_Response",
        disease_stage == "Advanced" & runif(n()) < 0.3 ~ sample(c("Stable_Disease", "Progressive_Disease"), n(), replace = TRUE),
        treatment_type == "Combination" & runif(n()) < 0.3 ~ sample(c("Complete_Response", "Partial_Response"), n(), replace = TRUE),
        TRUE ~ treatment_response
      )
    )
  
  return(healthcare_data)
}

# 5. Educational Assessment Data - Student Performance Analysis
create_education_data <- function() {
  n_students <- 1200
  
  education_data <- tibble(
    # Academic performance
    performance_level = factor(create_balanced_categories(
      c("Exceeds_Expectations", "Meets_Expectations", "Approaching_Expectations", "Below_Expectations"),
      n_students,
      c(0.20, 0.45, 0.25, 0.10)
    )),
    
    # Student demographics
    grade_level = factor(create_balanced_categories(
      c("Elementary", "Middle_School", "High_School", "College"),
      n_students,
      c(0.30, 0.25, 0.25, 0.20)
    )),
    
    # Academic subjects
    subject_area = factor(create_balanced_categories(
      c("Mathematics", "Language_Arts", "Science", "Social_Studies", "Arts", "Physical_Education"),
      n_students,
      c(0.25, 0.25, 0.20, 0.15, 0.10, 0.05)
    )),
    
    # Socioeconomic factors
    socioeconomic_status = factor(create_balanced_categories(
      c("High", "Middle", "Low"),
      n_students,
      c(0.30, 0.50, 0.20)
    )),
    
    school_type = factor(create_balanced_categories(
      c("Public", "Private", "Charter", "Homeschool"),
      n_students,
      c(0.70, 0.15, 0.12, 0.03)
    )),
    
    # Support factors
    parental_involvement = factor(create_balanced_categories(
      c("High", "Medium", "Low"),
      n_students,
      c(0.35, 0.45, 0.20)
    )),
    
    tutoring_support = factor(create_balanced_categories(
      c("None", "Occasional", "Regular"),
      n_students,
      c(0.60, 0.25, 0.15)
    )),
    
    # Technology access
    technology_access = factor(create_balanced_categories(
      c("Full_Access", "Limited_Access", "No_Access"),
      n_students,
      c(0.70, 0.25, 0.05)
    )),
    
    # Language factors
    primary_language = factor(create_balanced_categories(
      c("English", "Spanish", "Other"),
      n_students,
      c(0.75, 0.15, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic educational relationships
      performance_level = case_when(
        socioeconomic_status == "High" & parental_involvement == "High" & runif(n()) < 0.4 ~ "Exceeds_Expectations",
        socioeconomic_status == "Low" & tutoring_support == "None" & runif(n()) < 0.2 ~ "Below_Expectations",
        technology_access == "No_Access" & runif(n()) < 0.3 ~ sample(c("Approaching_Expectations", "Below_Expectations"), n(), replace = TRUE),
        TRUE ~ performance_level
      )
    )
  
  return(education_data)
}

# 6. Financial Portfolio Data - Investment Distribution Analysis
create_portfolio_data <- function() {
  n_investments <- 900
  
  portfolio_data <- tibble(
    # Investment allocation
    asset_class = factor(create_balanced_categories(
      c("Stocks", "Bonds", "Real_Estate", "Commodities", "Cash", "Crypto", "Alternative"),
      n_investments,
      c(0.35, 0.25, 0.15, 0.08, 0.07, 0.05, 0.05)
    )),
    
    allocation_percentage = case_when(
      asset_class == "Stocks" ~ sample(10:60, n_investments, replace = TRUE),
      asset_class == "Bonds" ~ sample(5:40, n_investments, replace = TRUE),
      asset_class == "Real_Estate" ~ sample(5:25, n_investments, replace = TRUE),
      asset_class == "Cash" ~ sample(2:15, n_investments, replace = TRUE),
      TRUE ~ sample(1:10, n_investments, replace = TRUE)
    ),
    
    # Investor demographics
    investor_age = factor(create_balanced_categories(
      c("Young_Adult", "Mid_Career", "Pre_Retirement", "Retirement"),
      n_investments,
      c(0.25, 0.35, 0.25, 0.15)
    )),
    
    risk_tolerance = factor(create_balanced_categories(
      c("Conservative", "Moderate", "Aggressive"),
      n_investments,
      c(0.30, 0.50, 0.20)
    )),
    
    # Investment strategy
    investment_goal = factor(create_balanced_categories(
      c("Growth", "Income", "Balanced", "Capital_Preservation"),
      n_investments,
      c(0.35, 0.25, 0.30, 0.10)
    )),
    
    time_horizon = factor(create_balanced_categories(
      c("Short_Term", "Medium_Term", "Long_Term"),
      n_investments,
      c(0.20, 0.30, 0.50)
    )),
    
    # Financial status
    net_worth_category = factor(create_balanced_categories(
      c("Under_100K", "100K-500K", "500K-1M", "1M-5M", "Over_5M"),
      n_investments,
      c(0.25, 0.35, 0.20, 0.15, 0.05)
    )),
    
    # Advisory relationship
    advisor_type = factor(create_balanced_categories(
      c("Self_Directed", "Robo_Advisor", "Human_Advisor", "Hybrid"),
      n_investments,
      c(0.30, 0.25, 0.35, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic portfolio relationships
      asset_class = case_when(
        investor_age == "Young_Adult" & risk_tolerance == "Aggressive" & runif(n()) < 0.3 ~ 
          sample(c("Stocks", "Crypto"), n(), replace = TRUE),
        investor_age == "Retirement" & risk_tolerance == "Conservative" & runif(n()) < 0.4 ~ 
          sample(c("Bonds", "Cash"), n(), replace = TRUE),
        TRUE ~ asset_class
      ),
      
      allocation_percentage = pmax(1, pmin(100, allocation_percentage))
    )
  
  return(portfolio_data)
}

# 7. Customer Satisfaction Data - Service Experience Analysis
create_customer_satisfaction_data <- function() {
  n_customers <- 750
  
  satisfaction_data <- tibble(
    # Satisfaction metrics
    overall_satisfaction = factor(create_balanced_categories(
      c("Very_Satisfied", "Satisfied", "Neutral", "Dissatisfied", "Very_Dissatisfied"),
      n_customers,
      c(0.25, 0.35, 0.25, 0.10, 0.05)
    ), levels = c("Very_Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very_Satisfied")),
    
    # Service channels
    service_channel = factor(create_balanced_categories(
      c("Phone", "Email", "Chat", "In_Person", "Social_Media", "Mobile_App"),
      n_customers,
      c(0.30, 0.25, 0.20, 0.15, 0.05, 0.05)
    )),
    
    # Issue resolution
    resolution_status = factor(create_balanced_categories(
      c("Resolved_First_Contact", "Resolved_Multiple_Contacts", "Escalated", "Unresolved"),
      n_customers,
      c(0.45, 0.30, 0.15, 0.10)
    )),
    
    # Customer characteristics
    customer_tenure = factor(create_balanced_categories(
      c("New", "1_Year", "2_5_Years", "5_Plus_Years"),
      n_customers,
      c(0.15, 0.25, 0.35, 0.25)
    )),
    
    customer_tier = factor(create_balanced_categories(
      c("Bronze", "Silver", "Gold", "Platinum"),
      n_customers,
      c(0.40, 0.35, 0.20, 0.05)
    )),
    
    # Issue characteristics
    issue_type = factor(create_balanced_categories(
      c("Billing", "Technical_Support", "Product_Info", "Complaint", "Account_Changes"),
      n_customers,
      c(0.25, 0.30, 0.20, 0.15, 0.10)
    )),
    
    issue_complexity = factor(create_balanced_categories(
      c("Simple", "Moderate", "Complex"),
      n_customers,
      c(0.50, 0.35, 0.15)
    )),
    
    # Demographics
    age_group = factor(create_balanced_categories(
      c("18-25", "26-35", "36-50", "51-65", "65+"),
      n_customers,
      c(0.15, 0.25, 0.30, 0.20, 0.10)
    )),
    
    communication_preference = factor(create_balanced_categories(
      c("Digital", "Voice", "Mixed"),
      n_customers,
      c(0.45, 0.30, 0.25)
    ))
  ) %>%
    mutate(
      # Create realistic satisfaction relationships
      overall_satisfaction = case_when(
        resolution_status == "Resolved_First_Contact" & runif(n()) < 0.6 ~ 
          sample(c("Very_Satisfied", "Satisfied"), n(), replace = TRUE),
        resolution_status == "Unresolved" & runif(n()) < 0.7 ~ 
          sample(c("Dissatisfied", "Very_Dissatisfied"), n(), replace = TRUE),
        customer_tier == "Platinum" & runif(n()) < 0.3 ~ "Very_Satisfied",
        TRUE ~ overall_satisfaction
      )
    )
  
  return(satisfaction_data)
}

# 8. Election Data - Voting Pattern Analysis
create_election_data <- function() {
  n_voters <- 1000
  
  election_data <- tibble(
    # Voting behavior
    candidate_choice = factor(create_balanced_categories(
      c("Candidate_A", "Candidate_B", "Candidate_C", "Undecided", "Not_Voting"),
      n_voters,
      c(0.35, 0.30, 0.20, 0.10, 0.05)
    )),
    
    # Voter demographics
    age_group = factor(create_balanced_categories(
      c("18-25", "26-35", "36-50", "51-65", "65+"),
      n_voters,
      c(0.15, 0.20, 0.25, 0.25, 0.15)
    )),
    
    education_level = factor(create_balanced_categories(
      c("High_School", "Some_College", "Bachelor", "Graduate"),
      n_voters,
      c(0.25, 0.30, 0.30, 0.15)
    )),
    
    # Geographic and economic
    region = factor(create_balanced_categories(
      c("Urban", "Suburban", "Rural"),
      n_voters,
      c(0.45, 0.35, 0.20)
    )),
    
    income_level = factor(create_balanced_categories(
      c("Low", "Middle", "Upper_Middle", "High"),
      n_voters,
      c(0.20, 0.40, 0.30, 0.10)
    )),
    
    # Political engagement
    political_affiliation = factor(create_balanced_categories(
      c("Democrat", "Republican", "Independent", "Other"),
      n_voters,
      c(0.35, 0.35, 0.25, 0.05)
    )),
    
    voting_frequency = factor(create_balanced_categories(
      c("Always", "Usually", "Sometimes", "Rarely"),
      n_voters,
      c(0.40, 0.30, 0.20, 0.10)
    )),
    
    # Information sources
    primary_news_source = factor(create_balanced_categories(
      c("Traditional_Media", "Social_Media", "Online_News", "Friends_Family", "Multiple_Sources"),
      n_voters,
      c(0.25, 0.20, 0.25, 0.10, 0.20)
    )),
    
    # Key issues
    top_issue = factor(create_balanced_categories(
      c("Economy", "Healthcare", "Education", "Environment", "Security", "Social_Issues"),
      n_voters,
      c(0.30, 0.20, 0.15, 0.15, 0.10, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic voting relationships
      candidate_choice = case_when(
        political_affiliation == "Democrat" & runif(n()) < 0.7 ~ "Candidate_A",
        political_affiliation == "Republican" & runif(n()) < 0.7 ~ "Candidate_B",
        political_affiliation == "Independent" & runif(n()) < 0.3 ~ "Candidate_C",
        voting_frequency == "Rarely" & runif(n()) < 0.3 ~ sample(c("Undecided", "Not_Voting"), n(), replace = TRUE),
        TRUE ~ candidate_choice
      )
    )
  
  return(election_data)
}

# 9. Resource Allocation Data - Budget Distribution Analysis
create_resource_allocation_data <- function() {
  n_allocations <- 600
  
  resource_data <- tibble(
    # Budget categories
    budget_category = factor(create_balanced_categories(
      c("Personnel", "Technology", "Marketing", "Operations", "R_and_D", "Facilities", "Other"),
      n_allocations,
      c(0.35, 0.20, 0.15, 0.12, 0.08, 0.07, 0.03)
    )),
    
    budget_amount = case_when(
      budget_category == "Personnel" ~ sample(50000:500000, n_allocations, replace = TRUE),
      budget_category == "Technology" ~ sample(10000:200000, n_allocations, replace = TRUE),
      budget_category == "Marketing" ~ sample(5000:150000, n_allocations, replace = TRUE),
      TRUE ~ sample(1000:100000, n_allocations, replace = TRUE)
    ),
    
    # Organizational characteristics
    organization_size = factor(create_balanced_categories(
      c("Small", "Medium", "Large", "Enterprise"),
      n_allocations,
      c(0.30, 0.35, 0.25, 0.10)
    )),
    
    industry_sector = factor(create_balanced_categories(
      c("Technology", "Healthcare", "Finance", "Manufacturing", "Retail", "Education", "Non_Profit"),
      n_allocations,
      c(0.20, 0.15, 0.15, 0.15, 0.12, 0.08, 0.15)
    )),
    
    # Planning characteristics
    planning_horizon = factor(create_balanced_categories(
      c("Annual", "Quarterly", "Multi_Year"),
      n_allocations,
      c(0.60, 0.25, 0.15)
    )),
    
    priority_level = factor(create_balanced_categories(
      c("Critical", "High", "Medium", "Low"),
      n_allocations,
      c(0.20, 0.35, 0.35, 0.10)
    )),
    
    # Performance metrics
    roi_category = factor(create_balanced_categories(
      c("High_ROI", "Medium_ROI", "Low_ROI", "Unknown"),
      n_allocations,
      c(0.25, 0.40, 0.25, 0.10)
    )),
    
    department = factor(create_balanced_categories(
      c("IT", "HR", "Sales", "Marketing", "Finance", "Operations", "Executive"),
      n_allocations,
      c(0.20, 0.15, 0.15, 0.15, 0.12, 0.13, 0.10)
    ))
  ) %>%
    mutate(
      # Create realistic budget relationships
      budget_amount = case_when(
        organization_size == "Enterprise" ~ round(budget_amount * 1.5),
        organization_size == "Small" ~ round(budget_amount * 0.6),
        priority_level == "Critical" ~ round(budget_amount * 1.3),
        TRUE ~ budget_amount
      ),
      
      budget_amount = pmax(1000, budget_amount)
    )
  
  return(resource_data)
}

# 10. Time Usage Data - Daily Activity Analysis
create_time_usage_data <- function() {
  n_entries <- 800
  
  time_data <- tibble(
    # Activity categories
    activity_type = factor(create_balanced_categories(
      c("Work", "Sleep", "Exercise", "Leisure", "Family_Time", "Commuting", "Household", "Personal_Care"),
      n_entries,
      c(0.25, 0.20, 0.08, 0.15, 0.12, 0.08, 0.07, 0.05)
    )),
    
    hours_spent = case_when(
      activity_type == "Work" ~ sample(6:12, n_entries, replace = TRUE),
      activity_type == "Sleep" ~ sample(6:10, n_entries, replace = TRUE),
      activity_type == "Exercise" ~ sample(1:3, n_entries, replace = TRUE),
      activity_type == "Leisure" ~ sample(1:5, n_entries, replace = TRUE),
      activity_type == "Family_Time" ~ sample(1:4, n_entries, replace = TRUE),
      TRUE ~ sample(1:3, n_entries, replace = TRUE)
    ),
    
    # Demographics
    age_category = factor(create_balanced_categories(
      c("Young_Adult", "Middle_Age", "Senior"),
      n_entries,
      c(0.35, 0.45, 0.20)
    )),
    
    employment_status = factor(create_balanced_categories(
      c("Full_Time", "Part_Time", "Student", "Retired", "Unemployed"),
      n_entries,
      c(0.50, 0.20, 0.15, 0.10, 0.05)
    )),
    
    # Lifestyle factors
    family_status = factor(create_balanced_categories(
      c("Single", "Married_No_Kids", "Married_With_Kids", "Single_Parent"),
      n_entries,
      c(0.30, 0.25, 0.35, 0.10)
    )),
    
    location_type = factor(create_balanced_categories(
      c("Urban", "Suburban", "Rural"),
      n_entries,
      c(0.45, 0.35, 0.20)
    )),
    
    # Schedule patterns
    work_schedule = factor(create_balanced_categories(
      c("Traditional", "Flexible", "Remote", "Shift_Work", "N_A"),
      n_entries,
      c(0.40, 0.25, 0.20, 0.10, 0.05)
    )),
    
    day_type = factor(create_balanced_categories(
      c("Weekday", "Weekend"),
      n_entries,
      c(0.70, 0.30)
    )),
    
    season = factor(create_balanced_categories(
      c("Spring", "Summer", "Fall", "Winter"),
      n_entries,
      c(0.25, 0.25, 0.25, 0.25)
    ))
  ) %>%
    mutate(
      # Create realistic time allocation relationships
      hours_spent = case_when(
        activity_type == "Work" & employment_status == "Part_Time" ~ sample(3:6, n(), replace = TRUE),
        activity_type == "Work" & employment_status == "Retired" ~ 0,
        activity_type == "Exercise" & age_category == "Senior" ~ sample(0:2, n(), replace = TRUE),
        day_type == "Weekend" & activity_type == "Work" ~ round(hours_spent * 0.3),
        TRUE ~ hours_spent
      ),
      
      hours_spent = pmax(0, pmin(24, hours_spent))
    )
  
  return(time_data)
}

# Generate all datasets
message("Generating jwaffle test datasets...")

# Create all datasets
market_research_data <- create_market_research_data()
survey_response_data <- create_survey_response_data() 
social_media_data <- create_social_media_data()
healthcare_data <- create_healthcare_data()
education_data <- create_education_data()
portfolio_data <- create_portfolio_data()
customer_satisfaction_data <- create_customer_satisfaction_data()
election_data <- create_election_data()
resource_allocation_data <- create_resource_allocation_data()
time_usage_data <- create_time_usage_data()

# Save datasets to package data directory (if it exists)
if (dir.exists("data")) {
  save(market_research_data, file = "data/jwaffle_market_research.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(market_research_data, "data/jwaffle_market_research.omv")
  message("✓ Created jwaffle_market_research.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(market_research_data, "data/jwaffle_market_research.omv")
  message("✓ Created jwaffle_market_research.omv")
}
  save(survey_response_data, file = "data/jwaffle_survey_response.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_response_data, "data/jwaffle_survey_response.omv")
  message("✓ Created jwaffle_survey_response.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(survey_response_data, "data/jwaffle_survey_response.omv")
  message("✓ Created jwaffle_survey_response.omv")
}
  save(social_media_data, file = "data/jwaffle_social_media.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(social_media_data, "data/jwaffle_social_media.omv")
  message("✓ Created jwaffle_social_media.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(social_media_data, "data/jwaffle_social_media.omv")
  message("✓ Created jwaffle_social_media.omv")
}
  save(healthcare_data, file = "data/jwaffle_healthcare.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(healthcare_data, "data/jwaffle_healthcare.omv")
  message("✓ Created jwaffle_healthcare.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(healthcare_data, "data/jwaffle_healthcare.omv")
  message("✓ Created jwaffle_healthcare.omv")
}
  save(education_data, file = "data/jwaffle_education.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(education_data, "data/jwaffle_education.omv")
  message("✓ Created jwaffle_education.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(education_data, "data/jwaffle_education.omv")
  message("✓ Created jwaffle_education.omv")
}
  save(portfolio_data, file = "data/jwaffle_portfolio.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(portfolio_data, "data/jwaffle_portfolio.omv")
  message("✓ Created jwaffle_portfolio.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(portfolio_data, "data/jwaffle_portfolio.omv")
  message("✓ Created jwaffle_portfolio.omv")
}
  save(customer_satisfaction_data, file = "data/jwaffle_customer_satisfaction.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(customer_satisfaction_data, "data/jwaffle_customer_satisfaction.omv")
  message("✓ Created jwaffle_customer_satisfaction.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(customer_satisfaction_data, "data/jwaffle_customer_satisfaction.omv")
  message("✓ Created jwaffle_customer_satisfaction.omv")
}
  save(election_data, file = "data/jwaffle_election.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(election_data, "data/jwaffle_election.omv")
  message("✓ Created jwaffle_election.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(election_data, "data/jwaffle_election.omv")
  message("✓ Created jwaffle_election.omv")
}
  save(resource_allocation_data, file = "data/jwaffle_resource_allocation.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(resource_allocation_data, "data/jwaffle_resource_allocation.omv")
  message("✓ Created jwaffle_resource_allocation.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(resource_allocation_data, "data/jwaffle_resource_allocation.omv")
  message("✓ Created jwaffle_resource_allocation.omv")
}
  save(time_usage_data, file = "data/jwaffle_time_usage.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(time_usage_data, "data/jwaffle_time_usage.omv")
  message("✓ Created jwaffle_time_usage.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(time_usage_data, "data/jwaffle_time_usage.omv")
  message("✓ Created jwaffle_time_usage.omv")
}
  
  message("✓ All datasets saved to data/ directory")
} else {
  message("Note: data/ directory not found. Datasets created in memory only.")
}

# Print dataset summaries
message("\n=== Dataset Summaries ===")

datasets <- list(
  "Market Research" = market_research_data,
  "Survey Response" = survey_response_data,
  "Social Media" = social_media_data,
  "Healthcare" = healthcare_data,
  "Education" = education_data,
  "Portfolio" = portfolio_data,
  "Customer Satisfaction" = customer_satisfaction_data,
  "Election" = election_data,
  "Resource Allocation" = resource_allocation_data,
  "Time Usage" = time_usage_data
)

for (name in names(datasets)) {
  data <- datasets[[name]]
  message(sprintf("\n%s Data:", name))
  message(sprintf("  - Observations: %d", nrow(data)))
  message(sprintf("  - Variables: %d", ncol(data)))
  message(sprintf("  - Key variables: %s", paste(names(data)[1:min(3, ncol(data))], collapse = ", ")))
}

message("\n=== Usage Examples ===")
message("# Basic waffle chart")
message("jwaffle(data = market_research_data, groups = 'preferred_brand')")
message("")
message("# Faceted waffle chart")
message("jwaffle(data = survey_response_data, groups = 'job_satisfaction', facet = 'department')")
message("")
message("# Weighted waffle chart")
message("jwaffle(data = social_media_data, groups = 'engagement_level', counts = 'engagement_count')")
message("")
message("# Custom styling")
message("jwaffle(data = healthcare_data, groups = 'treatment_response', color_palette = 'professional', mytitle = 'Treatment Outcomes')")

message("\n✓ jwaffle test data generation completed successfully!")
