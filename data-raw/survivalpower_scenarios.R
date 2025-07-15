# Survival Power Analysis Scenarios Dataset
# Pre-configured scenarios for common clinical trial designs

survivalpower_scenarios <- data.frame(
  Scenario = c(
    "Oncology Phase III",
    "Cardiology CV Outcomes",
    "Neurology Neuroprotection",
    "Rare Disease",
    "Infectious Disease",
    "Pediatric Oncology",
    "Geriatric Medicine",
    "Surgical Intervention",
    "Preventive Medicine",
    "Biomarker Validation"
  ),

  Disease_Area = c(
    "Oncology",
    "Cardiology",
    "Neurology",
    "Rare Disease",
    "Infectious Disease",
    "Pediatric Oncology",
    "Geriatrics",
    "Surgery",
    "Prevention",
    "Biomarker"
  ),

  Study_Type = c(
    "Phase III RCT",
    "Outcomes Trial",
    "Neuroprotection",
    "Phase II/III",
    "Treatment Trial",
    "Pediatric RCT",
    "Frailty Study",
    "Surgical RCT",
    "Prevention Trial",
    "Biomarker Study"
  ),

  Control_Hazard_Rate = c(
    0.083,   # 12 month median survival
    0.04,    # 20 month median survival
    0.02,    # Low progression rate
    0.1,     # Aggressive rare disease
    0.06,    # Moderate mortality
    0.05,    # Pediatric cancer
    0.15,    # High frailty mortality
    0.03,    # Surgical complications
    0.01,    # Prevention endpoint
    0.08     # Biomarker endpoint
  ),

  Treatment_Hazard_Rate = c(
    0.042,   # 24 month median (50% improvement)
    0.03,    # 25% risk reduction
    0.012,   # 40% risk reduction
    0.06,    # 40% improvement
    0.036,   # 40% risk reduction
    0.03,    # 40% improvement
    0.105,   # 30% risk reduction
    0.018,   # 40% risk reduction
    0.007,   # 30% risk reduction
    0.048    # 40% risk reduction
  ),

  Hazard_Ratio = c(
    0.50,    # Strong effect
    0.75,    # Moderate effect
    0.60,    # Good neuroprotection
    0.60,    # Significant improvement
    0.60,    # Good treatment effect
    0.60,    # Pediatric improvement
    0.70,    # Geriatric benefit
    0.60,    # Surgical benefit
    0.70,    # Prevention benefit
    0.60     # Biomarker effect
  ),

  Study_Duration_Months = c(
    36,      # 3 years
    60,      # 5 years (long-term outcomes)
    24,      # 2 years
    30,      # 2.5 years
    18,      # 1.5 years
    36,      # 3 years
    24,      # 2 years
    12,      # 1 year
    48,      # 4 years (prevention)
    24       # 2 years
  ),

  Accrual_Duration_Months = c(
    24,      # 2 years
    36,      # 3 years
    18,      # 1.5 years
    24,      # 2 years
    12,      # 1 year
    24,      # 2 years
    18,      # 1.5 years
    9,       # 9 months
    24,      # 2 years
    18       # 1.5 years
  ),

  Target_Power = c(
    0.90,    # 90% power for Phase III
    0.80,    # 80% power standard
    0.80,    # 80% power
    0.80,    # 80% power
    0.85,    # 85% power
    0.80,    # 80% power
    0.80,    # 80% power
    0.90,    # 90% power for surgery
    0.80,    # 80% power
    0.80     # 80% power
  ),

  Alpha_Level = c(
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.05,    # Two-sided 5%
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.025,   # One-sided 2.5%
    0.025    # One-sided 2.5%
  ),

  Allocation_Ratio = c(
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    2,       # 2:1 (more to treatment)
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    1,       # 1:1 randomization
    1        # 1:1 randomization
  ),

  Dropout_Rate = c(
    0.02,    # 2% annual dropout
    0.01,    # 1% annual dropout
    0.03,    # 3% annual dropout
    0.05,    # 5% annual dropout (rare disease)
    0.02,    # 2% annual dropout
    0.01,    # 1% annual dropout (pediatric)
    0.04,    # 4% annual dropout (geriatric)
    0.01,    # 1% annual dropout (surgical)
    0.02,    # 2% annual dropout
    0.02     # 2% annual dropout
  ),

  Expected_Sample_Size = c(
    300,     # Typical Phase III oncology
    4000,    # Large CV outcomes trial
    400,     # Neurology trial
    150,     # Small rare disease
    600,     # Infectious disease
    200,     # Pediatric trial
    800,     # Geriatric study
    250,     # Surgical trial
    2000,    # Prevention trial
    350      # Biomarker study
  ),

  Notes = c(
    "Standard Phase III cancer trial design",
    "Large cardiovascular outcomes study",
    "Neuroprotection with moderate effect size",
    "Rare disease with enriched population",
    "Infectious disease treatment trial",
    "Pediatric cancer with ethical considerations",
    "Geriatric frailty intervention study",
    "Surgical procedure vs standard care",
    "Primary prevention in high-risk population",
    "Biomarker-guided treatment strategy"
  ),

  stringsAsFactors = FALSE
)

# Save the dataset
usethis::use_data(survivalpower_scenarios, overwrite = TRUE)

# Create documentation
cat("Survival Power Analysis Scenarios Dataset Created\n")
cat("===============================================\n")
cat("Dataset: survivalpower_scenarios\n")
cat("Rows:", nrow(survivalpower_scenarios), "\n")
cat("Columns:", ncol(survivalpower_scenarios), "\n")
cat("\nScenarios included:\n")
for(i in 1:nrow(survivalpower_scenarios)) {
  cat(sprintf("%2d. %-20s (HR=%.2f, %d months)\n",
              i,
              survivalpower_scenarios$Scenario[i],
              survivalpower_scenarios$Hazard_Ratio[i],
              survivalpower_scenarios$Study_Duration_Months[i]))
}
