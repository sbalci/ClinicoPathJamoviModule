# ═══════════════════════════════════════════════════════════
# Test Data Generation: oddsratio
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Odds Ratio Table and Plot for Binary Outcomes

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══ 1. MAIN TEST DATASET ═══
n <- 200
oddsratio_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n)),
  
  # Binary outcome (mortality/event)
  outcome = sample(c("Alive", "Dead"), n, replace = TRUE, prob = c(0.7, 0.3)),
  
  # Categorical predictors
  treatment = sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE),
  stage = sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.6, 0.4)),
  biomarker_status = sample(c("Negative", "Positive"), n, replace = TRUE),
  
  # Continuous predictors
  age = round(rnorm(n, 65, 12)),
  tumor_size = round(rnorm(n, 4, 1.5), 1),
  psa_level = exp(rnorm(n, 2, 0.5))
)

# Add realistic correlation: Advanced stage → higher mortality
oddsratio_test <- oddsratio_test %>%
  mutate(outcome = ifelse(stage == "Advanced" & runif(n()) > 0.6, "Dead", outcome))

# ═══ 2. DIAGNOSTIC TEST DATA (2x2 table) ═══
n_diag <- 150
oddsratio_diagnostic <- tibble(
  id = 1:n_diag,
  # Gold standard
  disease_status = sample(c("Healthy", "Diseased"), n_diag, replace = TRUE, prob = c(0.6, 0.4)),
  # Test result (correlated with disease)
  test_result = case_when(
    disease_status == "Diseased" ~ sample(c("Negative", "Positive"), n_diag, replace = TRUE, prob = c(0.2, 0.8)),
    TRUE ~ sample(c("Negative", "Positive"), n_diag, replace = TRUE, prob = c(0.8, 0.2))
  ),
  # Additional predictor
  risk_factor = sample(c("Low", "High"), n_diag, replace = TRUE)
)

# ═══ 3. CASE-CONTROL STUDY ═══
n_case <- 100
oddsratio_casecontrol <- tibble(
  id = 1:(n_case*2),
  case_status = rep(c("Control", "Case"), each = n_case),
  exposure = sample(c("Unexposed", "Exposed"), n_case*2, replace = TRUE),
  age_group = sample(c("<50", "50-70", ">70"), n_case*2, replace = TRUE),
  smoking = sample(c("Never", "Former", "Current"), n_case*2, replace = TRUE)
)

# ═══ 4. MULTIPLE PREDICTORS ═══
n_multi <- 180
oddsratio_multiple <- tibble(
  id = 1:n_multi,
  event = sample(c("No", "Yes"), n_multi, replace = TRUE, prob = c(0.65, 0.35)),
  predictor1 = sample(c("A", "B"), n_multi, replace = TRUE),
  predictor2 = sample(c("Low", "Medium", "High"), n_multi, replace = TRUE),
  predictor3 = rnorm(n_multi, 50, 15),
  predictor4 = sample(0:1, n_multi, replace = TRUE),
  predictor5 = round(runif(n_multi, 20, 80))
)

# ═══ 5. SMALL/LARGE/EDGE CASES ═══
oddsratio_small <- tibble(
  id = 1:20,
  outcome = sample(c("No", "Yes"), 20, replace = TRUE),
  predictor = sample(c("A", "B"), 20, replace = TRUE)
)

oddsratio_large <- tibble(
  id = 1:500,
  outcome = sample(c("Event", "NoEvent"), 500, replace = TRUE),
  var1 = sample(paste0("Group", 1:5), 500, replace = TRUE),
  var2 = rnorm(500, 100, 20)
)

oddsratio_perfect <- tibble(
  id = 1:40,
  outcome = rep(c("No", "Yes"), each = 20),
  predictor = rep(c("Low", "High"), each = 20)  # Perfect separation
)

oddsratio_zerocell <- tibble(
  id = 1:30,
  outcome = c(rep("No", 15), rep("Yes", 15)),
  predictor = c(rep("A", 15), rep("B", 10), rep("A", 5))  # Zero cell
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  oddsratio_test = oddsratio_test,
  oddsratio_diagnostic = oddsratio_diagnostic,
  oddsratio_casecontrol = oddsratio_casecontrol,
  oddsratio_multiple = oddsratio_multiple,
  oddsratio_small = oddsratio_small,
  oddsratio_large = oddsratio_large,
  oddsratio_perfect = oddsratio_perfect,
  oddsratio_zerocell = oddsratio_zerocell
)

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  save_name <- dataset_name
  assign(save_name, data)
  save(list = save_name, file = here::here("data", paste0(dataset_name, ".rda")), compress = "xz")
  write.csv(data, file = here::here("data", paste0(dataset_name, ".csv")), row.names = FALSE)
  writexl::write_xlsx(data, path = here::here("data", paste0(dataset_name, ".xlsx")))
  jmvReadWrite::write_omv(dtaFrm = data, fleOut = here::here("data", paste0(dataset_name, ".omv")), frcWrt = TRUE)
}

cat("✅ Oddsratio test data generated: 8 datasets × 4 formats = 32 files\n")
