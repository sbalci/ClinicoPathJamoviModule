# ═══════════════════════════════════════════════════════════
# Test Data Generation: outcomeorganizer
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Outcome Organizer for Survival Analysis

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══ 1. OVERALL SURVIVAL (OS) ═══
n_os <- 150
outcomeorganizer_os <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_os)),
  vital_status = sample(c("Alive", "Dead"), n_os, replace = TRUE, prob = c(0.6, 0.4)),
  time_months = round(runif(n_os, 1, 60)),
  age = round(rnorm(n_os, 65, 10)),
  stage = sample(c("I", "II", "III", "IV"), n_os, replace = TRUE)
)

# ═══ 2. COMPETING RISKS ═══
n_comp <- 120
outcomeorganizer_compete <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_comp)),
  outcome_status = sample(
    c("Alive_NED", "Alive_Disease", "Dead_Disease", "Dead_Other"),
    n_comp, replace = TRUE, prob = c(0.4, 0.2, 0.25, 0.15)
  ),
  time = round(runif(n_comp, 1, 48)),
  treatment = sample(c("Surgery", "Chemo", "Radio"), n_comp, replace = TRUE)
)

# ═══ 3. PROGRESSION-FREE SURVIVAL (PFS) ═══
n_pfs <- 100
outcomeorganizer_pfs <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_pfs)),
  vital_status = sample(c("Alive", "Dead"), n_pfs, replace = TRUE, prob = c(0.7, 0.3)),
  progression = sample(c("No", "Yes"), n_pfs, replace = TRUE, prob = c(0.6, 0.4)),
  time_months = round(runif(n_pfs, 1, 36)),
  biomarker = sample(c("Positive", "Negative"), n_pfs, replace = TRUE)
)

# ═══ 4. RECURRENCE-FREE SURVIVAL (RFS) ═══
n_rfs <- 110
outcomeorganizer_rfs <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rfs)),
  vital = sample(c("Alive", "Dead"), n_rfs, replace = TRUE, prob = c(0.75, 0.25)),
  recurrence = sample(c("No", "Yes"), n_rfs, replace = TRUE, prob = c(0.65, 0.35)),
  fu_time = round(runif(n_rfs, 3, 60)),
  grade = sample(c("Low", "Intermediate", "High"), n_rfs, replace = TRUE)
)

# ═══ 5. CAUSE-SPECIFIC ═══
n_cause <- 90
outcomeorganizer_causespecific <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_cause)),
  death_status = sample(
    c("Alive", "Dead_Cancer", "Dead_Cardiac", "Dead_Other"),
    n_cause, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1)
  ),
  time = round(runif(n_cause, 1, 72)),
  risk_score = round(rnorm(n_cause, 50, 15))
)

# ═══ 6. MULTISTATE ═══
n_multi <- 80
outcomeorganizer_multistate <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_multi)),
  current_state = sample(
    c("Disease_Free", "Local_Recurrence", "Metastatic", "Dead"),
    n_multi, replace = TRUE, prob = c(0.4, 0.25, 0.2, 0.15)
  ),
  time = round(runif(n_multi, 1, 48))
)

# ═══ 7. DISEASE-FREE SURVIVAL (DFS) ═══
n_dfs <- 95
outcomeorganizer_dfs <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_dfs)),
  status = sample(c("NED", "Disease", "Dead"), n_dfs, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
  time_years = round(runif(n_dfs, 0.5, 10), 1),
  surgery_type = sample(c("Limited", "Extended"), n_dfs, replace = TRUE)
)

# ═══ 8. EDGE CASES ═══
# Small dataset
outcomeorganizer_small <- tibble(
  id = 1:15,
  outcome = sample(c("Event", "NoEvent"), 15, replace = TRUE),
  time = round(runif(15, 1, 24))
)

# All censored
outcomeorganizer_censored <- tibble(
  id = 1:30,
  status = rep("Alive", 30),
  time = round(runif(30, 1, 60))
)

# All events
outcomeorganizer_allevents <- tibble(
  id = 1:25,
  status = rep("Dead", 25),
  time = round(runif(25, 1, 48))
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  outcomeorganizer_os = outcomeorganizer_os,
  outcomeorganizer_compete = outcomeorganizer_compete,
  outcomeorganizer_pfs = outcomeorganizer_pfs,
  outcomeorganizer_rfs = outcomeorganizer_rfs,
  outcomeorganizer_causespecific = outcomeorganizer_causespecific,
  outcomeorganizer_multistate = outcomeorganizer_multistate,
  outcomeorganizer_dfs = outcomeorganizer_dfs,
  outcomeorganizer_small = outcomeorganizer_small,
  outcomeorganizer_censored = outcomeorganizer_censored,
  outcomeorganizer_allevents = outcomeorganizer_allevents
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

cat("✅ Outcomeorganizer test data: 10 datasets × 4 formats = 40 files\n")
