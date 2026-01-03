# ═══════════════════════════════════════════════════════════
# Test Data Generation: agepyramid
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the agepyramid jamovi function
#
# Generated: 2026-01-03
# Seed: 42
# Observations: 500

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# Sample size
n <- 500

# ═══════════════════════════════════════════════════════════
# Dataset 1: General Population (Standard)
# ═══════════════════════════════════════════════════════════
# Realistic population pyramid with balanced age distribution

agepyramid_test <- tibble(
  # Patient ID
  id = 1:n,

  # Gender (balanced)
  gender = sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.51, 0.49)),

  # Age - mixture of age groups simulating general population
  age = c(
    # Children (0-18): 20%
    sample(0:18, round(n * 0.20), replace = TRUE),
    # Young adults (19-35): 25%
    sample(19:35, round(n * 0.25), replace = TRUE),
    # Middle age (36-55): 30%
    sample(36:55, round(n * 0.30), replace = TRUE),
    # Older adults (56-75): 20%
    sample(56:75, round(n * 0.20), replace = TRUE),
    # Elderly (76+): 5%
    sample(76:95, n - round(n * 0.95), replace = TRUE)
  )
)

# Add slight age variation to make it more realistic
agepyramid_test$age <- pmax(0, pmin(100,
                                     agepyramid_test$age + rnorm(n, 0, 2)))
agepyramid_test$age <- round(agepyramid_test$age, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 2: Clinical Population (Cancer Patients)
# ═══════════════════════════════════════════════════════════
# Older population typical of cancer diagnosis

agepyramid_cancer <- tibble(
  id = 1:n,

  # Gender (slightly more males in cancer)
  gender = sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.45, 0.55)),

  # Age - skewed toward older adults (mean ~65)
  age = pmax(18, pmin(95, rnorm(n, mean = 65, sd = 12)))
) %>%
  mutate(age = round(age, 1))

# ═══════════════════════════════════════════════════════════
# Dataset 3: Pediatric Population
# ═══════════════════════════════════════════════════════════

agepyramid_pediatric <- tibble(
  id = 1:300,

  # Gender (balanced)
  gender = sample(c("Female", "Male"), 300, replace = TRUE),

  # Age - pediatric only (0-18)
  age = c(
    # Infants (0-2): 20%
    runif(60, 0, 2),
    # Preschool (3-5): 15%
    runif(45, 3, 5),
    # School age (6-12): 40%
    runif(120, 6, 12),
    # Adolescents (13-18): 25%
    runif(75, 13, 18)
  )
) %>%
  mutate(age = round(age, 1))

# ═══════════════════════════════════════════════════════════
# Dataset 4: Geriatric Population
# ═══════════════════════════════════════════════════════════

agepyramid_geriatric <- tibble(
  id = 1:200,

  # Gender (more females in elderly - higher life expectancy)
  gender = sample(c("Female", "Male"), 200, replace = TRUE, prob = c(0.60, 0.40)),

  # Age - 65+ only
  age = pmax(65, pmin(100, rnorm(200, mean = 78, sd = 8)))
) %>%
  mutate(age = round(age, 1))

# ═══════════════════════════════════════════════════════════
# Dataset 5: Reproductive Age Population
# ═══════════════════════════════════════════════════════════

agepyramid_reproductive <- tibble(
  id = 1:400,

  # Gender (balanced)
  gender = sample(c("Female", "Male"), 400, replace = TRUE),

  # Age - reproductive years (15-50)
  age = runif(400, 15, 50)
) %>%
  mutate(age = round(age, 1))

# ═══════════════════════════════════════════════════════════
# Dataset 6: Unbalanced Gender (for testing)
# ═══════════════════════════════════════════════════════════

agepyramid_unbalanced <- tibble(
  id = 1:300,

  # Highly unbalanced gender (70% female - e.g., breast cancer cohort)
  gender = sample(c("Female", "Male"), 300, replace = TRUE, prob = c(0.70, 0.30)),

  # Age - mixed population
  age = pmax(18, pmin(85, rnorm(300, mean = 58, sd = 15)))
) %>%
  mutate(age = round(age, 1))

# ═══════════════════════════════════════════════════════════
# Add some missing data (3% in main dataset)
# ═══════════════════════════════════════════════════════════

n_missing <- round(n * 0.03)
agepyramid_test$age[sample(n, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# Save in multiple formats
# ═══════════════════════════════════════════════════════════

# Ensure data directory exists
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

# 1. Standard dataset (RDA, CSV, XLSX, OMV)
save(agepyramid_test, file = here::here("data", "agepyramid_test.rda"), compress = "xz")
write.csv(agepyramid_test, file = here::here("data", "agepyramid_test.csv"), row.names = FALSE)

# Check if writexl is available
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(agepyramid_test, path = here::here("data", "agepyramid_test.xlsx"))
} else {
  message("writexl package not available. Skipping XLSX generation.")
}

# Check if jmvReadWrite is available
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  tryCatch({
    jmvReadWrite::write_omv(agepyramid_test, here::here("data", "agepyramid_test.omv"))
  }, error = function(e) {
    message("Could not create OMV file: ", e$message)
  })
} else {
  message("jmvReadWrite package not available. Skipping OMV generation.")
}

# 2. Additional datasets (RDA only for package)
save(agepyramid_cancer, file = here::here("data", "agepyramid_cancer.rda"), compress = "xz")
save(agepyramid_pediatric, file = here::here("data", "agepyramid_pediatric.rda"), compress = "xz")
save(agepyramid_geriatric, file = here::here("data", "agepyramid_geriatric.rda"), compress = "xz")
save(agepyramid_reproductive, file = here::here("data", "agepyramid_reproductive.rda"), compress = "xz")
save(agepyramid_unbalanced, file = here::here("data", "agepyramid_unbalanced.rda"), compress = "xz")

# ═══════════════════════════════════════════════════════════
# Document the datasets
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
Age Pyramid Test Datasets Generated
═══════════════════════════════════════════════════════════

Dataset 1: agepyramid_test (Standard Population)
  Observations:", nrow(agepyramid_test), "
  Variables:", ncol(agepyramid_test), "
  Missing data: ~3% in age
  Age range:", round(min(agepyramid_test$age, na.rm = TRUE)), "-",
    round(max(agepyramid_test$age, na.rm = TRUE)), "years
  Gender distribution:",
    table(agepyramid_test$gender)[1], "Female,",
    table(agepyramid_test$gender)[2], "Male

Dataset 2: agepyramid_cancer (Clinical Cancer Population)
  Observations:", nrow(agepyramid_cancer), "
  Age range:", round(min(agepyramid_cancer$age)), "-",
    round(max(agepyramid_cancer$age)), "years
  Mean age:", round(mean(agepyramid_cancer$age), 1), "

Dataset 3: agepyramid_pediatric (Pediatric 0-18)
  Observations:", nrow(agepyramid_pediatric), "
  Age range:", round(min(agepyramid_pediatric$age), 1), "-",
    round(max(agepyramid_pediatric$age), 1), "years

Dataset 4: agepyramid_geriatric (Geriatric 65+)
  Observations:", nrow(agepyramid_geriatric), "
  Age range:", round(min(agepyramid_geriatric$age), 1), "-",
    round(max(agepyramid_geriatric$age), 1), "years

Dataset 5: agepyramid_reproductive (Reproductive Age 15-50)
  Observations:", nrow(agepyramid_reproductive), "
  Age range:", round(min(agepyramid_reproductive$age), 1), "-",
    round(max(agepyramid_reproductive$age), 1), "years

Dataset 6: agepyramid_unbalanced (Unbalanced Gender)
  Observations:", nrow(agepyramid_unbalanced), "
  Gender distribution:",
    table(agepyramid_unbalanced$gender)[1], "Female,",
    table(agepyramid_unbalanced$gender)[2], "Male

═══════════════════════════════════════════════════════════
Variable Descriptions (agepyramid_test)
═══════════════════════════════════════════════════════════

- id: Patient identifier (1-", n, ")
- gender: Gender (Female, Male) - factor variable
- age: Age in years (continuous, 0-100)

═══════════════════════════════════════════════════════════
Usage Examples
═══════════════════════════════════════════════════════════

# Load standard dataset
data(agepyramid_test)
library(ClinicoPath)

# Basic age pyramid
agepyramid(
  data = agepyramid_test,
  age = 'age',
  gender = 'gender',
  female = 'Female',
  male = 'Male'
)

# With custom bin width
agepyramid(
  data = agepyramid_test,
  age = 'age',
  gender = 'gender',
  female = 'Female',
  male = 'Male',
  bin_width = 10
)

# Pediatric population
data(agepyramid_pediatric)
agepyramid(
  data = agepyramid_pediatric,
  age = 'age',
  gender = 'gender',
  female = 'Female',
  male = 'Male',
  age_groups = 'pediatric'
)

# Cancer population
data(agepyramid_cancer)
agepyramid(
  data = agepyramid_cancer,
  age = 'age',
  gender = 'gender',
  female = 'Female',
  male = 'Male',
  plot_title = 'Cancer Patient Age Distribution'
)

═══════════════════════════════════════════════════════════
")
