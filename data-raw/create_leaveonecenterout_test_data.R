# ──────────────────────────────────────────────────────────────────────────────
# Test data for leaveonecenterout (Leave-One-Center-Out Cross-Validation)
#
# Clinical scenario: Multi-center study of biomarker panel predicting
# treatment response in solid tumors. Five institutions contribute data
# with realistic between-center heterogeneity.
# ──────────────────────────────────────────────────────────────────────────────

set.seed(2026)

# ── Dataset 1: loocv_multicenter (N=200, 5 centers) ──────────────────────────
# Comprehensive dataset suitable for logistic, Cox, and linear model types

n <- 200
centers <- c("Hospital_A", "Hospital_B", "Hospital_C", "Hospital_D", "Hospital_E")
center_sizes <- c(60, 50, 40, 30, 20)  # Unequal sizes (realistic)

institution <- rep(centers, times = center_sizes)

# Center-specific effects (heterogeneity)
center_intercept <- c(-0.3, 0.0, 0.2, -0.1, 0.4)
center_effect <- center_intercept[match(institution, centers)]

# Continuous predictors
age <- round(rnorm(n, mean = 62, sd = 12))
age <- pmax(25, pmin(90, age))

ki67_score <- round(rnorm(n, mean = 25, sd = 15), 1)
ki67_score <- pmax(1, pmin(95, ki67_score))

tumor_size_mm <- round(rlnorm(n, meanlog = 3.0, sdlog = 0.5), 1)
tumor_size_mm <- pmax(5, pmin(150, tumor_size_mm))

ctdna_level <- round(rlnorm(n, meanlog = 1.5, sdlog = 1.0), 2)
ctdna_level <- pmax(0.01, pmin(500, ctdna_level))

# Factor predictors
grade <- factor(sample(c("G1", "G2", "G3"), n, replace = TRUE,
                       prob = c(0.2, 0.5, 0.3)),
                levels = c("G1", "G2", "G3"))

gender <- factor(sample(c("Male", "Female"), n, replace = TRUE,
                        prob = c(0.55, 0.45)))

stage <- factor(sample(c("I", "II", "III"), n, replace = TRUE,
                       prob = c(0.25, 0.45, 0.30)),
                levels = c("I", "II", "III"))

# Binary outcome: treatment response (for logistic)
# Build linear predictor with realistic effect sizes
lp <- -1.0 +
    0.02 * (age - 60) +
    0.03 * ki67_score +
    0.01 * tumor_size_mm +
    0.1 * log(ctdna_level + 1) +
    ifelse(grade == "G2", 0.3, ifelse(grade == "G3", 0.8, 0)) +
    ifelse(stage == "II", 0.2, ifelse(stage == "III", 0.5, 0)) +
    center_effect

prob_response <- 1 / (1 + exp(-lp))
treatment_response <- factor(
    ifelse(rbinom(n, 1, prob_response) == 1, "Responder", "Non-responder"),
    levels = c("Non-responder", "Responder"))

# Continuous outcome: tumor shrinkage % (for linear)
tumor_shrinkage <- round(
    -10 +
    0.3 * ki67_score +
    0.15 * tumor_size_mm +
    0.5 * log(ctdna_level + 1) +
    ifelse(grade == "G3", 5, ifelse(grade == "G2", 2, 0)) +
    center_effect * 8 +
    rnorm(n, 0, 8),
    1)

# Survival time and event (for Cox)
hazard <- exp(
    -3.5 +
    0.01 * (age - 60) +
    0.02 * ki67_score +
    0.005 * tumor_size_mm +
    0.05 * log(ctdna_level + 1) +
    ifelse(grade == "G3", 0.5, ifelse(grade == "G2", 0.2, 0)) +
    ifelse(stage == "III", 0.4, ifelse(stage == "II", 0.1, 0)) +
    center_effect * 0.3
)
os_time <- round(rexp(n, rate = hazard), 1)
os_time <- pmax(0.5, pmin(120, os_time))  # 0.5 to 120 months
cens_time <- runif(n, 12, 60)
os_status <- factor(
    ifelse(os_time <= cens_time, "Dead", "Alive"),
    levels = c("Alive", "Dead"))
os_time <- pmin(os_time, cens_time)
os_time <- round(os_time, 1)

# Add ~3% missing data (realistic)
missing_idx <- sample(n, round(n * 0.03))
ki67_score[missing_idx[1:2]] <- NA
tumor_size_mm[missing_idx[3:4]] <- NA
ctdna_level[missing_idx[5:6]] <- NA

loocv_multicenter <- data.frame(
    institution = factor(institution),
    treatment_response = treatment_response,
    tumor_shrinkage = tumor_shrinkage,
    os_time = os_time,
    os_status = os_status,
    age = age,
    ki67_score = ki67_score,
    tumor_size_mm = tumor_size_mm,
    ctdna_level = ctdna_level,
    grade = grade,
    gender = gender,
    stage = stage,
    stringsAsFactors = FALSE
)

# ── Dataset 2: loocv_small (N=45, 3 centers, edge cases) ─────────────────────
# Small dataset with minimum centers and unequal sizes for edge-case testing

set.seed(2027)
n2 <- 45
centers2 <- c("Clinic_X", "Clinic_Y", "Clinic_Z")
center_sizes2 <- c(20, 15, 10)  # Clinic_Z is small (tests <5 warning threshold)

institution2 <- rep(centers2, times = center_sizes2)
center_effect2 <- c(-0.2, 0.0, 0.5)[match(institution2, centers2)]

age2 <- round(rnorm(n2, 58, 14))
age2 <- pmax(30, pmin(85, age2))

marker1 <- round(rnorm(n2, 50, 20), 1)
marker1 <- pmax(0, pmin(100, marker1))

marker2 <- round(rlnorm(n2, 2, 0.8), 1)
marker2 <- pmax(0.5, marker2)

histology <- factor(sample(c("Type_A", "Type_B"), n2, replace = TRUE,
                           prob = c(0.6, 0.4)))

lp2 <- -0.5 + 0.03 * marker1 + 0.1 * log(marker2) +
    ifelse(histology == "Type_B", 0.5, 0) + center_effect2
prob2 <- 1 / (1 + exp(-lp2))
diagnosis <- factor(
    ifelse(rbinom(n2, 1, prob2) == 1, "Positive", "Negative"),
    levels = c("Negative", "Positive"))

# Survival for Cox testing
surv_time <- round(rexp(n2, rate = exp(-3 + 0.02 * marker1 + center_effect2 * 0.3)), 1)
surv_time <- pmax(1, pmin(60, surv_time))
surv_event <- factor(
    ifelse(runif(n2) < 0.6, "Event", "Censored"),
    levels = c("Censored", "Event"))

# Continuous outcome
continuous_outcome <- round(20 + 0.5 * marker1 + 2 * log(marker2) +
    center_effect2 * 5 + rnorm(n2, 0, 6), 1)

loocv_small <- data.frame(
    center = factor(institution2),
    diagnosis = diagnosis,
    continuous_outcome = continuous_outcome,
    surv_time = surv_time,
    surv_event = surv_event,
    age = age2,
    marker1 = marker1,
    marker2 = marker2,
    histology = histology,
    stringsAsFactors = FALSE
)

# ── Save datasets ─────────────────────────────────────────────────────────────

usethis::use_data(loocv_multicenter, overwrite = TRUE)
usethis::use_data(loocv_small, overwrite = TRUE)

# CSV exports
readr::write_csv(loocv_multicenter,
    here::here("data-raw", "non-rda", "loocv_multicenter.csv"))
readr::write_csv(loocv_small,
    here::here("data-raw", "non-rda", "loocv_small.csv"))

# OMV exports (jamovi native)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
    jmvReadWrite::write_omv(loocv_multicenter,
        here::here("data-raw", "non-rda", "loocv_multicenter.omv"))
    jmvReadWrite::write_omv(loocv_small,
        here::here("data-raw", "non-rda", "loocv_small.omv"))
}

cat("Created:\n")
cat("  data/loocv_multicenter.rda  (N=200, 5 centers, 12 variables)\n")
cat("  data/loocv_small.rda        (N=45, 3 centers, 9 variables)\n")
cat("  data-raw/non-rda/loocv_multicenter.csv\n")
cat("  data-raw/non-rda/loocv_small.csv\n")
