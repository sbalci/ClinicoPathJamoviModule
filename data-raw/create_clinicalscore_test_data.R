# ── Create Test Data for clinicalscore Function ──────────────────────────────
# Two datasets: logistic (diagnostic classification) and Cox (survival scoring)

set.seed(2026)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 1: Breast Cancer Recurrence Scoring (logistic, n=150)
# ═══════════════════════════════════════════════════════════════════════════════

n <- 150
# Predictors with known clinical relevance
age <- round(rnorm(n, 58, 12))
tumor_size_cm <- round(pmax(0.5, rnorm(n, 2.5, 1.5)), 1)
grade <- factor(sample(c("G1", "G2", "G3"), n, TRUE, c(0.25, 0.45, 0.30)),
                levels = c("G1", "G2", "G3"))
er_status <- factor(sample(c("Positive", "Negative"), n, TRUE, c(0.75, 0.25)))
her2_status <- factor(sample(c("Negative", "Positive"), n, TRUE, c(0.80, 0.20)))
ki67_pct <- round(pmax(1, c(
  rnorm(sum(grade == "G1"), 8, 4),
  rnorm(sum(grade == "G2"), 18, 8),
  rnorm(sum(grade == "G3"), 35, 12)
)[order(match(grade, c("G1", "G2", "G3")))]), 1)
ln_positive <- rpois(n, lambda = ifelse(grade == "G3", 3, ifelse(grade == "G2", 1, 0.3)))
lvi <- factor(sample(c("Absent", "Present"), n, TRUE,
              prob = ifelse(rep(grade == "G3", 2), c(0.4, 0.6), c(0.7, 0.3))[1:2]))

# Generate outcome correlated with predictors
logit <- -3 + 0.02 * age + 0.4 * tumor_size_cm +
  0.8 * (grade == "G2") + 1.5 * (grade == "G3") +
  -0.5 * (er_status == "Positive") + 0.6 * (her2_status == "Positive") +
  0.03 * ki67_pct + 0.3 * ln_positive + 0.7 * (lvi == "Present")
prob <- plogis(logit + rnorm(n, 0, 0.5))
recurrence <- factor(rbinom(n, 1, prob), labels = c("No", "Yes"))

# Add ~3% missing
ki67_pct[sample(n, 4)] <- NA
tumor_size_cm[sample(n, 3)] <- NA

clinicalscore_breast <- data.frame(
  recurrence = recurrence,
  age = age, tumor_size_cm = tumor_size_cm,
  grade = grade, er_status = er_status,
  her2_status = her2_status, ki67_pct = ki67_pct,
  ln_positive = ln_positive, lvi = lvi,
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 2: Lung Cancer Survival Scoring (Cox, n=200)
# ═══════════════════════════════════════════════════════════════════════════════

n2 <- 200
age2 <- round(rnorm(n2, 65, 10))
stage <- factor(sample(c("I", "II", "III", "IV"), n2, TRUE, c(0.20, 0.25, 0.30, 0.25)),
                levels = c("I", "II", "III", "IV"))
ecog <- sample(0:3, n2, TRUE, c(0.25, 0.35, 0.25, 0.15))
smoking <- factor(sample(c("Never", "Former", "Current"), n2, TRUE, c(0.15, 0.45, 0.40)))
histology <- factor(sample(c("Adeno", "Squamous", "Large_cell"), n2, TRUE, c(0.50, 0.35, 0.15)))

# Survival times correlated with stage
rate <- 0.01 * exp(0.3 * as.numeric(stage) + 0.2 * ecog +
                   0.15 * (smoking == "Current") - 0.1 * (histology == "Adeno"))
survival_months <- round(rexp(n2, rate), 1)
survival_months <- pmax(0.5, pmin(survival_months, 120))
event <- rbinom(n2, 1, prob = ifelse(survival_months < 60, 0.85, 0.30))

clinicalscore_lung <- data.frame(
  survival_months = survival_months,
  event = factor(event, labels = c("Alive", "Dead")),
  age = age2, stage = stage, ecog = ecog,
  smoking = smoking, histology = histology,
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Save
# ═══════════════════════════════════════════════════════════════════════════════

save(clinicalscore_breast, file = here::here("data", "clinicalscore_breast.rda"))
save(clinicalscore_lung, file = here::here("data", "clinicalscore_lung.rda"))
readr::write_csv(clinicalscore_breast, here::here("data-raw", "non-rda", "clinicalscore_breast.csv"))
readr::write_csv(clinicalscore_lung, here::here("data-raw", "non-rda", "clinicalscore_lung.csv"))

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinicalscore_breast, here::here("data-raw", "non-rda", "clinicalscore_breast.omv"))
  jmvReadWrite::write_omv(clinicalscore_lung, here::here("data-raw", "non-rda", "clinicalscore_lung.omv"))
}

cat("clinicalscore_breast:", nrow(clinicalscore_breast), "rows,", ncol(clinicalscore_breast), "cols\n")
cat("clinicalscore_lung:", nrow(clinicalscore_lung), "rows,", ncol(clinicalscore_lung), "cols\n")
