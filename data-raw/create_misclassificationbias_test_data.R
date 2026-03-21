# ── Create Test Data for misclassificationbias Function ──────────────────────
# Simulates pathology classification studies with known misclassification rates

set.seed(2026)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 1: Ki-67 Visual Estimation Study (n=200)
# Simulates inter-observer variability in Ki-67 grading
# ═══════════════════════════════════════════════════════════════════════════════

n <- 200

# True biological state (ground truth from digital counting)
true_high_ki67 <- rbinom(n, 1, 0.40)  # 40% are truly high Ki-67

# Survival outcome correlated with true Ki-67
recurrence_prob <- 0.15 + 0.35 * true_high_ki67 + rnorm(n, 0, 0.05)
recurrence <- factor(rbinom(n, 1, pmin(pmax(recurrence_prob, 0.05), 0.95)),
                     labels = c("No_Recurrence", "Recurrence"))

# Observed Ki-67 by visual estimation (misclassified)
# Sensitivity ~0.82 (some high Ki-67 called low)
# Specificity ~0.88 (some low Ki-67 called high)
sen_true <- 0.82; spec_true <- 0.88
observed_ki67 <- ifelse(true_high_ki67 == 1,
                        rbinom(n, 1, sen_true),      # truly high: sen% correctly classified
                        rbinom(n, 1, 1 - spec_true))  # truly low: (1-spec)% misclassified as high
ki67_grade <- factor(observed_ki67, labels = c("Low_Ki67", "High_Ki67"))

# Additional variables
age <- round(rnorm(n, 62, 12))
tumor_stage <- factor(sample(c("I", "II", "III"), n, TRUE, c(0.35, 0.40, 0.25)),
                      levels = c("I", "II", "III"))

misclassbias_ki67 <- data.frame(
  recurrence = recurrence,
  ki67_grade = ki67_grade,
  age = age,
  tumor_stage = tumor_stage,
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 2: IHC Scoring Discordance (n=150)
# p53 aberrant vs normal classification
# ═══════════════════════════════════════════════════════════════════════════════

n2 <- 150
diagnosis <- factor(c(rep("PanNET_G3", 75), rep("PanNEC", 75)))

# p53 with known differential misclassification
# Cases (PanNEC): pathologists better at detecting aberrant p53 (sen=0.90)
# Controls (PanNET G3): may overcall aberrant (spec=0.85)
true_p53_aberrant <- c(rbinom(75, 1, 0.10), rbinom(75, 1, 0.80))
p53_observed <- ifelse(diagnosis == "PanNEC",
  ifelse(true_p53_aberrant == 1, rbinom(n2/2, 1, 0.90), rbinom(n2/2, 1, 0.15)),
  ifelse(true_p53_aberrant == 1, rbinom(n2/2, 1, 0.85), rbinom(n2/2, 1, 0.10))
)
p53_status <- factor(p53_observed, labels = c("Normal", "Aberrant"))

misclassbias_p53 <- data.frame(
  diagnosis = diagnosis,
  p53_status = p53_status,
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Save
# ═══════════════════════════════════════════════════════════════════════════════

save(misclassbias_ki67, file = here::here("data", "misclassbias_ki67.rda"))
save(misclassbias_p53, file = here::here("data", "misclassbias_p53.rda"))
readr::write_csv(misclassbias_ki67, here::here("data-raw", "non-rda", "misclassbias_ki67.csv"))
readr::write_csv(misclassbias_p53, here::here("data-raw", "non-rda", "misclassbias_p53.csv"))

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(misclassbias_ki67, here::here("data-raw", "non-rda", "misclassbias_ki67.omv"))
}

cat("misclassbias_ki67:", nrow(misclassbias_ki67), "rows (true sen=", sen_true, ", spec=", spec_true, ")\n")
cat("misclassbias_p53:", nrow(misclassbias_p53), "rows (differential misclassification)\n")
