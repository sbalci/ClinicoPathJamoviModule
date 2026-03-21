# ── Create Test Data for nonparametric Function ──────────────────────────────
# Generates realistic clinical pathology data for testing all 12 test types:
#   Mann-Whitney U, Kruskal-Wallis, Wilcoxon signed-rank, Friedman,
#   Median, Van der Waerden, Mood's median, Cochran's Q, Page's trend,
#   McNemar, Sign test, Jonckheere-Terpstra

set.seed(2026)

n_per_group <- 30
n_total <- n_per_group * 4  # 4 groups for ordinal grading

# ── 1. Independent groups data (KW, Mann-Whitney, JT, trend tests) ──────────

# Simulate tumor Ki-67 index across WHO grades (ordinal: G1 < G2a < G2b < G3)
tumor_grade <- factor(
  rep(c("G1", "G2a", "G2b", "G3"), each = n_per_group),
  levels = c("G1", "G2a", "G2b", "G3"),
  ordered = TRUE
)

# Ki-67 increases with grade (right-skewed, realistic clinical range)
ki67_index <- c(
  pmax(0.5, rlnorm(n_per_group, meanlog = 0.5, sdlog = 0.6)),   # G1: median ~1.6%
  pmax(0.5, rlnorm(n_per_group, meanlog = 1.5, sdlog = 0.5)),   # G2a: median ~4.5%
  pmax(0.5, rlnorm(n_per_group, meanlog = 2.5, sdlog = 0.4)),   # G2b: median ~12%
  pmax(0.5, rlnorm(n_per_group, meanlog = 3.2, sdlog = 0.5))    # G3: median ~25%
)

# Mitotic count per 10 HPF (count data, not normally distributed)
mitotic_count <- c(
  rpois(n_per_group, lambda = 1),    # G1
  rpois(n_per_group, lambda = 5),    # G2a
  rpois(n_per_group, lambda = 12),   # G2b
  rpois(n_per_group, lambda = 25)    # G3
)

# Tumor size (cm) — partially correlated with grade
tumor_size <- c(
  pmax(0.3, rnorm(n_per_group, mean = 2.0, sd = 1.0)),
  pmax(0.3, rnorm(n_per_group, mean = 3.2, sd = 1.5)),
  pmax(0.3, rnorm(n_per_group, mean = 4.5, sd = 2.0)),
  pmax(0.3, rnorm(n_per_group, mean = 5.0, sd = 2.5))
)

# SSTR2A IHC score (ordinal 0-3)
sstr2a_score <- c(
  sample(0:3, n_per_group, replace = TRUE, prob = c(0.05, 0.05, 0.25, 0.65)),
  sample(0:3, n_per_group, replace = TRUE, prob = c(0.10, 0.15, 0.35, 0.40)),
  sample(0:3, n_per_group, replace = TRUE, prob = c(0.25, 0.30, 0.30, 0.15)),
  sample(0:3, n_per_group, replace = TRUE, prob = c(0.50, 0.25, 0.15, 0.10))
)

# Sex (balanced)
sex <- factor(sample(c("Male", "Female"), n_total, replace = TRUE))

# Institution (for multi-center testing)
institution <- factor(sample(c("Hospital_A", "Hospital_B", "Hospital_C"),
                             n_total, replace = TRUE, prob = c(0.4, 0.35, 0.25)))

# Binary outcome (2 groups only, for Mann-Whitney)
diagnosis_binary <- factor(
  ifelse(tumor_grade %in% c("G1", "G2a"), "PanNET_G1G2", "PanNET_G3_NEC"),
  levels = c("PanNET_G1G2", "PanNET_G3_NEC")
)

# ── 2. Paired data (Wilcoxon signed-rank, Sign test) ────────────────────────

n_paired <- 25
subject_id <- factor(paste0("Patient_", sprintf("%03d", rep(1:n_paired, 2))))
timepoint <- factor(rep(c("Pre_treatment", "Post_treatment"), each = n_paired),
                    levels = c("Pre_treatment", "Post_treatment"))

# Biomarker measured before and after treatment (paired)
biomarker_pre <- rlnorm(n_paired, meanlog = 2, sdlog = 0.5)
treatment_effect <- rnorm(n_paired, mean = -2, sd = 1.5)  # reduction
biomarker_post <- pmax(0.1, biomarker_pre + treatment_effect)
biomarker_level <- c(biomarker_pre, biomarker_post)

# Shuffle to simulate real data entry (not in matched order)
paired_idx <- sample(2 * n_paired)
subject_id_shuffled <- subject_id[paired_idx]
timepoint_shuffled <- timepoint[paired_idx]
biomarker_shuffled <- biomarker_level[paired_idx]

# ── 3. Repeated measures data (Friedman, Page's trend, Cochran's Q) ─────────

n_subjects_rm <- 20
subject_rm <- factor(paste0("Subj_", sprintf("%02d", rep(1:n_subjects_rm, 3))))
condition_rm <- factor(rep(c("Baseline", "Month_3", "Month_6"), each = n_subjects_rm),
                       levels = c("Baseline", "Month_3", "Month_6"))

# Continuous outcome for Friedman (tumor marker declining over time)
baseline_vals <- rnorm(n_subjects_rm, mean = 15, sd = 4)
marker_rm <- c(
  baseline_vals,
  baseline_vals - rnorm(n_subjects_rm, mean = 3, sd = 2),   # Month 3
  baseline_vals - rnorm(n_subjects_rm, mean = 6, sd = 2.5)  # Month 6
)

# Binary outcome for Cochran's Q (response yes/no across 3 conditions)
response_rm <- c(
  rbinom(n_subjects_rm, 1, prob = 0.3),   # Baseline: 30% respond
  rbinom(n_subjects_rm, 1, prob = 0.5),   # Month 3: 50% respond
  rbinom(n_subjects_rm, 1, prob = 0.7)    # Month 6: 70% respond
)

# ── 4. Paired categorical data (McNemar) ────────────────────────────────────

n_mcnemar <- 50
test_before <- factor(rbinom(n_mcnemar, 1, 0.4), labels = c("Negative", "Positive"))
# After treatment, some change
change_prob <- ifelse(test_before == "Positive", 0.3, 0.15)
test_after <- factor(
  ifelse(runif(n_mcnemar) < change_prob,
         ifelse(test_before == "Positive", "Negative", "Positive"),
         as.character(test_before)),
  levels = c("Negative", "Positive")
)

# ── 5. Add ~5% missing data ────────────────────────────────────────────────

add_missing <- function(x, pct = 0.05) {
  idx <- sample(length(x), size = round(length(x) * pct))
  x[idx] <- NA
  x
}

ki67_index <- add_missing(ki67_index)
mitotic_count <- add_missing(mitotic_count)
tumor_size <- add_missing(tumor_size)

# ── 6. Assemble datasets ───────────────────────────────────────────────────

# Main independent groups dataset
nonparametric_independent <- data.frame(
  tumor_grade = tumor_grade,
  diagnosis_binary = diagnosis_binary,
  ki67_index = round(ki67_index, 2),
  mitotic_count = mitotic_count,
  tumor_size_cm = round(tumor_size, 1),
  sstr2a_score = sstr2a_score,
  sex = sex,
  institution = institution,
  stringsAsFactors = TRUE
)

# Paired dataset
nonparametric_paired <- data.frame(
  subject_id = subject_id_shuffled,
  timepoint = timepoint_shuffled,
  biomarker_level = round(biomarker_shuffled, 2),
  stringsAsFactors = TRUE
)

# Repeated measures dataset
nonparametric_repeated <- data.frame(
  subject_id = subject_rm,
  condition = condition_rm,
  tumor_marker = round(marker_rm, 2),
  treatment_response = response_rm,
  stringsAsFactors = TRUE
)

# McNemar dataset
nonparametric_mcnemar <- data.frame(
  test_before = test_before,
  test_after = test_after,
  stringsAsFactors = TRUE
)

# ── 7. Save in multiple formats ────────────────────────────────────────────

# RDA
save(nonparametric_independent,
     file = here::here("data", "nonparametric_independent.rda"))
save(nonparametric_paired,
     file = here::here("data", "nonparametric_paired.rda"))
save(nonparametric_repeated,
     file = here::here("data", "nonparametric_repeated.rda"))
save(nonparametric_mcnemar,
     file = here::here("data", "nonparametric_mcnemar.rda"))

# CSV
readr::write_csv(nonparametric_independent,
                 here::here("data-raw", "non-rda", "nonparametric_independent.csv"))
readr::write_csv(nonparametric_paired,
                 here::here("data-raw", "non-rda", "nonparametric_paired.csv"))
readr::write_csv(nonparametric_repeated,
                 here::here("data-raw", "non-rda", "nonparametric_repeated.csv"))

# OMV (jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(nonparametric_independent,
                          here::here("data-raw", "non-rda", "nonparametric_independent.omv"))
  jmvReadWrite::write_omv(nonparametric_paired,
                          here::here("data-raw", "non-rda", "nonparametric_paired.omv"))
  jmvReadWrite::write_omv(nonparametric_repeated,
                          here::here("data-raw", "non-rda", "nonparametric_repeated.omv"))
}

cat("Test data generation complete.\n")
cat("  nonparametric_independent:", nrow(nonparametric_independent), "rows,",
    ncol(nonparametric_independent), "cols\n")
cat("  nonparametric_paired:", nrow(nonparametric_paired), "rows,",
    ncol(nonparametric_paired), "cols\n")
cat("  nonparametric_repeated:", nrow(nonparametric_repeated), "rows,",
    ncol(nonparametric_repeated), "cols\n")
cat("  nonparametric_mcnemar:", nrow(nonparametric_mcnemar), "rows,",
    ncol(nonparametric_mcnemar), "cols\n")
