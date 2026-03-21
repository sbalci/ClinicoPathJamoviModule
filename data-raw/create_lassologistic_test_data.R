# ── Create Test Data for lassologistic Function ──────────────────────────────
# Generates realistic diagnostic pathology data for LASSO logistic regression:
#   - Binary classification (PanNET G3 vs PanNEC)
#   - Mix of continuous and categorical predictors
#   - IHC markers with known discriminative power
#   - Realistic clinical correlations

set.seed(2026)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 1: PanNEN Diagnostic Classification (n=120)
# Modeled after Kinowaki et al. 2026 PanNET G3 vs PanNEC study
# ═══════════════════════════════════════════════════════════════════════════════

n <- 120
n_net <- 60  # PanNET G3
n_nec <- 60  # PanNEC

diagnosis <- factor(c(rep("PanNET_G3", n_net), rep("PanNEC", n_nec)))

# IHC markers with realistic discriminative power
# p53: aberrant in ~10% NET G3, ~80% NEC
p53_aberrant <- factor(c(
  rbinom(n_net, 1, 0.10),
  rbinom(n_nec, 1, 0.80)
), labels = c("Normal", "Aberrant"))

# Rb1: lost in ~7% NET G3, ~75% NEC
rb1_loss <- factor(c(
  rbinom(n_net, 1, 0.07),
  rbinom(n_nec, 1, 0.75)
), labels = c("Retained", "Lost"))

# SSTR2A score (ordinal 0-3): high in NET G3, low in NEC
sstr2a_score <- c(
  sample(0:3, n_net, replace = TRUE, prob = c(0.05, 0.05, 0.25, 0.65)),
  sample(0:3, n_nec, replace = TRUE, prob = c(0.50, 0.25, 0.15, 0.10))
)

# SSTR2A 3+ (binary threshold)
sstr2a_3plus <- factor(ifelse(sstr2a_score >= 3, "Positive", "Negative"))

# Ki-67 index (continuous, right-skewed)
ki67_pct <- c(
  pmax(3, pmin(95, rlnorm(n_net, meanlog = 3.0, sdlog = 0.4))),   # NET G3: median ~20%
  pmax(3, pmin(95, rlnorm(n_nec, meanlog = 3.8, sdlog = 0.5)))    # NEC: median ~45%
)

# Ki-67 >40% (binary threshold)
ki67_above40 <- factor(ifelse(ki67_pct > 40, "Above_40", "Below_40"))

# DAXX/ATRX loss: ~48% in NET G3, ~10% in NEC
daxx_atrx_loss <- factor(c(
  rbinom(n_net, 1, 0.48),
  rbinom(n_nec, 1, 0.10)
), labels = c("Retained", "Lost"))

# Morphologic features
organoid_pattern <- factor(c(
  rbinom(n_net, 1, 0.83),
  rbinom(n_nec, 1, 0.41)
), labels = c("Absent", "Present"))

plasmacytoid_cells <- factor(c(
  rbinom(n_net, 1, 0.83),
  rbinom(n_nec, 1, 0.38)
), labels = c("Absent", "Present"))

interstitial_reaction <- factor(c(
  rbinom(n_net, 1, 0.38),
  rbinom(n_nec, 1, 0.79)
), labels = c("Absent", "Present"))

coexist_carcinoma <- factor(c(
  rbinom(n_net, 1, 0.00),
  rbinom(n_nec, 1, 0.35)
), labels = c("No", "Yes"))

# Mitotic count per 10 HPF (count data)
mitotic_count <- c(
  rpois(n_net, lambda = 8),
  rpois(n_nec, lambda = 22)
)

# Continuous covariates
age_years <- round(c(
  rnorm(n_net, mean = 61, sd = 14),
  rnorm(n_nec, mean = 67, sd = 11)
))

tumor_size_cm <- round(c(
  pmax(0.5, rnorm(n_net, mean = 4.0, sd = 1.8)),
  pmax(0.5, rnorm(n_nec, mean = 3.1, sd = 1.8))
), 1)

sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)))

institution <- factor(sample(paste0("Center_", LETTERS[1:5]), n, replace = TRUE,
                             prob = c(0.25, 0.25, 0.20, 0.15, 0.15)))

# Add ~3% missing data to some variables
add_missing <- function(x, pct = 0.03) {
  idx <- sample(length(x), size = max(1, round(length(x) * pct)))
  x[idx] <- NA
  x
}

ki67_pct <- add_missing(round(ki67_pct, 1))
mitotic_count <- add_missing(mitotic_count)
tumor_size_cm <- add_missing(tumor_size_cm)

# Assemble main dataset
lassologistic_pannen <- data.frame(
  diagnosis = diagnosis,
  p53_aberrant = p53_aberrant,
  rb1_loss = rb1_loss,
  sstr2a_3plus = sstr2a_3plus,
  ki67_pct = ki67_pct,
  ki67_above40 = ki67_above40,
  daxx_atrx_loss = daxx_atrx_loss,
  organoid_pattern = organoid_pattern,
  plasmacytoid_cells = plasmacytoid_cells,
  interstitial_reaction = interstitial_reaction,
  coexist_carcinoma = coexist_carcinoma,
  mitotic_count = mitotic_count,
  age_years = age_years,
  tumor_size_cm = tumor_size_cm,
  sex = sex,
  institution = institution,
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 2: Small sample (n=40) for EPV testing
# ═══════════════════════════════════════════════════════════════════════════════

set.seed(2026)
n_small <- 40
lassologistic_small <- data.frame(
  outcome = factor(rep(c("neg", "pos"), each = n_small / 2)),
  x1 = c(rnorm(n_small / 2, 3, 2), rnorm(n_small / 2, 6, 2)),
  x2 = c(rnorm(n_small / 2, 5, 1.5), rnorm(n_small / 2, 7, 1.5)),
  x3 = rnorm(n_small),
  x4 = rnorm(n_small),
  x5 = rnorm(n_small),
  x6 = rnorm(n_small),
  x7 = rnorm(n_small),
  x8 = rnorm(n_small),
  stringsAsFactors = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# Save in multiple formats
# ═══════════════════════════════════════════════════════════════════════════════

# RDA
save(lassologistic_pannen, file = here::here("data", "lassologistic_pannen.rda"))
save(lassologistic_small, file = here::here("data", "lassologistic_small.rda"))

# CSV
readr::write_csv(lassologistic_pannen,
                 here::here("data-raw", "non-rda", "lassologistic_pannen.csv"))

# OMV
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(lassologistic_pannen,
                          here::here("data-raw", "non-rda", "lassologistic_pannen.omv"))
}

cat("Test data generation complete.\n")
cat("  lassologistic_pannen:", nrow(lassologistic_pannen), "rows,",
    ncol(lassologistic_pannen), "cols\n")
cat("  lassologistic_small:", nrow(lassologistic_small), "rows,",
    ncol(lassologistic_small), "cols\n")
