# ── Create Test Data for lassologistic Function ──────────────────────────────
# Generates realistic diagnostic pathology data for LASSO logistic regression:
#   - Binary classification (PanNET G3 vs PanNEC)
#   - Mix of continuous and categorical predictors
#   - IHC markers with known discriminative power
#   - Realistic clinical correlations

set.seed(2026)

# ═══════════════════════════════════════════════════════════════════════════════
# Dataset 1: PanNEN Diagnostic Classification (n=200)
# Loosely modeled on PanNET G3 vs PanNEC series.
#
# The classes OVERLAP on purpose. The first version (n = 120) separated them
# completely: mitoses Poisson(8) vs Poisson(22) nearly split the classes alone,
# coexisting carcinoma never occurred in a NET, and half the NET G3 had Ki-67 at or
# below 20% with mitoses mostly under 20/2 mm2, which is grade 2, not grade 3. Under
# the defaults the demo then showed an apparent AUC of 1.000, a calibration slope
# that could not be estimated, and scoring odds ratios in the tens of thousands. Now (seed 2026, all 12 candidates):
# apparent AUC 0.961 with the 1SE rule and 0.974 with Minimum CV Error, about 0.94
# in new data from this generator, and an estimable calibration slope.
# n = 200 keeps the events per model term above 5 with all 12 candidates.
# ═══════════════════════════════════════════════════════════════════════════════

n_net <- 100  # PanNET G3
n_nec <- 100  # PanNEC
n <- n_net + n_nec

diagnosis <- factor(c(rep("PanNET_G3", n_net), rep("PanNEC", n_nec)))

# A binary feature with probability p[1] in NET G3 and p[2] in NEC. levels = 0:1 keeps
# both labels even if a draw happens to contain one value only.
by_class <- function(p, labels) factor(c(rbinom(n_net, 1, p[1]), rbinom(n_nec, 1, p[2])),
                                       levels = 0:1, labels = labels)

# p53: aberrant in ~12% NET G3, ~65% NEC
p53_aberrant <- by_class(c(0.12, 0.65), c("Normal", "Aberrant"))

# Rb1: lost in ~8% NET G3, ~55% NEC
rb1_loss <- by_class(c(0.08, 0.55), c("Retained", "Lost"))

# SSTR2A score (ordinal 0-3): higher in NET G3, lower in NEC
sstr2a_score <- c(
  sample(0:3, n_net, replace = TRUE, prob = c(0.15, 0.15, 0.30, 0.40)),
  sample(0:3, n_nec, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15))
)

# SSTR2A 3+ (binary threshold)
sstr2a_3plus <- factor(ifelse(sstr2a_score >= 3, "Positive", "Negative"))

# Ki-67 index. Simulated NET G3 are grade 3 through Ki-67 (> 20%; WHO grades by the higher of
# Ki-67 and the mitotic count). NET G3 median ~36%, NEC ~60%, with the overlap seen in practice.
ki67_pct <- c(
  pmin(95, 20 + rlnorm(n_net, meanlog = log(16), sdlog = 0.75)),
  pmin(98, pmax(21, rnorm(n_nec, mean = 60, sd = 20)))
)

# Ki-67 >40% (binary threshold)
# Below_40 is the reference level, so the model term reads "Above_40 vs Below_40".
ki67_above40 <- factor(ifelse(ki67_pct > 40, "Above_40", "Below_40"), levels = c("Below_40", "Above_40"))

# DAXX/ATRX loss: ~40% in NET G3, ~12% in NEC
daxx_atrx_loss <- by_class(c(0.40, 0.12), c("Retained", "Lost"))

# Morphologic features (weakly discriminating)
organoid_pattern <- by_class(c(0.65, 0.50), c("Absent", "Present"))
plasmacytoid_cells <- by_class(c(0.50, 0.40), c("Absent", "Present"))
interstitial_reaction <- by_class(c(0.45, 0.60), c("Absent", "Present"))
coexist_carcinoma <- by_class(c(0.03, 0.20), c("No", "Yes"))

# Mitotic count per 2 mm2 (WHO): overdispersed counts that overlap between the grades. Many NET G3
# fall below the 20/2 mm2 grade 3 threshold, which is consistent with grade 3 by Ki-67.
mitotic_count <- c(
  rnbinom(n_net, mu = 14, size = 2),
  rnbinom(n_nec, mu = 22, size = 2)
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
                          here::here("data-raw", "non-rda", "lassologistic_pannen.omv"),
                          frcWrt = TRUE)
}

cat("Test data generation complete.\n")
cat("  lassologistic_pannen:", nrow(lassologistic_pannen), "rows,",
    ncol(lassologistic_pannen), "cols\n")
cat("  lassologistic_small:", nrow(lassologistic_small), "rows,",
    ncol(lassologistic_small), "cols\n")
