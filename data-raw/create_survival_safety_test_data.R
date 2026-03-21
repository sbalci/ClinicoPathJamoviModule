# ═══════════════════════════════════════════════════════════
# Safety Check Test Data: survival
# ═══════════════════════════════════════════════════════════
# Generates specialized datasets to test clinical safety
# checks added to the survival function:
#   - EPV (events per variable) warnings
#   - Extreme hazard ratio detection
#   - Cox convergence edge cases
#   - RMST tau default behavior
#
# Generated: 2026-03-22
# Seed: 2026
# ═══════════════════════════════════════════════════════════

library(here)

set.seed(2026)

# ───────────────────────────────────────────────────────────
# 1. Low-EPV dataset: many factor levels, few events
#    12 events across 6 levels → EPV = 2.0 (< 5 critical)
# ───────────────────────────────────────────────────────────

n_low_epv <- 60
survival_low_epv <- data.frame(
  PatientID = 1:n_low_epv,
  FollowUpMonths = round(abs(rnorm(n_low_epv, mean = 36, sd = 12)), 1),
  Status = factor(
    sample(c("Alive", "Dead"), n_low_epv, replace = TRUE, prob = c(0.80, 0.20)),
    levels = c("Alive", "Dead")
  ),
  TumorSubtype = factor(
    sample(paste0("Subtype_", LETTERS[1:6]), n_low_epv, replace = TRUE),
    levels = paste0("Subtype_", LETTERS[1:6])
  ),
  Age = round(rnorm(n_low_epv, mean = 60, sd = 10)),
  stringsAsFactors = FALSE
)

save(survival_low_epv, file = here("data", "survival_low_epv.rda"), compress = "xz")

# ───────────────────────────────────────────────────────────
# 2. Extreme HR dataset: rare category with very different
#    survival → produces HR > 10 or HR < 0.1
# ───────────────────────────────────────────────────────────

n_extreme <- 100
group <- factor(
  sample(c("Common", "Rare"), n_extreme, replace = TRUE, prob = c(0.85, 0.15)),
  levels = c("Common", "Rare")
)

# Rare group: very short survival (HR ~15-20x)
time_common <- round(rexp(sum(group == "Common"), rate = 1/60), 1)
time_rare <- round(rexp(sum(group == "Rare"), rate = 1/4), 1)

survival_extreme_hr <- data.frame(
  PatientID = 1:n_extreme,
  FollowUpMonths = ifelse(group == "Common", time_common, time_rare),
  Status = factor(
    ifelse(
      (group == "Common" & runif(n_extreme) < 0.4) |
      (group == "Rare" & runif(n_extreme) < 0.95),
      "Dead", "Alive"
    ),
    levels = c("Alive", "Dead")
  ),
  RiskGroup = group,
  Age = round(rnorm(n_extreme, mean = 65, sd = 10)),
  stringsAsFactors = FALSE
)

# Ensure minimum events
n_events <- sum(survival_extreme_hr$Status == "Dead")
if (n_events < 15) {
  idx <- which(survival_extreme_hr$Status == "Alive")
  flip <- sample(idx, min(15 - n_events, length(idx)))
  survival_extreme_hr$Status[flip] <- "Dead"
}

save(survival_extreme_hr, file = here("data", "survival_extreme_hr.rda"), compress = "xz")

# ───────────────────────────────────────────────────────────
# 3. RMST default tau dataset: short follow-up where tau
#    matters — useful for testing the tau explanation note
# ───────────────────────────────────────────────────────────

n_rmst <- 80
survival_rmst_test <- data.frame(
  PatientID = 1:n_rmst,
  FollowUpMonths = round(pmax(1, rexp(n_rmst, rate = 1/24)), 1),
  Status = factor(
    sample(c("Alive", "Dead"), n_rmst, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Alive", "Dead")
  ),
  Treatment = factor(
    sample(c("Standard", "Experimental"), n_rmst, replace = TRUE),
    levels = c("Standard", "Experimental")
  ),
  stringsAsFactors = FALSE
)

save(survival_rmst_test, file = here("data", "survival_rmst_test.rda"), compress = "xz")


# ───────────────────────────────────────────────────────────
# 4. Write CSV versions for non-R testing
# ───────────────────────────────────────────────────────────

write.csv(survival_low_epv, here("data-raw", "non-rda", "survival_low_epv.csv"), row.names = FALSE)
write.csv(survival_extreme_hr, here("data-raw", "non-rda", "survival_extreme_hr.csv"), row.names = FALSE)
write.csv(survival_rmst_test, here("data-raw", "non-rda", "survival_rmst_test.csv"), row.names = FALSE)

cat("Safety test datasets generated successfully.\n")
cat("  survival_low_epv.rda     - EPV < 5 (many levels, few events)\n")
cat("  survival_extreme_hr.rda  - Extreme HR (>10) between groups\n")
cat("  survival_rmst_test.rda   - RMST tau default testing\n")
