# Generate Simulated Dataset Based on Duan et al. 2023
# "Impact of tissue sampling on detection of venous invasion in colorectal cancer"
# Histopathology. doi:10.1111/his.15030

# This dataset simulates the key findings from Duan 2023:
# - VI detection sensitivity increases with number of blocks examined
# - Sensitivity at different block counts: 1=35%, 3=66%, 5=84%, 6=95%, 7=97%, 8=100%
# - EMVI sensitivity: 1=35%, 3=73%, 5=89%, 6=96%, 7-8=96%
# - N=217 total cases, 55% VI+, 37% EMVI+

set.seed(2023)

# Study parameters from Duan 2023
n_cases <- 217
vi_rate <- 0.55
emvi_rate <- 0.37

# VI detection probabilities by block (cumulative sensitivity from Figure 4A)
vi_sensitivity_curve <- c(0.35, 0.57, 0.66, 0.76, 0.84, 0.95, 0.97, 1.00)

# EMVI detection probabilities by block (cumulative sensitivity from Figure 4B)
emvi_sensitivity_curve <- c(0.35, 0.61, 0.73, 0.80, 0.89, 0.96, 0.96, 0.96)

# Generate case characteristics
cases <- data.frame(
  case_id = sprintf("CRC%03d", 1:n_cases),
  age = round(rnorm(n_cases, mean = 65, sd = 12)),
  sex = sample(c("Male", "Female"), n_cases, replace = TRUE, prob = c(0.53, 0.47)),
  site = sample(c("Right colon", "Left colon", "Rectum"),
                n_cases, replace = TRUE, prob = c(0.22, 0.39, 0.39)),
  stage = sample(c("I", "II", "III", "IV"),
                 n_cases, replace = TRUE, prob = c(0.15, 0.39, 0.40, 0.07)),
  tumor_size_cm = round(rgamma(n_cases, shape = 3, scale = 1.5), 1),
  neoadjuvant = sample(c(0, 1), n_cases, replace = TRUE, prob = c(0.79, 0.21)),
  blocks_examined = sample(2:8, n_cases, replace = TRUE, prob = c(0.02, 0.05, 0.10, 0.15, 0.25, 0.28, 0.15))
)

# Adjust blocks for very small tumors
cases$blocks_examined[cases$tumor_size_cm < 2] <- pmin(cases$blocks_examined[cases$tumor_size_cm < 2], 5)

# Generate VI status
cases$vi_positive <- rbinom(n_cases, 1, vi_rate)
n_vi_positive <- sum(cases$vi_positive)

# Generate EMVI status (subset of VI+)
# Among VI+, 37/55 = 67% are EMVI+
# Among VI-, all are EMVI-
cases$emvi_positive <- 0
cases$emvi_positive[cases$vi_positive == 1] <- rbinom(n_vi_positive, 1, 0.67)

# Generate first detection block for VI+ cases
# Use inverse CDF method to match observed sensitivity curve
cases$first_vi_block <- NA_integer_

for (i in which(cases$vi_positive == 1)) {
  max_blocks <- cases$blocks_examined[i]

  # Probability that VI is first detected in each block
  # P(first detected in block k) = P(detected by block k) - P(detected by block k-1)

  # Get cumulative probabilities up to max_blocks
  cum_probs <- vi_sensitivity_curve[1:min(max_blocks, length(vi_sensitivity_curve))]

  # Calculate marginal probabilities
  marginal_probs <- c(cum_probs[1], diff(cum_probs))

  # If not detected by last available block, assign remainder to last block
  if (sum(marginal_probs) < 1) {
    marginal_probs[length(marginal_probs)] <- marginal_probs[length(marginal_probs)] + (1 - sum(marginal_probs))
  }

  # Sample which block first detected
  cases$first_vi_block[i] <- sample(1:length(marginal_probs), 1, prob = marginal_probs)
}

# Generate first detection block for EMVI+ cases
cases$first_emvi_block <- NA_integer_

for (i in which(cases$emvi_positive == 1)) {
  max_blocks <- cases$blocks_examined[i]

  cum_probs <- emvi_sensitivity_curve[1:min(max_blocks, length(emvi_sensitivity_curve))]
  marginal_probs <- c(cum_probs[1], diff(cum_probs))

  if (sum(marginal_probs) < 1) {
    marginal_probs[length(marginal_probs)] <- marginal_probs[length(marginal_probs)] + (1 - sum(marginal_probs))
  }

  cases$first_emvi_block[i] <- sample(1:length(marginal_probs), 1, prob = marginal_probs)
}

# Generate linear spiculation (macroscopic finding)
# 22% of cases have LS
# Among LS+: 71% have EMVI (in non-neoadjuvant cases)
# Among LS-: 29% have EMVI
cases$linear_spiculation <- 0

# Only consider non-neoadjuvant cases for LS-EMVI association
non_neoadj <- cases$neoadjuvant == 0

# Assign LS to achieve desired association
# Start with random 22%
cases$linear_spiculation[sample(which(non_neoadj), round(0.22 * sum(non_neoadj)))] <- 1

# Adjust to get closer to 71% EMVI+ among LS+ and 29% among LS-
# This is approximate - real data would have exact match
ls_positive <- cases$linear_spiculation == 1 & non_neoadj
ls_negative <- cases$linear_spiculation == 0 & non_neoadj

# For LS+ cases: increase EMVI if currently 0, with probability
for (i in which(ls_positive)) {
  if (cases$emvi_positive[i] == 0 && runif(1) < 0.60) {
    cases$emvi_positive[i] <- 1
    cases$vi_positive[i] <- 1  # EMVI implies VI
    # Assign first detection block
    max_blocks <- cases$blocks_examined[i]
    cum_probs <- emvi_sensitivity_curve[1:min(max_blocks, length(emvi_sensitivity_curve))]
    marginal_probs <- c(cum_probs[1], diff(cum_probs))
    if (sum(marginal_probs) < 1) {
      marginal_probs[length(marginal_probs)] <- marginal_probs[length(marginal_probs)] + (1 - sum(marginal_probs))
    }
    cases$first_emvi_block[i] <- sample(1:length(marginal_probs), 1, prob = marginal_probs)
    cases$first_vi_block[i] <- min(cases$first_emvi_block[i],
                                     sample(1:cases$blocks_examined[i], 1))
  }
}

# Add tumor size category
cases$tumor_size_cat <- ifelse(cases$tumor_size_cm >= 5, "≥5 cm", "<5 cm")

# Add outcome labels
cases$vi_status <- factor(cases$vi_positive, levels = 0:1, labels = c("VI-", "VI+"))
cases$emvi_status <- factor(cases$emvi_positive, levels = 0:1, labels = c("EMVI-", "EMVI+"))
cases$ls_status <- factor(cases$linear_spiculation, levels = 0:1, labels = c("LS-", "LS+"))

# Reorder columns
duan2023_vi_blocks <- cases[, c("case_id", "age", "sex", "site", "stage",
                                 "tumor_size_cm", "tumor_size_cat", "neoadjuvant",
                                 "blocks_examined", "linear_spiculation", "ls_status",
                                 "vi_positive", "vi_status", "first_vi_block",
                                 "emvi_positive", "emvi_status", "first_emvi_block")]

# Verify sensitivity matches expected values
cat("\nVerification of Simulated Data:\n")
cat("================================\n\n")

cat("Overall Statistics:\n")
cat(sprintf("Total cases: %d\n", nrow(duan2023_vi_blocks)))
cat(sprintf("VI+ cases: %d (%.1f%%)\n", sum(duan2023_vi_blocks$vi_positive),
            100 * mean(duan2023_vi_blocks$vi_positive)))
cat(sprintf("EMVI+ cases: %d (%.1f%%)\n", sum(duan2023_vi_blocks$emvi_positive),
            100 * mean(duan2023_vi_blocks$emvi_positive)))
cat(sprintf("Linear spiculation: %d (%.1f%%)\n\n", sum(duan2023_vi_blocks$linear_spiculation),
            100 * mean(duan2023_vi_blocks$linear_spiculation)))

cat("VI Detection Sensitivity by Number of Blocks:\n")
vi_pos <- duan2023_vi_blocks$vi_positive == 1
for (n in 1:8) {
  detected <- sum(vi_pos & duan2023_vi_blocks$first_vi_block <= n, na.rm = TRUE)
  total_vi <- sum(vi_pos)
  sensitivity <- detected / total_vi
  cat(sprintf("  %d block%s: %.1f%% (%d/%d)\n", n, ifelse(n==1, "", "s"),
              100 * sensitivity, detected, total_vi))
}

cat("\nEMVI Detection Sensitivity by Number of Blocks:\n")
emvi_pos <- duan2023_vi_blocks$emvi_positive == 1
for (n in 1:8) {
  detected <- sum(emvi_pos & duan2023_vi_blocks$first_emvi_block <= n, na.rm = TRUE)
  total_emvi <- sum(emvi_pos)
  sensitivity <- detected / total_emvi
  cat(sprintf("  %d block%s: %.1f%% (%d/%d)\n", n, ifelse(n==1, "", "s"),
              100 * sensitivity, detected, total_emvi))
}

cat("\nLinear Spiculation vs EMVI (non-neoadjuvant cases only):\n")
non_neoadj_cases <- duan2023_vi_blocks[duan2023_vi_blocks$neoadjuvant == 0, ]
ls_pos <- non_neoadj_cases$linear_spiculation == 1
ls_neg <- non_neoadj_cases$linear_spiculation == 0
emvi_rate_ls_pos <- mean(non_neoadj_cases$emvi_positive[ls_pos])
emvi_rate_ls_neg <- mean(non_neoadj_cases$emvi_positive[ls_neg])
cat(sprintf("  LS+: %.1f%% EMVI+ (%d/%d)\n", 100 * emvi_rate_ls_pos,
            sum(non_neoadj_cases$emvi_positive[ls_pos]), sum(ls_pos)))
cat(sprintf("  LS-: %.1f%% EMVI+ (%d/%d)\n", 100 * emvi_rate_ls_neg,
            sum(non_neoadj_cases$emvi_positive[ls_neg]), sum(ls_neg)))

# Save datasets
write.csv(duan2023_vi_blocks,
          file = "data/duan2023_vi_blocks.csv",
          row.names = FALSE)

saveRDS(duan2023_vi_blocks,
        file = "data/duan2023_vi_blocks.rds")

cat("\n✓ Dataset saved to data/duan2023_vi_blocks.csv and .rds\n")
cat("✓ Ready for use in pathsampling module\n\n")

cat("Usage Instructions for pathsampling module:\n")
cat("==========================================\n")
cat("1. Load duan2023_vi_blocks.csv in jamovi\n")
cat("2. In pathsampling module:\n")
cat("   - Analysis Context: Tumor Sampling (VI/EMVI/PNI/Budding)\n")
cat("   - Total Samples: blocks_examined\n")
cat("   - First Detection: first_vi_block (for VI) or first_emvi_block (for EMVI)\n")
cat("   - Target Confidence: 0.95\n")
cat("   - Maximum Samples: 8\n")
cat("   - Enable: Show Empirical Cumulative Detection\n")
cat("   - Enable: Show Bootstrap Analysis\n")
cat("3. Optional stratification:\n")
cat("   - Group By: tumor_size_cat (compare <5cm vs ≥5cm)\n")
cat("   - Sample Type: stage (compare by AJCC stage)\n\n")

# Return dataset invisibly
invisible(duan2023_vi_blocks)
