# omentum analysis simulation

## https://chatgpt.com/share/68e80715-ad08-8002-8786-42e49bd189f0


# Set seed for reproducibility
set.seed(123)

# Define number of patients
N <- 1000

# Randomly assign number of omental sections per patient (between 5 and 10)
n_sections <- sample(5:10, N, replace = TRUE)

# Randomly assign whether metastasis is present (30% probability)
metastasis_present <- rbinom(N, 1, prob = 0.3)

# Prepare vectors/containers for simulation results
pos_count    <- integer(N)        # number of positive sections
first_pos    <- integer(N)        # index of first positive section (NA if none)
first_pos[]  <- NA               # initialize with NA
pos_blocks   <- vector("list", N) # list to hold indices of all positive sections

# Loop over each patient to simulate section results
for(i in 1:N) {
  if(metastasis_present[i] == 1) {
    # Metastasis present: choose involvement category
    if(runif(1) < 0.5) {
      # Heavy involvement: high per-section tumor probability (60–90%)
      p_section <- runif(1, min = 0.6, max = 0.9)
    } else {
      # Minimal involvement: low per-section tumor probability (10–30%)
      p_section <- runif(1, min = 0.1, max = 0.3)
    }
    # Simulate each section as positive or negative for tumor
    positives <- which(runif(n_sections[i]) < p_section)
    pos_count[i]  <- length(positives)
    pos_blocks[[i]] <- positives  # store indices of positive sections
    if(pos_count[i] > 0) {
      first_pos[i] <- positives[1]  # first positive block (they are in ascending order)
    }
    # Note: If pos_count[i] == 0 here, it means metastasis was present
    # but none of the sampled sections contained tumor (metastasis missed).
  } else {
    # No metastasis present
    pos_count[i]    <- 0
    pos_blocks[[i]] <- integer(0)   # empty vector
    # first_pos remains NA
  }
}

# Combine into a data frame
ovarian_data <- data.frame(
  patient_id         = 1:N,
  metastasis_present = metastasis_present,
  n_sections         = n_sections,
  pos_count          = pos_count,
  first_pos          = first_pos
)
# Add the list-column of positive block indices
ovarian_data$pos_blocks <- pos_blocks

# --- Enforce pathologist final diagnosis rule ---------------------------------
# Keep a copy of the originally simulated metastasis indicator (for reference)
ovarian_data$metastasis_present_sim <- ovarian_data$metastasis_present
# Final diagnosis: if none of the blocks are positive, metastasis_present must be 0
# (i.e., pathologist examined all blocks and found no metastasis)
ovarian_data$metastasis_present <- as.integer(ovarian_data$pos_count > 0)
# -----------------------------------------------------------------------------

# View first 6 rows of the simulated dataset
head(ovarian_data)



# Add a column for whether metastasis was detected (at least one positive section found)
ovarian_data$detected_any <- ifelse(ovarian_data$pos_count > 0, 1, 0)

# Filter to patients who truly have metastasis (ground truth from simulation)
metastatic_cases_gt <- subset(ovarian_data, metastasis_present_sim == 1)

head(metastatic_cases_gt)

# Empirical sensitivity (conditional on true metastasis)
empirical_detection <- aggregate(detected_any ~ n_sections, data = metastatic_cases_gt, FUN = mean)
empirical_detection


# Estimate per-section tumor probability q among TRUE metastasis-positive patients (includes misses with pos_count==0)
total_sections_metastatic <- sum(metastatic_cases_gt$n_sections)
total_tumor_sections      <- sum(metastatic_cases_gt$pos_count)
q_estimate <- total_tumor_sections / total_sections_metastatic
q_estimate


# Compute model-based detection probability for n = 1...10
n_vals <- 1:10
model_detect_prob <- 1 - (1 - q_estimate)^n_vals

# Combine empirical vs model for comparison
compare_detect <- data.frame(
  n_sections = n_vals,
  model_prob = model_detect_prob
)

# Merge empirical detection rates (for n that exist in data)
compare_detect <- merge(compare_detect, empirical_detection, by = "n_sections", all.x = TRUE)
names(compare_detect)[names(compare_detect) == "detected_any"] <- "empirical_prob"
compare_detect


# Logistic regression (SENSITIVITY): conditional on true metastasis
logit_sens <- glm(detected_any ~ n_sections, data = metastatic_cases_gt, family = binomial)
summary(logit_sens)
# OR per extra section (sensitivity)
or_sens <- exp(coef(logit_sens)["n_sections"])
ci_sens <- exp(confint(logit_sens, "n_sections"))
cat(sprintf("Sensitivity OR per additional section: %.2f (95%% CI %.2f–%.2f)\n", or_sens, ci_sens[1], ci_sens[2]))

# Logistic regression (MARGINAL): all patients (reflects prevalence × sensitivity)
logit_marg <- glm(detected_any ~ n_sections, data = ovarian_data, family = binomial)
summary(logit_marg)
# OR per extra section (marginal)
or_marg <- exp(coef(logit_marg)["n_sections"])
ci_marg <- exp(confint(logit_marg, "n_sections"))
cat(sprintf("Marginal OR per additional section: %.2f (95%% CI %.2f–%.2f)\n", or_marg, ci_marg[1], ci_marg[2]))


# Solve for n that gives 95% detection probability
q <- q_estimate  # per-section detection probability
required_n <- log(0.05) / log(1 - q)
required_n_ceil <- ceiling(required_n)
cat(sprintf("Estimated sections needed for ~95%% detection: %.1f (so at least %d sections)\n",
            required_n, required_n_ceil))


# Prepare data for plotting
# Empirical detection rate among metastatic cases by n
empirical_detection$detected_any <- empirical_detection$detected_any * 100  # convert to percentage
# Model curve (binomial) in percentage
plot_data <- data.frame(
  n_sections = n_vals,
  DetectionProb_model = model_detect_prob * 100
)
# Logistic regression predicted probabilities (SENSITIVITY, conditional on true metastasis)
newdata <- data.frame(n_sections = n_vals)
plot_data$DetectionProb_logistic_sens <- predict(logit_sens, newdata, type = "response") * 100

# Base plot
plot(NULL, xlim = c(1, 10), ylim = c(0, 100), xlab = "Number of Omental Sections",
     ylab = "Probability of Detecting Metastasis (%)", main = "Metastasis Detection vs. Sections")
# Add model curve
lines(plot_data$n_sections, plot_data$DetectionProb_model, col = "blue", lwd = 2, lty = 2)
# Add logistic regression curve
lines(plot_data$n_sections, plot_data$DetectionProb_logistic_sens, col = "red", lwd = 2)
# Add empirical points
points(empirical_detection$n_sections, empirical_detection$detected_any * 100,
       pch = 16, col = "black")
legend("bottomright", legend = c("Binomial Model (sensitivity)", "Logistic (sensitivity)", "Empirical (sensitivity)"),
       col = c("blue", "red", "black"), lty = c(2, 1, NA), pch = c(NA, NA, 16), lwd = 2)


# Filter to metastatic cases with 10 sections
cases_10 <- subset(ovarian_data, metastasis_present_sim == 1 & n_sections == 10)
missed_if_5 <- sum(cases_10$detected_any == 1 & !is.na(cases_10$first_pos) & cases_10$first_pos > 5)
total_with_met_10 <- nrow(cases_10)
cat(sprintf("Among %d metastasis-positive patients with 10 sections, %d (%.1f%%) had their first tumor focus in sections 6-10, and would be missed if only 5 sections were examined.\n",
            total_with_met_10, missed_if_5, 100 * missed_if_5/ total_with_met_10))


# Function to simulate detection given a fixed number of sections for all patients
simulate_detection <- function(N, n_sections_fixed, metastasis_prob = 0.3) {
  presences <- rbinom(N, 1, metastasis_prob)
  detected_flags <- integer(N)
  for(i in 1:N) {
    if(presences[i] == 1) {
      # draw a per-section p as before
      p_section <- if(runif(1) < 0.5) runif(1, 0.6, 0.9) else runif(1, 0.1, 0.3)
      # simulate n_sections_fixed trials
      if(any(runif(n_sections_fixed) < p_section)) {
        detected_flags[i] = 1
      }
    }
    # if presence[i] == 0, detected remains 0
  }
  mean(detected_flags)  # return overall detection rate (i.e., proportion of patients with metastasis detected)
}

# Simulate detection rates under two sampling policies
set.seed(456)
Nsim <- 10000
rate_5 <- simulate_detection(Nsim, n_sections_fixed = 5)
rate_10 <- simulate_detection(Nsim, n_sections_fixed = 10)
cat(sprintf("Overall metastasis detection rate: %.2f%% with 5 sections vs %.2f%% with 10 sections.\n",
            100*rate_5, 100*rate_10))
