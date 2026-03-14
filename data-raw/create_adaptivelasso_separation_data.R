# Test data: Adaptive LASSO separation / high-dimensional scenarios
# Creates datasets that stress-test edge cases unique to adaptive LASSO

library(dplyr)
set.seed(2026)

# =============================================================================
# Dataset 3: High-dimensional (p > n/2) with correlated blocks
# n=80, p=35 model columns (after dummy coding)
# =============================================================================
create_adaptivelasso_highdim <- function() {
  n <- 80

  # Time and event
  base_hazard <- rexp(n, rate = 0.04)
  censor <- runif(n, 20, 60)

  # Create 8 continuous predictors in 2 correlated blocks
  # Block 1: gene expression cluster (correlated)
  z1 <- rnorm(n)
  gene_a <- round(z1 + rnorm(n, sd = 0.3), 3)
  gene_b <- round(z1 + rnorm(n, sd = 0.3), 3)
  gene_c <- round(z1 + rnorm(n, sd = 0.5), 3)
  gene_d <- round(rnorm(n), 3)  # independent noise

  # Block 2: inflammatory markers (correlated)
  z2 <- rnorm(n)
  crp <- round(pmax(exp(z2 * 0.8 + rnorm(n, sd = 0.3)), 0.1), 2)
  il6 <- round(pmax(exp(z2 * 0.6 + rnorm(n, sd = 0.4)), 0.01), 2)
  tnf <- round(pmax(z2 * 2 + rnorm(n, sd = 1) + 5, 0.1), 2)
  fibrinogen <- round(pmax(z2 * 0.5 + rnorm(n, sd = 0.5) + 3, 1), 2)

  # 5 categorical predictors (adds ~10 dummy columns)
  molecular_subtype <- factor(sample(c("Luminal_A", "Luminal_B", "HER2", "Basal"),
    n, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
  grade <- factor(sample(1:3, n, replace = TRUE, prob = c(0.25, 0.45, 0.3)))
  lymph_node <- factor(sample(c("N0", "N1", "N2", "N3"),
    n, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10)))
  margin_status <- factor(sample(c("Negative", "Close", "Positive"),
    n, replace = TRUE, prob = c(0.60, 0.25, 0.15)))
  receptor_status <- factor(sample(c("ER+PR+", "ER+PR-", "ER-PR-"),
    n, replace = TRUE, prob = c(0.50, 0.20, 0.30)))

  # 4 more continuous noise variables
  bmi <- round(rnorm(n, 26, 5), 1)
  hemoglobin <- round(rnorm(n, 12.5, 1.5), 1)
  platelet <- round(rnorm(n, 250, 60))
  ldh <- round(pmax(rnorm(n, 200, 50), 80))

  # True risk model (only z1 block and grade matter)
  risk <- 0.4 * z1 + 0.3 * (as.numeric(grade) - 1) +
          0.15 * (molecular_subtype == "Basal") +
          0.1 * (lymph_node == "N2") + 0.2 * (lymph_node == "N3")

  surv_time <- rweibull(n, shape = 1.2, scale = exp(-risk / 1.2) * 30)
  observed_time <- round(pmin(surv_time, censor), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event <- factor(
    as.numeric(surv_time <= censor),
    levels = c(0, 1), labels = c("Alive", "Dead")
  )

  data.frame(
    patient_id = paste0("HD_", sprintf("%03d", 1:n)),
    time = observed_time,
    event = event,
    gene_a = gene_a, gene_b = gene_b, gene_c = gene_c, gene_d = gene_d,
    crp = crp, il6 = il6, tnf = tnf, fibrinogen = fibrinogen,
    molecular_subtype = molecular_subtype,
    grade = grade,
    lymph_node = lymph_node,
    margin_status = margin_status,
    receptor_status = receptor_status,
    bmi = bmi, hemoglobin = hemoglobin, platelet = platelet, ldh = ldh,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Generate and save
# =============================================================================
adaptivelasso_highdim_data <- create_adaptivelasso_highdim()

cat("Dataset 3: High-Dimensional Adaptive LASSO Data\n")
cat("Dimensions:", nrow(adaptivelasso_highdim_data), "x", ncol(adaptivelasso_highdim_data), "\n")
cat("Events:", sum(adaptivelasso_highdim_data$event == "Dead"), "/",
    nrow(adaptivelasso_highdim_data),
    "(", round(100 * mean(adaptivelasso_highdim_data$event == "Dead"), 1), "%)\n")
cat("Continuous predictors: 12\n")
cat("Categorical predictors: 5 (adds ~10 dummy columns)\n")
cat("Correlated blocks: gene_{a,b,c} and crp/il6/tnf/fibrinogen\n")
cat("Noise variables: gene_d, bmi, hemoglobin, platelet, ldh\n")

# Save
save(adaptivelasso_highdim_data,
     file = here::here("data", "adaptivelasso_highdim_data.rda"))
write.csv(adaptivelasso_highdim_data,
          file = here::here("data-raw", "non-rda", "adaptivelasso_highdim.csv"),
          row.names = FALSE)

cat("\nHigh-dimensional test dataset saved.\n")
