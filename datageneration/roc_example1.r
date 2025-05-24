# Example usage of ROC analysis in the meddecide module

# Load required libraries
library(pROC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ROCR)

# Create example data
set.seed(123)
n <- 100
test_values <- c(rnorm(n/2, mean = 10, sd = 2), rnorm(n/2, mean = 15, sd = 3))
true_status <- factor(rep(c("Healthy", "Ill"), each = n/2))
example_data <- data.frame(
  ID = 1:n,
  D.str = true_status,
  M1 = test_values
)

# Add some noise measurements
example_data$M2 <- example_data$M1 + rnorm(n, mean = 0, sd = 2)

# Basic ROC analysis using pROC
response <- as.numeric(example_data$D.str == "Ill")
basic_roc <- pROC::roc(
  response = response,
  predictor = example_data$M2,
  direction = ">",
  levels = c(0, 1)
)

# Extract key statistics
auc <- basic_roc$auc
auc_ci <- ci.auc(basic_roc)
coords <- coords(basic_roc, "best", ret = c("threshold", "sensitivity", "specificity"))

# Print results
cat("Basic ROC Analysis Results:\n")
cat("AUC:", auc, "\n")
cat("95% CI:", auc_ci[1], "-", auc_ci[3], "\n")
cat("Optimal threshold (Youden index):", coords["threshold"], "\n")
cat("Sensitivity at optimal threshold:", coords["sensitivity"], "\n")
cat("Specificity at optimal threshold:", coords["specificity"], "\n")

# Calculate additional metrics
prevalence <- mean(response)
ppv <- (coords["sensitivity"] * prevalence) /
       ((coords["sensitivity"] * prevalence) + ((1 - coords["specificity"]) * (1 - prevalence)))
npv <- (coords["specificity"] * (1 - prevalence)) /
       ((coords["specificity"] * (1 - prevalence)) + ((1 - coords["sensitivity"]) * prevalence))
lrp <- coords["sensitivity"] / (1 - coords["specificity"])
lrn <- (1 - coords["sensitivity"]) / coords["specificity"]

cat("Prevalence:", prevalence, "\n")
cat("PPV:", ppv, "\n")
cat("NPV:", npv, "\n")
cat("LR+:", lrp, "\n")
cat("LR-:", lrn, "\n")

# Plot ROC curve
plot(basic_roc, main = "ROC Curve")
abline(0, 1, lty = 2, col = "gray")

# Alternative ROC calculation using ROCR
pred <- prediction(example_data$M2, response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve using ROCR")
abline(0, 1, lty = 2, col = "gray")



