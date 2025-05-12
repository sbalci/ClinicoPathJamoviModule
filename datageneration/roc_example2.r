# Minimal example of ROC analysis for debugging
# This has fewer dependencies and simpler code

#' Perform a simple ROC analysis
#' @param data A data frame containing the variables
#' @param classvar The name of the classification variable (gold standard)
#' @param classpos The value of classvar that indicates a positive result
#' @param testvar The name of the test variable
#' @param direction Whether greater values indicate positive test ("greater") or negative test ("less")
#' @return A list containing the ROC analysis results
minimal_roc <- function(data, classvar, classpos, testvar, direction = "greater") {
  # Check inputs
  if (missing(data) || missing(classvar) || missing(testvar)) {
    stop("Data, classvar, and testvar must be provided")
  }

  # Handle case where variables are provided as strings vs. names
  if (is.character(classvar) && length(classvar) == 1) {
    classvar_name <- classvar
  } else {
    classvar_name <- deparse(substitute(classvar))
  }

  if (is.character(testvar) && length(testvar) == 1) {
    testvar_name <- testvar
  } else {
    testvar_name <- deparse(substitute(testvar))
  }

  # Remove missing values
  data <- data[complete.cases(data[, c(classvar_name, testvar_name)]), ]

  # Check if we have data
  if (nrow(data) == 0) {
    stop("No complete cases in data")
  }

  # Create response vector (0/1)
  response <- as.numeric(data[[classvar_name]] == classpos)
  predictor <- data[[testvar_name]]

  # If direction is "less", negate the predictor
  if (direction == "less") {
    predictor <- -predictor
  }

  # Sort data by predictor
  sorted_idx <- order(predictor, decreasing = TRUE)
  sorted_response <- response[sorted_idx]
  sorted_predictor <- predictor[sorted_idx]

  # Calculate basic stats
  n_pos <- sum(response == 1)
  n_neg <- sum(response == 0)

  # Calculate sensitivity and specificity at each threshold
  sens <- cumsum(sorted_response) / n_pos
  spec <- (n_neg - cumsum(1 - sorted_response)) / n_neg

  # AUC calculation using trapezoidal rule
  auc <- sum(diff(1 - spec) * (sens[-1] + sens[-length(sens)]) / 2)

  # Find optimal threshold (Youden index)
  j_index <- sens + spec - 1
  best_idx <- which.max(j_index)

  # Return results
  list(
    n_obs = length(response),
    n_pos = n_pos,
    n_neg = n_neg,
    auc = auc,
    thresholds = sorted_predictor,
    sensitivity = sens,
    specificity = spec,
    j_index = j_index,
    best_threshold = sorted_predictor[best_idx],
    best_sensitivity = sens[best_idx],
    best_specificity = spec[best_idx]
  )
}

# Example usage:
if (FALSE) {
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

  # Run minimal ROC analysis
  result <- minimal_roc(
    data = example_data,
    classvar = "D.str",
    classpos = "Ill",
    testvar = "M2"
  )

  # Print results
  cat("AUC:", result$auc, "\n")
  cat("Optimal threshold:", result$best_threshold, "\n")
  cat("Sensitivity at optimal threshold:", result$best_sensitivity, "\n")
  cat("Specificity at optimal threshold:", result$best_specificity, "\n")

  # Simple ROC plot
  plot(1 - result$specificity, result$sensitivity, type = "l",
       xlab = "1 - Specificity", ylab = "Sensitivity",
       main = "ROC Curve")
  abline(0, 1, lty = 2)
  points(1 - result$best_specificity, result$best_sensitivity, col = "red", pch = 19)
}
