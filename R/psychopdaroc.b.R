#' @title ROC Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import cutpointr
#' @importFrom MASS ginv
#' @importFrom stats pnorm qnorm pchisq quantile aggregate

# ============================================================================
# UTILITY FUNCTIONS - OUTSIDE THE MAIN CLASS
# ============================================================================

# Generate a formatted HTML table for sensitivity/specificity results
#' @param Title Title for the table
#' @param TP Number of true positives
#' @param FP Number of false positives
#' @param TN Number of true negatives
#' @param FN Number of false negatives
#' @return HTML string containing the formatted table
print.sensSpecTable <- function(Title, TP, FP, TN, FN) {
  html <- paste0(
    "<style type='text/css'>
        .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
        .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg .tg-s6z2{text-align:center}
        .tg .tg-uys7{border-color:inherit;text-align:center}
        .tg .tg-h0x1{text-align:center}
        </style>
        <table class='tg'>
        <tr>
            <th class='tg-0lax' colspan='4'>", Title, "</th>
        </tr>
        <tr>
            <td class='tg-s6z2'></td>
            <td class='tg-uys7' colspan='3'>DECISION BASED ON MEASURE</td>
        </tr>
        <tr>
            <td class='tg-h0x1' rowspan='3'>CRITERION</td>
            <td class='tg-h0x1'></td>
            <td class='tg-h0x1'>Negative</td>
            <td class='tg-h0x1'>Positive</td>
        </tr>
        <tr>
            <td class='tg-s6z2'>Negative</td>
            <td class='tg-s6z2'>", TN, " (TN)</td>
            <td class='tg-s6z2'>", FP, " (FP)</td>
        </tr>
        <tr>
            <td class='tg-h0x1'>Positive</td>
            <td class='tg-h0x1'>", FN, " (FN)</td>
            <td class='tg-h0x1'>", TP, " (TP)</td>
        </tr>
        <tr>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
        </tr>
        </table>"
  )
  return(html)
}

# Perform DeLong's test for comparing AUCs
#' @param data Matrix or data frame with test scores (columns = different tests)
#' @param classVar Vector of class labels (factor or character)
#' @param pos_class The positive class label
#' @param ref Reference test for comparisons (NULL = all pairwise comparisons)
#' @param conf.level Confidence level for intervals
#' @return List with test results, AUCs, etc.
deLong.test <- function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
  # Convert factor to character for consistent handling
  if (is.factor(classVar)) {
    classVar <- as.character(classVar)
  }

  # Validate positive class
  if (!pos_class %in% unique(classVar)) {
    stop("Specified positive class not found in data.")
  }

  # Check data dimensions
  if (ncol(data) < 2) {
    stop("Data must contain at least two columns (different measures).")
  }
  if (nrow(data) < 2) {
    stop("Data must contain at least two rows (observations).")
  }

  # Identify positive and negative cases
  id.pos <- classVar == pos_class
  nn <- sum(!id.pos)  # Number of negative cases
  np <- sum(id.pos)   # Number of positive cases
  nauc <- ncol(data)  # Number of tests

  # Validate we have both positive and negative cases
  if (np < 1 || nn < 1) {
    stop("Need both positive and negative cases for ROC analysis.")
  }

  # Set up comparison matrix
  if (is.null(ref)) {
    # All pairwise comparisons
    L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
    newa <- 0
    for (i in 1:(nauc - 1)) {
      newl <- nauc - i
      L[(newa + 1):(newa + newl), i] <- rep(1, newl)
      L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <- diag(-1, nrow = newl, ncol = newl)
      newa <- newa + newl
    }
  } else {
    # Comparisons against reference
    if (ref > nauc) {
      stop(paste("Reference must be one of the markers (1...", nauc, " in this case)", sep = ""))
    }
    L <- matrix(1, ncol = nauc, nrow = nauc - 1)
    L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
  }

  # Split data by class
  markern <- as.matrix(data[!id.pos,])
  markerp <- as.matrix(data[id.pos,])

  # Wilcoxon statistic function
  WK.STAT <- function(data, y) {
    r <- rank(c(data, y))
    n.data <- length(data)
    n.y <- length(y)
    STATISTIC <- sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
    return(STATISTIC)
  }

  # Calculate AUC for each test
  auc <- numeric(nauc)
  for (r in 1:nauc) {
    auc[r] <- WK.STAT(markerp[, r], markern[, r])
  }
  auc <- auc / (nn * np)  # Normalize to [0,1]

  # Handle AUCs < 0.5
  if (any(auc < 0.5)) {
    data[, auc < 0.5] <- -data[, auc < 0.5]
    auc[auc < 0.5] <- 1 - auc[auc < 0.5]
    markern <- as.matrix(data[!id.pos,])
    markerp <- as.matrix(data[id.pos,])
  }

  # Calculate placement values
  V10 <- matrix(0, nrow = np, ncol = nauc)
  V01 <- matrix(0, nrow = nn, ncol = nauc)

  tmn <- t(markern)
  tmp <- t(markerp)

  for (i in 1:np) {
    V10[i,] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
  }

  for (i in 1:nn) {
    V01[i,] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
  }

  V10 <- V10 / nn
  V01 <- V01 / np

  # Calculate covariance matrices
  W10 <- cov(V10)
  W01 <- cov(V01)
  S <- W10 / np + W01 / nn

  # Calculate standard errors and p-values
  q1 <- auc / (2 - auc)
  q2 <- 2 * auc^2 / (1 + auc)

  aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc^2) + (nn - 1) * (q2 - auc^2)) / (np * nn)
  zhalf <- (auc - 0.5) / sqrt(aucvar)
  phalf <- 1 - pnorm(zhalf)
  zdelong <- (auc - 0.5) / sqrt(diag(S))
  pdelong <- 1 - pnorm(zdelong)

  # Global test
  aucdiff <- L %*% auc
  z <- t(aucdiff) %*% MASS::ginv(L %*% S %*% t(L)) %*% aucdiff
  p <- pchisq(z, df = qr(L %*% S %*% t(L))$rank, lower.tail = FALSE)

  # Calculate confidence intervals
  quantil <- qnorm(1 - (1 - conf.level) / 2)

  if (is.null(ref)) {
    # All pairwise comparisons
    cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
    ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
    ctr <- 1
    rows <- character(nauc * (nauc - 1) / 2)
    pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)

    for (i in 1:(nauc - 1)) {
      for (j in (i + 1):nauc) {
        cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])
        LSL <- t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
        tmpz <- (aucdiff[ctr]) %*% MASS::ginv(LSL) %*% aucdiff[ctr]
        pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
        ci[ctr,] <- c(aucdiff[ctr] - quantil * sqrt(LSL), aucdiff[ctr] + quantil * sqrt(LSL))
        rows[ctr] <- paste(i, j, sep = " vs. ")
        ctr <- ctr + 1
      }
    }
  } else {
    # Comparisons against reference
    cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
    ci <- matrix(ncol = 2, nrow = nauc - 1)
    rows <- character(nauc - 1)
    pairp <- matrix(nrow = nauc - 1, ncol = 1)
    comp <- (1:nauc)[-ref]

    for (i in 1:(nauc - 1)) {
      cor.auc[i] <- S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])
      LSL <- t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
      tmpz <- aucdiff[i] %*% MASS::ginv(LSL) %*% aucdiff[i]
      pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
      ci[i,] <- c(aucdiff[i] - quantil * sqrt(LSL), aucdiff[i] + quantil * sqrt(LSL))
      rows[i] <- paste(ref, comp[i], sep = " vs. ")
    }
  }

  # Format results
  newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
  names(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation")
  rownames(newres) <- rows

  row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
  colnames(ci) <- c(paste0(100 * conf.level, "% CI (lower)"), paste0(100 * conf.level, "% CI (upper)"))

  names(auc) <- 1:nauc
  auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
  colnames(auc) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")

  # Return results
  ERG <- list(
    AUC = auc,
    difference = newres,
    covariance = S,
    global.z = z,
    global.p = p
  )
  class(ERG) <- "DeLong"
  return(ERG)
}

# Print method for DeLong test results
#' @export
print.DeLong <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Estimated AUC's:\n")
  print(format(round(x$AUC, digits = digits, ...), nsmall = digits, ...))
  cat("\nPairwise comparisons:\n")
  print(format(round(x$difference, digits = digits, ...), nsmall = digits, ...))
  cat(paste("\nOverall test:\n p-value =", format.pval(x$global.p, digits = digits), "\n"))
}

# ============================================================================
# UTILITY FUNCTIONS FROM psychopdaroc_utilities.R
# ============================================================================

# Convert raw test values to predicted probabilities
#' @param values Raw test values
#' @param actual Binary outcomes (0/1)
#' @param direction Direction of test (">=", "<=")
#' @return Vector of predicted probabilities
rawToProb <- function(values, actual, direction = ">=") {
  sorted_values <- sort(unique(values))
  probs <- numeric(length(values))

  for (i in seq_along(sorted_values)) {
    threshold <- sorted_values[i]

    if (direction == ">=") {
      predicted_pos <- values >= threshold
    } else {
      predicted_pos <- values <= threshold
    }

    tp <- sum(predicted_pos & actual == 1)
    fn <- sum(!predicted_pos & actual == 1)

    sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)

    value_indices <- which(values == threshold)
    probs[value_indices] <- ifelse(direction == ">=", sensitivity, 1 - sensitivity)
  }

  probs[probs < 0] <- 0
  probs[probs > 1] <- 1

  return(probs)
}

# Compute Net Reclassification Index (NRI)
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @return List containing NRI components
computeNRI <- function(new_values, ref_values, actual, direction = ">=", thresholds = NULL) {
  new_probs <- rawToProb(new_values, actual, direction)
  ref_probs <- rawToProb(ref_values, actual, direction)

  events <- actual == 1
  non_events <- actual == 0

  if (is.null(thresholds) || length(thresholds) == 0) {
    # Continuous NRI
    up_events <- sum(new_probs[events] > ref_probs[events])
    down_events <- sum(new_probs[events] < ref_probs[events])
    up_non_events <- sum(new_probs[non_events] > ref_probs[non_events])
    down_non_events <- sum(new_probs[non_events] < ref_probs[non_events])

    p_up_events <- up_events / sum(events)
    p_down_events <- down_events / sum(events)
    p_up_non_events <- up_non_events / sum(non_events)
    p_down_non_events <- down_non_events / sum(non_events)

  } else {
    # Categorical NRI
    ref_cats <- cut(ref_probs,
                    breaks = c(0, thresholds, 1),
                    labels = 1:(length(thresholds) + 1),
                    include.lowest = TRUE)

    new_cats <- cut(new_probs,
                    breaks = c(0, thresholds, 1),
                    labels = 1:(length(thresholds) + 1),
                    include.lowest = TRUE)

    move_up_event <- sum(as.numeric(new_cats[events]) > as.numeric(ref_cats[events]), na.rm = TRUE)
    move_down_event <- sum(as.numeric(new_cats[events]) < as.numeric(ref_cats[events]), na.rm = TRUE)
    move_up_non_event <- sum(as.numeric(new_cats[non_events]) > as.numeric(ref_cats[non_events]), na.rm = TRUE)
    move_down_non_event <- sum(as.numeric(new_cats[non_events]) < as.numeric(ref_cats[non_events]), na.rm = TRUE)

    p_up_events <- move_up_event / sum(events)
    p_down_events <- move_down_event / sum(events)
    p_up_non_events <- move_up_non_event / sum(non_events)
    p_down_non_events <- move_down_non_event / sum(non_events)
  }

  event_nri <- p_up_events - p_down_events
  non_event_nri <- p_down_non_events - p_up_non_events
  overall_nri <- event_nri + non_event_nri

  return(list(
    nri = overall_nri,
    event_nri = event_nri,
    non_event_nri = non_event_nri
  ))
}

# Bootstrap IDI calculation with confidence intervals
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param n_boot Number of bootstrap iterations
#' @return List with IDI and confidence intervals
bootstrapIDI <- function(new_values, ref_values, actual, direction = ">=", n_boot = 1000) {
  n <- length(actual)
  boot_idi <- numeric(n_boot)

  # Original IDI calculation
  new_probs <- rawToProb(new_values, actual, direction)
  ref_probs <- rawToProb(ref_values, actual, direction)

  original_idi <- (mean(new_probs[actual == 1]) - mean(new_probs[actual == 0])) -
    (mean(ref_probs[actual == 1]) - mean(ref_probs[actual == 0]))

  # Bootstrap
  for (i in 1:n_boot) {
    boot_idx <- sample(n, n, replace = TRUE)

    boot_new <- new_values[boot_idx]
    boot_ref <- ref_values[boot_idx]
    boot_actual <- actual[boot_idx]

    boot_new_probs <- rawToProb(boot_new, boot_actual, direction)
    boot_ref_probs <- rawToProb(boot_ref, boot_actual, direction)

    boot_idi[i] <- (mean(boot_new_probs[boot_actual == 1]) - mean(boot_new_probs[boot_actual == 0])) -
      (mean(boot_ref_probs[boot_actual == 1]) - mean(boot_ref_probs[boot_actual == 0]))
  }

  # Calculate confidence intervals
  ci_lower <- quantile(boot_idi, 0.025, na.rm = TRUE)
  ci_upper <- quantile(boot_idi, 0.975, na.rm = TRUE)

  # Calculate p-value
  p_value <- 2 * min(mean(boot_idi <= 0, na.rm = TRUE),
                     mean(boot_idi >= 0, na.rm = TRUE))

  return(list(
    idi = original_idi,
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    p_value = p_value
  ))
}

# Bootstrap NRI calculation with confidence intervals
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @param n_boot Number of bootstrap iterations
#' @return List with NRI components and confidence intervals
bootstrapNRI <- function(new_values, ref_values, actual, direction = ">=",
                         thresholds = NULL, n_boot = 1000) {
  n <- length(actual)
  boot_nri <- numeric(n_boot)
  boot_event_nri <- numeric(n_boot)
  boot_non_event_nri <- numeric(n_boot)

  # Original NRI calculation
  original_nri <- computeNRI(new_values, ref_values, actual, direction, thresholds)

  # Bootstrap
  for (i in 1:n_boot) {
    boot_idx <- sample(n, n, replace = TRUE)

    boot_new <- new_values[boot_idx]
    boot_ref <- ref_values[boot_idx]
    boot_actual <- actual[boot_idx]

    tryCatch({
      boot_result <- computeNRI(boot_new, boot_ref, boot_actual, direction, thresholds)
      boot_nri[i] <- boot_result$nri
      boot_event_nri[i] <- boot_result$event_nri
      boot_non_event_nri[i] <- boot_result$non_event_nri
    }, error = function(e) {
      boot_nri[i] <- NA
      boot_event_nri[i] <- NA
      boot_non_event_nri[i] <- NA
    })
  }

  # Remove failed iterations
  valid_idx <- !is.na(boot_nri)
  boot_nri <- boot_nri[valid_idx]
  boot_event_nri <- boot_event_nri[valid_idx]
  boot_non_event_nri <- boot_non_event_nri[valid_idx]

  # Calculate confidence intervals
  ci_lower <- quantile(boot_nri, 0.025, na.rm = TRUE)
  ci_upper <- quantile(boot_nri, 0.975, na.rm = TRUE)

  # Calculate p-value
  p_value <- 2 * min(mean(boot_nri <= 0, na.rm = TRUE),
                     mean(boot_nri >= 0, na.rm = TRUE))

  return(list(
    nri = original_nri$nri,
    event_nri = original_nri$event_nri,
    non_event_nri = original_nri$non_event_nri,
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    p_value = p_value
  ))
}

# Calculate precision-recall curve data
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @return List containing precision, recall, thresholds, and AUPRC
calculatePrecisionRecall <- function(x, class, pos_class) {
  # Convert to binary response
  response <- as.numeric(class == pos_class)

  # Sort values and create thresholds
  sorted_values <- sort(unique(x), decreasing = TRUE)
  precision <- numeric(length(sorted_values))
  recall <- numeric(length(sorted_values))

  # Calculate precision and recall for each threshold
  for (i in seq_along(sorted_values)) {
    threshold <- sorted_values[i]
    predicted_pos <- x >= threshold

    tp <- sum(predicted_pos & response == 1)
    fp <- sum(predicted_pos & response == 0)
    fn <- sum(!predicted_pos & response == 1)

    precision[i] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  }

  # Add points for complete curve
  precision <- c(1, precision, 0)
  recall <- c(0, recall, 1)
  sorted_values <- c(max(x, na.rm = TRUE) + 1, sorted_values, min(x, na.rm = TRUE) - 1)

  # Calculate AUPRC using trapezoidal rule
  auprc <- 0
  for (i in 2:length(recall)) {
    if (recall[i] > recall[i-1]) {
      auprc <- auprc + 0.5 * (precision[i] + precision[i-1]) * (recall[i] - recall[i-1])
    }
  }

  return(list(
    threshold = sorted_values,
    precision = precision,
    recall = recall,
    auprc = auprc
  ))
}

# Calculate comprehensive classifier performance metrics
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @return List containing comprehensive performance metrics
calculateComprehensiveMetrics <- function(x, class, pos_class) {
  # Validate inputs
  if (length(x) != length(class)) {
    stop("Test values and class labels must have same length")
  }

  # Remove missing values
  complete_cases <- !is.na(x) & !is.na(class)
  x <- x[complete_cases]
  class <- class[complete_cases]

  if (length(x) < 10) {
    warning("Very small sample size may produce unreliable results")
  }

  # Convert to binary response
  response <- as.numeric(class == pos_class)

  # Calculate basic ROC metrics
  thresholds <- sort(unique(x))
  best_youden <- -Inf
  best_threshold <- NA
  best_sens <- NA
  best_spec <- NA
  auc <- 0

  # Store all ROC points for AUC calculation
  roc_points <- data.frame(
    fpr = numeric(length(thresholds)),
    tpr = numeric(length(thresholds))
  )

  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    predicted_pos <- x >= threshold

    tp <- sum(predicted_pos & response == 1)
    fp <- sum(predicted_pos & response == 0)
    tn <- sum(!predicted_pos & response == 0)
    fn <- sum(!predicted_pos & response == 1)

    # Calculate rates
    sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    youden <- sensitivity + specificity - 1

    # Store for AUC calculation
    roc_points$tpr[i] <- sensitivity
    roc_points$fpr[i] <- 1 - specificity

    # Update best threshold based on Youden's index
    if (youden > best_youden) {
      best_youden <- youden
      best_threshold <- threshold
      best_sens <- sensitivity
      best_spec <- specificity
    }
  }

  # Calculate AUC using trapezoidal rule
  roc_points <- roc_points[order(roc_points$fpr), ]
  for (i in 2:nrow(roc_points)) {
    width <- roc_points$fpr[i] - roc_points$fpr[i-1]
    height <- (roc_points$tpr[i] + roc_points$tpr[i-1]) / 2
    auc <- auc + width * height
  }

  # Get predictions at optimal threshold
  predicted_pos <- x >= best_threshold

  # Calculate confusion matrix at optimal threshold
  tp <- sum(predicted_pos & response == 1)
  fp <- sum(predicted_pos & response == 0)
  tn <- sum(!predicted_pos & response == 0)
  fn <- sum(!predicted_pos & response == 1)

  # Calculate comprehensive metrics
  accuracy <- (tp + tn) / (tp + fp + tn + fn)
  balanced_accuracy <- (best_sens + best_spec) / 2

  # Calculate precision and F1 score
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- best_sens  # Same as sensitivity
  f1_score <- ifelse(precision + recall > 0,
                     2 * precision * recall / (precision + recall), 0)

  # Calculate Brier score
  prob_predictions <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  prob_predictions[prob_predictions < 0] <- 0
  prob_predictions[prob_predictions > 1] <- 1
  brier_score <- mean((prob_predictions - response)^2, na.rm = TRUE)

  # Calculate AUPRC
  pr_results <- calculatePrecisionRecall(x, class, pos_class)
  auprc <- pr_results$auprc

  return(list(
    auc = auc,
    auprc = auprc,
    brier = brier_score,
    f1_score = f1_score,
    accuracy = accuracy,
    balanced_accuracy = balanced_accuracy,
    sensitivity = best_sens,
    specificity = best_spec,
    precision = precision,
    optimal_threshold = best_threshold
  ))
}

# Calculate partial AUC using numerical integration
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @param spec_from Lower bound of specificity range
#' @param spec_to Upper bound of specificity range
#' @return List containing partial AUC results
calculatePartialAUC <- function(x, class, pos_class, spec_from = 0.8, spec_to = 1.0) {
  # Validate inputs
  if (spec_from >= spec_to) {
    stop("spec_from must be less than spec_to")
  }
  if (spec_from < 0 || spec_to > 1) {
    stop("Specificity bounds must be between 0 and 1")
  }

  # Convert to binary response
  response <- as.numeric(class == pos_class)

  # Calculate ROC curve points
  thresholds <- sort(unique(x))
  roc_data <- data.frame(
    threshold = thresholds,
    sensitivity = numeric(length(thresholds)),
    specificity = numeric(length(thresholds))
  )

  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    predicted_pos <- x >= threshold

    tp <- sum(predicted_pos & response == 1)
    fp <- sum(predicted_pos & response == 0)
    tn <- sum(!predicted_pos & response == 0)
    fn <- sum(!predicted_pos & response == 1)

    roc_data$sensitivity[i] <- tp / (tp + fn)
    roc_data$specificity[i] <- tn / (tn + fp)
  }

  # Filter for specificity range
  in_range <- roc_data$specificity >= spec_from & roc_data$specificity <= spec_to

  if (sum(in_range) < 2) {
    warning("Insufficient data points in specified specificity range")
    return(list(
      pAUC = NA,
      pAUC_normalized = NA,
      ci_lower = NA,
      ci_upper = NA,
      spec_range = paste0("(", spec_from, " - ", spec_to, ")")
    ))
  }

  # Extract points in range and sort by specificity
  partial_data <- roc_data[in_range, ]
  partial_data <- partial_data[order(partial_data$specificity), ]

  # Calculate partial AUC using trapezoidal rule
  partial_auc <- 0
  for (i in 2:nrow(partial_data)) {
    width <- (1 - partial_data$specificity[i-1]) - (1 - partial_data$specificity[i])
    height <- (partial_data$sensitivity[i] + partial_data$sensitivity[i-1]) / 2
    partial_auc <- partial_auc + width * height
  }

  # Normalize partial AUC to [0,1] range
  max_possible_pauc <- (1 - spec_from) - (1 - spec_to)
  normalized_pauc <- partial_auc / max_possible_pauc

  # Simple confidence interval
  n <- length(x)
  se_approx <- sqrt(partial_auc * (1 - partial_auc) / n)
  ci_lower <- max(0, partial_auc - 1.96 * se_approx)
  ci_upper <- min(max_possible_pauc, partial_auc + 1.96 * se_approx)

  return(list(
    pAUC = partial_auc,
    pAUC_normalized = normalized_pauc,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    spec_range = paste0("(", spec_from, " - ", spec_to, ")")
  ))
}

# Calculate bootstrap confidence intervals for ROC metrics
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return List containing bootstrap confidence intervals
calculateBootstrapCI <- function(x, class, pos_class, n_boot = 2000, conf_level = 0.95) {
  # Validate inputs
  if (n_boot < 100) {
    warning("Very few bootstrap iterations may produce unstable results")
  }

  # Remove missing values
  complete_cases <- !is.na(x) & !is.na(class)
  x <- x[complete_cases]
  class <- class[complete_cases]
  n <- length(x)

  if (n < 20) {
    warning("Small sample size may produce unreliable bootstrap results")
  }

  # Initialize storage for bootstrap results
  boot_auc <- numeric(n_boot)
  boot_threshold <- numeric(n_boot)
  boot_sensitivity <- numeric(n_boot)
  boot_specificity <- numeric(n_boot)

  # Original metrics for comparison
  original_metrics <- calculateComprehensiveMetrics(x, class, pos_class)

  # Bootstrap sampling
  for (i in 1:n_boot) {
    # Sample with replacement
    boot_indices <- sample(n, n, replace = TRUE)
    x_boot <- x[boot_indices]
    class_boot <- class[boot_indices]

    # Calculate metrics for bootstrap sample
    tryCatch({
      boot_metrics <- calculateComprehensiveMetrics(x_boot, class_boot, pos_class)
      boot_auc[i] <- boot_metrics$auc
      boot_threshold[i] <- boot_metrics$optimal_threshold
      boot_sensitivity[i] <- boot_metrics$sensitivity
      boot_specificity[i] <- boot_metrics$specificity
    }, error = function(e) {
      boot_auc[i] <- NA
      boot_threshold[i] <- NA
      boot_sensitivity[i] <- NA
      boot_specificity[i] <- NA
    })
  }

  # Remove failed bootstrap samples
  valid_boots <- !is.na(boot_auc)
  boot_auc <- boot_auc[valid_boots]
  boot_threshold <- boot_threshold[valid_boots]
  boot_sensitivity <- boot_sensitivity[valid_boots]
  boot_specificity <- boot_specificity[valid_boots]

  if (length(boot_auc) < n_boot * 0.5) {
    warning("Many bootstrap samples failed - results may be unreliable")
  }

  # Calculate confidence intervals using percentile method
  alpha <- 1 - conf_level
  lower_pct <- alpha / 2
  upper_pct <- 1 - alpha / 2

  # Helper function to calculate CI
  calc_ci <- function(boot_values, original_value) {
    if (length(boot_values) < 10) {
      return(list(estimate = original_value, ci_lower = NA, ci_upper = NA))
    }

    ci_lower <- quantile(boot_values, lower_pct, na.rm = TRUE)
    ci_upper <- quantile(boot_values, upper_pct, na.rm = TRUE)

    list(
      estimate = original_value,
      ci_lower = as.numeric(ci_lower),
      ci_upper = as.numeric(ci_upper)
    )
  }

  return(list(
    auc = calc_ci(boot_auc, original_metrics$auc),
    threshold = calc_ci(boot_threshold, original_metrics$optimal_threshold),
    sensitivity = calc_ci(boot_sensitivity, original_metrics$sensitivity),
    specificity = calc_ci(boot_specificity, original_metrics$specificity)
  ))
}

# ============================================================================
# MAIN ANALYSIS CLASS
# ============================================================================

psychopdarocClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "psychopdarocClass",
    inherit = psychopdarocBase,
    private = list(
      # ======================================================================
      # CLASS PRIVATE FIELDS
      # ======================================================================

      ## Storage for ROC data and analysis results
      .rocDataList = list(),
      .optimalCriteriaList = list(),
      .prevalenceList = list(),

      # ======================================================================
      # HELPER METHODS
      # ======================================================================

      # Determine positive class from data
      # @param classVar Class variable data
      # @param positiveClassOption User-specified positive class
      # @return Character string representing positive class
      .determinePositiveClass = function(classVar, positiveClassOption) {
        # Convert to factor if needed
        if (!is.factor(classVar)) {
          classVar <- factor(classVar)
        }

        unique_classes <- levels(classVar)

        # Validate we have exactly 2 classes
        if (length(unique_classes) != 2) {
          stop("Class variable must have exactly 2 levels")
        }

        # Determine positive class
        if (!is.null(positiveClassOption) && positiveClassOption != "") {
          # Use user-specified class if valid
          if (positiveClassOption %in% unique_classes) {
            return(positiveClassOption)
          } else {
            warning(paste("Specified positive class", positiveClassOption,
                          "not found. Using first level instead."))
            return(unique_classes[1])
          }
        } else {
          # Default to first level
          return(unique_classes[1])
        }
      },

      # Calculate cutpoint optimized for cost ratio
      # @param confusionMatrix Matrix with tp, fp, tn, fn values
      # @param prevalence The prevalence of the positive class
      # @param costRatio The cost ratio of false positives to false negatives
      # @return List with optimal index, threshold, and score
      .calculateCostRatioOptimal = function(confusionMatrix, prevalence, costRatio) {
        n_thresholds <- length(confusionMatrix$x.sorted)
        scores <- numeric(n_thresholds)

        for (i in 1:n_thresholds) {
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          sensitivity <- tp / (tp + fn)
          specificity <- tn / (tn + fp)

          scores[i] <- sensitivity * prevalence -
            (1 - specificity) * (1 - prevalence) * costRatio
        }

        best_idx <- which.max(scores)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          score = scores[best_idx]
        ))
      },

      # Calculate cutpoint with equal sensitivity and specificity
      # @param confusionMatrix Matrix with tp, fp, tn, fn values
      # @return List with optimal index, threshold, and difference
      .calculateEqualSensSpec = function(confusionMatrix) {
        n_thresholds <- length(confusionMatrix$x.sorted)
        differences <- numeric(n_thresholds)

        for (i in 1:n_thresholds) {
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          sensitivity <- tp / (tp + fn)
          specificity <- tn / (tn + fp)

          differences[i] <- abs(sensitivity - specificity)
        }

        best_idx <- which.min(differences)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          difference = differences[best_idx]
        ))
      },

      # Calculate cutpoint closest to (0,1) in ROC space
      # @param confusionMatrix Matrix with tp, fp, tn, fn values
      # @return List with optimal index, threshold, and distance
      .calculateClosestToOptimal = function(confusionMatrix) {
        n_thresholds <- length(confusionMatrix$x.sorted)
        distances <- numeric(n_thresholds)

        for (i in 1:n_thresholds) {
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          sensitivity <- tp / (tp + fn)
          specificity <- tn / (tn + fp)

          distances[i] <- sqrt((1 - specificity)^2 + (1 - sensitivity)^2)
        }

        best_idx <- which.min(distances)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          distance = distances[best_idx]
        ))
      },

      # Run basic ROC analysis using cutpointr or fallback method
      # @param dependentVar Numeric vector of test values
      # @param classVar Classification variable
      # @param pos_class Positive class label
      # @param method Cutpoint method
      # @param metric Metric function
      # @param direction Direction of classification
      # @param score Manual cutpoint score (if applicable)
      # @param tol_metric Tolerance for metric
      # @param boot_runs Number of bootstrap runs
      # @param break_ties Function for breaking ties
      # @return Results object with ROC data
      .runROCAnalysis = function(dependentVar, classVar, pos_class, method, metric,
                                 direction, score = NULL, tol_metric = NULL,
                                 boot_runs = 0, break_ties = mean) {
        result_success <- FALSE
        result_message <- NULL

        tryCatch({
          # Primary method using cutpointr
          results <- cutpointr::cutpointr(
            x = dependentVar,
            class = classVar,
            subgroup = NULL,
            method = method,
            cutpoint = score,
            metric = metric,
            direction = direction,
            pos_class = pos_class,
            tol_metric = tol_metric,
            boot_runs = boot_runs,
            break_ties = break_ties,
            na.rm = TRUE
          )
          result_success <- TRUE
        }, error = function(e) {
          result_message <- e$message
        })

        # If cutpointr failed, use alternative implementation
        if (!result_success) {
          warning(paste("Standard ROC analysis failed:", result_message,
                        "Using alternative implementation."))

          # Convert to binary response
          response <- as.numeric(classVar == pos_class)

          # Create ROC data structure
          roc_data <- data.frame(
            x.sorted = sort(unique(dependentVar)),
            direction = ifelse(direction == ">=", ">", "<")
          )

          # Calculate confusion matrix for each threshold
          for(i in 1:nrow(roc_data)) {
            threshold <- roc_data$x.sorted[i]
            if(direction == ">=") {
              predicted_pos <- dependentVar >= threshold
            } else {
              predicted_pos <- dependentVar <= threshold
            }

            roc_data$tp[i] <- sum(predicted_pos & response == 1)
            roc_data$fp[i] <- sum(predicted_pos & response == 0)
            roc_data$tn[i] <- sum(!predicted_pos & response == 0)
            roc_data$fn[i] <- sum(!predicted_pos & response == 1)
          }

          # Calculate sensitivity and specificity
          sens <- roc_data$tp / (roc_data$tp + roc_data$fn)
          spec <- roc_data$tn / (roc_data$tn + roc_data$fp)

          # Calculate AUC
          roc_points <- data.frame(
            specificity = spec,
            sensitivity = sens
          )
          roc_points <- roc_points[order(1-roc_points$specificity),]

          auc <- 0
          for(i in 2:nrow(roc_points)) {
            x_diff <- (1-roc_points$specificity[i]) - (1-roc_points$specificity[i-1])
            y_avg <- (roc_points$sensitivity[i] + roc_points$sensitivity[i-1])/2
            auc <- auc + x_diff * y_avg
          }

          # Find optimal cutpoint using Youden's index
          youdens_j <- sens + spec - 1
          optimal_idx <- which.max(youdens_j)

          # Build minimal result object
          results <- list(
            optimal_cutpoint = roc_data$x.sorted[optimal_idx],
            roc_curve = list(roc_data),
            AUC = auc
          )
        }

        return(results)
      },

      # Get method function from method name
      # @param methodName Name of the method
      # @return Function object for the method
      .getMethodFunction = function(methodName) {
        method_map <- list(
          "maximize_metric" = cutpointr::maximize_metric,
          "minimize_metric" = cutpointr::minimize_metric,
          "maximize_loess_metric" = cutpointr::maximize_loess_metric,
          "minimize_loess_metric" = cutpointr::minimize_loess_metric,
          "maximize_spline_metric" = cutpointr::maximize_spline_metric,
          "minimize_spline_metric" = cutpointr::minimize_spline_metric,
          "maximize_boot_metric" = cutpointr::maximize_boot_metric,
          "minimize_boot_metric" = cutpointr::minimize_boot_metric,
          "oc_youden_kernel" = cutpointr::oc_youden_kernel,
          "oc_youden_normal" = cutpointr::oc_youden_normal,
          "oc_manual" = cutpointr::oc_manual
        )

        # For custom methods, use maximize_metric as placeholder
        if (methodName %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
          return(cutpointr::maximize_metric)
        }

        # Return the method or default to maximize_metric
        return(method_map[[methodName]] %||% cutpointr::maximize_metric)
      },

      # Get metric function from metric name
      # @param metricName Name of the metric
      # @return Function object for the metric
      .getMetricFunction = function(metricName) {
        metric_map <- list(
          "youden" = cutpointr::youden,
          "sum_sens_spec" = cutpointr::sum_sens_spec,
          "accuracy" = cutpointr::accuracy,
          "sum_ppv_npv" = cutpointr::sum_ppv_npv,
          "prod_sens_spec" = cutpointr::prod_sens_spec,
          "prod_ppv_npv" = cutpointr::prod_ppv_npv,
          "cohens_kappa" = cutpointr::cohens_kappa,
          "abs_d_sens_spec" = cutpointr::abs_d_sens_spec,
          "roc01" = cutpointr::roc01,
          "abs_d_ppv_npv" = cutpointr::abs_d_ppv_npv,
          "F1_score" = cutpointr::F1_score
        )

        # Return the metric or default to youden
        return(metric_map[[metricName]] %||% cutpointr::youden)
      },

      # Process a single variable for ROC analysis
      # @param var Variable name
      # @param data Full dataset
      # @param subGroup Subgroup variable (if any)
      # @param params List of analysis parameters
      # @return List with analysis results
      .processVariable = function(var, data, subGroup = NULL, params) {
        # Extract data for analysis
        if (is.null(subGroup)) {
          dependentVar <- as.numeric(data[, var])
          classVar <- data[, params$classVarName]
        } else {
          varParts <- strsplit(var, split = "_")[[1]]
          varName <- varParts[1]
          groupName <- paste(varParts[-1], collapse="_")

          dependentVar <- as.numeric(data[subGroup == groupName, varName])
          classVar <- data[subGroup == groupName, params$classVarName]
        }

        # Run ROC analysis
        results <- private$.runROCAnalysis(
          dependentVar = dependentVar,
          classVar = classVar,
          pos_class = params$positiveClass,
          method = params$method,
          metric = params$metric,
          direction = params$direction,
          score = params$score,
          tol_metric = params$tol_metric,
          boot_runs = params$boot_runs,
          break_ties = params$break_ties
        )

        # Apply custom methods if specified
        if (params$methodName %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
          confusionMatrix <- results$roc_curve[[1]]

          # Calculate prevalence
          n_pos <- sum(classVar == params$positiveClass)
          n_neg <- sum(classVar != params$positiveClass)
          prevalence <- n_pos / (n_pos + n_neg)

          if (params$methodName == "oc_cost_ratio") {
            cost_results <- private$.calculateCostRatioOptimal(
              confusionMatrix,
              prevalence,
              params$costRatio
            )
            results$optimal_cutpoint <- confusionMatrix$x.sorted[cost_results$optimal_idx]
          }
          else if (params$methodName == "oc_equal_sens_spec") {
            eq_results <- private$.calculateEqualSensSpec(confusionMatrix)
            results$optimal_cutpoint <- confusionMatrix$x.sorted[eq_results$optimal_idx]
          }
          else if (params$methodName == "oc_closest_01") {
            closest_results <- private$.calculateClosestToOptimal(confusionMatrix)
            results$optimal_cutpoint <- confusionMatrix$x.sorted[closest_results$optimal_idx]
          }
        }

        return(list(
          results = results,
          dependentVar = dependentVar,
          classVar = classVar,
          prevalence = prevalence
        ))
      },

      # ======================================================================
      # INITIALIZATION METHOD
      # ======================================================================

      .init = function() {
        # Set visibility of optional outputs
        if (self$options$showCriterionPlot)
          self$results$criterionPlot$setVisible(TRUE)
        if (self$options$showPrevalencePlot)
          self$results$prevalencePlot$setVisible(TRUE)
        if (self$options$showDotPlot)
          self$results$dotPlot$setVisible(TRUE)
        if (self$options$precisionRecallCurve)
          self$results$precisionRecallPlot$setVisible(TRUE)
      },

      # ======================================================================
      # MAIN ANALYSIS METHOD
      # ======================================================================

      .run = function() {
        # Check for required inputs
        if (is.null(self$options$classVar) || is.null(self$options$dependentVars)) {
          self$.showInstructions()
          return()
        }

        # Hide instructions and show procedure notes
        self$results$instructions$setVisible(FALSE)
        self$.showProcedureNotes()

        # Set up analysis parameters
        params <- self$.setupAnalysisParameters()

        # Get data and variables
        data <- self$data
        vars <- self$.prepareVariables(data, params)

        # Storage for results
        aucList <- list()
        plotDataList <- data.frame()

        # Process each variable
        for (var in vars) {
          # Initialize result items
          self$.initializeResultItems(var)

          # Process variable
          var_results <- private$.processVariable(
            var = var,
            data = data,
            subGroup = params$subGroup,
            params = params
          )

          # Extract results
          results <- var_results$results
          dependentVar <- var_results$dependentVar
          classVar <- var_results$classVar
          prevalence <- var_results$prevalence

          # Generate tables and store results
          self$.generateResultTables(var, results, dependentVar, classVar, params)

          # Store AUC for summary
          aucList[[var]] <- results$AUC

          # Prepare plot data
          if (self$options$plotROC) {
            plotData <- self$.preparePlotData(var, results)
            if (self$options$combinePlots) {
              plotDataList <- rbind(plotDataList, plotData)
            } else {
              self$.setIndividualPlotStates(var, plotData, results)
            }
          }

          # Store data for additional analyses
          private$.prevalenceList[[var]] <- prevalence
        }

        # Create combined plots if requested
        if (self$options$plotROC && self$options$combinePlots && nrow(plotDataList) > 0) {
          self$.createCombinedPlots(plotDataList)
        }

        # Populate summary tables
        self$.populateSummaryTables(aucList, data, params)

        # Perform additional analyses
        self$.performAdditionalAnalyses(data, vars, params)

        # Perform statistical comparisons
        self$.performStatisticalComparisons(data, params)
      },

      # ======================================================================
      # SUPPORTING METHODS
      # ======================================================================

      # Show instructions when inputs are missing
      .showInstructions = function() {
        self$results$instructions$setContent(
          "<html>
                    <head></head>
                    <body>
                    <p>This function was originally developed by Lucas Friesen.
                    The testroc function with additional features is now part of the meddecide module.</p>
                    <div class='instructions'>
                    <p><b>ROC Analysis for Medical Decision Making</b></p>
                    <p>This analysis creates Receiver Operating Characteristic (ROC) curves
                    and calculates optimal cutpoints for diagnostic tests.</p>
                    <p>To get started:</p>
                    <ol>
                    <li>Place the test result variable(s) in the 'Dependent Variable' slot</li>
                    <li>Place the binary classification (gold standard) in the 'Class Variable' slot</li>
                    <li>[Optional] Place a grouping variable in the 'Group Variable' slot</li>
                    </ol>
                    <p>The ROC analysis helps you determine optimal cut-off values for classifying cases.</p>
                    </div>
                    </body>
                    </html>"
        )
      },

      # Show procedure notes with analysis details
      .showProcedureNotes = function() {
        notes <- paste0(
          "<html><body>",
          "<p>Procedure Notes</p><hr>",
          "<p>The ROC analysis has been completed using the following specifications:</p>",
          "<p>&nbsp;</p>",
          "<p>Measure Variable(s): ", paste(unlist(self$options$dependentVars), collapse = ", "), "</p>",
          "<p>Class Variable: ", self$options$classVar, "</p>",
          "<p>Positive Class: ",
          ifelse(self$options$positiveClass == "",
                 "First level (will be determined from data)",
                 self$options$positiveClass), "</p>"
        )

        if (!is.null(self$options$subGroup)) {
          notes <- paste0(notes, "<p>Sub-Group Variable: ", self$options$subGroup, "</p>")
        }

        notes <- paste0(notes,
                        "<p>&nbsp;</p>",
                        "<p>Method: ", self$options$method, "</p>",
                        "<p>All Observed Cutpoints: ", self$options$allObserved, "</p>",
                        "<p>Metric: ", self$options$metric, "</p>",
                        "<p>Direction: ", self$options$direction, "</p>",
                        "<p>Tie Breakers: ", self$options$break_ties, "</p>",
                        "<p>Metric Tolerance: ", self$options$tol_metric, "</p>"
        )

        if (self$options$boot_runs > 0) {
          notes <- paste0(notes, "<p>Bootstrap Runs: ", self$options$boot_runs, "</p>")
        }

        notes <- paste0(notes, "<hr /></body></html>")
        self$results$procedureNotes$setContent(notes)
      },

      # Set up analysis parameters
      # @return List of analysis parameters
      .setupAnalysisParameters = function() {
        # Get method
        if (self$options$method == "oc_manual") {
          method <- cutpointr::oc_manual
          if (self$options$specifyCutScore == "") {
            stop("Please specify a cut score when using the 'Custom cut score' method.")
          }
          score <- as.numeric(self$options$specifyCutScore)
        } else {
          method <- private$.getMethodFunction(self$options$method)
          score <- NULL
        }

        # Get metric
        metric <- private$.getMetricFunction(self$options$metric)

        # Get break ties function
        break_ties <- switch(self$options$break_ties,
                             "c" = c,
                             "mean" = mean,
                             "median" = median,
                             mean  # default
        )

        # Set up tolerance
        tol_metric <- if (self$options$method %in% c(
          "maximize_metric", "minimize_metric",
          "maximize_loess_metric", "minimize_loess_metric",
          "maximize_spline_metric", "minimize_spline_metric"
        )) self$options$tol_metric else NULL

        # Get subgroup if present
        subGroup <- if (!is.null(self$options$subGroup)) self$data[, self$options$subGroup] else NULL

        # Determine positive class
        sample_class <- if (is.null(subGroup)) {
          self$data[, self$options$classVar]
        } else {
          self$data[!is.na(subGroup), self$options$classVar]
        }

        positiveClass <- private$.determinePositiveClass(sample_class, self$options$positiveClass)

        return(list(
          method = method,
          methodName = self$options$method,
          metric = metric,
          score = score,
          break_ties = break_ties,
          tol_metric = tol_metric,
          direction = self$options$direction,
          boot_runs = as.numeric(self$options$boot_runs),
          subGroup = subGroup,
          positiveClass = positiveClass,
          classVarName = self$options$classVar,
          costRatio = self$options$costRatioFP
        ))
      },

      # Prepare variables for analysis
      # @param data Dataset
      # @param params Analysis parameters
      # @return Vector of variable names to analyze
      .prepareVariables = function(data, params) {
        vars <- self$options$dependentVars

        if (!is.null(params$subGroup)) {
          # Create combined variable names for subgroups
          uniqueGroups <- unique(params$subGroup)
          vars <- apply(expand.grid(vars, uniqueGroups), 1,
                        function(x) paste(x, collapse="_"))
        }

        return(vars)
      },

      # Initialize result items for a variable
      # @param var Variable name
      .initializeResultItems = function(var) {
        if (!var %in% self$results$resultsTable$itemKeys) {
          self$results$sensSpecTable$addItem(key = var)
          self$results$resultsTable$addItem(key = var)

          if (!self$options$combinePlots) {
            self$results$plotROC$addItem(key = var)

            if (self$options$showCriterionPlot)
              self$results$criterionPlot$addItem(key = var)
            if (self$options$showPrevalencePlot)
              self$results$prevalencePlot$addItem(key = var)
            if (self$options$showDotPlot)
              self$results$dotPlot$addItem(key = var)
          }
        }
      },

      # Generate result tables for a variable
      # @param var Variable name
      # @param results Analysis results
      # @param dependentVar Dependent variable data
      # @param classVar Class variable data
      # @param params Analysis parameters
      .generateResultTables = function(var, results, dependentVar, classVar, params) {
        # Determine cutpoints to display
        if (!self$options$allObserved) {
          resultsToDisplay <- unlist(results$optimal_cutpoint)
        } else {
          resultsToDisplay <- sort(unique(dependentVar))
        }

        # Get confusion matrix
        confusionMatrix <- results$roc_curve[[1]]

        # Generate sensitivity/specificity tables if requested
        if (self$options$sensSpecTable) {
          for (i in seq_along(resultsToDisplay)) {
            cp <- resultsToDisplay[i]
            idx <- which.min(abs(confusionMatrix$x.sorted - cp))

            sensSpecRes <- print.sensSpecTable(
              Title = paste0("Scale: ", var, " | Cut Score: ",
                             round(confusionMatrix$x.sorted[idx], 4)),
              TP = confusionMatrix$tp[idx],
              FP = confusionMatrix$fp[idx],
              TN = confusionMatrix$tn[idx],
              FN = confusionMatrix$fn[idx]
            )

            sensTable <- self$results$sensSpecTable$get(key = var)
            sensTable$setContent(sensSpecRes)
          }
        }

        # Calculate metrics
        metrics <- self$.calculateMetrics(confusionMatrix, params$metric)

        # Create results table
        table <- self$results$resultsTable$get(key = var)
        table$setTitle(paste0("Scale: ", var))

        for (i in seq_along(resultsToDisplay)) {
          cp <- resultsToDisplay[i]
          idx <- which.min(abs(confusionMatrix$x.sorted - cp))

          row <- list(
            cutpoint = confusionMatrix$x.sorted[idx],
            sensitivity = metrics$sensitivity[idx],
            specificity = metrics$specificity[idx],
            ppv = metrics$ppv[idx],
            npv = metrics$npv[idx],
            youden = metrics$youden[idx],
            AUC = results$AUC,
            metricValue = metrics$metricValue[idx]
          )

          table$addRow(rowKey = as.character(i), values = row)
        }

        # Store ROC data
        private$.rocDataList[[var]] <- data.frame(
          threshold = confusionMatrix$x.sorted,
          sensitivity = metrics$sensitivity,
          specificity = metrics$specificity,
          ppv = metrics$ppv,
          npv = metrics$npv,
          youden = metrics$youden,
          stringsAsFactors = FALSE
        )

        # Store optimal criterion
        j_max_idx <- which.max(metrics$youden)
        private$.optimalCriteriaList[[var]] <- list(
          threshold = confusionMatrix$x.sorted[j_max_idx],
          sensitivity = metrics$sensitivity[j_max_idx],
          specificity = metrics$specificity[j_max_idx],
          ppv = metrics$ppv[j_max_idx],
          npv = metrics$npv[j_max_idx],
          youden = metrics$youden[j_max_idx]
        )

        # Store raw data for dot plot
        rawData <- data.frame(
          value = dependentVar,
          class = as.factor(ifelse(classVar == params$positiveClass, "Positive", "Negative")),
          threshold = private$.optimalCriteriaList[[var]]$threshold,
          direction = params$direction,
          stringsAsFactors = FALSE
        )
        attr(private$.rocDataList[[var]], "rawData") <- rawData
      },

      # Calculate metrics from confusion matrix
      # @param confusionMatrix Confusion matrix data
      # @param metric Metric function
      # @return List of calculated metrics
      .calculateMetrics = function(confusionMatrix, metric) {
        list(
          sensitivity = cutpointr::sensitivity(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          ),
          specificity = cutpointr::specificity(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          ),
          ppv = cutpointr::ppv(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          ),
          npv = cutpointr::npv(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          ),
          youden = cutpointr::youden(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          ),
          metricValue = metric(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )
        )
      },

      # Prepare plot data for a variable
      # @param var Variable name
      # @param results Analysis results
      # @return Data frame for plotting
      .preparePlotData = function(var, results) {
        confusionMatrix <- results$roc_curve[[1]]
        metrics <- self$.calculateMetrics(confusionMatrix, private$.getMetricFunction(self$options$metric))
        j_max_idx <- which.max(metrics$youden)

        data.frame(
          var = rep(var, length(confusionMatrix$x.sorted)),
          cutpoint = confusionMatrix$x.sorted,
          sensitivity = metrics$sensitivity,
          specificity = metrics$specificity,
          ppv = metrics$ppv,
          npv = metrics$npv,
          AUC = rep(results$AUC, length(confusionMatrix$x.sorted)),
          youden = metrics$youden,
          j_max_idx = j_max_idx,
          stringsAsFactors = FALSE
        )
      },

      # Set plot states for individual plots
      # @param var Variable name
      # @param plotData Plot data
      # @param results Analysis results
      .setIndividualPlotStates = function(var, plotData, results) {
        # ROC plot
        image <- self$results$plotROC$get(key = var)
        image$setTitle(paste0("ROC Curve: ", var))
        image$setState(plotData)

        # Additional plots
        if (self$options$showCriterionPlot) {
          criterionImage <- self$results$criterionPlot$get(key = var)
          criterionImage$setTitle(paste0("Sensitivity and Specificity vs. Threshold: ", var))
          criterionImage$setState(private$.rocDataList[[var]])
        }

        if (self$options$showPrevalencePlot) {
          prevImage <- self$results$prevalencePlot$get(key = var)
          prevImage$setTitle(paste0("Predictive Values vs. Prevalence: ", var))
          prevImage$setState(list(
            optimal = private$.optimalCriteriaList[[var]],
            prevalence = private$.prevalenceList[[var]]
          ))
        }

        if (self$options$showDotPlot) {
          rawData <- attr(private$.rocDataList[[var]], "rawData")
          dotImage <- self$results$dotPlot$get(key = var)
          dotImage$setTitle(paste0("Dot Plot: ", var))
          dotImage$setState(rawData)
        }
      },

      # Create combined plots
      # @param plotDataList Combined plot data
      .createCombinedPlots = function(plotDataList) {
        # Combined ROC plot
        self$results$plotROC$addItem(key = 1)
        image <- self$results$plotROC$get(key = 1)
        image$setTitle("ROC Curve: Combined")
        image$setState(plotDataList)

        # Combined criterion plot
        if (self$options$showCriterionPlot) {
          self$results$criterionPlot$addItem(key = 1)
          criterionImage <- self$results$criterionPlot$get(key = 1)
          criterionImage$setTitle("Sensitivity and Specificity vs. Threshold: Combined")

          combinedCriterionData <- data.frame()
          for (var in names(private$.rocDataList)) {
            varData <- private$.rocDataList[[var]]
            varData$var <- var
            combinedCriterionData <- rbind(combinedCriterionData, varData)
          }
          criterionImage$setState(combinedCriterionData)
        }

        # Handle dot plot message
        if (self$options$showDotPlot) {
          self$results$dotPlotMessage$setContent(
            "<p>Dot plots aren't available in combined plot mode.
                        Please uncheck 'Combine plots' to view individual dot plots.</p>"
          )
          self$results$dotPlotMessage$setVisible(TRUE)
          self$results$dotPlot$setVisible(FALSE)
        }
      },

      # Populate summary tables
      # @param aucList List of AUC values
      # @param data Dataset
      # @param params Analysis parameters
      .populateSummaryTables = function(aucList, data, params) {
        aucSummaryTable <- self$results$aucSummaryTable

        for (var in names(aucList)) {
          # Get AUC value
          auc_value <- aucList[[var]]

          # Get class counts
          if (is.null(params$subGroup)) {
            classVar <- data[, params$classVarName]
          } else {
            varParts <- strsplit(var, split = "_")[[1]]
            groupName <- paste(varParts[-1], collapse="_")
            classVar <- data[params$subGroup == groupName, params$classVarName]
          }

          n_pos <- sum(classVar == params$positiveClass)
          n_neg <- sum(classVar != params$positiveClass)

          # Calculate statistics
          auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))
          z_critical <- qnorm(0.975)
          auc_lci <- max(0, auc_value - z_critical * auc_se)
          auc_uci <- min(1, auc_value + z_critical * auc_se)

          z_stat <- (auc_value - 0.5) / auc_se
          p_val <- 2 * (1 - pnorm(abs(z_stat)))

          # Add or update row
          tryCatch({
            aucSummaryTable$setRow(rowKey = var, values = list(
              variable = as.character(var),
              auc = auc_value,
              ci_lower = auc_lci,
              ci_upper = auc_uci,
              p = p_val
            ))
          }, error = function(e) {
            aucSummaryTable$addRow(rowKey = var, values = list(
              variable = as.character(var),
              auc = auc_value,
              ci_lower = auc_lci,
              ci_upper = auc_uci,
              p = p_val
            ))
          })
        }

        # Create threshold table if requested
        if (self$options$showThresholdTable) {
          self$.createThresholdTable()
        }
      },

      # Create threshold performance table
      .createThresholdTable = function() {
        thresholdTable <- self$results$thresholdTable
        thresholdTable$clear()

        for (var in names(private$.rocDataList)) {
          rocData <- private$.rocDataList[[var]]
          prevalence <- private$.prevalenceList[[var]]

          # Select thresholds to display
          n_thresholds <- min(nrow(rocData), self$options$maxThresholds)
          step <- max(1, floor(nrow(rocData) / n_thresholds))
          indices <- seq(1, nrow(rocData), by = step)

          # Add optimal point
          optimal_idx <- which.max(rocData$youden)
          indices <- sort(unique(c(indices, optimal_idx)))

          # Add rows
          for (i in indices) {
            plr <- rocData$sensitivity[i] / (1 - rocData$specificity[i])
            nlr <- (1 - rocData$sensitivity[i]) / rocData$specificity[i]

            if (!is.finite(plr)) plr <- NA
            if (!is.finite(nlr)) nlr <- NA

            accuracy <- (rocData$sensitivity[i] * prevalence) +
              (rocData$specificity[i] * (1 - prevalence))

            thresholdTable$addRow(rowKey = paste0(var, "_", i), values = list(
              threshold = rocData$threshold[i],
              sensitivity = rocData$sensitivity[i],
              specificity = rocData$specificity[i],
              accuracy = accuracy,
              ppv = rocData$ppv[i],
              npv = rocData$npv[i],
              plr = plr,
              nlr = nlr,
              youden = rocData$youden[i]
            ))
          }
        }
      },

      # Perform additional analyses
      # @param data Dataset
      # @param vars Variable names
      # @param params Analysis parameters
      .performAdditionalAnalyses = function(data, vars, params) {
        # Partial AUC
        if (self$options$partialAUC) {
          self$.calculatePartialAUCs(data, vars, params)
        }

        # Bootstrap CIs
        if (self$options$bootstrapCI) {
          self$.calculateBootstrapCIs(data, vars, params)
        }

        # Precision-Recall curves
        if (self$options$precisionRecallCurve) {
          self$.calculatePrecisionRecall(data, vars, params)
        }

        # Comprehensive classifier comparison
        if (self$options$compareClassifiers) {
          self$.compareClassifiers(data, vars, params)
        }
      },

      # Calculate partial AUCs
      # @param data Dataset
      # @param vars Variable names
      # @param params Analysis parameters
      .calculatePartialAUCs = function(data, vars, params) {
        self$results$partialAUCTable$setVisible(TRUE)

        for (var in vars) {
          # Get data
          var_data <- self$.getVariableData(var, data, params)

          # Calculate partial AUC
          pAUC_results <- calculatePartialAUC(
            x = var_data$dependentVar,
            class = var_data$classVar,
            pos_class = params$positiveClass,
            spec_from = self$options$partialAUCfrom,
            spec_to = self$options$partialAUCto
          )

          if (!is.null(pAUC_results)) {
            self$results$partialAUCTable$addRow(rowKey = var, values = list(
              variable = var,
              pAUC = pAUC_results$pAUC,
              pAUC_normalized = pAUC_results$pAUC_normalized,
              ci_lower = pAUC_results$ci_lower,
              ci_upper = pAUC_results$ci_upper,
              spec_range = pAUC_results$spec_range
            ))
          }
        }
      },

      # Calculate bootstrap confidence intervals
      # @param data Dataset
      # @param vars Variable names
      # @param params Analysis parameters
      .calculateBootstrapCIs = function(data, vars, params) {
        self$results$bootstrapCITable$setVisible(TRUE)

        for (var in vars) {
          # Get data
          var_data <- self$.getVariableData(var, data, params)

          # Calculate bootstrap CIs
          bootstrap_results <- calculateBootstrapCI(
            x = var_data$dependentVar,
            class = var_data$classVar,
            pos_class = params$positiveClass,
            n_boot = self$options$bootstrapReps,
            conf_level = 0.95
          )

          if (!is.null(bootstrap_results)) {
            for (param in names(bootstrap_results)) {
              self$results$bootstrapCITable$addRow(
                rowKey = paste0(var, "_", param),
                values = list(
                  variable = var,
                  parameter = param,
                  estimate = bootstrap_results[[param]]$estimate,
                  ci_lower = bootstrap_results[[param]]$ci_lower,
                  ci_upper = bootstrap_results[[param]]$ci_upper
                )
              )
            }
          }
        }
      },

      # Calculate precision-recall curves
      # @param data Dataset
      # @param vars Variable names
      # @param params Analysis parameters
      .calculatePrecisionRecall = function(data, vars, params) {
        if (self$options$combinePlots) {
          self$results$precisionRecallPlot$addItem(key = 1)
          pr_plot_data <- data.frame()
        }

        for (var in vars) {
          # Get data
          var_data <- self$.getVariableData(var, data, params)

          # Calculate precision-recall
          pr_results <- calculatePrecisionRecall(
            x = var_data$dependentVar,
            class = var_data$classVar,
            pos_class = params$positiveClass
          )

          if (!is.null(pr_results)) {
            if (self$options$combinePlots) {
              pr_plot_data <- rbind(
                pr_plot_data,
                data.frame(
                  threshold = pr_results$threshold,
                  precision = pr_results$precision,
                  recall = pr_results$recall,
                  auprc = rep(pr_results$auprc, length(pr_results$threshold)),
                  var = rep(var, length(pr_results$threshold))
                )
              )
            } else {
              self$results$precisionRecallPlot$addItem(key = var)
              pr_plot <- self$results$precisionRecallPlot$get(key = var)
              pr_plot$setTitle(paste0("Precision-Recall Curve: ", var))
              pr_plot$setState(
                data.frame(
                  threshold = pr_results$threshold,
                  precision = pr_results$precision,
                  recall = pr_results$recall,
                  auprc = rep(pr_results$auprc, length(pr_results$threshold))
                )
              )
            }
          }
        }

        if (self$options$combinePlots && nrow(pr_plot_data) > 0) {
          pr_plot <- self$results$precisionRecallPlot$get(key = 1)
          pr_plot$setTitle("Precision-Recall Curves: Combined")
          pr_plot$setState(pr_plot_data)
        }
      },

      # Compare classifiers
      # @param data Dataset
      # @param vars Variable names
      # @param params Analysis parameters
      .compareClassifiers = function(data, vars, params) {
        self$results$rocComparisonTable$setVisible(TRUE)

        for (var in vars) {
          # Get data
          var_data <- self$.getVariableData(var, data, params)

          # Calculate metrics
          metrics <- calculateComprehensiveMetrics(
            x = var_data$dependentVar,
            class = var_data$classVar,
            pos_class = params$positiveClass
          )

          if (!is.null(metrics)) {
            self$results$rocComparisonTable$addRow(rowKey = var, values = list(
              variable = var,
              auc = metrics$auc,
              auprc = metrics$auprc,
              brier = metrics$brier,
              f1_score = metrics$f1_score,
              accuracy = metrics$accuracy,
              balanced_accuracy = metrics$balanced_accuracy
            ))
          }
        }
      },

      # Get variable data for analysis
      # @param var Variable name
      # @param data Dataset
      # @param params Analysis parameters
      # @return List with dependentVar and classVar
      .getVariableData = function(var, data, params) {
        if (is.null(params$subGroup)) {
          dependentVar <- as.numeric(data[, var])
          classVar <- data[, params$classVarName]
        } else {
          varParts <- strsplit(var, split = "_")[[1]]
          varName <- varParts[1]
          groupName <- paste(varParts[-1], collapse="_")

          dependentVar <- as.numeric(data[params$subGroup == groupName, varName])
          classVar <- data[params$subGroup == groupName, params$classVarName]
        }

        return(list(
          dependentVar = dependentVar,
          classVar = classVar
        ))
      },

      # Perform statistical comparisons
      # @param data Dataset
      # @param params Analysis parameters
      .performStatisticalComparisons = function(data, params) {
        # DeLong's test
        if (self$options$delongTest) {
          self$.performDeLongTest(data, params)
        }

        # IDI and NRI
        if (self$options$calculateIDI || self$options$calculateNRI) {
          self$.calculateIDINRI(data, params)
        }
      },

      # Perform DeLong's test
      # @param data Dataset
      # @param params Analysis parameters
      .performDeLongTest = function(data, params) {
        if (length(self$options$dependentVars) < 2) {
          stop("Please specify at least two dependent variables to use DeLong's test.")
        }

        if (!is.null(self$options$subGroup)) {
          stop("DeLong's test does not currently support the group variable.")
        }

        # Run DeLong's test
        delongResults <- deLong.test(
          data = data.frame(lapply(data[, self$options$dependentVars], as.numeric)),
          classVar = as.character(data[, params$classVarName]),
          ref = NULL,
          pos_class = params$positiveClass,
          conf.level = 0.95
        )

        # Display results
        self$results$delongTest$setVisible(TRUE)
        self$results$delongTest$setContent(
          paste0(capture.output(print.DeLong(delongResults)), collapse = "\n")
        )

        # Format table
        delongTable <- self$results$delongComparisonTable
        diff_data <- delongResults$difference

        for (i in 1:nrow(diff_data)) {
          comparison <- rownames(diff_data)[i]
          delongTable$addRow(rowKey = comparison, values = list(
            comparison = comparison,
            auc_diff = diff_data[i, "AUC Difference"],
            ci_lower = diff_data[i, "CI(lower)"],
            ci_upper = diff_data[i, "CI(upper)"],
            z = sqrt(qchisq(1 - diff_data[i, "P.Value"], df = 1)),
            p = diff_data[i, "P.Value"]
          ))
        }
      },

      # Calculate IDI and NRI
      # @param data Dataset
      # @param params Analysis parameters
      .calculateIDINRI = function(data, params) {
        if (length(self$options$dependentVars) < 2) {
          stop("Please specify at least two dependent variables to calculate IDI/NRI.")
        }

        # Get reference variable
        refVar <- if (!is.null(self$options$refVar) && self$options$refVar != "") {
          self$options$refVar
        } else {
          self$options$dependentVars[1]
        }

        # Get actual class values
        classVar <- data[, params$classVarName]
        actual_binary <- as.numeric(classVar == params$positiveClass)

        # Get reference values
        ref_values <- as.numeric(data[, refVar])

        # Calculate IDI if requested
        if (self$options$calculateIDI) {
          for (var in self$options$dependentVars) {
            if (var != refVar) {
              var_values <- as.numeric(data[, var])

              idi_result <- bootstrapIDI(
                new_values = var_values,
                ref_values = ref_values,
                actual = actual_binary,
                direction = params$direction,
                n_boot = as.numeric(self$options$idiNriBootRuns)
              )

              self$results$idiTable$addRow(rowKey = var, values = list(
                variable = var,
                refVar = refVar,
                idi = idi_result$idi,
                ci_lower = idi_result$ci_lower,
                ci_upper = idi_result$ci_upper,
                p = idi_result$p_value
              ))
            }
          }
        }

        # Calculate NRI if requested
        if (self$options$calculateNRI) {
          # Parse thresholds
          thresholds <- NULL
          if (!is.null(self$options$nriThresholds) && self$options$nriThresholds != "") {
            thresholds <- as.numeric(unlist(strsplit(self$options$nriThresholds, ",")))
            thresholds <- thresholds[!is.na(thresholds)]
            thresholds <- thresholds[thresholds > 0 & thresholds < 1]
          }

          for (var in self$options$dependentVars) {
            if (var != refVar) {
              var_values <- as.numeric(data[, var])

              nri_result <- bootstrapNRI(
                new_values = var_values,
                ref_values = ref_values,
                actual = actual_binary,
                thresholds = thresholds,
                direction = params$direction,
                n_boot = as.numeric(self$options$idiNriBootRuns)
              )

              self$results$nriTable$addRow(rowKey = var, values = list(
                variable = var,
                refVar = refVar,
                nri = nri_result$nri,
                event_nri = nri_result$event_nri,
                non_event_nri = nri_result$non_event_nri,
                ci_lower = nri_result$ci_lower,
                ci_upper = nri_result$ci_upper,
                p = nri_result$p_value
              ))
            }
          }
        }
      },

      # ======================================================================
      # PLOTTING METHODS
      # ======================================================================

      # Plot ROC curves
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotROC = function(image, ggtheme, theme, ...) {
        plotData <- data.frame(image$state)

        if (nrow(plotData) == 0) return(FALSE)

        # Create base plot
        if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
          # Multiple variables in one plot
          plot <- ggplot2::ggplot(plotData,
                                  ggplot2::aes(
                                    x = 1 - specificity,
                                    y = sensitivity,
                                    color = var,
                                    linetype = var
                                  )) +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
            ggplot2::geom_line(size = 1) +
            ggplot2::scale_color_brewer(palette = "Set1") +
            ggplot2::scale_linetype_manual(values = rep(c("solid", "dashed", "dotted", "longdash"),
                                                        length.out = length(unique(plotData$var))))
        } else {
          # Single variable plot
          plot <- ggplot2::ggplot(plotData,
                                  ggplot2::aes(
                                    x = 1 - specificity,
                                    y = sensitivity
                                  )) +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
            ggplot2::geom_line(size = 1) +
            ggplot2::geom_point(size = 0.5, alpha = ifelse(self$options$cleanPlot, 0, 0.7))
        }

        # Add common elements
        plot <- plot +
          ggplot2::xlab("1 - Specificity (False Positive Rate)") +
          ggplot2::ylab("Sensitivity (True Positive Rate)") +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1)

        # Apply theme
        if (self$options$cleanPlot) {
          plot <- plot +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              panel.grid.minor = ggplot2::element_blank(),
              plot.title = ggplot2::element_blank(),
              plot.subtitle = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(color = "black", fill = NA)
            )

          # Handle legend for clean plots
          if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
            if (self$options$legendPosition == "none") {
              plot <- plot + ggplot2::theme(legend.position = "none")
            } else {
              plot <- plot + ggplot2::theme(
                legend.position = self$options$legendPosition,
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            }
          }
        } else {
          plot <- plot + ggtheme
        }

        # Mark optimal points
        if (!self$options$cleanPlot || self$options$showOptimalPoint) {
          if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
            # Find optimal points for each variable
            optimal_points <- data.frame()
            for (var_name in unique(plotData$var)) {
              var_data <- plotData[plotData$var == var_name,]
              j_max_idx <- which.max(var_data$youden)
              if (length(j_max_idx) > 0) {
                optimal_points <- rbind(optimal_points, var_data[j_max_idx,])
              }
            }

            if (nrow(optimal_points) > 0) {
              plot <- plot +
                ggplot2::geom_point(
                  data = optimal_points,
                  ggplot2::aes(
                    x = 1 - specificity,
                    y = sensitivity,
                    color = var
                  ),
                  size = 3, shape = 18
                )
            }
          } else {
            # Single variable optimal point
            if ('j_max_idx' %in% names(plotData)) {
              j_max_idx <- unique(plotData$j_max_idx)
              if (length(j_max_idx) == 1 && !is.na(j_max_idx)) {
                plot <- plot +
                  ggplot2::geom_point(
                    data = plotData[j_max_idx,],
                    ggplot2::aes(
                      x = 1 - specificity,
                      y = sensitivity
                    ),
                    size = 3, shape = 18, color = "red"
                  )

                if (!self$options$cleanPlot) {
                  plot <- plot +
                    ggplot2::annotate(
                      "text",
                      x = 1 - plotData$specificity[j_max_idx] + 0.05,
                      y = plotData$sensitivity[j_max_idx],
                      label = paste("J =", round(plotData$youden[j_max_idx], 3)),
                      hjust = 0
                    )
                }
              }
            }
          }
        }

        # Add AUC annotations
        if (!self$options$cleanPlot) {
          if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
            # Multiple AUC annotations
            auc_data <- aggregate(AUC ~ var, data = plotData, FUN = function(x) x[1])
            auc_data$AUC_formatted <- sprintf("AUC = %.3f", auc_data$AUC)
            auc_data$x <- 0.75
            auc_data$y <- seq(0.3, 0.1, length.out = nrow(auc_data))

            plot <- plot +
              ggplot2::geom_text(data = auc_data,
                                 ggplot2::aes(x = x, y = y, label = AUC_formatted, color = var),
                                 hjust = 0, show.legend = FALSE)
          } else {
            # Single AUC annotation
            if (nrow(plotData) > 0) {
              auc_value <- unique(plotData$AUC)[1]
              plot <- plot + ggplot2::annotate(
                "text",
                x = 0.75,
                y = 0.25,
                label = sprintf("AUC = %.3f", auc_value)
              )
            }
          }
        }

        print(plot)
        return(TRUE)
      },

      # Plot sensitivity/specificity vs criterion
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotCriterion = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || (is.data.frame(plotData) && nrow(plotData) == 0))
          return(FALSE)

        # Check if this is a combined plot
        if ("var" %in% names(plotData)) {
          # Multiple variables
          plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = threshold, group = var, color = var)) +
            ggplot2::geom_line(ggplot2::aes(y = sensitivity, linetype = "Sensitivity")) +
            ggplot2::geom_line(ggplot2::aes(y = specificity, linetype = "Specificity")) +
            ggplot2::scale_linetype_manual(name = "Metric",
                                           values = c("Sensitivity" = "solid", "Specificity" = "dashed")) +
            ggplot2::labs(
              x = "Threshold",
              y = "Value",
              color = "Variable",
              title = "Sensitivity and Specificity vs. Threshold"
            )
        } else {
          # Single variable - reshape data
          plot_data_long <- tidyr::gather(
            plotData,
            key = "metric",
            value = "value",
            sensitivity, specificity
          )

          plot_data_long$metric <- factor(
            plot_data_long$metric,
            levels = c("sensitivity", "specificity"),
            labels = c("Sensitivity", "Specificity")
          )

          plot <- ggplot2::ggplot(
            plot_data_long,
            ggplot2::aes(x = threshold, y = value, color = metric)
          ) +
            ggplot2::geom_line() +
            ggplot2::labs(
              x = "Threshold",
              y = "Value",
              color = "Metric",
              title = "Sensitivity and Specificity vs. Threshold"
            )

          # Add optimal threshold line
          optimal_idx <- which.max(plotData$youden)
          if (length(optimal_idx) > 0) {
            opt_threshold <- plotData$threshold[optimal_idx]

            plot <- plot + ggplot2::geom_vline(
              xintercept = opt_threshold,
              linetype = "dotted",
              color = "darkgray"
            ) +
              ggplot2::annotate(
                "text",
                x = opt_threshold,
                y = 0.1,
                label = sprintf("Optimal: %.3f", opt_threshold),
                hjust = -0.1
              )
          }
        }

        plot <- plot + ggtheme

        print(plot)
        return(TRUE)
      },

      # Plot PPV/NPV vs prevalence
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotPrevalence = function(image, ggtheme, theme, ...) {
        state <- image$state

        if (is.null(state))
          return(FALSE)

        optimal <- state$optimal
        prevalence <- state$prevalence

        if (is.null(optimal) || is.null(prevalence))
          return(FALSE)

        # Create prevalence sequence
        prev_seq <- seq(0.01, 0.99, by = 0.01)

        # Calculate PPV and NPV for different prevalence values
        ppv_vals <- (optimal$sensitivity * prev_seq) /
          ((optimal$sensitivity * prev_seq) + ((1 - optimal$specificity) * (1 - prev_seq)))

        npv_vals <- (optimal$specificity * (1 - prev_seq)) /
          ((optimal$specificity * (1 - prev_seq)) + ((1 - optimal$sensitivity) * prev_seq))

        # Create data frame for plotting
        plot_data <- data.frame(
          prevalence = c(prev_seq, prev_seq),
          value = c(ppv_vals, npv_vals),
          metric = factor(
            rep(c("PPV", "NPV"), each = length(prev_seq)),
            levels = c("PPV", "NPV"),
            labels = c("Positive Predictive Value", "Negative Predictive Value")
          )
        )

        # Create plot
        plot <- ggplot2::ggplot(
          plot_data,
          ggplot2::aes(x = prevalence, y = value, color = metric)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_vline(
            xintercept = prevalence,
            linetype = "dashed",
            color = "darkgray"
          ) +
          ggplot2::labs(
            title = "Predictive Values vs. Disease Prevalence",
            subtitle = paste0(
              "At Optimal Threshold = ", round(optimal$threshold, 3),
              " (Sens = ", round(optimal$sensitivity * 100, 1),
              "%, Spec = ", round(optimal$specificity * 100, 1), "%)"
            ),
            x = "Disease Prevalence",
            y = "Value",
            color = "Metric"
          ) +
          ggplot2::annotate(
            "text",
            x = prevalence,
            y = 0.1,
            label = sprintf("Sample Prevalence: %.2f", prevalence),
            hjust = -0.1
          ) +
          ggtheme

        print(plot)
        return(TRUE)
      },

      # Plot dot plot showing distribution of test values by class
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotDot = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || nrow(plotData) == 0)
          return(FALSE)

        # Create plot
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(x = class, y = value, color = class)
        ) +
          # Add jittered points
          ggplot2::geom_jitter(
            width = 0.3,
            height = 0,
            alpha = 0.7,
            size = 3
          ) +
          # Add horizontal line at threshold
          ggplot2::geom_hline(
            yintercept = plotData$threshold[1],
            linetype = "dashed",
            color = "darkgray"
          ) +
          # Set color palette
          ggplot2::scale_color_manual(
            values = c("Negative" = "darkblue", "Positive" = "darkred")
          ) +
          # Add labels
          ggplot2::labs(
            title = "Distribution of Values by Class",
            x = "Class",
            y = "Value"
          ) +
          # Add annotation with threshold info
          ggplot2::annotate(
            "text",
            x = 1.5,
            y = plotData$threshold[1],
            label = paste0(
              "Threshold: ", round(plotData$threshold[1], 3),
              "\nDirection: ", plotData$direction[1]
            ),
            hjust = 0
          ) +
          # Remove legend
          ggplot2::theme(legend.position = "none") +
          ggtheme

        # Add boxplots
        plot <- plot +
          ggplot2::geom_boxplot(
            alpha = 0.3,
            width = 0.5,
            outlier.shape = NA  # Hide outliers since we're showing all points
          )

        print(plot)
        return(TRUE)
      },

      # Plot precision-recall curves
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotPrecisionRecall = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || nrow(plotData) == 0)
          return(FALSE)

        # Determine if this is a combined or individual plot
        if ("var" %in% names(plotData)) {
          # Combined plot with multiple variables
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = recall,
              y = precision,
              color = var,
              linetype = var
            )
          ) +
            ggplot2::geom_line(size = 1) +
            ggplot2::scale_color_brewer(palette = "Set1") +
            ggplot2::scale_linetype_manual(
              values = rep(c("solid", "dashed", "dotted", "longdash"),
                           length.out = length(unique(plotData$var)))
            )
        } else {
          # Individual plot
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = recall,
              y = precision
            )
          ) +
            ggplot2::geom_line(size = 1) +
            ggplot2::geom_point(size = 0.5, alpha = 0.7)
        }

        # Add AUPRC annotations
        if ("auprc" %in% names(plotData)) {
          if ("var" %in% names(plotData)) {
            # For combined plot
            auprc_data <- aggregate(auprc ~ var, data = plotData, FUN = function(x) x[1])
            auprc_data$label <- sprintf("AUPRC = %.3f", auprc_data$auprc)
            auprc_data$x <- 0.25
            auprc_data$y <- seq(0.3, 0.1, length.out = nrow(auprc_data))

            plot <- plot +
              ggplot2::geom_text(
                data = auprc_data,
                ggplot2::aes(x = x, y = y, label = label, color = var),
                hjust = 0,
                show.legend = FALSE
              )
          } else {
            # For individual plot
            plot <- plot +
              ggplot2::annotate(
                "text",
                x = 0.25,
                y = 0.25,
                label = sprintf("AUPRC = %.3f", unique(plotData$auprc)[1])
              )
          }
        }

        # Add labels and theme
        plot <- plot +
          ggplot2::xlab("Recall (Sensitivity)") +
          ggplot2::ylab("Precision (Positive Predictive Value)") +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1) +
          ggtheme

        print(plot)
        return(TRUE)
      }
    )
  )
