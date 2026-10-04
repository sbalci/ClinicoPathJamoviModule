.enhancedROCValidation <- function(data, options) {
  positive <- options$positiveClass
  negative <- options$validationNegative
  if (is.null(positive) || !nzchar(positive) || !nzchar(negative) || positive == negative) {
    stop("Select a positive reference level and enter a distinct negative reference level.")
  }
  if (options$direction == "auto") stop("Frozen-model evaluation requires an explicit score direction.")
  if (options$cohortRole == "frozen" &&
      (!nzchar(trimws(options$modelVersion)) || options$thresholdSource == "exploratory")) {
    stop("Frozen evaluation requires a model version and an independently selected threshold. Use Development / exploratory for data-selected thresholds.")
  }
  cutoffs <- .dvNumbers(options$impactCutoffs)
  if (!length(cutoffs)) stop("Enter at least one fixed score cutoff.")
  ref <- as.character(data[[options$outcome]])
  extra <- !is.na(ref) & !ref %in% c(positive, negative)
  if (any(extra)) stop(paste("Unmapped reference levels; explicitly recode before evaluation:",
                             paste(unique(ref[extra]), collapse = ", ")))
  score <- as.matrix(data[options$predictors])
  if (!is.numeric(score)) stop("Scores must be numeric.")
  n <- nrow(data)
  patient_selected <- !is.null(options$patientID) && nzchar(options$patientID)
  site_selected <- !is.null(options$siteID) && nzchar(options$siteID)
  partition_selected <- !is.null(options$partitionID) && nzchar(options$partitionID)
  patient <- if (patient_selected) as.character(data[[options$patientID]]) else as.character(seq_len(n))
  site <- if (site_selected) as.character(data[[options$siteID]]) else rep("All", n)
  if (anyNA(patient) || any(!nzchar(patient))) stop("Patient IDs must be complete; none were silently dropped.")
  if (anyNA(site) || any(!nzchar(site))) stop("Site IDs must be complete; none were silently dropped.")
  if (partition_selected && !patient_selected) stop("A partition audit requires patient identifiers.")
  if (patient_selected) {
    groups <- split(seq_len(n), patient)
    if (any(vapply(groups, function(i) length(unique(ref[i][!is.na(ref[i])])) > 1, logical(1)))) {
      stop("Reference status is inconsistent within a patient.")
    }
    if (any(vapply(groups, function(i) length(unique(site[i])) > 1, logical(1)))) {
      stop("Patient IDs span sites; use globally unique patient IDs and resolve cross-site duplicates.")
    }
    if (partition_selected) {
      partition <- as.character(data[[options$partitionID]])
      if (anyNA(partition) || any(!nzchar(partition))) stop("Partition labels must be complete.")
      if (any(vapply(groups, function(i) length(unique(partition[i])) > 1, logical(1)))) {
        stop("Patient overlap across partitions detected. Resolve leakage before evaluation.")
      }
    }
  }
  valid_scores <- apply(is.finite(score), 1, all)
  status <- ifelse(is.na(ref), "Missing reference",
                   ifelse(!valid_scores, "Missing / nonfinite score", "Analyzed common cases"))
  keep <- status == "Analyzed common cases"
  ledger <- data.frame(status = c("Input rows", "Missing reference", "Missing / nonfinite score",
                                 "Analyzed common cases", "Analyzed positive", "Analyzed negative"),
                       n = c(n, sum(status == "Missing reference"),
                             sum(status == "Missing / nonfinite score"), sum(keep),
                             sum(ref[keep] == positive), sum(ref[keep] == negative)))
  if (!any(keep)) stop("No complete reference and score rows remain.")
  y <- as.integer(ref[keep] == positive)
  score <- score[keep, , drop = FALSE]
  patient <- patient[keep]
  site <- site[keep]
  probability <- options$predictionScale == "probability"
  higher <- options$direction == "higher"
  if (probability && (any(score < 0 | score > 1) || !higher)) {
    stop("Probability inputs must be in [0,1] for the selected positive class; direction must be higher.")
  }
  context <- c(
    "Cohort role (user declared)" = options$cohortRole,
    "Sampling design" = options$samplingDesign,
    "Model version" = options$modelVersion,
    "Reference standard" = options$referenceDescription,
    "Positive / negative" = paste(positive, "/", negative),
    "Threshold source" = options$thresholdSource,
    "Unit of observation" = options$analysisUnit,
    "Prediction scale" = options$predictionScale,
    "Analyzed independent units" = as.character(length(unique(patient))),
    "Positive independent units" = as.character(length(unique(patient[y == 1]))),
    "Negative independent units" = as.character(length(unique(patient[y == 0]))),
    "Resampling unit" = if (patient_selected) "Patient; all slides retained" else "Row; independence assumed",
    "Site inference" = "Conditional on observed sites; no site-population extrapolation",
    "Partition audit" = if (partition_selected) "No patient overlap in supplied partition labels" else "Not assessed; no partition manifest",
    "Interpretation" = "Frozen scores; no refitting or threshold optimization. Slide-weighted metrics; paired differences use common cases.")
  rows <- list()
  subsets <- list(All = seq_along(y))
  if (site_selected) for (s in unique(site)) subsets[[paste0("Site: ", s)]] <- which(site == s)
  for (s in names(subsets)) {
    idx <- subsets[[s]]
    for (cutoff in cutoffs) {
      enough_clusters <- all(vapply(split(patient[idx], site[idx]), function(z)
          length(unique(z)) >= 2L, logical(1)))
      if (options$validationBootstrap && enough_clusters) {
        result <- .dvBootstrap(y[idx], score[idx, , drop = FALSE], patient[idx], cutoff,
          higher, probability, options$validationDraws, options$seed,
          options$confidenceLevel / 100, site[idx])
      } else {
        point <- vapply(seq_len(ncol(score)), function(j)
          .dvScoreMetrics(y[idx], score[idx, j], cutoff, higher, probability), numeric(13))
        result <- do.call(rbind, lapply(seq_len(ncol(score)), function(j)
          data.frame(model = colnames(score)[j], metric = rownames(point), estimate = point[, j])))
        if (ncol(score) > 1) for (j in 2:ncol(score)) result <- rbind(result,
          data.frame(model = paste(colnames(score)[j], "minus", colnames(score)[1]),
                     metric = rownames(point), estimate = point[, j] - point[, 1]))
        result$lower <- result$upper <- NA_real_
        result$valid <- result$failed <- 0L
        result$status <- ifelse(is.finite(result$estimate), if (options$validationBootstrap) "Too few independent clusters per site" else "CI not requested", "Metric undefined")
      }
      result$n <- length(idx)
      result$positives <- sum(y[idx])
      result$negatives <- sum(1-y[idx])
      result$clusters <- length(unique(patient[idx]))
      result$site <- s
      result$cutoff <- cutoff
      rows[[length(rows)+1L]] <- result
    }
  }
  workload <- list()
  if (isTRUE(options$clinicalImpact)) {
    scenarios <- .dvNumbers(options$prevalenceScenarios, 0, 1)
    observed <- isTRUE(options$useObservedPrevalence)
    prev <- c(if (observed) mean(y) else options$prevalence, scenarios)
    for (j in seq_len(ncol(score))) for (cutoff in cutoffs) for (i in seq_along(prev)) {
      z <- .dvCounts(y, score[, j], cutoff, higher)
      se <- if (sum(y)) unname(z['tp']/sum(y)) else NA_real_
      sp <- if (sum(1-y)) unname(z['tn']/sum(1-y)) else NA_real_
      w <- .dvWorkload(se, sp, prev[i], if (i == 1 && observed) length(y) else
          options$workloadPopulationSize, options$utilityThreshold)
      workload[[length(workload)+1L]] <- cbind(data.frame(predictor = colnames(score)[j],
          cutoff = cutoff, source = if (i == 1 && observed) "Observed" else "Projected",
          unit = options$analysisUnit), w)
    }
  }
  list(workload = if (length(workload)) do.call(rbind, workload) else data.frame(),
       context = data.frame(setting = names(context), value = unname(context)),
       ledger = ledger, metrics = do.call(rbind, rows))
}
