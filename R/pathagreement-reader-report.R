.paReaderLayout <- function(o, prepared = NULL) {
  vars <- if (isTRUE(prepared$ready)) prepared$vars else
    if (o$readerWorkflow == "numeric_cutoff") o$readerScores else o$vars
  np <- if (length(vars) >= 2L) {
    if (o$readerPairs == "selected") 1L else choose(length(vars), 2)
  } else 0L
  band <- (o$readerWorkflow == "numeric_cutoff" && nzchar(o$nearCutoffWidth)) ||
    (o$readerWorkflow == "ordinal_cutoff" && nzchar(o$nearCutoffLevels))
  pops <- if (o$readerPopulation == "both") 2L else 1L
  case_count <- 0L
  if (isTRUE(prepared$ready)) {
    case_count <- min(o$readerCaseLimit, sum(vapply(prepared$pairs, function(pair) {
      sum(prepared$calls[, pair[1]] != prepared$calls[, pair[2]], na.rm = TRUE)
    }, numeric(1))))
  }
  if (!is.null(prepared$long)) np <- 0L
  advanced <- isTRUE(prepared$ready) && (o$readerNondiagnostic || o$readerFormat == "long")
  mapping_n <- if (advanced) sum(vapply(prepared$original_raw,
    function(x) length(unique(as.character(x))), integer(1))) else 0L
  transitions <- if (isTRUE(prepared$ready) && o$readerTransitions)
    tryCatch(.paReaderTransitions(prepared), error = function(e) list()) else list()
  modality_k <- if (o$modalityPopulation == "all_observed") 4L else 3L
  modality_ready <- isTRUE(prepared$ready) && !is.null(prepared$long) &&
    o$readerModalityComparison && prepared$reference_ready
  list(readerSettings = seq_len(24L), readerDataSummary = seq_len(length(vars) + 1L),
       readerAccuracyTable = seq_len(length(vars) * pops * 3L),
       cutoffAgreementTable = seq_len(np * (1L + band) * 5L),
       readerOriginalAgreement = seq_len(np), kappaBalancedTable = 1L,
       cutoffDisagreementCases = seq_len(case_count),
       readerAvailability = seq_len(if (advanced) length(vars) * 16L else 0L),
       readerMapping = seq_len(mapping_n),
       readerReferenceExclusions = seq_len(if (advanced)
         length(unique(prepared$reference_raw[prepared$reference_state != "eligible"])) else 0L),
       readerObservedAgreement = seq_len(if (advanced && o$readerAllObservedAgreement &&
         prepared$reference_ready) length(vars) * 4L else 0L),
       readerModalitySummary = seq_len(if (modality_ready) modality_k * 2L else 0L),
       readerModalityContrast = seq_len(if (modality_ready) modality_k else 0L),
       readerTransitionsTable = seq_len(NROW(transitions$readerTransitionsTable)),
       readerTransitionMatrix = seq_len(NROW(transitions$readerTransitionMatrix)))
}

.paReaderReport <- function(d, checkpoint = NULL) {
  o <- d$options
  # Rows are accumulated in lists; this avoids quadratic rbind work for many pairs.
  output <- stats::setNames(lapply(names(.paReaderLayout(o, d)), function(x) list()),
                           names(.paReaderLayout(o, d)))
  add <- function(name, values) {
    output[[name]][[length(output[[name]]) + 1L]] <<- values
  }
  n <- nrow(d$calls)
  cluster_n <- function(idx) if (is.null(d$clusters)) length(idx) else length(unique(d$clusters[idx]))
  options_text <- function(x) if (length(x)) paste(x, collapse = ", ") else "Not selected"
  settings <- list(
    Status = "Ready", Workflow = o$readerWorkflow, Readers = options_text(d$vars),
    Rule = d$rule, Reference = d$reference_message,
    Population = o$readerPopulation, `Ordinal order` = options_text(d$scale),
    `Near-cutoff definition` = if (!d$has_band) "Not requested" else
      if (o$readerWorkflow == "numeric_cutoff") paste("Either reader within", o$nearCutoffWidth, "of cutoff (inclusive)") else
        paste("Either reader in:", gsub("\n", "; ", o$nearCutoffLevels, fixed = TRUE)),
    `Sampling unit` = if (is.null(d$clusters)) "Independent case" else paste("Cluster:", o$readerCluster),
    `Confidence level` = as.character(o$readerConfidence),
    `Bootstrap requested` = as.character(o$readerIntervals && o$readerBootstrap),
    `Bootstrap draws` = as.character(o$bootstrapSamples), Seed = as.character(o$seed),
    `Case rows` = as.character(n), `Metadata rows removed` = as.character(d$metadata_n),
    `Readers and weighting` = "Fixed selected readers; specimen-weighted estimates",
    `Pair selection` = if (o$readerPairs == "all") "All pairs" else paste(o$readerPair1, "vs", o$readerPair2),
    `Display limit` = as.character(o$readerCaseLimit),
    `Case identifier` = if (length(o$caseID)) o$caseID else "Original row number",
    `Reference interpretation` = switch(o$referenceType, legacy = "Not specified; interpretation must be declared",
      disease = "Disease reference standard", consensus = "Expert consensus comparator",
      method = "Comparison method"),
    `Reference description` = o$referenceDescription, `Target definition` = o$targetDefinition,
    `Nondiagnostic label` = if (o$readerNondiagnostic) o$readerNondiagnosticLevel else "Not declared",
    `Input format` = o$readerFormat)
  for (key in names(settings)) add("readerSettings", list(setting = key, value = settings[[key]]))
  for (j in seq_along(d$vars)) {
    missing <- is.na(d$original_raw[[j]])
    add("readerDataSummary", list(reader = d$vars[j], total = n, missing = sum(missing),
      indeterminate = sum(!missing & is.na(d$calls[, j])), usable = sum(!is.na(d$calls[, j]))))
  }
  add("readerDataSummary", list(reader = "Reference", total = n,
    missing = sum(is.na(d$reference_raw)),
    indeterminate = sum(!is.na(d$reference_raw) & is.na(d$reference)),
    usable = sum(!is.na(d$reference))))

  intervals <- function(df, statistic, idx, point, binomial = list()) {
    result <- data.frame(metric = names(point), estimate = unname(point),
      lower = NA_real_, upper = NA_real_, attempted = 0L, valid = 0L,
      undefined = 0L, boundary = 0L, constant = FALSE,
      status = if (o$readerIntervals) "Bootstrap not requested" else "Intervals not requested")
    result$status[!is.finite(point)] <- "Point estimate is undefined"
    if (o$readerIntervals && o$readerBootstrap) {
      result <- .readerResample(df, statistic,
        clusters = if (is.null(d$clusters)) NULL else d$clusters[idx],
        samples = o$bootstrapSamples, confidence = o$readerConfidence,
        seed = o$seed, checkpoint = checkpoint)
    }
    # Exact intervals are appropriate only for independent Bernoulli cases.
    if (o$readerIntervals && is.null(d$clusters)) {
      for (metric in names(binomial)) {
        k <- match(metric, result$metric)
        counts <- binomial[[metric]]
        ci <- .paReaderExactCI(counts[1], counts[2], o$readerConfidence)
        result[k, c("lower", "upper")] <- ci
        result$status[k] <- if (all(is.finite(ci))) "Exact binomial" else "No eligible cases"
        result[k, c("attempted", "valid", "undefined", "boundary")] <- 0L
        result$constant[k] <- FALSE
      }
    } else if (o$readerIntervals && !o$readerBootstrap && !is.null(d$clusters)) {
      result$status[is.finite(point)] <- "Cluster intervals require bootstrap"
    }
    result
  }
  ci_fields <- function(ci, k) as.list(ci[k, c("estimate", "lower", "upper", "attempted",
    "valid", "undefined", "boundary", "constant", "status"), drop = FALSE])
  if (d$reference_ready) {
    populations <- switch(o$readerPopulation, both = c("available", "shared"), o$readerPopulation)
    for (j in seq_along(d$vars)) for (population in populations) {
      idx <- which(if (population == "shared") d$shared else
        !is.na(d$calls[, j]) & !is.na(d$reference))
      df <- data.frame(call = d$calls[idx, j], gold = d$reference[idx])
      vals <- .paReaderAccuracy(df$call, df$gold)
      stat <- function(z) .paReaderAccuracy(z$call, z$gold)[c("sensitivity", "specificity", "balanced_accuracy")]
      ci <- intervals(df, stat, idx, stat(df), list(
        sensitivity = c(vals[["tp"]], vals[["tp"]] + vals[["fn"]]),
        specificity = c(vals[["tn"]], vals[["tn"]] + vals[["fp"]])))
      for (k in seq_len(nrow(ci))) add("readerAccuracyTable", c(list(
        reader = d$vars[j], population = population, n = length(idx), units = cluster_n(idx),
        excluded = n - length(idx), tp = vals[["tp"]], fn = vals[["fn"]],
        tn = vals[["tn"]], fp = vals[["fp"]], metric = if (o$referenceType %in% c("consensus", "method"))
          c("PPA", "NPA", "Mean PPA and NPA")[k] else ci$metric[k],
        numerator = c(vals[["tp"]], vals[["tn"]], NA_real_)[k],
        denominator = c(vals[["tp"]] + vals[["fn"]], vals[["tn"]] + vals[["fp"]], NA_real_)[k]),
        ci_fields(ci, k)))
    }
  }
  for (pair in d$pairs) {
    if (!is.null(checkpoint)) checkpoint()
    i <- pair[1]; j <- pair[2]
    pair_name <- paste(d$vars[i], "vs", d$vars[j])
    observed <- !is.na(d$calls[, i]) & !is.na(d$calls[, j])
    scopes <- if (d$has_band) c("All paired cases", "Near cutoff") else "All paired cases"
    for (scope in scopes) {
      idx <- which(observed & if (scope == "Near cutoff") (d$near[, i] | d$near[, j]) else TRUE)
      df <- data.frame(x = d$calls[idx, i], y = d$calls[idx, j])
      vals <- .paReaderAgreement(df$x, df$y)
      metrics <- c("observed_agreement", "discordance", "kappa", "positive_agreement", "negative_agreement")
      stat <- function(z) .paReaderAgreement(z$x, z$y)[metrics]
      ci <- intervals(df, stat, idx, stat(df), list(
        observed_agreement = c(vals[["both_positive"]] + vals[["both_negative"]], length(idx)),
        discordance = c(vals[["first_positive"]] + vals[["second_positive"]], length(idx))))
      for (k in seq_len(nrow(ci))) add("cutoffAgreementTable", c(list(
        pair = pair_name, population = scope, n = length(idx), units = cluster_n(idx),
        excluded = n - length(idx), both_positive = vals[["both_positive"]],
        first_positive = vals[["first_positive"]], second_positive = vals[["second_positive"]],
        both_negative = vals[["both_negative"]], metric = ci$metric[k]), ci_fields(ci, k)))
    }
    if (o$readerWorkflow == "ordinal_cutoff") {
      idx <- which(observed)
      add("readerOriginalAgreement", list(pair = pair_name, n = length(idx), weight = o$wght,
        kappa = .paReaderOrdinal(d$raw[[i]][idx], d$raw[[j]][idx], o$wght)))
    }
    disagree <- which(observed & d$calls[, i] != d$calls[, j])
    remaining <- max(0L, o$readerCaseLimit - length(output$cutoffDisagreementCases))
    for (row in utils::head(disagree, remaining)) {
      add("cutoffDisagreementCases", list(row = d$rows[row], case = d$ids[row], pair = pair_name,
        score1 = as.character(d$raw[[i]][row]), score2 = as.character(d$raw[[j]][row]),
        call1 = if (d$calls[row, i]) "Positive" else "Negative",
        call2 = if (d$calls[row, j]) "Positive" else "Negative",
        near = if (!d$has_band) "Not requested" else as.character(d$near[row, i] || d$near[row, j])))
    }
  }
  if (o$showKappaBalanced) {
    if (o$readerPairs != "selected" || !d$reference_ready ||
        !o$compositeReader %in% d$vars[d$pairs[[1]]]) {
      add("kappaBalancedTable", list(status = "Select one reader pair, its accuracy reader, and reference labels."))
    } else {
      pair <- d$pairs[[1]]
      idx <- which(stats::complete.cases(d$calls[, pair, drop = FALSE]) & !is.na(d$reference))
      r <- match(o$compositeReader, d$vars)
      df <- data.frame(x = d$calls[idx, pair[1]], y = d$calls[idx, pair[2]],
                       call = d$calls[idx, r], gold = d$reference[idx])
      acc <- .paReaderAccuracy(df$call, df$gold)
      kap <- .paReaderAgreement(df$x, df$y)[["kappa"]]
      stat <- function(z) c(kappa_balanced = .paReaderComposite(
        .paReaderAccuracy(z$call, z$gold)[["balanced_accuracy"]],
        .paReaderAgreement(z$x, z$y)[["kappa"]]))
      ci <- intervals(df, stat, idx, stat(df))
      add("kappaBalancedTable", c(list(reader = o$compositeReader,
        pair = paste(d$vars[pair], collapse = " vs "), n = length(idx),
        sensitivity = acc[["sensitivity"]], specificity = acc[["specificity"]],
        ba = acc[["balanced_accuracy"]], kappa = kap), ci_fields(ci, 1)))
    }
  }
  output <- lapply(output, function(rows) {
    if (!length(rows)) return(data.frame())
    do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  })
  if (o$readerNondiagnostic || o$readerFormat == "long") {
    status_report <- .paReaderStatusReport(d, checkpoint)
    output[names(status_report)] <- status_report
  }
  modality <- .paReaderModalityReport(d, checkpoint)
  output[names(modality)] <- modality
  transitions <- .paReaderTransitions(d)
  output[names(transitions)] <- transitions
  output
}
