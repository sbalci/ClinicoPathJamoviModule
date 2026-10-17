# jamovi parses table cells as HTML, so a label such as "<LOD" would render blank.
# Display sinks only; never apply to a value used for matching.
.diagCellText <- function(x) gsub("<(?=[A-Za-z/!?])", "< ", x, perl = TRUE)

# Half-up rounding to one decimal, as the tables display (sprintf rounds ties to even).
.diagHalfUp1 <- function(x) sign(x) * floor(abs(x) * 10 + 0.5) / 10

# Every user-visible string of these helpers. File-level code cannot call .(), so an
# analysis that wants translated output passes utils::modifyList(.diagText, <translated>)
# as `text`; callers that pass nothing get this English.
.diagText <- list(
  ref_interpretation = "Reference interpretation", ref_description = "Comparator description",
  ref_target = "Target definition", ref_rules = "Positive and negative rules",
  ref_population = "Analysis population", ref_unit = "Sampling unit",
  ref_unit_value = "Independent cases; repeated reads require a reader/cluster analysis",
  not_specified = "Not specified",
  mode_legacy = "Not specified", mode_disease = "Disease reference",
  mode_consensus = "Expert consensus", mode_method = "Comparative method",
  note_legacy = "Reference interpretation is not specified. Existing diagnostic wording is retained for compatibility; it requires a suitable disease reference. Choose consensus or comparative method when measuring agreement.",
  note_comparator = "Agreement with this comparator does not establish disease status. PPA/NPA condition on the comparator; symmetric positive/negative specific agreement uses different denominators. Population-prevalence adjustment and disease-risk outputs are not used.",
  note_disease = "Diagnostic interpretation assumes the selected reference establishes the target disease status.",
  metric_ppa = "Positive percent agreement (PPA)", metric_npa = "Negative percent agreement (NPA)",
  metric_opa = "Overall percent agreement (OPA)",
  metric_rpos = "Reference positive given positive call",
  metric_rneg = "Reference negative given negative call",
  metric_psa = "Positive specific agreement (symmetric)",
  metric_nsa = "Negative specific agreement (symmetric)",
  method_not_requested = "Not requested", method_no_denominator = "No eligible denominator",
  method_exact = "Exact binomial (independent cases)",
  method_symmetric = "Symmetric ratio; binomial CI not applicable",
  call_positive = "Positive", call_negative = "Negative",
  note_denominators = "PPA/NPA condition on reference-positive/reference-negative cases. Reverse conditional agreement is not a patient's probability of disease. Symmetric specific agreement is not PPA/NPA. All binary estimates use determinate reference/test pairs under the displayed recoding rule.",
  bin_missing_positive = "Select an explicit positive level. Both positive and negative levels are required; no level is inferred.",
  bin_missing_negative = "Select an explicit negative level. Both positive and negative levels are required; no level is inferred.",
  bin_unavailable_positive = "The positive level \"%1$s\" is not available. Available levels: %2$s.",
  bin_unavailable_negative = "The negative level \"%1$s\" is not available. Available levels: %2$s.",
  bin_identical = "Choose different positive and negative levels.",
  bin_other = "Observed levels outside the selected positive/negative pair: %s. Select 'Exclude other observed levels' to omit these results, or explicitly recode the variable before analysis. Other levels are never treated as negative.",
  report_rate = "%1$.1f%% (%2$d/%3$d)", report_unavailable = "unavailable (denominator 0)",
  report_test = "%1$s: PPA %2$s, NPA %3$s; %4$d analyzed cases, %5$d excluded.",
  report_close = "These quantities describe agreement with the selected comparator. The tables state the denominators and interval methods. Differences between tests require paired comparisons on common cases.",
  plot_test = "Test", plot_row = "Selected row", plot_call = "Call / reference",
  plot_agreement = "Agreement", plot_gaps = "Not estimable (denominator 0), not drawn: %s",
  class_pos_match = "Positive match", class_pos_call = "Positive call / negative reference",
  class_neg_call = "Negative call / positive reference", class_neg_match = "Negative match",
  class_excluded = "Excluded")

# Comparator semantics are independent of the binary count and filtering rules.
.diagReferenceMode <- function(type = "legacy") {
  type <- match.arg(type, c("legacy", "disease", "consensus", "method"))
  list(type = type, comparator = type %in% c("consensus", "method"), legacy = type == "legacy",
       name = switch(type, legacy = "Not specified", disease = "Disease reference",
                     consensus = "Expert consensus", method = "Comparative method"))
}

.diagReferenceDefinition <- function(results, options, rule, population, text = .diagText) {
  mode <- .diagReferenceMode(options$referenceType)
  # rowKey stays the English label so keys do not change with the interface language.
  rows <- list(
    list(key = "Reference interpretation", label = text$ref_interpretation,
         value = text[[paste0("mode_", mode$type)]]),
    list(key = "Comparator description", label = text$ref_description,
         value = options$referenceDescription),
    list(key = "Target definition", label = text$ref_target, value = options$targetDefinition),
    list(key = "Positive and negative rules", label = text$ref_rules, value = rule),
    list(key = "Analysis population", label = text$ref_population, value = population),
    list(key = "Sampling unit", label = text$ref_unit, value = text$ref_unit_value))
  table <- results$referenceDefinition
  .deleteTableRows(table)
  for (row in rows) {
    value <- row$value
    table$addRow(rowKey = row$key, values = list(setting = row$label,
      value = if (is.null(value) || !nzchar(trimws(value))) text$not_specified
              else .diagCellText(value)))
  }
  table$setNote("interpretation", if (mode$legacy) text$note_legacy
    else if (mode$comparator) text$note_comparator else text$note_disease)
  invisible(mode)
}

.diagComparatorMetrics <- function(call, reference, ci = TRUE, text = .diagText) {
  stopifnot(length(call) == length(reference), is.logical(call), is.logical(reference))
  ok <- !is.na(call) & !is.na(reference)
  x <- call[ok]; g <- reference[ok]
  a <- sum(x & g); b <- sum(x & !g); c <- sum(!x & g); d <- sum(!x & !g)
  numerator <- c(a, d, a + d, a, d, 2 * a, 2 * d)
  denominator <- c(a + c, b + d, a + b + c + d, a + b, c + d,
                   2 * a + b + c, 2 * d + b + c)
  out <- data.frame(
    metric = c(text$metric_ppa, text$metric_npa, text$metric_opa, text$metric_rpos,
      text$metric_rneg, text$metric_psa, text$metric_nsa),
    numerator = numerator, denominator = denominator,
    estimate = ifelse(denominator > 0, numerator / denominator, NA_real_),
    lower = NA_real_, upper = NA_real_, method = text$method_not_requested)
  for (i in seq_len(nrow(out))) {
    if (!denominator[i]) out$method[i] <- text$method_no_denominator
    else if (ci && i <= 5L) {
      limits <- stats::binom.test(numerator[i], denominator[i])$conf.int
      out$lower[i] <- limits[1]; out$upper[i] <- limits[2]
      out$method[i] <- text$method_exact
    } else if (ci) out$method[i] <- text$method_symmetric
  }
  list(metrics = out, counts = c(a = a, b = b, c = c, d = d),
       n = sum(ok), excluded = sum(!ok))
}

# decisioncompare requires two explicit levels in every reference mode. Other
# observed levels are excluded only by request; they can never become negatives.
.diagComparatorBinary <- function(x, positive, negative = NULL, exclude = FALSE, text = .diagText) {
  raw <- as.character(x)
  available <- if (is.factor(x)) levels(x) else unique(raw[!is.na(raw)])
  available <- available[!is.na(available)]
  for (direction in c("positive", "negative")) {
    level <- if (direction == "positive") positive else negative
    if (length(level) != 1L || is.na(level) || !nzchar(level))
      stop(text[[paste0("bin_missing_", direction)]], call. = FALSE)
    if (!level %in% available)
      stop(sprintf(text[[paste0("bin_unavailable_", direction)]], level,
        paste(available, collapse = ", ")), call. = FALSE)
  }
  if (positive == negative)
    stop(text$bin_identical, call. = FALSE)
  other <- !is.na(raw) & !raw %in% c(positive, negative)
  if (any(other) && !isTRUE(exclude)) {
    counts <- table(raw[other])
    stop(sprintf(text$bin_other,
      paste0(names(counts), " (n=", as.integer(counts), ")", collapse = ", ")), call. = FALSE)
  }
  out <- raw == positive
  out[is.na(raw) | other] <- NA
  out
}

.diagComparatorClassifications <- function(call, reference, text = .diagText) {
  out <- rep(NA_character_, length(call))
  ok <- !is.na(call) & !is.na(reference)
  out[ok] <- ifelse(call[ok],
    ifelse(reference[ok], text$class_pos_match, text$class_pos_call),
    ifelse(reference[ok], text$class_neg_call, text$class_neg_match))
  out
}

.diagFillComparator <- function(results, comparisons, ci = TRUE, text = .diagText) {
  .deleteTableRows(results$comparatorMetrics)
  .deleteTableRows(results$comparatorCounts)
  .deleteTableRows(results$comparatorPopulation)
  summaries <- lapply(comparisons, function(z) .diagComparatorMetrics(z$call, z$reference, ci, text))
  for (test in names(summaries)) {
    s <- summaries[[test]]
    for (i in seq_len(nrow(s$metrics))) {
      values <- c(list(test = .diagCellText(test)), as.list(s$metrics[i, ]))
      values[vapply(values, function(x) length(x) == 1L && is.na(x), logical(1))] <- list(NULL)
      results$comparatorMetrics$addRow(rowKey = paste(match(test, names(summaries)), i), values = values)
    }
    results$comparatorCounts$addRow(rowKey = paste(match(test, names(summaries)), "positive"), values = list(test = .diagCellText(test), call = text$call_positive,
      refPositive = s$counts[["a"]], refNegative = s$counts[["b"]]))
    results$comparatorCounts$addRow(rowKey = paste(match(test, names(summaries)), "negative"), values = list(test = .diagCellText(test), call = text$call_negative,
      refPositive = s$counts[["c"]], refNegative = s$counts[["d"]]))
    results$comparatorPopulation$addRow(rowKey = match(test, names(summaries)), values = list(test = .diagCellText(test),
      selected = length(comparisons[[test]]$call), analyzed = s$n, excluded = s$excluded))
  }
  results$comparatorMetrics$setNote("denominators", text$note_denominators)
  summaries
}

.diagComparatorReport <- function(summaries, text = .diagText) {
  rate <- function(m, i) if (m$denominator[i] == 0) text$report_unavailable else
    sprintf(text$report_rate, .diagHalfUp1(100 * m$estimate[i]), m$numerator[i], m$denominator[i])
  pieces <- vapply(names(summaries), function(nm) {
    s <- summaries[[nm]]; m <- s$metrics
    sprintf(text$report_test, as.character(jmvcore::htmlEscape(nm)), rate(m, 1), rate(m, 2),
      as.integer(s$n), as.integer(s$excluded))
  }, character(1))
  paste0("<p>", paste(pieces, collapse = "</p><p>"), "</p><p>", text$report_close, "</p>")
}

.diagComparatorPlot <- function(image, ggtheme, kind = "bar", text = .diagText) {
  state <- image$state
  if (is.null(state) || !nrow(state$data)) return(FALSE)
  print(.diagComparatorPlotObject(state$data, kind, text) + ggtheme)
  TRUE
}

# The plot's aes() columns use the .data pronoun; the import travels with this file.
#' @importFrom rlang .data
NULL

# Builds the comparator plot from its saved state (kept separate so tests can inspect it).
.diagComparatorPlotObject <- function(d, kind = "bar", text = .diagText) {
  if (kind == "heatmap") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$test, y = .data$row, fill = .data$result)) +
      ggplot2::geom_tile() +
      ggplot2::labs(x = text$plot_test, y = text$plot_row, fill = text$plot_call)
  } else {
    metrics <- c("PPA", "NPA", "OPA")
    d$metric <- factor(d$metric, levels = metrics)
    gaps <- d[is.na(d$estimate), , drop = FALSE]
    if (kind == "radar") {
      # A filled polygon on a discrete polar axis closes back through the middle spoke,
      # so draw a closed path on a continuous angle: the first metric is repeated at x = 4.
      d$x <- as.integer(d$metric)
      d <- d[order(d$test, d$x), ]
      wrap <- d[d$x == 1L, ]
      wrap$x <- 4L
      ring <- rbind(d, wrap)
      p <- ggplot2::ggplot(ring, ggplot2::aes(x = .data$x, y = .data$estimate, group = .data$test,
                                              colour = .data$test)) +
        ggplot2::geom_path(na.rm = TRUE) +
        ggplot2::geom_point(data = d, na.rm = TRUE) +
        ggplot2::scale_x_continuous(limits = c(1, 4), breaks = 1:3, labels = metrics) +
        ggplot2::coord_polar() +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
        ggplot2::labs(x = NULL, y = text$plot_agreement, colour = text$plot_test)
    } else {
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$metric, y = .data$estimate, fill = .data$test)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single"), na.rm = TRUE) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
        ggplot2::labs(x = NULL, y = text$plot_agreement, fill = text$plot_test)
    }
    # A metric with a zero denominator is not estimable; say so rather than draw nothing.
    if (nrow(gaps))
      p <- p + ggplot2::labs(caption = sprintf(text$plot_gaps,
        paste(gaps$test, gaps$metric, collapse = "; ")))
  }
  p
}

.diagComparatorPlotState <- function(comparisons, summaries, text = .diagText) {
  bars <- do.call(rbind, lapply(names(summaries), function(nm) {
    data.frame(test = nm, metric = c("PPA", "NPA", "OPA"),
               estimate = summaries[[nm]]$metrics$estimate[1:3])
  }))
  heat <- do.call(rbind, lapply(names(comparisons), function(nm) {
    z <- comparisons[[nm]]
    idx <- seq_len(min(length(z$call), 1000L))
    calls <- .diagComparatorClassifications(z$call[idx], z$reference[idx], text)
    calls[is.na(calls)] <- text$class_excluded
    data.frame(test = nm, row = idx, result = calls)
  }))
  list(bar = list(data = bars), heatmap = list(data = heat))
}
