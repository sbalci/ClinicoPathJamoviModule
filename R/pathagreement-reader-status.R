# Observed-result estimands never alter the ordinary determinate 2 x 2 counts.
.paReaderObserved <- function(call, reference, status) {
  eligible <- !is.na(reference) & status %in% c("determinate", "nondiagnostic")
  positive <- eligible & !is.na(reference) & reference
  negative <- eligible & !is.na(reference) & !reference
  match_positive <- !is.na(call) & call & positive
  match_negative <- !is.na(call) & !call & negative
  numerator <- c(sum(match_positive), sum(match_negative),
                 sum(match_positive | match_negative), sum(eligible & status == "determinate"))
  denominator <- c(sum(positive), sum(negative), sum(eligible), sum(eligible))
  data.frame(metric = c("Positive-call agreement", "Negative-call agreement",
                        "Definitive agreement", "Determinate-result yield"),
    numerator = numerator, denominator = denominator,
    estimate = ifelse(denominator > 0, numerator / denominator, NA_real_))
}

.paReaderStatusReport <- function(d, checkpoint = NULL) {
  availability <- mapping <- observed <- list()
  for (j in seq_along(d$vars)) {
    state <- d$status[, j]
    reference_group <- ifelse(d$reference_state != "eligible", d$reference_state,
                               ifelse(d$reference, "positive", "negative"))
    for (g in c("positive", "negative", "missing", "excluded_level")) {
      for (s in c("determinate", "nondiagnostic", "missing", "unmapped")) {
        availability[[length(availability) + 1L]] <- data.frame(reader = d$vars[j],
          reference = g, result = s, n = sum(reference_group == g & state == s))
      }
    }
    labels <- as.character(d$original_raw[[j]])
    for (value in unique(labels)) {
      idx <- if (is.na(value)) is.na(labels) else !is.na(labels) & labels == value
      mapping[[length(mapping) + 1L]] <- data.frame(reader = d$vars[j],
        value = if (is.na(value)) "(missing)" else value, status = unique(state[idx])[1],
        call = if (all(is.na(d$calls[idx, j]))) "Unavailable" else
          if (all(d$calls[idx, j], na.rm = TRUE)) "Positive" else "Negative", n = sum(idx))
    }
    if (!d$options$readerAllObservedAgreement || !d$reference_ready) next
    tab <- .paReaderObserved(d$calls[, j], d$reference, state)
    tab$reader <- d$vars[j]
    tab$population <- "Observed readings with eligible reference"
    tab$lower <- tab$upper <- NA_real_
    tab$status <- "Intervals not requested"
    tab$attempted <- tab$undefined <- 0L
    if (d$options$readerIntervals) {
      if (is.null(d$clusters)) {
        ci <- t(vapply(seq_len(nrow(tab)), function(k)
          .paReaderExactCI(tab$numerator[k], tab$denominator[k], d$options$readerConfidence), numeric(2)))
        tab$lower <- ci[, 1]; tab$upper <- ci[, 2]
        tab$status <- ifelse(tab$denominator > 0, "Exact binomial; independent cases", "No eligible readings")
      } else if (d$options$readerBootstrap) {
        df <- data.frame(call = d$calls[, j], reference = d$reference, status = state)
        stat <- function(z) stats::setNames(.paReaderObserved(z$call, z$reference, z$status)$estimate,
                                            c("positive", "negative", "definitive", "yield"))
        ci <- .readerResample(df, stat, d$clusters, d$options$bootstrapSamples,
          d$options$readerConfidence, d$options$seed, checkpoint)
        tab$lower <- ci$lower; tab$upper <- ci$upper; tab$status <- ci$status
        tab$attempted <- ci$attempted; tab$undefined <- ci$undefined
      } else tab$status <- "Cluster intervals require bootstrap"
    }
    observed[[length(observed) + 1L]] <- tab
  }
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  ref_excluded <- which(d$reference_state != "eligible")
  reference_exclusions <- lapply(unique(d$reference_raw[ref_excluded]), function(value) {
    idx <- if (is.na(value)) is.na(d$reference_raw[ref_excluded]) else
      !is.na(d$reference_raw[ref_excluded]) & d$reference_raw[ref_excluded] == value
    data.frame(value = if (is.na(value)) "(missing)" else value,
      status = if (is.na(value)) "missing" else "excluded_level", n = sum(idx))
  })
  list(readerAvailability = bind(availability), readerMapping = bind(mapping),
       readerObservedAgreement = bind(observed),
       readerReferenceExclusions = bind(reference_exclusions))
}
