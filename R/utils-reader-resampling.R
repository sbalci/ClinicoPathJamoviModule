# Joint resampling for fixed readers. Estimands retain their own masks within
# each resampled whole case/cluster. Invalid draws are reported, never removed.
.readerResample <- function(data, statistic, clusters = NULL, samples = 1000L,
                            confidence = 0.95, seed = 42L, checkpoint = NULL) {
  stopifnot(is.data.frame(data), is.function(statistic), length(samples) == 1L,
            is.finite(samples), samples >= 20, samples == as.integer(samples),
            length(confidence) == 1L, is.finite(confidence), confidence > 0, confidence < 1)
  point <- statistic(data)
  if (is.null(names(point)) || anyDuplicated(names(point))) stop("Statistics need unique names.")
  n <- nrow(data)
  if (!is.null(clusters)) {
    if (length(clusters) != n || anyNA(clusters) || any(!nzchar(as.character(clusters)))) {
      stop("Cluster IDs must be present for every resampled row.")
    }
  }
  groups <- if (is.null(clusters)) as.list(seq_len(n)) else split(seq_len(n), clusters, drop = TRUE)
  units <- length(groups)
  out <- data.frame(metric = names(point), estimate = unname(point),
                    lower = NA_real_, upper = NA_real_, attempted = 0L, valid = 0L,
                    undefined = 0L, boundary = 0L, constant = FALSE,
                    status = "Fewer than two sampling units", stringsAsFactors = FALSE)
  if (units < 2L) return(out)
  withr::local_seed(seed)
  draws <- matrix(NA_real_, samples, length(point))
  for (b in seq_len(samples)) {
    if (!is.null(checkpoint) && b %% 25L == 0L) checkpoint()
    idx <- unlist(groups[sample.int(units, units, replace = TRUE)], use.names = FALSE)
    value <- statistic(data[idx, , drop = FALSE])
    if (!identical(names(value), names(point))) stop("Resampled statistics changed names.")
    draws[b, ] <- value
  }
  alpha <- (1 - confidence) / 2
  for (j in seq_along(point)) {
    x <- draws[, j]
    ok <- is.finite(x)
    out$attempted[j] <- samples
    out$valid[j] <- sum(ok)
    out$undefined[j] <- sum(!ok)
    out$boundary[j] <- sum(x[ok] %in% c(-1, 0, 1))
    out$constant[j] <- all(ok) && diff(range(x)) < .Machine$double.eps^0.5
    out$status[j] <- if (!is.finite(point[j])) "Point estimate is undefined" else
      if (any(!ok)) "Undefined bootstrap draws; CI unavailable" else
      if (out$constant[j]) "Constant bootstrap distribution; CI unavailable" else "Percentile bootstrap"
    if (identical(out$status[j], "Percentile bootstrap")) {
      out[j, c("lower", "upper")] <- stats::quantile(x, c(alpha, 1 - alpha), names = FALSE)
    }
  }
  out
}
