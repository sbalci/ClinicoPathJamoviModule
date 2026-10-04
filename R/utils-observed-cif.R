# Observed-time state probabilities, with a common risk set for every cause.
.observedCifReport <- function(time, status, cutpoints, group = NULL) {
  if (is.null(group)) group <- rep("Overall", length(time))
  if (length(time) != length(status) || length(time) != length(group) ||
      anyNA(time) || anyNA(status) || anyNA(group) ||
      any(!is.finite(time) | time < 0) || any(!status %in% 0:2)) {
    stop("Cumulative incidence requires complete finite times and event codes 0, 1, 2.")
  }
  cutpoints <- sort(unique(cutpoints[is.finite(cutpoints) & cutpoints >= 0]))
  rows <- list()
  omitted <- character()
  for (g in unique(as.character(group))) {
    keep <- as.character(group) == g
    d <- data.frame(time = time[keep], status = status[keep])
    times <- cutpoints[cutpoints <= max(d$time)]
    if (any(cutpoints > max(d$time))) omitted <- c(omitted, g)
    if (!length(times)) next
    d$state <- factor(d$status, levels = 0:2,
                      labels = c("censored", "target", "competing"))
    if (any(d$status != 0)) {
      fit <- survival::survfit(survival::Surv(time, state) ~ 1, data = d)
      s <- summary(fit, times = times, extend = TRUE)
      estimate <- s$pstate[, match(c("(s0)", "target", "competing"), s$states),
                           drop = FALSE]
      lower <- s$lower[, match(c("(s0)", "target", "competing"), s$states),
                       drop = FALSE]
      upper <- s$upper[, match(c("(s0)", "target", "competing"), s$states),
                       drop = FALSE]
    } else {
      estimate <- cbind(rep(1, length(times)), 0, 0)
      lower <- upper <- matrix(NA_real_, nrow = length(times), ncol = 3)
    }
    boundary <- estimate <= 0 | estimate >= 1
    lower[boundary] <- upper[boundary] <- NA_real_
    for (j in seq_along(times)) {
      for (k in 1:3) {
        rows[[length(rows) + 1L]] <- data.frame(
          group = g, time = times[j],
          cause = c("Event-free", "Event of interest", "Competing event(s)")[k],
          estimate = estimate[j, k], lower = lower[j, k], upper = upper[j, k],
          n_risk = sum(d$time >= times[j]),
          n_events = sum(d$time <= times[j] &
            if (k == 1) d$status != 0 else d$status == k - 1L),
          n_censored = sum(d$time <= times[j] & d$status == 0),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  list(table = if (length(rows)) do.call(rbind, rows) else data.frame(),
       omitted = omitted)
}

.populateObservedCif <- function(table, time, status, cutpoints, group = NULL) {
  result <- .observedCifReport(time, status, cutpoints, group)
  for (i in seq_len(nrow(result$table))) {
    table$addRow(rowKey = i, values = as.list(result$table[i, ]))
  }
  table$setNote("estimand", paste(
    "Aalen–Johansen estimates within each group's observed follow-up.",
    "Event-free probability and both cumulative incidences sum to one.",
    "Competing events are pooled according to the selected outcome mapping.",
    "Risk counts are just before the stated time; event and censoring counts are cumulative",
    "through that time. Event-free rows count all terminal events.",
    "Boundary confidence intervals are unavailable. A late plateau does not establish cure."
  ))
  table$setNote("support", if (length(result$omitted)) paste0(
    "Time points beyond observed follow-up were omitted for: ",
    paste(result$omitted, collapse = ", "), "."
  ) else NULL)
  invisible(result)
}
