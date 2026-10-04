.multistateHistory <- function(data, entry, exit, from, to, id, covariates = NULL,
                               structure = "standard") {
  if (is.null(id)) stop("A subject identifier is required to preserve multistate histories.")
  vars <- unique(c(entry, exit, from, to, id, covariates))
  d <- data[, vars, drop = FALSE]
  if (anyNA(d)) stop("Missing trajectory values must be resolved; individual history rows cannot be dropped.")
  if (any(!as.character(d[[from]]) %in% as.character(0:2)) ||
      any(!as.character(d[[to]]) %in% as.character(0:2)))
    stop("States must use the exact codes 0, 1, and 2.")
  out <- data.frame(id = as.character(d[[id]]), start = d[[entry]], stop = d[[exit]],
    from = suppressWarnings(as.integer(as.character(d[[from]]))),
    to = suppressWarnings(as.integer(as.character(d[[to]]))))
  if (!is.numeric(out$start) || !is.numeric(out$stop) ||
      any(!is.finite(out$start) | !is.finite(out$stop) | out$start < 0 | out$stop <= out$start))
    stop("Every history interval requires finite 0 <= entry < exit times.")
  if (anyNA(out) || any(!out$from %in% 0:2 | !out$to %in% 0:2))
    stop("States must be coded 0 (event-free), 1 (ill), and 2 (dead).")
  edges <- switch(structure,
    standard = rbind(c(0, 1), c(0, 2), c(1, 2)),
    illness_death = rbind(c(0, 1), c(0, 2), c(1, 2)),
    progressive = rbind(c(0, 1), c(1, 2)),
    competing = rbind(c(0, 1), c(0, 2)),
    competing_risks = rbind(c(0, 1), c(0, 2)),
    reversible = rbind(c(0, 1), c(0, 2), c(1, 0), c(1, 2)),
    stop("Custom transition structures are not implemented."))
  observed <- paste(out$from, out$to)
  allowed <- paste(edges[, 1], edges[, 2])
  if (any(out$from != out$to & !observed %in% allowed) || any(out$from == 2))
    stop("An observed transition is incompatible with the selected structure or leaves death.")
  if (length(covariates)) {
    x <- d[, covariates, drop = FALSE]
    names(x) <- paste0("x", seq_along(covariates))
    out <- cbind(out, x)
  }
  out <- out[order(out$id, out$start, out$stop), , drop = FALSE]
  for (g in split(out, out$id)) {
    if (nrow(g) > 1 && (any(tail(g$start, -1) != head(g$stop, -1)) ||
                       any(tail(g$from, -1) != head(g$to, -1))))
      stop("Each subject's intervals must be contiguous, non-overlapping, and state-consistent.")
  }
  tmat <- matrix(NA_integer_, 3, 3, dimnames = list(c("Event-free", "Ill", "Dead"),
                                                  c("Event-free", "Ill", "Dead")))
  for (i in seq_len(nrow(edges))) tmat[edges[i, 1] + 1L, edges[i, 2] + 1L] <- i
  risk <- lapply(seq_len(nrow(edges)), function(i) {
    r <- out[out$from == edges[i, 1], , drop = FALSE]
    r$event <- as.integer(r$to == edges[i, 2] & r$to != r$from)
    r$transition <- rep(i, nrow(r))
    r
  })
  list(data = out, risk = risk, edges = edges, tmat = tmat,
       covariates = if (length(covariates)) paste0("x", seq_along(covariates)) else character(),
       labels = covariates)
}

.multistateAJ <- function(history, times, conf_level = .95) {
  d <- history$data
  times <- sort(unique(times[is.finite(times) & times >= min(d$start) & times <= max(d$stop)]))
  if (!any(d$from != d$to)) stop("No transitions were observed; multistate probabilities are not estimated.")
  d$destination <- factor(ifelse(d$from == d$to, "censor", as.character(d$to)),
                          levels = c("censor", "0", "1", "2"))
  d$initial <- factor(as.character(d$from), levels = c("0", "1", "2"))
  fit <- survival::survfit(survival::Surv(start, stop, destination) ~ 1, data = d,
                           id = id, istate = initial, conf.int = conf_level)
  if (!length(times)) {
    empty <- matrix(numeric(), nrow = 0, ncol = 3)
    return(list(fit = fit, time = numeric(), probability = empty, lower = empty, upper = empty))
  }
  s <- summary(fit, times = times, extend = TRUE)
  indices <- match(c("0", "1", "2"), s$states)
  p <- s$pstate[, indices, drop = FALSE]
  lo <- s$lower[, indices, drop = FALSE]
  hi <- s$upper[, indices, drop = FALSE]
  lo[p <= 0 | p >= 1] <- hi[p <= 0 | p >= 1] <- NA_real_
  list(fit = fit, time = times, probability = p, lower = lo, upper = hi)
}

.multistateRestricted <- function(aj, horizon) {
  if (!is.finite(horizon) || horizon <= 0 || horizon > max(aj$fit$time))
    stop("Restricted time-in-state requires a positive horizon within observed follow-up.")
  times <- sort(unique(c(0, aj$fit$time[aj$fit$time < horizon], horizon)))
  s <- summary(aj$fit, times = head(times, -1), extend = TRUE)
  drop(crossprod(diff(times), s$pstate[, match(c("0", "1", "2"), s$states), drop = FALSE]))
}

.multistateRestrictedBootstrap <- function(history, horizon, B, seed,
                                           checkpoint = function() NULL) {
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  subjects <- split(history$data, history$data$id)
  draws <- matrix(NA_real_, B, 3)
  for (b in seq_len(B)) {
    checkpoint()
    chosen <- sample.int(length(subjects), replace = TRUE)
    d <- do.call(rbind, lapply(seq_along(chosen), function(j) {
      x <- subjects[[chosen[j]]]
      x$id <- as.character(j)
      x
    }))
    draws[b, ] <- tryCatch({
      aj <- .multistateAJ(list(data = d), horizon)
      .multistateRestricted(aj, horizon)
    }, error = function(e) rep(NA_real_, 3))
  }
  draws
}
