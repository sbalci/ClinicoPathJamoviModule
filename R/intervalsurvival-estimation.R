.intervalSurvivalData <- function(left, right, status = NULL, covariates = NULL) {
  if (!is.numeric(left) || !is.numeric(right)) stop("Interval bounds must be numeric.")
  if (!is.null(status)) {
    code <- as.character(status)
    if (any(!is.na(code) & !code %in% as.character(0:3)))
      stop("Censoring status must be 0 (right), 1 (interval), 2 (left), or 3 (exact).")
    right[which(code == "0")] <- Inf
    left[which(code == "2")] <- 0
    exact <- which(code == "3")
    left[exact] <- ifelse(is.na(left[exact]), right[exact], left[exact])
    right[exact] <- left[exact]
  }
  complete <- !is.na(left) & !is.na(right)
  if (!is.null(status)) complete <- complete & !is.na(status)
  if (!is.null(covariates)) complete <- complete & stats::complete.cases(covariates)
  d <- data.frame(left = left[complete], right = right[complete])
  if (any(!is.finite(d$left) | d$left < 0 | d$right < d$left | d$right <= 0))
    stop("Require finite non-negative lower bounds and positive upper bounds at least as large.")
  if (any(d$left == 0 & is.infinite(d$right)))
    stop("An interval (0, infinity) contains no event-time information.")
  if (!is.null(covariates) && ncol(covariates)) {
    x <- covariates[complete, , drop = FALSE]
    names(x) <- paste0("x", seq_len(ncol(x)))
    d <- cbind(d, x)
  }
  list(data = d, excluded = sum(!complete))
}

.intervalSurvivalFit <- function(d, model, max_iterations = 500L) {
  vars <- setdiff(names(d), c("left", "right"))
  f <- stats::reformulate(if (length(vars)) vars else "1",
    response = "Surv(left, right, type = 'interval2')", env = asNamespace("survival"))
  if (model == "nonparametric") {
    f <- stats::as.formula("Surv(left, right, type = 'interval2') ~ 0",
                           env = asNamespace("survival"))
    return(icenReg::ic_np(f, data = d))
  }
  if (model == "cox_ph") return(icenReg::ic_sp(f, data = d, model = "ph",
    bs_samples = 0, controls = icenReg::makeCtrls_icsp(maxIter = max_iterations)))
  dist <- switch(model, aft_weibull = "weibull", aft_lognormal = "lnorm",
    aft_loglogistic = "loglogistic", aft_exponential = "exponential", aft_gamma = "gamma",
    stop("Unsupported interval-censored model."))
  icenReg::ic_par(f, data = d, model = "aft", dist = dist)
}

.intervalSurvivalPredict <- function(fit, d, times, nonparametric = FALSE) {
  vapply(times, function(t) {
    newdata <- if (nonparametric || !length(setdiff(names(d), c("left", "right")))) NULL else d
    p <- 1 - icenReg::getFitEsts(fit, newdata = newdata, q = t)
    if (!length(p) || any(!is.finite(p) | p < 0 | p > 1))
      stop("Invalid fitted survival probability.")
    mean(p)
  }, numeric(1))
}

.intervalSurvivalHtml <- function(d) {
  escape <- function(x) htmltools::htmlEscape(as.character(x))
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(vapply(seq_len(nrow(d)), function(i) paste0("<tr>",
      paste0("<td>", escape(unlist(d[i, ], use.names = FALSE)), "</td>", collapse = ""),
      "</tr>"), character(1)), collapse = ""), "</tbody></table>")
}
