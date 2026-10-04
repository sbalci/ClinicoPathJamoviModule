# Engine adapters keep latent susceptibility separate from the survival asymptote.
.cureStatus <- function(x, event = "1", censor = "0", competing = "") {
  codes <- c(censor, event, if (nzchar(competing)) competing)
  if (any(!nzchar(codes)) || anyDuplicated(codes)) {
    stop("Event, censoring, and competing-event values must be distinct.")
  }
  decoded <- match(as.character(x), codes) - 1L
  if (anyNA(decoded)) stop("The status variable contains an unmapped event or censoring value.")
  decoded
}

.cureSeed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(code)
}

.cureDesign <- function(data, time, status, incidence, latency = incidence,
                        background = NULL, np_covariate = NULL,
                        profile = "standardized") {
  encode <- function(vars, prefix) {
    if (!length(vars)) return(matrix(numeric(), nrow(data), 0))
    # Safe internal names avoid formula parsing of user-supplied column names.
    d <- data[, vars, drop = FALSE]
    names(d) <- paste0("v", seq_along(vars))
    m <- stats::model.matrix(~ ., d)[, -1, drop = FALSE]
    labels <- colnames(m)
    for (i in rev(seq_along(vars))) {
      labels <- sub(paste0("^v", i, "(?=[^0-9]|$)"), vars[i], labels, perl = TRUE)
    }
    colnames(m) <- paste0(prefix, seq_len(ncol(m)))
    attr(m, "labels") <- labels
    m
  }
  z <- encode(incidence, "inc")
  x <- encode(latency, "lat")
  d <- data.frame(.time = data[[time]], .status = data[[status]], z, x,
                  check.names = FALSE)
  if (!is.null(background)) d$.background <- data[[background]]
  if (!is.null(np_covariate)) d$.np <- data[[np_covariate]]
  profiles <- d
  if (profile == "reference") {
    profiles <- d[1, , drop = FALSE]
    profiles[, c(colnames(z), colnames(x))] <- 0
    if (".np" %in% names(d)) profiles$.np <- stats::median(d$.np)
  }
  list(data = d, profiles = profiles, incidence = colnames(z), latency = colnames(x),
       labels = c(stats::setNames(attr(z, "labels"), colnames(z)),
                  stats::setNames(attr(x, "labels"), colnames(x))),
       profile = if (profile == "reference")
         "Reference: numeric predictors = 0; factors = reference levels; npcure covariate = median"
         else "Cohort standardized: mean prediction over the original complete-case cohort")
}

.cureFit <- function(d, spec) {
  if (!any(d$.status == 1)) stop("No events remain at this observation horizon.")
  if (any(d$.status == 2)) stop("Single-event cure models cannot discard competing events.")
  form <- function(vars, response = NULL) {
    stats::reformulate(if (length(vars)) vars else "1", response = response,
                      env = asNamespace("survival"))
  }
  inc_form <- form(spec$incidence, "Surv(.time, .status)")
  lat_form <- form(spec$latency)
  engine <- spec$engine
  if (engine == "mixture") {
    if (spec$link != "logit") stop("smcure currently supports verified logit incidence only.")
    if (!length(spec$latency) || !length(spec$incidence))
      stop("smcure requires incidence and latency predictors; select predictors or a parametric model.")
    fit <- NULL
    invisible(utils::capture.output(fit <- smcure::smcure(
      formula = form(spec$latency, "Surv(.time, .status)"),
      cureform = form(spec$incidence), data = d, model = spec$latency_type,
      link = "logit", Var = FALSE, emmax = 200, eps = 1e-7
    )))
    if (any(!is.finite(c(fit$b, fit$beta)))) stop("Non-finite cure model coefficients.")
  } else if (engine == "nonmixture") {
    if (spec$link == "cloglog") stop("flexsurvcure does not support a complementary log-log incidence link.")
    dist <- switch(spec$distribution, exponential = "exp", lognormal = "lnorm",
                   loglogistic = "llogis", "weibull")
    anc <- NULL
    if (isTRUE(spec$latency_explicit) && length(spec$latency)) {
      par <- switch(dist, exp = "rate", lnorm = "meanlog", "scale")
      anc <- stats::setNames(list(lat_form), par)
    }
    fit <- flexsurvcure::flexsurvcure(inc_form, data = d, dist = dist,
                                     link = if (spec$link == "logit") "logistic" else spec$link,
                                     mixture = FALSE, anc = anc)
    if (!is.null(fit$opt$convergence) && fit$opt$convergence != 0)
      stop("Parametric cure model optimization did not converge.")
    if (any(!is.finite(stats::coef(fit)))) stop("Non-finite cure model coefficients.")
  } else if (engine == "cure") {
    if (spec$link == "cloglog" || spec$distribution == "loglogistic")
      stop("cuRe does not support the selected link or distribution.")
    if (!".background" %in% names(d) || any(!is.finite(d$.background) | d$.background < 0))
      stop("Excess-mortality cure models require non-negative event-time background hazards.")
    # Default cuRe distributional parameters have intercepts only.
    # Its formula.surv interface requires a distribution-specific list.
    if (isTRUE(spec$latency_explicit) && length(spec$latency))
      stop("Separate latency predictors are currently supported in smcure and flexsurvcure.")
    fit <- cuRe::fit.cure.model(inc_form, data = d, bhazard = d$.background,
                                dist = spec$distribution, link = spec$link)
    if (!is.null(fit$optim$convergence) && fit$optim$convergence != 0)
      stop("The excess-mortality cure model did not converge.")
  } else if (engine == "npcure") {
    if (!".np" %in% names(d) || !is.numeric(d$.np) || length(unique(d$.np)) < 2)
      stop("npcure requires a varying numeric covariate.")
    fit <- d
  } else stop("Unknown cure model engine.")
  list(fit = fit, spec = spec)
}

.curePredict <- function(object, profiles) {
  spec <- object$spec
  p <- switch(spec$engine,
    mixture = {
      z <- cbind(1, as.matrix(profiles[, spec$incidence, drop = FALSE]))
      1 - stats::plogis(drop(z %*% object$fit$b))
    },
    nonmixture = vapply(summary(object$fit, newdata = profiles, t = 0,
      fn = function(t, theta, ...) theta, ci = FALSE), function(x) x$est[1], numeric(1)),
    cure = vapply(stats::predict(object$fit, newdata = profiles,
      type = "curerate", var.type = "n"), function(x) x$Estimate[1], numeric(1)),
    npcure = {
      d <- object$fit
      x0 <- sort(unique(profiles$.np))
      args <- list(x = d$.np, t = d$.time, d = d$.status, x0 = x0, conflevel = 0)
      if (spec$bandwidth != "auto") {
        h <- switch(spec$bandwidth, small = .1, medium = .3, large = .5)
        args$h <- rep(h, length(x0))
      } else {
        args$bootpars <- npcure::controlpars(B = 99L)
      }
      fit <- do.call(npcure::probcure, args)
      fit$q[match(profiles$.np, fit$x0)]
    }
  )
  # Some engines collapse identical intercept-only predictions to one row.
  if (spec$engine %in% c("nonmixture", "cure") && !length(spec$incidence) &&
      length(p) == 1L)
    p <- rep(p, nrow(profiles))
  if (length(p) != nrow(profiles) || any(!is.finite(p) | p < 0 | p > 1))
    stop("The engine did not produce valid cure probabilities for all prediction profiles.")
  p
}

.cureTruncate <- function(d, horizon) {
  if (length(horizon) != 1 || !is.finite(horizon) || horizon <= 0)
    stop("Analysis horizons must be finite and positive.")
  d$.status[d$.time > horizon] <- 0L
  d$.time <- pmin(d$.time, horizon)
  d
}

.cureEstimate <- function(d, spec, profiles, bootstrap = 0L, seed = 1L,
                          checkpoint = function() NULL) {
  .cureSeed(seed, {
    warnings <- character()
    fit_one <- function(x) withCallingHandlers(.cureFit(x, spec), warning = function(w) {
      warnings <<- unique(c(warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    })
    object <- fit_one(d)
    estimate <- mean(.curePredict(object, profiles))
    draws <- rep(NA_real_, bootstrap)
    for (b in seq_len(bootstrap)) {
      checkpoint()
      draws[b] <- tryCatch({
        boot_fit <- fit_one(d[sample.int(nrow(d), replace = TRUE), , drop = FALSE])
        mean(.curePredict(boot_fit, profiles))
      }, error = function(e) NA_real_)
    }
    success <- sum(is.finite(draws))
    enough <- bootstrap > 0 && success >= max(20, ceiling(.8 * bootstrap))
    ci <- if (enough) unname(stats::quantile(draws, c(.025, .975), na.rm = TRUE))
          else c(NA_real_, NA_real_)
    analytic <- bootstrap == 0 && spec$engine == "nonmixture" &&
      length(spec$incidence) == 0
    if (analytic) ci <- unname(object$fit$res["theta", c("L95%", "U95%")])
    list(object = object, estimate = estimate, lower = ci[1], upper = ci[2],
         attempted = bootstrap, successful = success, warnings = warnings,
         ci_method = if (analytic) "Transformed Wald interval for intercept-only cure parameter"
           else if (enough) "Percentile bootstrap at fixed original prediction profiles"
           else if (bootstrap > 0) "Unavailable: fewer than 80% (and 20) successful bootstrap fits"
           else "Not requested")
  })
}
