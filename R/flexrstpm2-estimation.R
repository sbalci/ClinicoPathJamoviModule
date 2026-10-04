.flexRstpmDesign <- function(data, time, outcome, event, covariates, tvc = NULL,
                             background = NULL) {
  vars <- unique(c(time, outcome, covariates, tvc, background))
  d <- data[, vars, drop = FALSE]
  keep <- stats::complete.cases(d)
  d <- d[keep, , drop = FALSE]
  if (length(unique(as.character(d[[outcome]]))) > 2)
    stop("A binary event indicator is required; competing events cannot be silently censored.")
  if (!event %in% as.character(d[[outcome]])) stop("The selected event value is absent.")
  out <- data.frame(time = d[[time]], event = as.integer(as.character(d[[outcome]]) == event))
  if (!is.numeric(out$time) || any(!is.finite(out$time) | out$time <= 0))
    stop("Flexible survival models require finite positive follow-up times.")
  predictors <- unique(c(covariates, tvc))
  labels <- columns <- tvc_columns <- character()
  if (length(predictors)) {
    x <- d[, predictors, drop = FALSE]
    names(x) <- paste0("v", seq_along(predictors))
    mm <- stats::model.matrix(~ ., data = x)
    assign <- attr(mm, "assign")[-1]
    labels <- colnames(mm)[-1]
    for (i in rev(seq_along(predictors)))
      labels <- sub(paste0("^v", i, "(?=[^0-9]|$)"), predictors[i], labels, perl = TRUE)
    mm <- mm[, -1, drop = FALSE]
    columns <- paste0("x", seq_len(ncol(mm)))
    colnames(mm) <- columns
    tvc_columns <- columns[assign %in% match(tvc, predictors)]
    out <- cbind(out, mm)
  }
  if (!is.null(background)) {
    out$background <- d[[background]]
    if (any(!is.finite(out$background) | out$background < 0))
      stop("Background hazards must be finite, non-negative and use the same time units.")
  }
  list(data = out, columns = columns, tvc = tvc_columns,
       labels = stats::setNames(labels, columns), excluded = sum(!keep))
}

.flexRstpmFit <- function(d, columns, tvc = NULL, df = 4L, tvc_df = 3L,
                         scale = "hazard", cure = FALSE, relative = FALSE,
                         robust = FALSE, knots = NULL, boundary = NULL) {
  if (sum(d$event) < 5) stop("At least five events are required for the flexible survival model.")
  scale <- match.arg(scale, c("hazard", "odds", "normal"))
  if (length(knots) && (any(!is.finite(knots) | knots <= 0) ||
                       is.unsorted(knots, strictly = TRUE)))
    stop("Spline knots must be finite, positive and strictly increasing.")
  if (length(boundary) && (length(boundary) != 2L ||
                          any(!is.finite(boundary) | boundary <= 0) ||
                          boundary[1] >= boundary[2]))
    stop("Boundary knots must be two finite positive times in increasing order.")
  if (length(knots) && length(boundary) &&
      any(knots <= boundary[1] | knots >= boundary[2]))
    stop("Interior spline knots must lie strictly inside the boundary knots.")
  if (relative && (is.null(d$background) || !is.numeric(d$background) ||
                   any(!is.finite(d$background) | d$background < 0)))
    stop("Relative survival requires finite, non-negative background hazards.")
  f <- stats::reformulate(if (length(columns)) columns else "1",
    response = "Surv(time, event)", env = asNamespace("rstpm2"))
  smooth_args <- list()
  if (length(knots)) smooth_args$knots <- log(knots)
  else smooth_args$df <- df
  if (length(boundary)) smooth_args$Boundary.knots <- log(boundary)
  if (cure) smooth_args$cure <- TRUE
  args <- list(formula = f, data = d, df = df, penalised = FALSE,
    link.type = switch(scale, hazard = "PH", odds = "PO", normal = "probit"),
    tvc = if (length(tvc)) stats::setNames(as.list(rep(tvc_df, length(tvc))), tvc) else NULL,
    cure = cure, robust = robust, smooth.args = smooth_args)
  # gsm re-evaluates bhazard and its model call inside the data frame. Pass
  # evaluated arguments so wrapper-local conditionals cannot change its rows.
  if (relative) args$bhazard <- d$background
  fit <- do.call(rstpm2::gsm, args)
  if (!is.null(fit@details$convergence) && fit@details$convergence != 0)
    stop("The flexible survival model did not converge.")
  if (any(!is.finite(stats::coef(fit))) || any(!is.finite(stats::vcov(fit))))
    stop("The fitted model has non-finite coefficients or covariance.")
  fit
}

.flexRstpmPredict <- function(fit, profile, times, type = "surv", level = .95, var = NULL) {
  if (!is.data.frame(profile) || nrow(profile) != 1L)
    stop("Prediction requires one explicit covariate profile.")
  if (!length(times) || any(!is.finite(times) | times <= 0))
    stop("Prediction times must be finite and positive.")
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("The confidence level must lie strictly between zero and one.")
  d <- profile[rep(1, length(times)), , drop = FALSE]
  d$time <- times
  args <- list(object = fit, newdata = d, type = type, se.fit = TRUE, level = level)
  if (!is.null(var)) args$var <- var
  p <- do.call(rstpm2::predict, args)
  if (!is.data.frame(p) || !all(c("Estimate", "lower", "upper") %in% names(p)))
    stop("The model did not return prediction uncertainty.")
  if (any(!is.finite(p$Estimate) | p$Estimate < 0) ||
      (type == "surv" && any(p$Estimate > 1)))
    stop("The fitted model returned invalid predictions; inspect its fit and extrapolation horizon.")
  data.frame(time = times, estimate = p$Estimate, lower = p$lower, upper = p$upper)
}
