#' @title Flexible Parametric Survival Models
#' @importFrom R6 R6Class
#' @return An R6 class generator using rstpm2 model fits and predictions.
flexrstpm2Class <- R6::R6Class("flexrstpm2Class", inherit = flexrstpm2Base,
  private = list(
    .run = function() {
      if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
        self$results$todo$setContent("Select positive follow-up time, binary event status, and an event level.")
        return()
      }
      tryCatch(private$.analyse(), error = function(e)
        self$results$todo$setContent(htmltools::htmlEscape(conditionMessage(e))))
    },
    .analyse = function() {
      o <- self$options
      if (o$link_function != "log" || length(o$group_variable))
        stop("Choose the fitted hazard/odds/probit scale and covariates; separate link transforms and grouping controls are unavailable.")
      if (nzchar(trimws(o$smooth_formula)))
        stop("Custom smooth formulas are not supported; use the spline and time-varying controls.")
      if (o$cure_fraction && o$endpoint_type == "all_cause_death")
        stop("A positive all-cause survival asymptote is not a cure estimand; use excess mortality with matched background hazards.")
      relative <- o$relative_survival || o$endpoint_type == "excess_mortality"
      if (relative && is.null(o$bhazard)) stop("Relative survival requires matched event-time background hazards.")
      design <- .flexRstpmDesign(self$data, o$elapsedtime, o$outcome, o$outcomeLevel,
        o$covariates, o$time_varying_covariates, if (relative) o$bhazard else NULL)
      d <- design$data
      parse_times <- function(x) {
        if (!nzchar(trimws(x))) return(numeric())
        z <- suppressWarnings(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
        if (any(!is.finite(z) | z <= 0)) stop("Spline knots and prediction times must be finite and positive.")
        sort(unique(z))
      }
      knots <- parse_times(o$knots)
      boundary <- parse_times(o$boundary_knots)
      if (length(boundary) && length(boundary) != 2) stop("Provide exactly two boundary knots.")
      if (length(knots) && length(boundary) && any(knots <= boundary[1] | knots >= boundary[2]))
        stop("Internal knots must lie strictly inside the boundary knots.")
      fit_model <- function(data, df = o$df) .flexRstpmFit(data, design$columns, design$tvc,
        df = df, tvc_df = o$tvc_df, scale = o$scale, cure = o$cure_fraction,
        relative = relative, robust = o$robust_se, knots = knots, boundary = boundary)
      private$.checkpoint()
      fit <- fit_model(d)
      beta <- stats::coef(fit)
      se <- sqrt(diag(stats::vcov(fit)))
      z <- stats::qnorm(1 - (1 - o$confidence_level) / 2)
      for (i in seq_along(beta)) self$results$coefficientsTable$addRow(rowKey = i,
        values = list(term = names(beta)[i], coefficient = beta[i], se = se[i],
          z_value = beta[i] / se[i], p_value = 2 * stats::pnorm(-abs(beta[i] / se[i])),
          lower_ci = beta[i] - z * se[i], upper_ci = beta[i] + z * se[i]))
      self$results$coefficientsTable$setNote("scale", paste(
        "Coefficients are on the fitted link scale; spline and non-PH coefficients are not global log hazard ratios.",
        paste(names(design$labels), design$labels, sep = " = ", collapse = "; ")))
      for (stat in c("AIC", "BIC", "Log likelihood")) {
        value <- switch(stat, AIC = stats::AIC(fit), BIC = stats::BIC(fit),
                        "Log likelihood" = as.numeric(methods::selectMethod("logLik", "mle2")(fit)))
        self$results$modelFit$addRow(rowKey = stat,
          values = list(statistic = stat, value = value, interpretation = "Fitted likelihood statistic"))
      }
      times <- parse_times(o$prediction_times)
      if (!length(times)) stop("Provide at least one prediction time.")
      profile <- d[1, , drop = FALSE]
      profile[, design$columns] <- 0
      pred <- .flexRstpmPredict(fit, profile, times, level = o$confidence_level)
      haz <- .flexRstpmPredict(fit, profile, times, type = "hazard", level = o$confidence_level)
      ci <- function(lo, hi) sprintf("%.4f to %.4f", lo, hi)
      for (i in seq_along(times)) {
        self$results$survivalPredictions$addRow(rowKey = i, values = list(
          time_point = times[i], survival_probability = pred$estimate[i],
          confidence_interval = ci(pred$lower[i], pred$upper[i]),
          hazard_rate = haz$estimate[i], hazard_ratio = NA_real_))
        self$results$hazardAnalysis$addRow(rowKey = i, values = list(
          time_point = times[i], hazard_rate = haz$estimate[i],
          hazard_confidence_interval = ci(haz$lower[i], haz$upper[i]), hazard_pattern = "Model prediction"))
        if (relative) self$results$relativeSurvivalAnalysis$addRow(rowKey = i, values = list(
          time_point = times[i], observed_survival = NA_real_, expected_survival = NA_real_,
          relative_survival = pred$estimate[i], excess_mortality = haz$estimate[i]))
      }
      self$results$relativeSurvivalAnalysis$setNote("background", paste(
        "Background hazards must be correctly matched externally to event times and input time units.",
        "An event-time hazard column alone does not define a future population survival curve;",
        "expected and observed all-cause survival at this reference profile are not inferred."
      ))
      self$results$survivalPredictions$setNote("profile", paste(
        "Numeric predictors = 0; factors = reference levels. Predictions beyond observed follow-up are extrapolations.",
        if (relative) "These probabilities describe relative survival, not all-cause survival." else "",
        "Confidence intervals are computed by rstpm2 from fitted parameter uncertainty."
      ))
      grid <- seq(max(min(d$time) / 2, .0001), max(max(d$time), o$extrapolation_time), length.out = 150)
      curve <- .flexRstpmPredict(fit, profile, grid, level = o$confidence_level)
      hazard_curve <- .flexRstpmPredict(fit, profile, grid, "hazard", o$confidence_level)
      curve$observed <- grid <= max(d$time)
      hazard_curve$observed <- curve$observed
      self$results$survivalCurves$setState(curve)
      self$results$hazardCurves$setState(hazard_curve)
      self$results$hazardFunctionPlot$setState(hazard_curve)
      tvc_curves <- list()
      for (v in design$tvc) {
        hr <- .flexRstpmPredict(fit, profile, times, "hr", o$confidence_level, v)
        for (i in seq_along(times)) self$results$timeVaryingTable$addRow(
          rowKey = paste(v, i), values = list(covariate = design$labels[[v]],
            time_point = times[i], effect = hr$estimate[i], se = NA_real_,
            lower_ci = hr$lower[i], upper_ci = hr$upper[i]))
        p <- .flexRstpmPredict(fit, profile, grid, "hr", o$confidence_level, v)
        p$covariate <- design$labels[[v]]
        tvc_curves[[v]] <- p
      }
      if (length(tvc_curves)) self$results$timeVaryingPlots$setState(do.call(rbind, tvc_curves))
      if (o$derivative_analysis) {
        step <- pmax(times * .001, .000001)
        minus <- .flexRstpmPredict(fit, profile, times - step, "hazard")$estimate
        plus <- .flexRstpmPredict(fit, profile, times + step, "hazard")$estimate
        for (i in seq_along(times)) self$results$derivativeAnalysis$addRow(rowKey = i,
          values = list(time_point = times[i], first_derivative = (plus[i] - minus[i]) / (2 * step[i]),
            second_derivative = (plus[i] - 2 * haz$estimate[i] + minus[i]) / step[i]^2,
            acceleration_pattern = "Numerical derivative of fitted hazard; uncertainty unavailable"))
      }
      if (o$model_comparison && length(knots)) {
        self$results$modelComparison$setNote("comparison",
          "Automatic degrees-of-freedom comparisons are unavailable with fixed manual knots.")
      } else if (o$model_comparison) {
        for (df in sort(unique(pmax(1L, c(o$df - 1L, o$df, o$df + 1L))))) {
          private$.checkpoint()
          candidate <- tryCatch(if (df == o$df) fit else fit_model(d, df), error = function(e) NULL)
          if (is.null(candidate)) next
          self$results$modelComparison$addRow(rowKey = df, values = list(model = paste("Spline df", df),
            parameters = length(stats::coef(candidate)), log_likelihood = as.numeric(methods::selectMethod("logLik", "mle2")(candidate)),
            aic = stats::AIC(candidate), bic = stats::BIC(candidate), likelihood_ratio_test = NA_real_))
        }
        self$results$modelComparison$setNote("comparison", "Models use the same cohort; automatic knot changes do not guarantee nesting, so no likelihood-ratio p-value is reported.")
      }
      if (o$bootstrap_validation) {
        had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
        if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
        on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
          else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
        set.seed(o$bootstrap_seed)
        draws <- matrix(NA_real_, o$bootstrap_samples, length(times))
        for (b in seq_len(o$bootstrap_samples)) {
          private$.checkpoint()
          draws[b, ] <- tryCatch({
            fb <- fit_model(d[sample.int(nrow(d), replace = TRUE), , drop = FALSE])
            .flexRstpmPredict(fb, profile, times)$estimate
          }, error = function(e) rep(NA_real_, length(times)))
        }
        success <- sum(stats::complete.cases(draws))
        for (i in seq_along(times)) {
          vals <- draws[, i]
          valid <- is.finite(vals)
          enough <- sum(valid) >= max(20, ceiling(.8 * o$bootstrap_samples))
          self$results$bootstrapValidation$addRow(rowKey = i, values = list(
            validation_metric = paste("Survival at", times[i]), original_estimate = pred$estimate[i],
            bootstrap_mean = if (enough) mean(vals[valid]) else NA_real_,
            bootstrap_se = if (enough) stats::sd(vals[valid]) else NA_real_,
            bias = if (enough) mean(vals[valid]) - pred$estimate[i] else NA_real_,
            percentile_ci = if (enough) paste(signif(stats::quantile(vals[valid],
              c((1 - o$confidence_level) / 2, (1 + o$confidence_level) / 2)), 4), collapse = " to ") else "Unavailable"))
        }
        self$results$bootstrapValidation$setNote("resampling", paste(
          success, "successful /", o$bootstrap_samples, "attempted subject resamples.",
          "This estimates prediction uncertainty at fixed profiles, not external validation or optimism correction."))
      }
      self$results$goodnessOfFitTests$setNote("unavailable", "No omnibus model-validity test or cure-identification test is claimed.")
      self$results$timeRatioAnalysis$setNote("unavailable", "Time ratios are not inferred by exponentiating PH/PO/probit coefficients.")
      self$results$modelSummary$setContent(paste0("<p>rstpm2 ", o$scale,
        " spline model; ", nrow(d), " complete observations, ", sum(d$event), " events.</p>",
        "<p>Time-varying effects are fitted spline interactions, with dummy coding for factors.</p>",
        if (o$cure_fraction) "<p>The cure constraint controls the unobserved tail; it is an assumption, not evidence of cure.</p>" else ""))
      self$results$analysisSummary$setContent(paste("Fitted", o$scale,
        "model to", nrow(d), "complete observations with", sum(d$event),
        "events. Report the prediction profile and distinguish extrapolation from observed support."))
      self$results$clinicalSummary$setContent("Report supported-time probabilities, model assumptions, and uncertainty. Hazard ratios do not directly imply changes in event counts or a clinical cure guarantee.")
    },
    .plotSurvival = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate)) +
        ggplot2::geom_line(ggplot2::aes(linetype = observed)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = .15) + ggtheme)
      TRUE
    },
    .plotHazard = function(image, ggtheme, theme, ...)
      private$.plotSurvival(image, ggtheme, theme, ...),
    .plotTimeVarying = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate, color = covariate)) +
        ggplot2::geom_line() + ggplot2::labs(y = "Time-specific hazard ratio") + ggtheme)
      TRUE
    },
    .emptyPlot = function(...) FALSE
  ))
