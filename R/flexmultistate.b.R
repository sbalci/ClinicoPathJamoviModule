#' @title Flexible Parametric Multi-State Models
#' @importFrom R6 R6Class
#' @return An R6 class generator for clock-forward parametric multistate models.
flexmultistateClass <- R6::R6Class("flexmultistateClass", inherit = flexmultistateBase,
  private = list(
    .run = function() {
      o <- self$options
      if (any(vapply(list(o$time, o$status, o$start_state, o$time_entry, o$subject_id),
                     is.null, logical(1)))) {
        self$results$todo$setContent(paste(
          "Select subject ID, entry and exit times, and the state before and after each interval.",
          "States are 0 = event-free, 1 = ill, 2 = dead. A same-state final interval denotes censoring.",
          "Complete histories are required; the former one-row transition-code input is insufficient."
        ))
        return()
      }
      tryCatch(private$.analyse(), error = function(e)
        self$results$todo$setContent(htmltools::htmlEscape(conditionMessage(e))))
    },
    .analyse = function() {
      o <- self$options
      if (length(o$shared_effects) || length(o$time_varying_effects) || !o$transition_specific)
        stop("This model currently supports separate transition-specific clock-forward effects only.")
      h <- .multistateHistory(self$data, o$time_entry, o$time, o$start_state,
                              o$status, o$subject_id, o$predictors, o$model_type)
      models <- list()
      curves <- list()
      times <- suppressWarnings(as.numeric(strsplit(o$prediction_times, ",", fixed = TRUE)[[1]]))
      times <- sort(unique(times[is.finite(times) & times > 0]))
      if (!length(times)) stop("Enter positive finite prediction times.")
      grid <- sort(unique(c(times, seq(max(times) / 200, max(times), length.out = 100))))
      profile <- h$data[1, h$covariates, drop = FALSE]
      for (j in h$covariates) profile[[j]] <- if (is.factor(h$data[[j]]))
        factor(levels(h$data[[j]])[1], levels = levels(h$data[[j]])) else 0
      for (k in seq_along(h$risk)) {
        private$.checkpoint()
        r <- h$risk[[k]]
        label <- paste(h$edges[k, 1], h$edges[k, 2], sep = " -> ")
        self$results$transitionMatrix$addRow(rowKey = k, values = list(
          from_state = as.character(h$edges[k, 1]), to_state = as.character(h$edges[k, 2]),
          transition_name = label, n_transitions = sum(r$event),
          person_years = sum(r$stop - r$start)))
        if (!any(r$event)) stop(paste("No events for transition", label,
          "- a parametric transition distribution is not estimable; no probability matrix is reported."))
        f <- stats::reformulate(if (length(h$covariates)) h$covariates else "1",
          response = "Surv(start, stop, event)", env = asNamespace("survival"))
        fit <- if (o$hazard_distribution == "royston_parmar")
          flexsurv::flexsurvspline(f, data = r, k = o$spline_knots, scale = o$time_scale)
        else flexsurv::flexsurvreg(f, data = r,
          dist = switch(o$hazard_distribution, weibull_spline = "weibull",
                         lognormal_spline = "lnorm", piecewise_exp = "exp"))
        if (fit$opt$convergence != 0 || any(!is.finite(stats::coef(fit))))
          stop(paste("Transition fit failed to converge:", label))
        models[[k]] <- fit
        cf <- fit$res.t
        for (j in seq_len(nrow(cf))) self$results$transitionModels$addRow(
          rowKey = paste(k, j), values = list(transition = label, parameter = rownames(cf)[j],
            estimate = cf[j, "est"], se = cf[j, "se"], z_value = NA_real_, p_value = NA_real_,
            hr = NA_real_, hr_lower = NA_real_, hr_upper = NA_real_))
        self$results$modelComparison$addRow(rowKey = k, values = list(
          transition = label, loglik = fit$loglik, aic = stats::AIC(fit),
          bic = stats::BIC(fit), n_parameters = length(stats::coef(fit))))
        for (type in c("hazard", "cumhaz")) {
          pred <- summary(fit, newdata = if (ncol(profile)) profile else NULL,
                          t = grid, type = type, ci = FALSE)[[1]]
          curves[[paste(k, type)]] <- data.frame(time = pred$time, estimate = pred$est,
                                               transition = label, type = type)
        }
      }
      had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
      on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
        else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
      set.seed(o$random_seed)
      state_curve <- list()
      for (t in times) {
        private$.checkpoint()
        p <- flexsurv::pmatrix.fs(models, trans = h$tmat, t = t,
          newdata = if (ncol(profile)) profile else NULL,
          ci = o$confidence_intervals, B = o$ci_samples)
        lower <- attr(p, "lower")
        upper <- attr(p, "upper")
        if (is.null(lower)) lower <- upper <- matrix(NA_real_, 3, 3)
        for (i in 1:3) for (j in 1:3) self$results$transitionProbTable$addRow(
          rowKey = paste(t, i, j), values = list(time_point = t,
            from_state = rownames(p)[i], to_state = colnames(p)[j], probability = p[i, j],
            prob_lower = lower[i, j], prob_upper = upper[i, j]))
        for (j in 1:3) self$results$stateProbTable$addRow(rowKey = paste(t, j), values = list(
          time_point = t, state = colnames(p)[j], probability = p[1, j],
          prob_lower = lower[1, j], prob_upper = upper[1, j]))
        state_curve[[length(state_curve) + 1L]] <- data.frame(time = t,
          state = colnames(p), estimate = as.numeric(p[1, ]))
      }
      if (o$expected_sojourn) {
        restricted <- flexsurv::totlos.fs(models, trans = h$tmat, t = max(times),
          newdata = if (ncol(profile)) profile else NULL,
          ci = o$confidence_intervals, B = o$ci_samples)
        lower <- attr(restricted, "lower")
        upper <- attr(restricted, "upper")
        for (j in 1:3) self$results$sojournTable$addRow(rowKey = j, values = list(
          state = colnames(h$tmat)[j], expected_time = restricted[1, j],
          sojourn_lower = if (is.null(lower)) NA_real_ else lower[1, j],
          sojourn_upper = if (is.null(upper)) NA_real_ else upper[1, j]))
      }
      if (o$microsimulation) {
        sim <- flexsurv::sim.fmsm(models, trans = h$tmat, t = max(times),
          newdata = if (ncol(profile)) profile else NULL, start = 1,
          M = o$n_simulation, tidy = TRUE)
        self$results$microsimResults$setContent(paste0(
          "Generated ", o$n_simulation, " trajectories from event-free state through time ",
          max(times), "; returned ", nrow(sim), " transition/history rows."))
      }
      self$results$summary$setContent(paste(
        "Clock-forward Markov model with separately fitted outgoing transition hazards.",
        "Prediction profile: numeric covariates zero and factors at reference levels.",
        "State probabilities start event-free at time zero. Confidence intervals use fitted-parameter simulation.",
        "Predictions beyond observed follow-up are model extrapolations; they do not establish cure."
      ))
      self$results$sojournTable$setTitle("Restricted expected time in state")
      self$results$sojournTable$setNote("horizon", paste("Integrated to", max(times),
        "input time units, starting event-free. Not an unlimited sojourn duration."))
      self$results$transitionMatrix$getColumn("person_years")$setTitle("Exposure (input time units)")
      self$results$transitionModels$setNote("scale",
        "Parameters use the fitted model's transformed scale; they are not universally log hazard ratios.")
      curve <- do.call(rbind, curves)
      self$results$hazardPlot$setState(curve[curve$type == "hazard", , drop = FALSE])
      self$results$cumhazPlot$setState(curve[curve$type == "cumhaz", , drop = FALSE])
      self$results$survivalPlot$setState(do.call(rbind, state_curve))
    },
    .plotHazards = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate, color = transition)) +
        ggplot2::geom_line() + ggtheme)
      TRUE
    },
    .plotCumHaz = function(image, ggtheme, theme, ...)
      private$.plotHazards(image, ggtheme, theme, ...),
    .plotSurvival = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate, color = state)) +
        ggplot2::geom_line() + ggplot2::coord_cartesian(ylim = c(0, 1)) + ggtheme)
      TRUE
    },
    .plotTransitionDiagram = function(...) FALSE
  ))
