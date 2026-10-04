#' @title Cure Models for Long-term Survivors
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An R6 class generator for the cure-model analysis backend.
curemodelsClass <- R6::R6Class(
  "curemodelsClass", inherit = curemodelsBase,
  private = list(
    .run = function() {
      if (is.null(self$options$time) || is.null(self$options$status)) {
        self$results$todo$setContent(paste(
          "Select follow-up time, event status, and the endpoint interpretation.",
          "Specify the event and censoring values explicitly.",
          "Cure fractions depend on tail and model assumptions; a late plateau alone is insufficient."
        ))
        return()
      }
      tryCatch(private$.analyse(), error = function(e) {
        self$results$errors$setContent(htmltools::htmlEscape(conditionMessage(e)))
      })
    },

    .analyse = function() {
      o <- self$options
      extra <- c(if (o$endpoint_type == "excess_mortality") o$background_hazard_var,
                 if (o$model_type %in% c("npcure", "all")) o$npcure_covariate)
      vars <- unique(c(o$time, o$status, o$predictors, o$latency_predictors, extra))
      d <- self$data[, vars, drop = FALSE]
      complete <- stats::complete.cases(d)
      d <- d[complete, , drop = FALSE]
      if (nrow(d) < 30) stop("At least 30 complete observations are required for cure modeling.")
      time <- d[[o$time]]
      if (!is.numeric(time) || any(!is.finite(time) | time < 0))
        stop("Follow-up times must be finite, non-negative numbers.")
      d[[o$status]] <- .cureStatus(d[[o$status]], o$event_value,
                                  o$censor_value, o$competing_value)
      status <- d[[o$status]]
      threshold <- o$cure_threshold
      mfu <- .medianFollowUp(time, status == 0)
      self$results$followupSupport$addRow(rowKey = "cohort", values = list(
        n = nrow(d), excluded = sum(!complete), events = sum(status == 1),
        competing = sum(status == 2), censored = sum(status == 0),
        assessment_time = threshold, n_risk = sum(time >= threshold),
        late_events = sum(time >= threshold & status != 0),
        max_time = max(time), median_followup = if (isTRUE(mfu$reverse)) mfu$value else NA_real_
      ))
      self$results$followupSupport$setNote("support", paste(
        "At risk means still under observation just before the assessment time;",
        "late events include all terminal events at or after it.",
        "No late events, especially with an empty or small risk set, do not establish cure.",
        "Follow-up is estimated by reverse Kaplan–Meier; no threshold classifies patients as cured."
      ))
      if (!isTRUE(mfu$reverse)) self$results$followupSupport$setNote("median",
        "The reverse-Kaplan–Meier median follow-up is not estimable and is left blank.")
      horizons <- suppressWarnings(as.numeric(trimws(strsplit(o$analysis_horizons, ",",
                                                                fixed = TRUE)[[1]])))
      valid <- is.finite(horizons) & horizons > 0 & horizons <= max(time)
      horizons <- sort(unique(horizons[valid]))
      observed_times <- sort(unique(c(horizons, threshold[threshold <= max(time)], max(time))))
      .populateObservedCif(self$results$observedRisk, time, status, observed_times)
      self$results$observedRisk$setNote("endpoint", if (o$endpoint_type == "excess_mortality")
        "These are observed all-cause probabilities, distinct from model-based relative-survival cure."
        else "Use the event mapping to interpret the target and competing causes.")
      if (o$endpoint_type == "all_cause_death") {
        stop(paste("A positive lifetime survival asymptote for all-cause death is not a cure estimand.",
                   "Observed probabilities are reported. For statistical cure choose excess mortality",
                   "and provide matched event-time background hazards."))
      }
      if (any(status == 2)) {
        stop(paste("Competing events are retained in the observed-risk table.",
                   "The available single-event cure engines cannot identify recurrence cure",
                   "by censoring competing deaths; no cure fit was produced."))
      }
      if (o$endpoint_type == "excess_mortality" &&
          (o$model_type != "cure" || is.null(o$background_hazard_var) ||
           !isTRUE(o$use_background_mortality))) {
        stop("Excess-mortality analysis requires the cuRe model and a matched background-hazard variable.")
      }
      if (o$endpoint_type != "excess_mortality" && o$model_type == "cure")
        stop("Select the excess-mortality endpoint to use the background-mortality cure model.")
      if (o$endpoint_type != "excess_mortality" && o$use_background_mortality)
        stop("Background mortality requires the excess-mortality endpoint and the cuRe model.")
      latency <- if (length(o$latency_predictors)) o$latency_predictors else o$predictors
      design <- .cureDesign(d, o$time, o$status, o$predictors, latency,
        background = if (o$endpoint_type == "excess_mortality") o$background_hazard_var else NULL,
        np_covariate = if (o$model_type %in% c("npcure", "all")) o$npcure_covariate else NULL,
        profile = o$prediction_profile)
      engines <- if (o$model_type == "all") c("mixture", "nonmixture", "npcure") else o$model_type
      boot <- if (o$bootstrap_ci) o$n_bootstrap else 0L
      notes <- character()
      fitted <- list()
      for (engine in engines) {
        private$.checkpoint()
        spec <- list(engine = engine, incidence = design$incidence, latency = design$latency,
          latency_explicit = length(o$latency_predictors) > 0, link = o$cure_link,
          distribution = o$survival_dist, latency_type = o$smcure_model_type,
          bandwidth = o$npcure_bandwidth)
        result <- tryCatch(.cureEstimate(design$data, spec, design$profiles, boot,
          o$bootstrap_seed, function() private$.checkpoint()), error = identity)
        if (inherits(result, "error")) {
          notes <- c(notes, paste(engine, conditionMessage(result), sep = ": "))
          next
        }
        fitted[[engine]] <- result
        private$.showFit(result, design)
        if (length(result$warnings)) notes <- c(notes,
          paste(engine, paste(result$warnings, collapse = "; "), sep = ": "))
        if (o$sensitivity_analysis) {
          specs <- list(spec)
          if (o$sensitivity_alternatives && engine == "mixture") {
            alternative <- spec
            alternative$latency_type <- if (spec$latency_type == "ph") "aft" else "ph"
            specs <- c(specs, list(alternative))
          }
          if (o$sensitivity_alternatives && engine %in% c("nonmixture", "cure")) {
            for (distribution in setdiff(c("weibull", "exponential", "lognormal"),
                                         spec$distribution)) {
              alternative <- spec
              alternative$distribution <- distribution
              specs <- c(specs, list(alternative))
            }
          }
          for (s in specs) for (h in horizons) {
            private$.checkpoint()
            truncated <- .cureTruncate(design$data, h)
            r <- tryCatch(.cureEstimate(truncated, s, design$profiles, boot,
              o$bootstrap_seed, function() private$.checkpoint()), error = identity)
            ok <- !inherits(r, "error")
            label <- paste(engine, if (engine == "mixture") s$latency_type else s$distribution)
            self$results$sensitivityTable$addRow(
              rowKey = paste(label, h), values = list(model = label, horizon = h,
                n = nrow(truncated), events = sum(truncated$.status == 1),
                n_risk = sum(design$data$.time >= h),
                recensored = sum(design$data$.time > h),
                estimate = if (ok) r$estimate else NA_real_,
                lower = if (ok) r$lower else NA_real_, upper = if (ok) r$upper else NA_real_,
                bootstrap_success = if (ok) r$successful else 0L,
                bootstrap_attempts = if (ok) r$attempted else 0L,
                status = if (!ok) conditionMessage(r) else if (engine == "mixture")
                  "Fit returned; EM convergence not exposed" else "Fit returned"
              ))
          }
        }
      }
      if (o$sensitivity_analysis) {
        self$results$sensitivityAnalysis$setContent(paste(
          "Each row refits the model after administrative censoring at the stated horizon.",
          "The original complete-case cohort and prediction profiles are held fixed.",
          "Later events are recoded as censored at the horizon. Unsupported horizons are omitted.",
          "AIC is not compared across horizons. Stability does not establish cure identification."
        ))
        if (!length(horizons)) notes <- c(notes,
          "No analysis horizon is positive and within observed follow-up; sensitivity table is empty.")
      }
      self$results$summary$setContent(paste0(
        "<p><b>Prediction profile:</b> ", htmltools::htmlEscape(design$profile), "</p>",
        "<p><b>Endpoint:</b> ", htmltools::htmlEscape(o$endpoint_type), "</p>",
        "<p>Model-based asymptotes are conditional on the model and its unobserved-tail assumptions.</p>"
      ))
      self$results$interpretation$setContent(paste(
        "Report observed-time event probabilities and follow-up support alongside modeled cure.",
        "Susceptibility coefficients in smcure increase the probability of eventually experiencing",
        "the event; cure probabilities use their complement. The susceptible distribution is",
        "assumed proper; smcure completes its tail at the largest failure time.",
        "Relative-survival statistical cure means absence of excess mortality, not immortality.",
        "Neither a fitted cure fraction nor a late event-free interval justifies changing clinical follow-up."
      ))
      self$results$warnings$setContent(htmltools::htmlEscape(paste(unique(notes), collapse = "\n")))
      if (!length(fitted)) self$results$errors$setContent(
        htmltools::htmlEscape(paste("No cure model could be fitted.", paste(notes, collapse = "; "))))
      if (length(fitted)) {
        plot_data <- do.call(rbind, lapply(names(fitted), function(n) {
          r <- fitted[[n]]
          data.frame(model = n, estimate = r$estimate, lower = r$lower, upper = r$upper)
        }))
        self$results$cureFractionPlot$setState(plot_data)
      }
      km <- survival::survfit(survival::Surv(time, status) ~ 1)
      self$results$survivalPlot$setState(data.frame(
        time = c(0, km$time), estimate = c(1, km$surv),
        lower = c(1, km$lower), upper = c(1, km$upper)))
    },

    .showFit = function(result, design) {
      object <- result$object
      engine <- object$spec$engine
      fit <- object$fit
      self$results$cureTable$addRow(rowKey = engine, values = list(
        group = paste(engine, design$profile, sep = ": "),
        cure_fraction = result$estimate, cure_ci_lower = result$lower,
        cure_ci_upper = result$upper, uncured_median = NA_real_
      ))
      self$results$cureTable$setNote("uncertainty", paste(
        "CIs use a seeded, bounded percentile bootstrap at fixed original prediction profiles,",
        "or a transformed Wald interval for an intercept-only flexsurvcure model.",
        "At least 80% of requested fits and 20 successful fits are required.",
        "An observed-event median is not a median among latent susceptible patients."
      ))
      self$results$fitDiagnostics$addRow(rowKey = engine, values = list(
        model = engine, successful = result$successful, attempted = result$attempted,
        ci_method = result$ci_method,
        convergence = if (engine == "mixture") "EM convergence not exposed by smcure"
                      else "Engine returned finite probabilities"
      ))
      coef <- switch(engine,
        mixture = c(stats::setNames(as.numeric(fit$b),
          paste0("Susceptibility: ", c("Intercept", design$labels[object$spec$incidence]))),
          stats::setNames(as.numeric(fit$beta), paste0("Latency: ",
            if (length(fit$beta) == length(object$spec$latency) + 1)
              c("Intercept", design$labels[object$spec$latency])
            else design$labels[object$spec$latency]))),
        nonmixture = stats::coef(fit),
        cure = tryCatch(stats::coef(fit), error = function(e) numeric()),
        numeric())
      if (is.numeric(coef)) for (i in seq_along(coef)) {
        self$results$modelTable$addRow(rowKey = paste(engine, i), values = list(
          parameter = paste(engine, names(coef)[i]), estimate = unname(coef[i]),
          std_error = NA_real_, z_value = NA_real_, p_value = NA_real_,
          ci_lower = NA_real_, ci_upper = NA_real_
        ))
      }
      self$results$modelTable$setNote("scale", paste(
        "smcure incidence coefficients are susceptibility logits, not cure logits.",
        "Other coefficients are on the engine's transformed estimation scale.",
        "Probability uncertainty is reported in the cure summary; coefficient inference is unavailable."
      ))
      if (isTRUE(self$options$goodness_of_fit)) {
        self$results$goodnessOfFit$addRow(rowKey = engine, values = list(
          test_name = engine, statistic = NA_real_, p_value = NA_real_,
          interpretation = "No formal cure-identification or goodness-of-fit test is claimed."
        ))
      }
      if (inherits(fit, "flexsurvreg")) {
        self$results$modelComparison$addRow(rowKey = engine, values = list(
          model = engine, aic = stats::AIC(fit), bic = stats::BIC(fit), loglik = fit$loglik))
      }
    },

    .renderCureFractionPlot = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d) || !nrow(d)) return(FALSE)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = model, y = estimate)) +
        ggplot2::geom_point() + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper), width = .15, na.rm = TRUE) +
        ggplot2::coord_cartesian(ylim = c(0, 1)) +
        ggplot2::labs(x = "Model", y = "Model-based cure probability") + ggtheme
      print(p)
      TRUE
    },

    .plotSurvival = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d) || !nrow(d)) return(FALSE)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = estimate)) +
        ggplot2::geom_step() + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = lower, ymax = upper), alpha = .15) +
        ggplot2::coord_cartesian(ylim = c(0, 1)) +
        ggplot2::labs(x = "Observed time", y = "Observed event-free probability") + ggtheme
      print(p)
      TRUE
    }
  )
)
