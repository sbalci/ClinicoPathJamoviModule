#' @title Illness-Death Models
#' @importFrom R6 R6Class
#' @return An R6 class generator for exact-time illness-death analysis.
illnessdeathClass <- R6::R6Class("illnessdeathClass", inherit = illnessdeathBase,
  private = list(
    .run = function() {
      o <- self$options
      if (any(vapply(list(o$time_entry, o$time_exit, o$state_from, o$state_to, o$subject_id),
                     is.null, logical(1)))) {
        self$results$instructions$setContent(paste(
          "Select subject ID, entry/exit times, and states before/after each interval.",
          "States: 0 = event-free, 1 = ill, 2 = dead. Same-state final intervals are censored.",
          "Each subject's history must be contiguous and complete. Times use a common clock."
        ))
        return()
      }
      tryCatch(private$.analyse(), error = function(e)
        self$results$todo$setContent(htmltools::htmlEscape(conditionMessage(e))))
    },
    .analyse = function() {
      o <- self$options
      if (o$common_baseline || !o$transition_specific || o$penalization_parameter != 0)
        stop("Separate transition-specific baseline hazards and effects are required; shared or penalized fits are unavailable.")
      if (o$interval_censoring) stop("This model requires exact transition times; interval-censored transitions need a separate likelihood.")
      if (o$frailty_model || o$time_varying_effects || length(o$time_varying_covariates) ||
          length(o$stratification_variable) || nzchar(trimws(o$interaction_terms)))
        stop("Frailty, additional time-varying effects, interactions, and stratification are not implemented in this exact-time model.")
      if (o$baseline_hazard != "nonparametric" || o$estimation_method != "partial")
        stop("This implementation uses transition-specific Cox partial likelihood and Aalen–Johansen probabilities.")
      h <- .multistateHistory(self$data, o$time_entry, o$time_exit, o$state_from,
        o$state_to, o$subject_id, o$covariates, o$model_structure)
      d <- h$data
      times <- suppressWarnings(as.numeric(strsplit(o$prediction_times, ",", fixed = TRUE)[[1]]))
      aj <- .multistateAJ(h, times, o$confidence_level)
      ci_text <- function(lo, hi) if (is.finite(lo) && is.finite(hi))
        sprintf("%.3f to %.3f", lo, hi) else "Unavailable at boundary"
      for (i in seq_along(aj$time)) {
        self$results$stateProbabilities$addRow(rowKey = i, values = list(
          time_point = aj$time[i], initial_state = "Observed cohort",
          prob_healthy = aj$probability[i, 1], prob_ill = aj$probability[i, 2],
          prob_dead = aj$probability[i, 3],
          prob_healthy_ci = ci_text(aj$lower[i, 1], aj$upper[i, 1]),
          prob_ill_ci = ci_text(aj$lower[i, 2], aj$upper[i, 2]),
          prob_dead_ci = ci_text(aj$lower[i, 3], aj$upper[i, 3])))
      }
      self$results$stateProbabilities$setNote("estimand", paste(
        "Unadjusted Aalen–Johansen state occupation probabilities for the observed cohort,",
        paste0("with pointwise ", 100 * o$confidence_level, "% confidence intervals."),
        "These are not arbitrary-initial-state transition matrices.",
        "Times outside observed support are omitted. Competing-risk state 1 is an absorbing competing event."
      ))
      hazards <- list()
      for (k in seq_along(h$risk)) {
        private$.checkpoint()
        r <- h$risk[[k]]
        label <- paste(h$edges[k, 1], h$edges[k, 2], sep = " -> ")
        exposure <- sum(r$stop - r$start)
        self$results$transitionSummary$addRow(rowKey = k, values = list(
          transition = label, n_transitions = sum(r$event), n_censored = sum(r$event == 0),
          total_time = exposure, transition_rate = if (exposure > 0) sum(r$event) / exposure else NA_real_,
          median_time = NA_real_))
        if (!any(r$event)) next
        f <- stats::reformulate(if (length(h$covariates)) h$covariates else "1",
          response = "Surv(start, stop, event)", env = asNamespace("survival"))
        fit <- survival::coxph(f, data = r, ties = o$tie_method, cluster = id,
                              x = TRUE, y = TRUE, model = TRUE)
        bh <- survival::basehaz(fit, centered = FALSE)
        hazards[[length(hazards) + 1L]] <- data.frame(time = bh$time,
          estimate = bh$hazard, transition = label)
        if (length(stats::coef(fit))) {
          beta <- stats::coef(fit)
          se <- sqrt(diag(stats::vcov(fit)))
          z <- stats::qnorm(1 - (1 - o$confidence_level) / 2)
          for (j in seq_along(beta)) self$results$hazardRatios$addRow(
            rowKey = paste(k, j), values = list(transition = label, covariate = names(beta)[j],
              hazard_ratio = exp(beta[j]), lower_ci = exp(beta[j] - z * se[j]),
              upper_ci = exp(beta[j] + z * se[j]),
              p_value = 2 * stats::pnorm(-abs(beta[j] / se[j])), significance = ""))
          if (o$goodness_of_fit) {
            ph <- tryCatch(survival::cox.zph(fit)$table, error = function(e) NULL)
            if (!is.null(ph)) for (j in seq_len(nrow(ph)))
              self$results$goodnessOfFit$addRow(rowKey = paste(k, j), values = list(
                test_type = paste(label, rownames(ph)[j], "PH"), statistic = ph[j, "chisq"],
                degrees_freedom = ph[j, "df"], p_value = ph[j, "p"],
                interpretation = "Tests transition PH; does not test the Markov assumption."))
          }
        }
        if (o$residual_analysis) {
          residuals <- stats::residuals(fit, type = "martingale")
          self$results$residualAnalysis$addRow(rowKey = k, values = list(
            transition = label, residual_type = "Martingale", mean_residual = mean(residuals),
            sd_residual = stats::sd(residuals), test_statistic = NA_real_, p_value = NA_real_))
        }
      }
      self$results$transitionSummary$setNote("risk", paste(
        "Every eligible interval contributes to each outgoing transition risk set.",
        "Non-events include competing exits and administrative/loss-to-follow-up censoring.",
        "Exposure is measured in the input time units; no latent sojourn median is inferred."
      ))
      self$results$hazardRatios$setNote("model", paste(
        "Separate transition-specific cause-specific Cox models, with subject-clustered variance.",
        "Covariates x1, x2, ... follow the selected covariate order.",
        "Occupation probabilities above are unadjusted and do not use these regression fits."
      ))
      if (o$sojourn_times && min(d$start) == 0 && o$prediction_horizon <= max(d$stop)) {
        means <- .multistateRestricted(aj, o$prediction_horizon)
        limits <- matrix(NA_real_, 2, 3)
        if (o$bootstrap_ci) {
          draws <- .multistateRestrictedBootstrap(h, o$prediction_horizon,
            o$bootstrap_samples, o$random_seed, function() private$.checkpoint())
          successful <- sum(stats::complete.cases(draws))
          if (successful >= max(20, ceiling(.8 * o$bootstrap_samples))) {
            limits <- apply(draws, 2, stats::quantile,
              probs = c((1 - o$confidence_level) / 2, (1 + o$confidence_level) / 2),
              na.rm = TRUE)
          }
          self$results$sojournAnalysis$setNote("bootstrap", paste(
            successful, "successful /", o$bootstrap_samples,
            "attempted subject-bootstrap fits. Intervals require at least 80% and 20 successful fits."))
        }
        for (j in 1:3) self$results$sojournAnalysis$addRow(rowKey = j, values = list(
          state = c("Event-free", "Ill", "Dead")[j], mean_sojourn = means[j],
          median_sojourn = NA_real_, sojourn_lower_ci = limits[1, j],
          sojourn_upper_ci = limits[2, j], sojourn_variance = NA_real_))
      }
      self$results$sojournAnalysis$setTitle("Restricted mean time in state")
      self$results$sojournAnalysis$setNote("horizon", paste0(
        "Occupation probabilities integrated from time 0 through ", o$prediction_horizon,
        ". Requires an observed baseline at time 0 and a horizon within follow-up.",
        " This is restricted time in state, not an unlimited sojourn duration; enable subject bootstrap for uncertainty."))
      self$results$transitionMatrix$setNote("unavailable",
        "Arbitrary-initial-state Markov transition matrices are not estimated from cohort occupation probabilities.")
      self$results$crossValidationResults$setNote("unavailable",
        "Predictive cross-validation is not implemented; no simulated validation scores are reported.")
      curve <- data.frame(time = rep(aj$fit$time, 3), state = rep(c("Event-free", "Ill", "Dead"),
        each = length(aj$fit$time)), estimate = as.vector(aj$fit$pstate[, match(c("0", "1", "2"), aj$fit$states)]))
      self$results$stateProbabilitiesPlot$setState(curve)
      self$results$cumulativeIncidencePlot$setState(curve[curve$state == "Dead", , drop = FALSE])
      if (length(hazards)) self$results$transitionHazardsPlot$setState(do.call(rbind, hazards))
      self$results$instructions$setContent("Exact-time, clock-forward histories; death is absorbing. All reported statistics are computed from the supplied histories.")
    },
    .plotStateProbabilities = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate, color = state)) +
        ggplot2::geom_step() + ggplot2::labs(y = "State occupation probability") + ggtheme)
      TRUE
    },
    .plotTransitionHazards = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, estimate, color = transition)) +
        ggplot2::geom_step() + ggplot2::labs(y = "Baseline cumulative cause-specific hazard") + ggtheme)
      TRUE
    },
    .plotCumulativeIncidence = function(image, ggtheme, theme, ...)
      private$.plotStateProbabilities(image, ggtheme, theme, ...),
    .plotTransitionDiagram = function(...) FALSE,
    .plotSojournDistributions = function(...) FALSE,
    .plotResiduals = function(...) FALSE
  ))
