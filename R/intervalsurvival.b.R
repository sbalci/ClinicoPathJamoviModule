#' @title Interval-Censored Survival Analysis
#' @importFrom R6 R6Class
#' @return An R6 class generator for interval-censored survival analysis.
#' @export
intervalsurvivalClass <- R6::R6Class("intervalsurvivalClass", inherit = intervalsurvivalBase,
  private = list(
    .run = function() {
      o <- self$options
      if (is.null(o$left_time) || is.null(o$right_time)) {
        self$results$instructions$setContent(paste(
          "Supply lower and upper event-time bounds. Equal positive bounds denote exact events;",
          "an infinite upper bound denotes right censoring; a zero lower bound denotes left censoring.",
          "Optional status codes: 0 = right, 1 = interval, 2 = left, 3 = exact."
        ))
        return()
      }
      tryCatch(private$.analyse(), error = function(e)
        self$results$model_results$setContent(htmltools::htmlEscape(conditionMessage(e))))
    },
    .analyse = function() {
      o <- self$options
      if (length(o$stratification_vars)) stop("Stratified interval regression is not implemented.")
      if (o$estimation_method != "em_algorithm")
        stop("Select automatic package likelihood estimation; separate optimizer choices are unavailable.")
      if (o$baseline_hazard_smooth || o$imputation_method != "midpoint")
        stop("Baseline smoothing and imputation are not supported; this analysis fits the interval likelihood directly.")
      if (o$model_type == "nonparametric" && length(o$covariates))
        stop("The nonparametric estimator is unadjusted; remove covariates or select a regression model.")
      raw <- .intervalSurvivalData(self$data[[o$left_time]], self$data[[o$right_time]],
        if (is.null(o$status_var)) NULL else self$data[[o$status_var]],
        self$data[, o$covariates, drop = FALSE])
      d <- raw$data
      if (nrow(d) < 5) stop("At least five informative observations are required.")
      nright <- sum(is.infinite(d$right))
      nexact <- sum(d$left == d$right)
      nleft <- sum(d$left == 0 & is.finite(d$right))
      self$results$data_summary$setContent(.intervalSurvivalHtml(data.frame(
        N = nrow(d), Excluded = raw$excluded, Exact = nexact, Left = nleft,
        Interval = nrow(d) - nright - nexact - nleft, Right = nright)))
      private$.checkpoint()
      fit <- .intervalSurvivalFit(d, o$model_type, o$max_iterations)
      times <- suppressWarnings(as.numeric(strsplit(o$prediction_times, ",", fixed = TRUE)[[1]]))
      times <- sort(unique(times[is.finite(times) & times >= 0]))
      if (!length(times)) stop("Enter at least one finite non-negative prediction time.")
      np <- o$model_type == "nonparametric"
      support <- max(c(d$left, d$right[is.finite(d$right)]))
      if (np) times <- times[times <= support]
      estimate <- .intervalSurvivalPredict(fit, d, times, np)
      lower <- upper <- rep(NA_real_, length(times))
      success <- 0L
      if (o$bootstrap_ci) {
        had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
        if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
        on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
          else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
        set.seed(o$bootstrap_seed)
        draws <- matrix(NA_real_, o$bootstrap_samples, length(times))
        for (b in seq_len(o$bootstrap_samples)) {
          private$.checkpoint()
          draws[b, ] <- tryCatch({
            fb <- .intervalSurvivalFit(d[sample.int(nrow(d), replace = TRUE), , drop = FALSE],
                                       o$model_type, o$max_iterations)
            .intervalSurvivalPredict(fb, d, times, np)
          }, error = function(e) rep(NA_real_, length(times)))
        }
        success <- sum(stats::complete.cases(draws))
        if (success >= max(20, ceiling(.8 * o$bootstrap_samples)) && length(times)) {
          alpha <- (1 - o$confidence_level) / 2
          lower <- apply(draws, 2, stats::quantile, alpha, na.rm = TRUE)
          upper <- apply(draws, 2, stats::quantile, 1 - alpha, na.rm = TRUE)
        }
      }
      self$results$survival_estimates$setContent(paste0(
        .intervalSurvivalHtml(data.frame(Time = times, Survival = estimate,
          Lower = lower, Upper = upper, Extrapolated = times > support)),
        "<p>Regression predictions average over the complete-case cohort. NPMLE/PH predictions",
        " use icenReg's uniform interpolation within Turnbull intervals; event times remain",
        " interval-censored in the likelihood. Blank intervals mean no adequate bootstrap.</p>"))
      if (np) {
        curves <- icenReg::getSCurves(fit)
        self$results$covariate_effects$setContent(paste0(
          "<p>Turnbull support intervals; survival inside each interval is not uniquely identified.</p>",
          .intervalSurvivalHtml(data.frame(curves$Tbull_ints,
                                          Survival = curves$S_curves[[1]]))))
      } else {
        cf <- stats::coef(fit)
        self$results$covariate_effects$setContent(paste0(
          .intervalSurvivalHtml(data.frame(Parameter = names(cf), Estimate = unname(cf))),
          if (o$model_type == "cox_ph") "<p>Regression effects are log hazard ratios.</p>"
          else "<p>Regression effects use the AFT parameterization; baseline parameters retain their native estimation scale.</p>"))
      }
      self$results$model_results$setContent(paste0(
        "<p>Fitted ", htmltools::htmlEscape(o$model_type), " using the full interval likelihood.</p>",
        "<p>Infinite right-censoring bounds are preserved. No midpoint imputation is performed.</p>",
        "<p>This single-event model does not jointly model interval-censored recurrence and competing death.</p>"))
      self$results$model_diagnostics_summary$setContent(paste0(
        "Engine iterations: ", fit$iterations, ". Log likelihood: ", signif(fit$llk, 6),
        ". A separate convergence flag is not exposed by this adapter. ",
        "Bootstrap fits successful: ", success, " / ", if (o$bootstrap_ci) o$bootstrap_samples else 0,
        ". Finite fitted probabilities do not by themselves validate PH/AFT assumptions."))
      for (item in c("hazard_estimates", "goodness_of_fit_results", "model_comparison_table", "residual_summary"))
        self$results[[item]]$setContent("This diagnostic is not implemented for the interval likelihood; no statistic is reported.")
      grid <- sort(unique(c(0, times, seq(0, support, length.out = 150))))
      self$results$survival_curves_plot$setState(data.frame(time = grid,
        survival = .intervalSurvivalPredict(fit, d, grid, np)))
      self$results$interval_plot$setState(data.frame(left = d$left,
        right = ifelse(is.finite(d$right), d$right, support * 1.05),
        censored = is.infinite(d$right), row = seq_len(nrow(d))))
    },
    .plot_survival_curves = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      print(ggplot2::ggplot(d, ggplot2::aes(time, survival)) + ggplot2::geom_line() +
        ggplot2::coord_cartesian(ylim = c(0, 1)) + ggtheme)
      TRUE
    },
    .plot_intervals = function(image, ggtheme, theme, ...) {
      d <- image$state
      if (is.null(d)) return(FALSE)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = left, xend = right, y = row, yend = row)) +
        ggplot2::geom_segment(data = d[!d$censored, , drop = FALSE]) +
        ggplot2::geom_segment(data = d[d$censored, , drop = FALSE],
          arrow = grid::arrow(length = grid::unit(.08, "inches"))) +
        ggplot2::labs(x = "Observed interval (arrows denote right censoring)", y = "Observation") + ggtheme
      print(p)
      TRUE
    },
    .plot_hazard_function = function(...) FALSE,
    .plot_residuals = function(...) FALSE,
    .plot_model_comparison = function(...) FALSE,
    .plot_convergence = function(...) FALSE
  ))
