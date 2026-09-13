# Independent acceptance cases from the full survivalPower audit.
sp_fixed <- function(...) {
  a <- survivalPowerClass$new(options = survivalPowerOptions$new(...), data = data.frame())
  a$run()
  a
}
sp_private <- function(a) a$.__enclos_env__$private
sp_row <- function(table, label) table$asDF$value[table$asDF$parameter == label]

test_that("achieved power drives assessment, independently of requested power", {
  for (target in c(0.5, 0.8, 0.99)) {
    a <- sp_fixed(analysis_type = "power", sample_size_input = 10,
                  control_median_survival = 240, accrual_period = 1,
                  follow_up_period = 0, power_level = target, show_summary = TRUE)
    expect_equal(sp_private(a)$primary_numbers$power, 0.05002987929, tolerance = 1e-8)
    row <- subset(a$results$regulatory_table$asDF, regulatory_aspect == "Statistical Power")
    expect_equal(row$compliance_status, "Insufficient")
    expect_match(a$results$notices$content, "Low Calculated Power", fixed = TRUE)
    expect_match(a$results$notices$content, "Few Expected Events", fixed = TRUE)
    expect_false(any(grepl("clinically realistic", paste(a$results$regulatory_table$asDF), ignore.case = TRUE)))
  }
})

test_that("solved durations and calendar event milestones propagate to displays", {
  a <- sp_fixed(analysis_type = "duration", sample_size_input = 500,
                show_summary = TRUE, show_interpretation = TRUE)
  expect_equal(sp_private(a)$primary_numbers$duration, 45.5756201746, tolerance = 1e-4)
  timeline <- a$results$accrual_timeline_plot$state$data
  expect_equal(max(timeline$end), sp_private(a)$primary_numbers$duration)
  expect_false("Analysis" %in% timeline$phase)
  expect_match(a$results$natural_language_summary$content,
               "24 months recruitment + 21.6 months follow-up = 45.6 months", fixed = TRUE)
  expect_equal(max(a$results$survival_curves_plot$state$data$time), max(timeline$end))
  a <- sp_fixed(analysis_type = "duration", sample_size_input = 1000)
  # Independent integration of uniform entry and competing event/dropout hazards.
  event_count <- function(t) {
    rates <- log(2) / 12 * c(1, 0.75)
    eta <- -log1p(-0.05) / 12
    sum(vapply(rates, function(rate) 500 / 24 * integrate(function(entry) {
      rate / (rate + eta) * (-expm1(-(rate + eta) * (t - entry)))
    }, 0, min(t, 24))$value, 0))
  }
  half <- uniroot(function(t) event_count(t) - 190, c(0, 24))$root
  expect_equal(half, 15.3372909008, tolerance = 1e-6)
  expect_equal(sp_row(a$results$study_duration_results, "50% Events Time"), "15.3 months")
})

test_that("detectable effect summaries use the solved HR and its events", {
  analyses <- lapply(c(0.2, 0.75), function(hr) sp_fixed(
    analysis_type = "effect_size", sample_size_input = 200, effect_size = hr,
    show_summary = TRUE, show_interpretation = TRUE))
  for (a in analyses) {
    d <- sp_private(a)$.resolved_design()
    expect_equal(d$hr, 0.60345916595, tolerance = 1e-5)
    expect_equal(d$events, 123.0646033385, tolerance = 0.002)
    expect_equal(sp_row(a$results$effect_size_results, "Expected Events"), "123 events")
    curve <- a$results$survival_curves_plot$state$data
    treatment <- subset(curve, group == "Treatment")
    expect_equal(treatment$survival, exp(-log(2) / 12 * d$hr * treatment$time))
  }
  expect_equal(analyses[[1]]$results$effect_size_results$asDF,
               analyses[[2]]$results$effect_size_results$asDF)
})

test_that("cluster enrollment uses complete allocation blocks and discloses limitations", {
  for (ratio in c(0.5, 1, 2, 1.5)) {
    a <- sp_fixed(study_design = "cluster_randomized", allocation_ratio = ratio)
    p <- sp_private(a)
    clusters <- p$.cluster_allocation(p$primary_numbers$n)
    expect_equal(clusters, round(clusters))
    expect_equal(unname(clusters[1] / clusters[2]), ratio)
    expect_equal(sum(clusters) * 50, p$primary_numbers$n)
    expect_match(a$results$notices$content, "Cluster Design Approximation", fixed = TRUE)
  }
  small <- sp_fixed(study_design = "cluster_randomized", cluster_size = 100,
                    icc = 0.01, effect_size = 0.1)
  expect_equal(sp_private(small)$primary_numbers$n, 200)
  expect_match(small$results$notices$content, "Few Independent Clusters", fixed = TRUE)
  invalid <- sp_fixed(study_design = "cluster_randomized", analysis_type = "power",
                      cluster_size = 100, sample_size_input = 250)
  expect_match(invalid$results$notices$content, "ERROR: Incomplete Cluster Allocation", fixed = TRUE)
  expect_null(sp_private(invalid)$primary_numbers$power)
})

test_that("event-free simulations have finite uncertainty and explicit diagnostics", {
  expect_warning(a <- sp_fixed(analysis_type = "power", sample_size_input = 10,
    control_median_survival = 240, accrual_period = 1, follow_up_period = 0,
    run_simulation_validation = TRUE, simulation_runs = 1000), NA)
  sim <- sp_private(a)$simulation_cache
  expect_equal(sim$empirical_power, 0)
  expect_equal(c(sim$ci_lower, sim$ci_upper), as.numeric(binom.test(0, 1000)$conf.int))
  expect_gt(sim$ci_upper, 0)
  expect_equal(sim$zero_event_trials, 984)
  expect_equal(sim$failed_tests, 0)
  expect_equal(sim$valid_sims, 1000)
  expect_false(sim$convergence$converged)
  expect_match(a$results$notices$content, "Event-free Simulations", fixed = TRUE)
})

test_that("NI objectives are consistent across every narrative and analysis mode", {
  for (mode in c("sample_size", "power", "effect_size", "duration")) {
    a <- sp_fixed(test_type = "non_inferiority", effect_size = 1, alpha_level = 0.025,
                  analysis_type = mode, sample_size_input = 1000,
                  show_summary = TRUE, show_interpretation = TRUE, show_glossary = TRUE)
    for (txt in c(a$results$clinical_interpretation$content,
                  a$results$natural_language_summary$content)) {
      expect_match(txt, "rule out a hazard ratio of 1.25", fixed = TRUE)
      expect_match(txt, "one-sided", fixed = TRUE)
      expect_false(grepl("detect a hazard ratio of 1", txt, fixed = TRUE))
    }
    expect_match(a$results$statistical_glossary$content, "instantaneous event rates", fixed = TRUE)
    expect_false(grepl("means 25% lower risk", a$results$statistical_glossary$content, fixed = TRUE))
    if (mode == "effect_size") {
      expect_match(a$results$clinical_interpretation$content, "largest true hazard ratio", fixed = TRUE)
      expect_false(grepl("minimum detectable", a$results$clinical_interpretation$content,
                        ignore.case = TRUE))
    }
  }
})

test_that("Cox headlines, plots, sensitivity and inverse calculations share a method", {
  for (hr in c(0.5, 0.75, 1.5)) {
    a <- sp_fixed(test_type = "cox_regression", effect_size = hr, allocation_ratio = 5,
                  sensitivity_analysis = TRUE)
    p <- sp_private(a)
    n <- p$primary_numbers$n
    curve <- a$results$sample_size_plot$state$data
    expect_equal(curve$sample_size[abs(curve$hazard_ratio - hr) < 1e-8], n)
    expect_equal(p$.sensitivity_value(), n)
    power <- sp_fixed(test_type = "cox_regression", effect_size = hr, allocation_ratio = 5,
                      analysis_type = "power", sample_size_input = n)
    power_curve <- a$results$power_curve_plot$state$data
    expect_equal(power_curve$power[abs(power_curve$hazard_ratio - hr) < 1e-8],
                 sp_private(power)$primary_numbers$power, tolerance = 1e-8)
    upstream <- gsDesign::nSurv(lambdaC = log(2) / 12, hr = hr, eta = -log1p(-0.05) / 12,
      ratio = 0.2, gamma = n / 24, R = 24, T = 36, minfup = 12,
      alpha = 0.025, sided = 1, beta = NULL, method = "LachinFoulkes")
    opposite <- gsDesign::nSurv(lambdaC = log(2) / 12, hr = hr, eta = -log1p(-0.05) / 12,
      ratio = 0.2, gamma = n / 24, R = 24, T = 36, minfup = 12,
      alpha = 0.975, sided = 1, beta = NULL, method = "LachinFoulkes")
    expect_equal(sp_private(power)$primary_numbers$power,
                 upstream$power + 1 - opposite$power, tolerance = 1e-8)
  }
  solved <- sp_fixed(test_type = "cox_regression", analysis_type = "effect_size")
  hr <- sp_private(solved)$primary_numbers$hr_detectable
  verification <- sp_fixed(test_type = "cox_regression", analysis_type = "power", effect_size = hr)
  expect_equal(sp_private(verification)$primary_numbers$power, 0.8, tolerance = 1e-5)
})

test_that("equivalent effect specifications support signed differences and full HR bounds", {
  for (hr in c(0.1, 0.15, 0.75, 1.5, 5)) {
    delta <- exp(-log(2) * hr) - 0.5
    rmst <- function(rate) -expm1(-rate * 36) / rate
    rmst_delta <- rmst(log(2) / 12 * hr) - rmst(log(2) / 12)
    direct <- sp_fixed(effect_size = hr)
    for (args in list(list(effect_size_type = "survival_difference", effect_size = delta),
                     list(effect_size_type = "median_ratio", effect_size = 1 / hr),
                     list(effect_size_type = "rmst_difference", rmst_difference = rmst_delta,
                          rmst_tau = 36))) {
      converted <- do.call(sp_fixed, args)
      expect_equal(sp_private(converted)$primary_numbers$n, sp_private(direct)$primary_numbers$n)
    }
  }
  five_points <- sp_fixed(effect_size_type = "survival_difference", effect_size = 0.05)
  expect_true(is.finite(sp_private(five_points)$primary_numbers$n))
  harm <- sp_fixed(effect_size_type = "rmst_difference", rmst_difference = -3)
  expect_gt(sp_private(harm)$.get_effect_hr(), 1)
})

test_that("unsupported sensitivity modes produce no placeholder scenarios or hidden plots", {
  for (mode in c("effect_size", "duration")) {
    a <- sp_fixed(analysis_type = mode, sample_size_input = 500,
                  sensitivity_analysis = TRUE)
    expect_equal(a$results$sensitivity_analysis_table$rowCount, 0)
    expect_null(a$results$sensitivity_plot$state)
    expect_null(a$results$sample_size_plot$state)
    expect_match(a$results$notices$content, "Sensitivity Analysis Not Applicable", fixed = TRUE)
  }
})

test_that("preset help accurately describes R and GUI behavior", {
  a <- sp_fixed(clinical_preset = "cardio_prevention")
  expect_equal(sp_private(a)$primary_numbers, sp_private(sp_fixed())$primary_numbers)
  spec <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "survivalPower.a.yaml"))
  preset <- Filter(function(x) x$name == "clinical_preset", spec$options)[[1]]
  expect_match(preset$description$R, "R calls use the explicit numeric", fixed = TRUE)
})

test_that("simulation failures retain events and warnings without claiming convergence", {
  # Exercise a mix of no events, failed tests, warned rejections and non-rejections.
  local_mocked_bindings(survdiff = function(formula, ...) {
    events <- sum(environment(formula)$sim_data$event)
    if (events == 3) stop("injected fitting failure")
    if (events == 4) warning("injected numerical warning")
    list(chisq = if (events == 4) 10 else 0)
  }, .package = "survival")
  a <- survivalPowerClass$new(options = survivalPowerOptions$new(
    analysis_type = "power", run_simulation_validation = TRUE, simulation_runs = 1000),
    data = data.frame())
  p <- sp_private(a)
  trial <- 0L
  unlockBinding(".simulate_trial_data", p)
  p$.simulate_trial_data <- function(...) {
    trial <<- trial + 1L
    events <- c(0, 3, 4, 5)[(trial - 1L) %% 4L + 1L]
    data.frame(event = c(rep(1, events), rep(0, 6 - events)))
  }
  expect_warning(a$run(), NA)
  sim <- p$simulation_cache
  expect_equal(sim$failed_tests, 250)
  expect_equal(sim$zero_event_trials, 250)
  expect_equal(sim$numerical_warnings, 250)
  expect_equal(sim$valid_sims, 750)
  expect_equal(sim$avg_events, 3)
  expect_equal(sim$event_counts[2], 3)
  expect_true(is.na(sim$p_values[2]))
  expect_equal(sim$empirical_power, 1 / 3)
  expect_equal(c(sim$ci_lower, sim$ci_upper), as.numeric(binom.test(250, 750)$conf.int))
  expect_false(sim$convergence$converged)
  expect_match(sim$convergence$message, "Validation is limited", fixed = TRUE)
  expect_match(a$results$notices$content, "conditional on the 750 evaluable trials", fixed = TRUE)
  expect_match(a$results$notices$content, "Simulation Numerical Warnings", fixed = TRUE)
  expect_equal(a$results$simulation_validation_table$asDF$agreement[1],
               "Conditional estimate; inspect failures")
  p$simulation_cache <- NULL
  p$.simulate_trial_data <- function(...) data.frame(event = rep(1, 3))
  expect_null(p$.run_simulation_analysis())
  expect_match(a$results$notices$content, "No simulated log-rank tests produced", fixed = TRUE)
})

test_that("Cox simulation comparison uses achieved power of the selected method", {
  for (mode in c("sample_size", "power")) {
    a <- sp_fixed(test_type = "cox_regression", analysis_type = mode,
      effect_size = 0.5, allocation_ratio = 5, run_simulation_validation = TRUE,
      simulation_runs = 1000)
    p <- sp_private(a)
    d <- p$.resolved_design()
    expected <- p$.basic_power_calc(d$n, d$hr, d$alpha)
    row <- subset(a$results$simulation_validation_table$asDF, metric == "Statistical Power")
    expect_equal(row$analytical, expected)
    if (mode == "power") expect_equal(row$analytical, p$primary_numbers$power)
    expect_false(grepl("Schoenfeld approximation", a$results$notices$content, fixed = TRUE))
  }
})

test_that("sequential Cox power preserves null alpha and gates missing upstream support", {
  for (spending in c("obrien_fleming", "pocock")) {
    a <- sp_fixed(test_type = "cox_regression", analysis_type = "power",
      interim_analyses = 2, alpha_spending = spending, effect_size = 1)
    expect_equal(sp_private(a)$primary_numbers$power, 0.05, tolerance = 1e-6)
  }
  a <- survivalPowerClass$new(options = survivalPowerOptions$new(
    test_type = "cox_regression", interim_analyses = 1,
    alpha_spending = "obrien_fleming"), data = data.frame())
  p <- sp_private(a)
  unlockBinding(".has_gs_surv_power", p)
  p$.has_gs_surv_power <- function() FALSE
  a$run()
  expect_match(a$results$notices$content, "ERROR: Cox Sequential Power Unavailable", fixed = TRUE)
  expect_null(sp_private(a)$primary_numbers$n)
  expect_error(sp_private(a)$.cox_power(200, 0.75, 0.05, 12, 24, 12), "gsSurvPower")
})

test_that("sequential Cox enrollment attains target power in both effect directions", {
  for (spending in c("obrien_fleming", "pocock")) {
    for (hr in c(0.5, 1.5)) {
      for (interim in c(1L, 5L)) {
        args <- list(test_type = "cox_regression", interim_analyses = interim,
          alpha_spending = spending, effect_size = hr, allocation_ratio = 5)
        sized <- do.call(sp_fixed, args)
        n <- sp_private(sized)$primary_numbers$n
        powered <- do.call(sp_fixed, c(args,
          list(analysis_type = "power", sample_size_input = n)))
        actual <- sp_private(powered)$primary_numbers$power
        expect_gte(actual, 0.799)
        expect_lt(actual, 0.81)
      }
    }
  }
})

test_that("notices prioritize severity and deduplicate repeated diagnostics", {
  a <- sp_fixed()
  p <- sp_private(a)
  p$.noticeList <- list()
  for (type in c("INFO", "WARNING", "ERROR", "STRONG_WARNING", "ERROR")) {
    p$.addNotice(type, type, "Diagnostic")
  }
  expect_length(p$.noticeList, 4)
  blocks <- strsplit(a$results$notices$content, "\n\n", fixed = TRUE)[[1]]
  expect_match(blocks[1], "^ERROR: ERROR")
  expect_match(blocks[2], "^STRONG WARNING: STRONG_WARNING")
  expect_match(blocks[3], "^WARNING: WARNING")
  expect_match(blocks[4], "^INFO")
})
