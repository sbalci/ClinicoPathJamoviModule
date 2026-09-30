skip_if_not_installed("gsDesign")

# The generated wrapper returns the results Group, so outputs are read as
# model$<item>$content. Private state needs the analysis object itself.
run_analysis <- function(...) {
  analysis <- classicalSurvivalPowerClass$new(
    options = classicalSurvivalPowerOptions$new(...), data = data.frame())
  analysis$init()
  analysis$run()
  analysis
}

lf_design <- function(...) {
  modifyList(list(lambda1 = 0.083, lambda2 = 0.042, Ts = 24, Tr = 12,
                  eta = 0, ratio = 1, alpha = 0.025, sided = 1,
                  entry = "unif", gamma = NA_real_), list(...))
}

test_that("default design reports the Lachin-Foulkes sample size, rounded up", {
  model <- classicalSurvivalPower(
    calculation_type = "sample_size", method = "lachin_foulkes",
    hazard_control = 0.083, hazard_treatment = 0.042,
    study_duration = 24, accrual_duration = 12, dropout_rate = 0,
    allocation_ratio = 1, alpha = 0.025, beta = 0.1, sided = "one_sided",
    entry_type = "unif", show_summary = TRUE, show_interpretation = TRUE
  )
  expect_true(inherits(model, "Group"))
  required <- do.call(gsDesign::nSurvival, c(lf_design(), list(beta = 0.1)))
  expect_equal(ceiling(required$n), 140)
  # Rounding to the nearest patient (139) would fall short of 90% power.
  expect_lt(.classicalSurvivalPower_lf_power(139, lf_design())$power, 0.9)

  content <- model$power_results$content
  expect_match(content, "Required Sample Size:</strong> 140 patients", fixed = TRUE)
  expect_match(content, "Statistical Power:</strong> 90%", fixed = TRUE)
  expect_match(content, "Power with 140 patients:</strong> 90.1%", fixed = TRUE)
  # Expected events at the displayed 140 patients, rounded up like the sample size.
  expect_equal(ceiling(140 * required$nEvents / required$n), 91)
  expect_match(content, "Required Events:</strong> 91 events", fixed = TRUE)
  expect_match(model$interpretation$content, "Total enrollment:</strong> 140 patients", fixed = TRUE)
  expect_match(model$interpretation$content, "Events required:</strong> 91 events", fixed = TRUE)
})

test_that("Lachin-Foulkes power inverts nSurvival across designs", {
  designs <- list(
    lf_design(), lf_design(sided = 2, alpha = 0.05),
    lf_design(entry = "expo", gamma = 0.1), lf_design(ratio = 2, eta = 0.02),
    lf_design(lambda1 = 0.042, lambda2 = 0.083, ratio = 0.5),
    lf_design(entry = "expo", gamma = -0.2, eta = 0.05, sided = 2, alpha = 0.05)
  )
  for (design in designs) {
    for (target_beta in c(0.5, 0.2, 0.1, 0.01, 1e-6)) {
      required <- do.call(gsDesign::nSurvival, c(design, list(beta = target_beta)))
      actual <- .classicalSurvivalPower_lf_power(required$n, design)
      expect_equal(actual$power, 1 - target_beta, tolerance = 1e-9)
      expect_equal(actual$events, required$nEvents, tolerance = 1e-9)
    }
  }
})

test_that("Lachin-Foulkes power rises with sample size and has no upper limit", {
  design <- lf_design()
  n80 <- do.call(gsDesign::nSurvival, c(design, list(beta = 0.2)))$n
  powers <- .classicalSurvivalPower_lf_power(seq(n80 / 2, 2 * n80, length.out = 50), design)$power
  expect_true(all(diff(powers) > 0))
  expect_equal(.classicalSurvivalPower_lf_power(n80, design)$power, 0.8, tolerance = 1e-9)
  expect_lt(powers[1], 0.8)
  expect_gt(powers[50], 0.8)

  # Independent value: pnorm((sqrt(n) |log HR| - z_a sqrt(V0)) / sqrt(V1)) with the
  # event probabilities integrated over entry time.
  expect_equal(.classicalSurvivalPower_lf_power(50, design)$power, 0.5030484415, tolerance = 1e-9)
  n40 <- do.call(gsDesign::nSurvival, c(design, list(beta = 0.6)))$n
  expect_equal(.classicalSurvivalPower_lf_power(n40, design)$power, 0.4, tolerance = 1e-9)

  # The old root search stopped above nSurvival(beta = .Machine$double.eps)$n (about 1388 here).
  limit <- do.call(gsDesign::nSurvival, c(design, list(beta = .Machine$double.eps)))$n
  big <- .classicalSurvivalPower_lf_power(c(2 * limit, 1e4, 1e6), design)$power
  expect_true(all(big > 1 - 1e-12))

  expect_error(.classicalSurvivalPower_lf_power(0, design), "positive finite")
  expect_error(.classicalSurvivalPower_lf_power(Inf, design), "positive finite")
  expect_error(.classicalSurvivalPower_lf_power(10, lf_design(lambda2 = 0.083)), "no finite sample size")
  # A study shorter than accrual gives a negative event probability; nSurvival warns "NaNs produced".
  expect_error(suppressWarnings(.classicalSurvivalPower_lf_power(10, lf_design(Ts = 5))), "no finite sample size")
  expect_error(.classicalSurvivalPower_lf_power(10, lf_design(entry = "expo", gamma = 0)), "no finite sample size")
})

test_that("Lachin-Foulkes power mode reports the solved power at the entered sample size", {
  model <- classicalSurvivalPower(
    calculation_type = "power", method = "lachin_foulkes",
    sample_size_input = 50, show_interpretation = FALSE, export_power_curve = TRUE
  )
  content <- model$power_results$content
  # The old ratio formula printed 72.1% ("Power (Estimated)"); the design gives 50.3%.
  expect_match(content, "Statistical Power:</strong> 50.3%", fixed = TRUE)
  expect_match(content, "Entered Sample Size:</strong> 50 patients", fixed = TRUE)
  expect_match(content, "Expected Events:</strong> 32 events", fixed = TRUE)
  expect_false(grepl("Power (Estimated)", content, fixed = TRUE))
  expect_match(model$export_summary$content, "Power curve:</strong> 50 points", fixed = TRUE)

  # A sample size the UI allows, far above the old solver limit.
  large <- classicalSurvivalPower(
    calculation_type = "power", sample_size_input = 10000,
    show_interpretation = FALSE, show_power_plot = TRUE, export_power_curve = TRUE
  )
  expect_match(large$power_results$content, "Statistical Power:</strong> 100%", fixed = TRUE)
  expect_match(large$export_summary$content, "Power curve:</strong> 50 points computed", fixed = TRUE)
  expect_match(large$export_summary$content, "plotted in 'Power Curve Analysis'", fixed = TRUE)
})

test_that("Schoenfeld power is computed directly and is right for HR > 1", {
  model <- classicalSurvivalPower(
    calculation_type = "power", method = "schoenfeld",
    events_input = 50, show_interpretation = FALSE
  )
  expected <- gsDesign::nEvents(hr = 0.6, alpha = 0.025, ratio = 1, sided = 1, n = 50, tbl = TRUE)$Power
  expect_match(model$power_results$content,
               paste0("Statistical Power:</strong> ", round(expected * 100, 1), "%"), fixed = TRUE)
  expect_match(model$power_results$content, "Entered Events:</strong> 50 events", fixed = TRUE)

  # HR > 1 has the power of 1 / HR, which every gsDesign version computes correctly
  # (nEvents() got HR > 1 wrong before 3.10.0).
  worse <- run_analysis(calculation_type = "power", method = "schoenfeld", hazard_ratio = 1.5,
                        events_input = 100, allocation_ratio = 2, sided = "two_sided", alpha = 0.05)
  reference <- gsDesign::nEvents(hr = 1 / 1.5, alpha = 0.05, ratio = 2, sided = 2, n = 100, tbl = TRUE)$Power
  expect_equal(worse$.__enclos_env__$private$.results_data$power, reference, tolerance = 1e-12)
  # Allocation r:1 and 1:r carry the same information.
  mirrored <- run_analysis(calculation_type = "power", method = "schoenfeld", hazard_ratio = 1.5,
                           events_input = 100, allocation_ratio = 0.5, sided = "two_sided", alpha = 0.05)
  expect_equal(mirrored$.__enclos_env__$private$.results_data$power, reference, tolerance = 1e-12)

  detect <- classicalSurvivalPower(calculation_type = "hazard_ratio", method = "schoenfeld", events_input = 50)
  expect_match(detect$power_results$content, "Entered Events:</strong> 50 events", fixed = TRUE)
})

test_that("Schoenfeld curve agrees with the entered events and increases", {
  analysis <- run_analysis(
    calculation_type = "power", method = "schoenfeld", events_input = 50,
    power_plot_range = "25,100", export_power_curve = TRUE, show_interpretation = FALSE
  )
  priv <- analysis$.__enclos_env__$private
  curve <- priv$.generate_power_curve_data()
  expect_equal(nrow(curve), 50L)
  expect_equal(range(curve$amount), c(25, 100))
  expect_null(attr(curve, "note"))
  expect_true(any(curve$amount == 50))
  expect_equal(curve$power[curve$amount == 50], priv$.results_data$power, tolerance = 1e-12)
  expect_true(all(diff(curve$power) > 0))
  expect_match(analysis$results$export_summary$content, "Number of events versus power", fixed = TRUE)
})

test_that("power curve range is sorted, positive and contains the design point", {
  curve_of <- function(...) {
    analysis <- run_analysis(export_power_curve = TRUE, ...)
    list(curve = analysis$.__enclos_env__$private$.generate_power_curve_data(), analysis = analysis)
  }
  # 5 events (the UI minimum) used to give min = max = 10 and an empty plot;
  # 6 to 9 events put the point left of the curve.
  for (events in 5:9) {
    curve <- curve_of(method = "schoenfeld", calculation_type = "power", events_input = events)$curve
    expect_equal(nrow(curve), 50L)
    expect_true(any(curve$amount == events))
    expect_gt(min(curve$amount), 0)
  }
  # Reversed and zero-start ranges used to blank the curve without a word.
  reversed <- curve_of(power_plot_range = "500,50")
  expect_equal(range(reversed$curve$amount), c(50, 500))
  expect_match(attr(reversed$curve, "note"), "changed to 50 to 500", fixed = TRUE)
  expect_match(reversed$analysis$results$export_summary$content, "changed to 50 to 500", fixed = TRUE)
  zero <- curve_of(power_plot_range = "0,100")$curve
  expect_gt(min(zero$amount), 0)
  expect_true(any(zero$amount == 140))  # the required sample size stays in view
  unreadable <- curve_of(power_plot_range = "fifty")$curve
  expect_equal(nrow(unreadable), 50L)
  expect_match(attr(unreadable, "note"), "two numbers", fixed = TRUE)
})

test_that("the design-point marker sits on the curve at the displayed count", {
  skip_if_not_installed("ggplot2")
  marker_of <- function(...) {
    analysis <- run_analysis(show_power_plot = TRUE, ...)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    analysis$.__enclos_env__$private$.plot_power_curve(NULL, NULL, NULL)
    points <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), ggplot2::last_plot()$layers)
    list(point = points[[1]]$data, analysis = analysis)
  }
  # 161.07 events are required; 162 are displayed, which give more than the 90% target.
  schoenfeld <- marker_of(method = "schoenfeld")
  achieved <- gsDesign::nEvents(hr = 0.6, alpha = 0.025, ratio = 1, sided = 1, n = 162, tbl = TRUE)$Power
  expect_equal(schoenfeld$point$amount, 162)
  expect_equal(schoenfeld$point$power, achieved, tolerance = 1e-12)
  expect_match(schoenfeld$analysis$results$power_results$content,
               "Power with 162 events:</strong> 90.2%", fixed = TRUE)
  lachin <- marker_of()
  expect_equal(lachin$point$amount, 140)
  expect_equal(lachin$point$power, .classicalSurvivalPower_lf_power(140, lf_design())$power, tolerance = 1e-12)
})

test_that("invalid designs are rejected with a specific message", {
  # jamovi shows a rejected analysis in its error state; through the R wrapper the
  # rejection is raised. It used to be caught and printed as red text, or not
  # caught at all and shown as 'Required Sample Size: Inf/NaN'.
  for (mode in c("sample_size", "power")) {
    expect_error(classicalSurvivalPower(calculation_type = mode, hazard_treatment = 0.083),
                 "hazard rates are equal")
    expect_error(classicalSurvivalPower(calculation_type = mode, study_duration = 5),
                 "shorter than the accrual duration")
    expect_error(classicalSurvivalPower(calculation_type = mode, entry_type = "expo"),
                 "non-zero gamma")
    expect_error(classicalSurvivalPower(calculation_type = mode, entry_type = "expo", gamma = 0.083),
                 "undefined")
  }
  expect_error(classicalSurvivalPower(calculation_type = "events"), "cannot perform")
  expect_error(classicalSurvivalPower(calculation_type = "hazard_ratio"), "cannot perform")
  expect_error(classicalSurvivalPower(method = "schoenfeld", hazard_ratio = 1), "hazard ratio of 1")

  # Still defined: Schoenfeld power at HR = 1 is the one-sided alpha; exponential entry with gamma != 0.
  null_power <- classicalSurvivalPower(method = "schoenfeld", calculation_type = "power", hazard_ratio = 1)
  expect_match(null_power$power_results$content, "Statistical Power:</strong> 2.5%", fixed = TRUE)
  expo <- classicalSurvivalPower(entry_type = "expo", gamma = 0.1)
  expect_match(expo$power_results$content, "Required Sample Size:</strong>", fixed = TRUE)
})

test_that("the reporting summary only says 'plotted' when a curve was computed", {
  analysis <- classicalSurvivalPowerClass$new(
    options = classicalSurvivalPowerOptions$new(export_power_curve = TRUE, show_power_plot = TRUE),
    data = data.frame()
  )
  analysis$init()
  analysis$.__enclos_env__$private$.generate_export_summary()  # before any calculation
  content <- analysis$results$export_summary$content
  expect_match(content, "0 points computed", fixed = TRUE)
  expect_false(grepl("plotted", content, fixed = TRUE))
})

test_that("power is set by beta alone and the unused 'power' option is gone", {
  expect_false("power" %in% names(formals(classicalSurvivalPower)))
  eighty <- classicalSurvivalPower(beta = 0.2)
  expect_match(eighty$power_results$content, "Statistical Power:</strong> 80%", fixed = TRUE)
})

test_that("the power curve is redrawn when any option it depends on changes", {
  results <- classicalSurvivalPowerResults$new(options = classicalSurvivalPowerOptions$new())
  clear_with <- unlist(results$power_plot$.__enclos_env__$private$.clearWith)
  expect_true(all(c("hazard_control", "hazard_treatment", "hazard_ratio", "study_duration",
                    "accrual_duration", "dropout_rate", "allocation_ratio", "alpha", "beta",
                    "sided", "entry_type", "gamma", "sample_size_input", "events_input",
                    "power_plot_range") %in% clear_with))
})
