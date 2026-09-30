skip_if_not_installed("gsDesign")
# A parked analysis (T/P/D menuGroup) is deleted from the generated module.
skip_if_not(exists("classicalSurvivalPowerClass"), "classicalSurvivalPower is not in this module")

# The generated wrapper returns the results Group, so outputs are read as
# model$<item>$content. Private state needs the analysis object itself.
run_analysis <- function(...) {
  analysis <- classicalSurvivalPowerClass$new(
    options = classicalSurvivalPowerOptions$new(...), data = data.frame())
  analysis$init()
  analysis$run()
  analysis
}

# Draws the power curve from its image into a null device; returns the ggplot drawn.
draw_power_curve <- function(analysis) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::set_last_plot(NULL)
  expect_true(analysis$.__enclos_env__$private$.plot_power_curve(analysis$results$power_plot, NULL, NULL))
  ggplot2::last_plot()
}
layer_with <- function(plot, geom) {
  plot$layers[[which(vapply(plot$layers, function(layer) inherits(layer$geom, geom), logical(1)))]]
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
    list(point = layer_with(draw_power_curve(analysis), "GeomPoint")$data, analysis = analysis)
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

test_that("the design-point label stays inside the plot near 100% power", {
  skip_if_not_installed("ggplot2")
  label_of <- function(...) {
    analysis <- run_analysis(show_power_plot = TRUE, ...)
    plot <- draw_power_curve(analysis)
    grDevices::pdf(NULL)  # layer_data() builds the plot; keep it off the default device
    on.exit(grDevices::dev.off())
    text <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1)))
    list(label = ggplot2::layer_data(plot, text), point = analysis$results$power_plot$state$point)
  }
  # 0.05 above the point, the label passed the 100% end of the scale and ggplot dropped it.
  for (args in list(list(beta = 0.01), list(calculation_type = "power", sample_size_input = 10000),
                    list(method = "schoenfeld", calculation_type = "power", events_input = 500))) {
    shown <- do.call(label_of, args)
    expect_gt(shown$point$power, 0.95)
    expect_true(is.finite(shown$label$y))
    expect_equal(shown$label$y, shown$point$power - 0.05)
  }
  low <- label_of(calculation_type = "power", sample_size_input = 50)
  expect_equal(low$label$y, low$point$power + 0.05)
  # The design point at the right end of the range: the label ends there, inside the panel.
  expect_equal(label_of(power_plot_range = "0,100")$label$hjust, 1)
})

test_that("the effect size reads as an increase for HR > 1 and a reduction for HR < 1", {
  effect <- function(...) classicalSurvivalPower(...)$interpretation$content
  expect_match(effect(), "HR = 0.506 represents a 49.4% reduction in hazard", fixed = TRUE)
  treatment_worse <- effect(hazard_treatment = 0.12)
  expect_match(treatment_worse, "HR = 1.446 represents a 44.6% increase in hazard", fixed = TRUE)
  expect_false(grepl("-[0-9.]+% reduction", treatment_worse))
  expect_match(effect(method = "schoenfeld", hazard_ratio = 1.5),
               "HR = 1.5 represents a 50% increase in hazard", fixed = TRUE)
  expect_match(effect(method = "schoenfeld", calculation_type = "power", hazard_ratio = 1),
               "HR = 1 represents no difference in hazard", fixed = TRUE)
})

test_that("Lachin-Foulkes sample size rounds each arm up and shows the allocation ratio", {
  # 2:1 needs n = 159.01: 106.01 treatment and 53.00 control, so 107 + 54 = 161, not 160.
  two_to_one <- classicalSurvivalPower(allocation_ratio = 2)
  expect_match(two_to_one$power_results$content,
               "Required Sample Size:</strong> 161 patients (107 treatment, 54 control)", fixed = TRUE)
  # The power of the arms shown, at their own 107:54 ratio.
  expect_match(two_to_one$power_results$content,
               paste0("Power with 161 patients:</strong> ",
                      round(100 * .classicalSurvivalPower_lf_power(161, lf_design(ratio = 107 / 54))$power, 1), "%"),
               fixed = TRUE)
  for (ratio in c(0.5, 1, 2, 3)) {
    model <- classicalSurvivalPower(allocation_ratio = ratio)
    required <- do.call(gsDesign::nSurvival, c(lf_design(ratio = ratio), list(beta = 0.1)))
    share <- required$n * c(ratio, 1) / (1 + ratio)
    arms <- ceiling(share)
    expect_true(all(arms >= share))
    split <- paste0(sum(arms), " patients (", arms[1], " treatment, ", arms[2], " control)")
    expect_match(model$power_results$content, paste0("Required Sample Size:</strong> ", split), fixed = TRUE)
    expect_match(model$interpretation$content, paste0("Total enrollment:</strong> ", split), fixed = TRUE)
    expect_match(model$power_results$content, paste0("Allocation Ratio (T:C):</strong> ", ratio, ":1"), fixed = TRUE)
  }
  entered <- classicalSurvivalPower(calculation_type = "power", allocation_ratio = 2)
  expect_match(entered$power_results$content, "Allocation Ratio (T:C):</strong> 2:1", fixed = TRUE)
})

test_that("unticking the study design summary clears the previous summary", {
  analysis <- classicalSurvivalPowerClass$new(
    options = classicalSurvivalPowerOptions$new(show_summary = FALSE), data = data.frame())
  analysis$init()
  # jamovi restores this item from the previous run: show_summary is not in its clearWith.
  analysis$results$power_results$setContent("<h4>Power Analysis Results</h4><p>previous run</p>")
  analysis$run()
  expect_identical(analysis$results$power_results$content, "")
})

test_that("both plots draw on jamovi's export path from the image state alone", {
  skip_if_not_installed("ggplot2")
  # Export, resize and .omv reopen: a new analysis object, init() only, the saved image
  # state restored, then the engine's own .createImage(). .run() is never called.
  export_draw <- function(live, name) {
    fresh <- classicalSurvivalPowerClass$new(options = live$options, data = data.frame())
    fresh$init()
    image <- fresh$results[[name]]
    image$setState(live$results[[name]]$state)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    fresh$.createImage(image$.__enclos_env__$private$.renderFun, image)
  }
  for (args in list(list(), list(calculation_type = "power", sample_size_input = 300),
                    list(method = "schoenfeld"), list(method = "schoenfeld", calculation_type = "hazard_ratio"))) {
    live <- do.call(run_analysis, c(list(show_power_plot = TRUE, show_timeline_plot = TRUE), args))
    state <- live$results$power_plot$state
    expect_true(is.data.frame(state$curve))
    # Plot-ready numbers and labels only: no fitted model, no dataset.
    expect_true(all(vapply(state, function(x) is.null(x) || is.data.frame(x) || is.atomic(x), logical(1))))
    expect_lt(length(serialize(state, NULL)), 5000)
    expect_true(export_draw(live, "power_plot"))
    if (live$options$method == "lachin_foulkes") {
      expect_equal(live$results$timeline_plot$state, list(accrual_duration = 12, study_duration = 24))
      expect_true(export_draw(live, "timeline_plot"))
    }
  }
  # No state (the calculation stopped, or the plot was off during the run): nothing drawn.
  empty <- classicalSurvivalPowerClass$new(
    options = classicalSurvivalPowerOptions$new(show_power_plot = TRUE, show_timeline_plot = TRUE),
    data = data.frame())
  empty$init()
  expect_false(empty$.__enclos_env__$private$.plot_power_curve(empty$results$power_plot, NULL, NULL))
  expect_false(empty$.__enclos_env__$private$.plot_timeline(empty$results$timeline_plot, NULL, NULL))
})

test_that("an exponential-entry overflow is named as such, not blamed on the design", {
  big <- list(hazard_control = 10, hazard_treatment = 8, dropout_rate = 1, entry_type = "expo",
              accrual_duration = 60, study_duration = 120)
  for (mode in c("sample_size", "power")) {
    expect_error(do.call(classicalSurvivalPower, c(big, gamma = -5, calculation_type = mode)),
                 "Numerical overflow")
  }
  # The rejection matches gsDesign exactly: exp((10 + 1 - gamma) * 60) is Inf below this gamma.
  edge <- 11 - log(.Machine$double.xmax) / 60
  design <- function(gamma) {
    c(lf_design(lambda1 = 10, lambda2 = 8, eta = 1, Tr = 60, Ts = 120, entry = "expo", gamma = gamma),
      list(beta = 0.1))
  }
  expect_true(is.nan(do.call(gsDesign::nSurvival, design(edge - 1e-6))$n))
  expect_true(is.finite(do.call(gsDesign::nSurvival, design(edge + 1e-6))$n))
  expect_error(do.call(classicalSurvivalPower, c(big, gamma = edge - 1e-6)), "Numerical overflow")
  computed <- do.call(classicalSurvivalPower, c(big, gamma = edge + 1e-6))
  expect_match(computed$power_results$content, "Required Sample Size:</strong>", fixed = TRUE)
})

test_that("the power shown is that of the displayed arms, and reaches the target", {
  results_of <- function(analysis) analysis$.__enclos_env__$private$.results_data
  # 10:1 needs n = 38.08, shown as 35 treatment + 4 control. The nominal 10:1 split of 39 has
  # 81.0% power; the 35/4 arms have 85.7% (closed form, integrated over entry: 0.8570519726).
  small <- run_analysis(hazard_control = 0.3, hazard_treatment = 0.05, allocation_ratio = 10,
                        beta = 0.2, show_power_plot = TRUE)
  content <- small$results$power_results$content
  expect_match(content, "Required Sample Size:</strong> 39 patients (35 treatment, 4 control)", fixed = TRUE)
  expect_match(content, "Power with 39 patients:</strong> 85.7%", fixed = TRUE)
  expect_equal(results_of(small)$achieved_power, 0.8570519726, tolerance = 1e-9)
  # The marker is that design point (the arms' power). The curve is drawn at the PLANNED
  # 10:1 allocation, so at 39 it shows the nominal 81.0%, not the arms' 85.7%.
  state <- small$results$power_plot$state
  expect_equal(state$point$amount, 39)
  expect_equal(state$point$power, results_of(small)$achieved_power, tolerance = 1e-12)
  planned <- lf_design(lambda1 = 0.3, lambda2 = 0.05, ratio = 10)
  expect_equal(state$curve$power[state$curve$amount == 39],
               .classicalSurvivalPower_lf_power(39, planned)$power, tolerance = 1e-12)
  expect_lt(state$curve$power[state$curve$amount == 39], state$point$power - 0.04)

  # Each arm rounded up can still fall short: n = 3.39 at 0.4:1 gives 1 treatment + 3 control,
  # with 49.9% power for a 50% target. A treatment patient is added (closed form 0.6714852691);
  # a control patient would have lowered the power to 47.7%.
  short <- lf_design(lambda1 = 0.005, lambda2 = 0.1, alpha = 0.05)
  expect_lt(.classicalSurvivalPower_lf_power(4, modifyList(short, list(ratio = 1 / 3)))$power, 0.5)
  bumped <- run_analysis(hazard_control = 0.005, hazard_treatment = 0.1, allocation_ratio = 0.4,
                         beta = 0.5, alpha = 0.05)
  expect_match(bumped$results$power_results$content,
               "Required Sample Size:</strong> 5 patients (2 treatment, 3 control)", fixed = TRUE)
  expect_equal(results_of(bumped)$achieved_power, 0.6714852691, tolerance = 1e-9)
  # ... and the summary says why the total exceeds the rounded shares
  expect_match(bumped$results$power_results$content,
               "1 patient(s) were added beyond the arms' rounded shares", fixed = TRUE)
  expect_false(grepl("were added beyond", content, fixed = TRUE))

  # Across ratios and effect sizes: every arm covers its share of n, and the power of the
  # arms shown, which is the power displayed, is at least the target.
  for (ratio in c(0.1, 0.25, 0.4, 1, 2.5, 4, 10)) for (treatment in c(0.005, 0.02, 0.06)) for (beta in c(0.05, 0.2, 0.5)) {
    analysis <- run_analysis(hazard_treatment = treatment, allocation_ratio = ratio, beta = beta)
    arms <- results_of(analysis)$arm_sizes
    design <- lf_design(lambda2 = treatment, ratio = ratio)
    share <- do.call(gsDesign::nSurvival, c(design, list(beta = beta)))$n * c(ratio, 1) / (1 + ratio)
    expect_true(all(arms >= share))
    at_arms <- .classicalSurvivalPower_lf_power(sum(arms), modifyList(design, list(ratio = arms[[1]] / arms[[2]])))$power
    expect_equal(results_of(analysis)$achieved_power, at_arms, tolerance = 1e-12)
    expect_gte(at_arms, 1 - beta)
    expect_match(analysis$results$power_results$content,
                 paste0("Power with ", sum(arms), " patients:</strong> ", round(100 * at_arms, 1), "%"), fixed = TRUE)
  }
})

test_that("an HR that displays as 1 reads 'no difference', not '0% increase'", {
  effect <- function(...) classicalSurvivalPower(...)$interpretation$content
  for (hr in c(1.0004, 0.9996)) {
    shown <- effect(method = "schoenfeld", calculation_type = "power", hazard_ratio = hr)
    expect_match(shown, "HR = 1 represents no difference in hazard", fixed = TRUE)
    expect_false(grepl("represents a 0% ", shown, fixed = TRUE))
  }
  # Lachin-Foulkes hazards 0.083 and 0.08304: HR 1.00048.
  expect_match(effect(calculation_type = "power", hazard_treatment = 0.08304),
               "HR = 1 represents no difference in hazard", fixed = TRUE)
  # Past the rounding boundary the sentence matches the HR shown.
  expect_match(effect(method = "schoenfeld", calculation_type = "power", hazard_ratio = 1.0006),
               "HR = 1.001 represents a 0.1% increase in hazard", fixed = TRUE)
})

test_that("large counts print in full, never in scientific notation", {
  skip_if_not_installed("ggplot2")
  off_screen <- function(expr) {  # plots are built on a null device, never Rplots.pdf
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expr
  }
  # A bare format() here is jmvcore::format, which returns a number unchanged, so 2e5 used
  # to read "2e+05 patients (1e+05 treatment, 1e+05 control)".
  near <- lf_design(lambda2 = 0.082)
  beta <- uniroot(function(b) do.call(gsDesign::nSurvival, c(near, list(beta = b)))$n / 2 - (1e5 - 0.5),
                  c(0.01, 0.5), tol = 1e-12)$root
  big <- run_analysis(hazard_treatment = 0.082, beta = beta, show_power_plot = TRUE)
  expect_match(big$results$power_results$content,
               "Required Sample Size:</strong> 200000 patients (100000 treatment, 100000 control)", fixed = TRUE)
  expect_match(big$results$power_results$content, "Power with 200000 patients:</strong>", fixed = TRUE)
  expect_match(big$results$interpretation$content,
               "Total enrollment:</strong> 200000 patients (100000 treatment, 100000 control)", fixed = TRUE)
  plot <- draw_power_curve(big)
  expect_match(layer_with(plot, "GeomText")$aes_params$label, "Current: 200000 subjects", fixed = TRUE)
  axis <- off_screen(ggplot2::layer_scales(plot)$x$get_labels())
  expect_true(all(c("100000", "200000") %in% axis))

  # Schoenfeld: exactly 100000 events.
  beta_events <- uniroot(function(b) gsDesign::nEvents(hr = 0.98, alpha = 0.025, beta = b) - (1e5 - 0.5),
                         c(0.01, 0.5), tol = 1e-12)$root
  events <- classicalSurvivalPower(method = "schoenfeld", calculation_type = "events", hazard_ratio = 0.98,
                                   beta = beta_events)
  expect_match(events$power_results$content, "Required Events:</strong> 100000 events", fixed = TRUE)
  expect_match(events$power_results$content, "Power with 100000 events:</strong>", fixed = TRUE)
  expect_match(events$interpretation$content, "Target events:</strong> 100000 events", fixed = TRUE)

  # HR 1001 is a 100000% increase; a reversed plot range; a follow-up of 0.00001 time units.
  hr_1001 <- classicalSurvivalPower(calculation_type = "power", hazard_control = 0.001, hazard_treatment = 1.001)
  expect_match(hr_1001$interpretation$content, "HR = 1001 represents a 100000% increase in hazard", fixed = TRUE)
  reversed <- classicalSurvivalPower(calculation_type = "power", power_plot_range = "200000,100000",
                                     export_power_curve = TRUE)
  expect_match(reversed$export_summary$content, "changed to 100 to 200000", fixed = TRUE)
  brief <- run_analysis(study_duration = 12.00001, show_timeline_plot = TRUE)
  expect_match(brief$results$interpretation$content, "12 time units for enrollment + 0.00001 additional follow-up", fixed = TRUE)
  expect_true(off_screen(brief$.__enclos_env__$private$.plot_timeline(brief$results$timeline_plot, NULL, NULL)))
  labels <- vapply(Filter(function(layer) inherits(layer$geom, "GeomText"), ggplot2::last_plot()$layers),
                   function(layer) layer$aes_params$label, character(1))
  expect_true(any(grepl("0.00001 time units", labels, fixed = TRUE)))

  shown <- c(big$results$power_results$content, big$results$interpretation$content,
             events$power_results$content, events$interpretation$content, hr_1001$interpretation$content,
             reversed$export_summary$content, brief$results$interpretation$content, axis, labels)
  expect_false(any(grepl("[0-9]e[+-]?[0-9]", shown)))
})
