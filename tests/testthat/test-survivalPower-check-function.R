sp_check <- function(...) {
  analysis <- survivalPowerClass$new(
    options = survivalPowerOptions$new(...), data = data.frame()
  )
  analysis$run()
  analysis
}

test_that("multi-arm non-inferiority uses the adjusted one-sided alpha throughout", {
  args <- list(test_type = "non_inferiority", effect_size = 1, alpha_level = 0.025)
  for (looks in c(0L, 2L)) {
    sequential <- list(interim_analyses = looks, alpha_spending = "obrien_fleming")
    pair_args <- modifyList(args, list(alpha_level = 0.0125))
    pair <- do.call(sp_check, c(pair_args, sequential))
    multi <- do.call(sp_check, c(args, sequential, list(
      study_design = "multi_arm", multiple_comparisons = "bonferroni"
    )))
    p <- pair$.__enclos_env__$private
    m <- multi$.__enclos_env__$private
    expect_equal(m$primary_numbers$n, ceiling(p$primary_numbers$n * 1.5))
    expect_equal(m$primary_numbers$events, p$primary_numbers$events)
    expect_equal(multi$results$multi_arm_table$asDF$power, rep(80, 2), tolerance = 0.2)
    expect_equal(m$.one_sided_alpha(), 0.0125)
    ni <- multi$results$non_inferiority_table$asDF
    expect_equal(ni$value[ni$parameter == "One-sided Alpha"], "0.0125")
    curve <- multi$results$power_curve_plot$state$data
    expect_equal(curve$power[which.min(abs(curve$hazard_ratio - 1))], 0.8,
                 tolerance = 0.005)
  }
})

test_that("shared control power uses the correlation of the treatment contrasts", {
  skip_if_not_installed("mvtnorm")
  a <- sp_check(study_design = "multi_arm", number_of_arms = 3,
                allocation_ratio = 2, multiple_comparisons = "bonferroni")
  table <- a$results$multi_arm_table$asDF
  # Var(C) / Var(E-C) = (1/nC) / (1/nC + 1/nE) = 1 / (1 + ratio).
  critical_minus_mean <- -qnorm(table$power[1] / 100)
  expected <- 1 - as.numeric(mvtnorm::pmvnorm(
    upper = rep(critical_minus_mean, 2), corr = matrix(c(1, 1/3, 1/3, 1), 2)
  ))
  expect_equal(table$total_study_power, rep(round(expected * 100, 1), 2), tolerance = 0.1)
})

test_that("failed calculations do not produce completion messages or derived plots", {
  for (args in list(
    list(effect_size = 1),
    list(analysis_type = "duration", sample_size_input = 10),
    list(analysis_type = "effect_size", sample_size_input = 10)
  )) {
    a <- do.call(sp_check, c(args, list(show_interpretation = TRUE, show_summary = TRUE)))
    expect_match(a$results$notices$content, "ERROR: Calculation Not Available", fixed = TRUE)
    expect_false(grepl("Analysis Complete", a$results$notices$content, fixed = TRUE))
    expect_null(a$results$power_curve_plot$state)
    expect_true(is.null(a$results$clinical_interpretation$content) ||
                  !nzchar(a$results$clinical_interpretation$content))
  }
})

test_that("simulation validation refuses designs the simulator does not implement", {
  for (args in list(
    list(study_design = "multi_arm"),
    list(study_design = "cluster_randomized"),
    list(interim_analyses = 1, alpha_spending = "pocock"),
    list(analysis_type = "effect_size")
  )) {
    a <- do.call(sp_check, c(args, list(run_simulation_validation = TRUE,
                                      sensitivity_analysis = TRUE)))
    expect_match(a$results$notices$content, "Simulation Validation Not Applicable", fixed = TRUE)
    expect_equal(a$results$simulation_validation_table$rowCount, 0)
    expect_false("Simulation Validation" %in% a$results$assumptions_table$asDF$assumption)
  }
})

test_that("sensitivity scenarios keep the selected design and multiplicity adjustment", {
  for (design in list(
    list(study_design = "multi_arm"),
    list(study_design = "cluster_randomized"),
    list(interim_analyses = 2, alpha_spending = "pocock"),
    list(test_type = "non_inferiority", effect_size = 1, alpha_level = 0.025,
         study_design = "multi_arm", interim_analyses = 2, alpha_spending = "pocock")
  )) {
    for (mode in c("sample_size", "power")) {
      args <- c(design, list(analysis_type = mode, sample_size_input = 1500))
      base <- do.call(sp_check, args)$.__enclos_env__$private
      target <- do.call(sp_check, modifyList(args, list(alpha_level = 0.02)))$
        .__enclos_env__$private
      key <- if (mode == "sample_size") "n" else "power"
      expect_equal(base$.sensitivity_value(alpha = 0.02), target$primary_numbers[[key]],
                   tolerance = 1e-7)
    }
  }
})

# Model the options protobuf sent by the desktop so .load() can determine which
# results to clear; constructing Options in R alone does not set this request.
sp_check_options_pb <- function(options) {
  getFromNamespace("initProtoBuf", "jmvcore")()
  encode <- function(value) {
    item <- RProtoBuf::P("jamovi.coms.AnalysisOption")$new()
    if (is.logical(value)) item$o <- as.integer(value)
    else if (is.integer(value)) item$i <- value
    else if (is.numeric(value)) item$d <- value
    else item$s <- value
    item
  }
  keys <- names(options$.__enclos_env__$private$.options)
  pb <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
  pb$hasNames <- TRUE
  pb$names <- keys
  pb$options <- lapply(keys, function(key) encode(options$option(key)$value))
  options$fromProtoBuf(pb)
}

test_that("saved results clear when a formerly valid design becomes unsupported", {
  skip_if_not_installed("RProtoBuf")
  new_analysis <- function(...) {
    a <- survivalPowerClass$new(options = survivalPowerOptions$new(...),
      data = data.frame(), datasetId = "1", analysisId = 1L, revision = 1L)
    sp_check_options_pb(a$options)
    a
  }
  original <- new_analysis(show_interpretation = TRUE, show_summary = TRUE)
  original$run()
  state_file <- tempfile()
  on.exit(unlink(state_file), add = TRUE)
  original$.setStatePathSource(function() state_file)
  expect_false(inherits(original$.save(), "try-error"))
  expect_gt(file.info(state_file)$size, 0)
  unchanged <- new_analysis(show_interpretation = TRUE, show_summary = TRUE)
  unchanged$.setStatePathSource(function() state_file)
  unchanged$init()
  unchanged$postInit()
  unchanged$.load()
  expect_equal(unchanged$results$power_summary$asDF, original$results$power_summary$asDF)
  expect_equal(unchanged$results$power_curve_plot$state, original$results$power_curve_plot$state)
  for (changed in list(list(accrual_pattern = "custom"), list(study_design = "crossover"))) {
    restored <- do.call(new_analysis, c(changed, list(
      show_interpretation = TRUE, show_summary = TRUE
    )))
    restored$.setStatePathSource(function() state_file)
    restored$init()
    restored$postInit()
    expect_no_error(restored$.load())
    restored$run()
    expect_match(restored$results$notices$content, "ERROR:", fixed = TRUE)
    expect_true(is.na(restored$results$power_summary$asDF$calculated_value[1]))
    expect_null(restored$results$power_curve_plot$state)
    expect_null(restored$results$survival_curves_plot$state)
    content <- restored$results$clinical_interpretation$content
    expect_true(length(content) == 0 || !nzchar(content))
  }
})

test_that("two-sided power equals alpha at the null and includes both rejection tails", {
  for (test in c("log_rank", "cox_regression")) {
    a <- sp_check(test_type = test, analysis_type = "power", effect_size = 1)
    expect_equal(a$.__enclos_env__$private$primary_numbers$power, 0.05)
  }
  p <- sp_check()$.__enclos_env__$private
  for (hr in c(0.5, 0.99, 1, 1.01, 2)) {
    ncp <- 20 * 0.25 * log(hr)^2
    expected <- pchisq(qchisq(0.95, 1), df = 1, ncp = ncp, lower.tail = FALSE)
    expect_equal(p$.power_from_events(20, hr, 0.05, 1), expected, tolerance = 1e-8)
  }
})
