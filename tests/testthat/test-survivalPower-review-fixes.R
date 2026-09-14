# Independent acceptance cases for the 2026-09-13 review.
sp_review <- function(...) {
  analysis <- survivalPowerClass$new(options = survivalPowerOptions$new(...), data = data.frame())
  analysis$run()
  analysis
}
sp_review_private <- function(analysis) analysis$.__enclos_env__$private
sp_review_events <- function(hr, median = 12) {
  lambda <- log(2) / median * hr
  eta <- -log1p(-0.05) / 12
  integrate(function(entry) {
    lambda / (lambda + eta) * (-expm1(-(lambda + eta) * (36 - entry)))
  }, 0, 24)$value / 24
}

test_that("multi-arm whole-study and comparison events use their actual populations", {
  for (arms in c(3, 10)) for (ratio in c(0.5, 1, 2)) for (hr in c(0.1, 0.75, 1.5)) {
    a <- sp_review(study_design = "multi_arm", number_of_arms = arms,
                   allocation_ratio = ratio, effect_size = hr,
                   analysis_type = "power", sample_size_input = 1000)
    design <- sp_review_private(a)$.resolved_design()
    nE <- 1000 / (ratio + arms - 1)
    expected_comparison <- nE * (ratio * sp_review_events(1) + sp_review_events(hr))
    expected_whole <- nE * (ratio * sp_review_events(1) + (arms - 1) * sp_review_events(hr))
    expect_equal(design$events, expected_whole, tolerance = 1e-8)
    expect_equal(design$comparison_events, expected_comparison, tolerance = 1e-8)
    expect_equal(sp_review_private(a)$primary_numbers$events, expected_comparison, tolerance = 1e-8)
  }
  a <- sp_review(study_design = "multi_arm", number_of_arms = 10,
                 analysis_type = "effect_size", sample_size_input = 1000,
                 multiple_comparisons = "bonferroni")
  design <- sp_review_private(a)$.resolved_design()
  truth <- 100 * (sp_review_events(1) + 9 * sp_review_events(design$hr))
  expect_equal(design$events, truth, tolerance = 1e-8)
  expect_equal(subset(a$results$effect_size_results$asDF,
                      parameter == "Expected Events")$value, "499 events")
})

test_that("NI interpretation describes power without promising a conclusion", {
  for (n in c(10, 200, 1000)) {
    a <- sp_review(test_type = "non_inferiority", analysis_type = "power",
                   effect_size = 1, alpha_level = 0.025, sample_size_input = n)
    power <- sp_review_private(a)$primary_numbers$power
    row <- subset(a$results$non_inferiority_table$asDF, parameter == "True Hazard Ratio")
    expect_match(row$clinical_interpretation, paste0(round(power * 100, 1), "%"), fixed = TRUE)
    expect_false(grepl("Should demonstrate", row$clinical_interpretation, fixed = TRUE))
    margin <- subset(a$results$non_inferiority_table$asDF, parameter == "Non-inferiority Margin")
    expect_match(margin$clinical_interpretation, "disease-specific justification", fixed = TRUE)
  }
})

test_that("NI effect inversion ignores the unused starting HR but keeps feasibility guards", {
  solved <- vapply(c(0.1, 1, 1.5, 5), function(hr) {
    a <- sp_review(test_type = "non_inferiority", analysis_type = "effect_size",
                   effect_size = hr, sample_size_input = 1000, alpha_level = 0.025)
    expect_equal(subset(a$results$non_inferiority_table$asDF,
                        parameter == "Sample Size Requirement")$value, "1000 subjects")
    expect_false(grepl("Effect Not Below", a$results$notices$content, fixed = TRUE))
    sp_review_private(a)$primary_numbers$hr_detectable
  }, 0)
  reference <- uniroot(function(hr) {
    events <- 500 * (sp_review_events(1) + sp_review_events(hr))
    log(1.25 / hr) * sqrt(events / 4) - qnorm(0.975) - qnorm(0.8)
  }, c(0.1, 1.25), tol = 1e-10)$root
  expect_equal(solved, rep(reference, 4), tolerance = 1e-8)
  for (mode in c("sample_size", "power", "duration")) {
    a <- sp_review(test_type = "non_inferiority", analysis_type = mode, effect_size = 1.5)
    expect_match(a$results$notices$content, "Effect Not Below", fixed = TRUE)
    expect_length(sp_review_private(a)$primary_numbers, 0)
  }
})

test_that("joint fixed power integrates the correct two-sided or NI region", {
  skip_if_not_installed("mvtnorm")
  for (test in c("log_rank", "cox_regression", "non_inferiority")) {
    for (hr in c(0.6, 1, 1.2)) {
      a <- sp_review(study_design = "multi_arm", number_of_arms = 3,
                     test_type = test, analysis_type = "power", effect_size = hr,
                     sample_size_input = 900, allocation_ratio = 2,
                     alpha_level = 0.05, multiple_comparisons = "bonferroni")
      p <- sp_review_private(a)
      # Independently derive the normal model from integrated event probabilities.
      eC <- 450 * sp_review_events(1)
      eE <- 225 * sp_review_events(hr)
      info <- (eC + eE) * 2 / 9
      critical <- qnorm(1 - 0.025 / 2)
      rho <- 1 / 3
      mean_z <- abs(log(hr)) * sqrt(info)
      if (test == "cox_regression") {
        pooled_hr <- (2 + hr) / 3
        v0 <- 1 / (450 * sp_review_events(pooled_hr)) +
          1 / (225 * sp_review_events(pooled_hr))
        v1 <- 1 / eC + 1 / eE
        mean_z <- abs(log(hr)) / sqrt(v1)
        critical <- critical * sqrt(v0 / v1)
        rho <- (1 / eC) / v1
      } else if (test == "non_inferiority") {
        mean_z <- log(1.25 / hr) * sqrt(info)
        critical <- qnorm(1 - 0.025)
      }
      lower <- if (test == "non_inferiority") -Inf else -critical
      sigma <- matrix(c(1, rho, rho, 1), 2)
      truth <- 1 - as.numeric(mvtnorm::pmvnorm(
        lower = rep(lower, 2), upper = rep(critical, 2),
        mean = rep(mean_z, 2), sigma = sigma, algorithm = mvtnorm::Miwa(steps = 128)))
      actual <- p$.disjunctive_power(mean_z, critical, 2, rho, test != "non_inferiority")
      expect_equal(actual, truth, tolerance = 1e-7)
      expect_equal(a$results$multi_arm_table$asDF$total_study_power,
                   rep(round(truth * 100, 1), 2))
      marginal <- pnorm(mean_z - critical) +
        if (test != "non_inferiority") pnorm(-mean_z - critical) else 0
      expect_equal(p$primary_numbers$power, marginal, tolerance = 1e-8)
    }
  }
  a <- sp_review(study_design = "multi_arm", number_of_arms = 10,
                 analysis_type = "power", effect_size = 1, alpha_level = 0.1,
                 multiple_comparisons = "bonferroni")
  expect_equal(a$results$multi_arm_table$asDF$total_study_power, rep(7.4, 9))
})

test_that("sequential multi-arm family power is withheld with a visible explanation", {
  for (test in c("log_rank", "cox_regression", "non_inferiority")) {
    a <- sp_review(study_design = "multi_arm", number_of_arms = 3,
                   test_type = test, interim_analyses = 2, alpha_spending = "pocock",
                   analysis_type = "power", sample_size_input = 1000)
    table <- a$results$multi_arm_table$asDF
    expect_true(all(is.finite(table$power)))
    expect_true(all(is.na(table$total_study_power)))
    expect_match(a$results$notices$content, "joint arm-and-look", fixed = TRUE)
  }
})

test_that("analytical multi-arm calculations preserve present and absent RNG states", {
  withr::local_preserve_seed()
  set.seed(712)
  before <- .Random.seed
  first <- sp_review(study_design = "multi_arm", number_of_arms = 5)
  expect_identical(.Random.seed, before)
  second <- sp_review(study_design = "multi_arm", number_of_arms = 5)
  expect_identical(first$results$multi_arm_table$asDF, second$results$multi_arm_table$asDF)
  rm(".Random.seed", envir = .GlobalEnv)
  invisible(sp_review(study_design = "multi_arm", number_of_arms = 10))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("numeric cache reuses identical requests and separates changed inputs", {
  a <- sp_review(test_type = "cox_regression", sensitivity_analysis = TRUE)
  p <- sp_review_private(a)
  calls <- 0L
  original <- p$.uncached_sample_size_calc
  unlockBinding(".uncached_sample_size_calc", p)
  p$.uncached_sample_size_calc <- function(...) {
    calls <<- calls + 1L
    original(...)
  }
  n <- p$.basic_sample_size_calc(0.8, 0.75, 0.05)
  expect_equal(calls, 0L)
  changed <- p$.basic_sample_size_calc(0.8, 0.74, 0.05)
  expect_equal(calls, 1L)
  expect_false(identical(n, changed))
  expect_equal(p$.basic_sample_size_calc(0.8, 0.74, 0.05), changed)
  expect_equal(calls, 1L)
  dropout <- a$options$option("dropout_rate")
  dropout$value <- 0.2
  more_dropout <- p$.basic_sample_size_calc(0.8, 0.74, 0.05)
  expect_equal(calls, 2L)
  expect_gt(more_dropout, changed)
  dropout$value <- 0.05
  expect_equal(p$.basic_sample_size_calc(0.8, 0.74, 0.05), changed)
  expect_equal(calls, 2L)
})
sp_review_po <- function() {
  path <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
  blocks <- strsplit(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n\n+")[[1]]
  blocks <- blocks[grepl("#: R/survivalPower.b.R", blocks, fixed = TRUE)]
  field <- function(block, name) {
    lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
    start <- grep(paste0("^", name, " "), lines)
    if (!length(start)) return("")
    tokens <- sub(paste0("^", name, " "), "", lines[start])
    i <- start + 1L
    while (i <= length(lines) && startsWith(lines[[i]], '"')) {
      tokens <- c(tokens, lines[[i]])
      i <- i + 1L
    }
    paste0(vapply(tokens, jsonlite::fromJSON, ""), collapse = "")
  }
  setNames(lapply(blocks, field, "msgstr"), vapply(blocks, field, "", name = "msgid"))
}

test_that("affected messages have complete Turkish entries and matching placeholders", {
  catalog <- sp_review_po()
  ast <- parse(testthat::test_path("..", "..", "R", "survivalPower.b.R"))
  messages <- character()
  walk <- function(x) {
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
    if (is.call(x) && identical(paste(deparse(x[[1]]), collapse = ""), "jmvcore::.")) {
      messages <<- c(messages, x[[2]])
    }
    for (i in seq_along(x)) {
      if (!is.symbol(x[[i]]) || nzchar(as.character(x[[i]]))) walk(x[[i]])
    }
  }
  walk(ast)
  placeholders <- function(s) sort(regmatches(s, gregexpr("\\{[[:alnum:]]+\\}", s))[[1]])
  for (message in unique(messages)) {
    expect_identical(trimws(message), message)
    expect_true(nzchar(catalog[[message]]), info = message)
    expect_equal(placeholders(catalog[[message]]), placeholders(message), info = message)
  }
})

test_that("translated NI reports retain numeric results and complete localized sentences", {
  catalog <- sp_review_po()
  local_mocked_bindings(`.` = function(text) {
    if (!is.null(catalog[[text]]) && nzchar(catalog[[text]])) catalog[[text]] else text
  }, .package = "jmvcore")
  a <- sp_review(test_type = "non_inferiority", analysis_type = "power",
                 effect_size = 1, sample_size_input = 200, alpha_level = 0.025)
  p <- sp_review_private(a)
  sentence <- p$.generate_report_sentence()
  expect_match(sentence, "26.1", fixed = TRUE)
  expect_match(sentence, "1.25", fixed = TRUE)
  expect_match(sentence, "tek y", fixed = TRUE)
  expect_false(grepl("significance level|subjects|\\{", sentence))
  expect_equal(p$primary_numbers$power, 0.2614700030223, tolerance = 1e-8)
})
