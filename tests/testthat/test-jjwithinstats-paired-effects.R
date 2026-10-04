library(testthat)

jw_effect_env <- new.env(parent = globalenv())
source(testthat::test_path("..", "..", "R", "jjwithinstats-paired-effects.R"),
       local = jw_effect_env)
jw_effects <- jw_effect_env$.jjWithinPairedEffects
jw_pvalues <- jw_effect_env$.jjWithinPairedPValues

test_that("paired means and analytic intervals match the direct paired reference test", {
  before <- c(13, 8, 30, 16, 5, 19, 7, 22)
  after <- before + c(-2, -4, 3, -8, 1, -5, -1, 0)
  result <- jw_effects(data.frame(before, after), ci_method = "standard")
  reference <- stats::t.test(after, before, paired = TRUE)
  expect_equal(result$estimate, unname(reference$estimate))
  expect_equal(c(result$lower, result$upper), as.numeric(reference$conf.int))
  expect_equal(result$n, 8L)
  expect_equal(jw_pvalues(data.frame(before, after), "parametric")$p, reference$p.value)
})

test_that("median difference and Walsh pseudomedian are not conflated", {
  d <- c(-4, -1, 0, 1, 2, 13, 20)
  data <- data.frame(before = seq_along(d) + 10, after = seq_along(d) + 10 + d)
  walsh <- outer(d, d, "+") / 2
  reference <- stats::median(walsh[upper.tri(walsh, diag = TRUE)])
  mean_result <- jw_effects(data, samples = 200)
  median_result <- jw_effects(data, estimator = "median", samples = 200)
  hl_result <- jw_effects(data, estimator = "hodges_lehmann", samples = 200)
  expect_equal(mean_result$estimate, sum(d) / length(d))
  expect_equal(median_result$estimate, 1)
  expect_equal(hl_result$estimate, reference)
  expect_false(isTRUE(all.equal(median_result$estimate, hl_result$estimate)))
  expect_match(hl_result$effect, "Hodges-Lehmann")
})

test_that("the signed-rank interval agrees with the R reference for nondegenerate differences", {
  d <- c(-2.2, -1.7, -0.1, 0.8, 1.4, 2.1, 2.9, 3.7, 4.6, 5.2,
         5.9, 6.8, 7.3, 8.1, 9.6, 10.4, 12.7, 14.1, 15.3, 18.7)
  result <- jw_effects(data.frame(a = rep(30, length(d)), b = 30 + d),
                       estimator = "hodges_lehmann", ci_method = "standard")
  reference <- stats::wilcox.test(d, conf.int = TRUE, exact = FALSE, correct = TRUE)
  expect_equal(c(result$lower, result$upper), as.numeric(reference$conf.int), tolerance = 1e-10)
})

test_that("bootstrap draws preserve cases, seeds and row-order invariance", {
  set.seed(991)
  before <- c(9, 14, 8, 22, 7, 11, 25, 17)
  data <- data.frame(before, after = before + c(1, 3, -2, 7, 8, -1, 4, -5))
  data$third <- data$after + 5
  old <- .Random.seed
  a <- jw_effects(data, samples = 600, seed = 51)
  expect_identical(.Random.seed, old)
  b <- jw_effects(data[8:1, ], samples = 600, seed = 51)
  expect_identical(a, b)
  expect_equal(a$estimate[2] - a$estimate[1], 5)
  expect_equal(a$lower[2] - a$lower[1], 5)
  expect_equal(a$upper[2] - a$upper[1], 5)
  expect_true(is.na(a$lower[3]))
  expect_match(a$note[3], "identical")
  invisible(jw_effects(data, estimator = "hodges_lehmann", ci_method = "standard"))
  expect_identical(.Random.seed, old)
  invisible(jw_effects(data, estimator = "hodges_lehmann", samples = 200))
  expect_identical(.Random.seed, old)
})

test_that("bootstrap mean limits recover the enumerated paired bootstrap distribution", {
  data <- data.frame(a = c(10, 20, 30), b = c(10, 22, 34))
  draws <- expand.grid(rep(list(1:3), 3))
  exact <- apply(draws, 1, function(i) mean(c(0, 2, 4)[i]))
  reference <- unname(stats::quantile(exact, c(0.1, 0.9)))
  result <- jw_effects(data, samples = 10000, confidence = 0.8, seed = 321)
  expect_equal(c(result$lower, result$upper), reference, tolerance = 1e-12)
})

test_that("ratios use paired geometric means and reject nonpositive values without deletion", {
  data <- data.frame(before = c(1, 2, 10, 20, 30), after = c(2, 3, 8, 6, 15))
  result <- jw_effects(data, ratios = TRUE, ci_method = "standard")
  log_ratio <- log(data$after / data$before)
  expect_equal(result$estimate[2], exp(mean(log_ratio)))
  expect_equal(c(result$lower[2], result$upper[2]),
               exp(as.numeric(stats::t.test(log_ratio)$conf.int)))
  expect_gt(abs(result$estimate[2] - mean(data$after) / mean(data$before)), 0.1)
  data$before[1] <- 0
  invalid <- jw_effects(data, ratios = TRUE, samples = 200)
  expect_equal(invalid$n, rep(5L, 2))
  expect_true(is.finite(invalid$estimate[1]))
  expect_true(is.na(invalid$estimate[2]))
  expect_match(invalid$note[2], "strictly positive")
})

test_that("all-zero changes and tied changes never manufacture a confidence interval", {
  for (estimator in c("mean", "median", "hodges_lehmann")) {
    for (method in c("standard", "bootstrap")) {
      zero <- jw_effects(data.frame(a = 1:5, b = 1:5), estimator = estimator,
                         ci_method = method, samples = 200)
      expect_equal(zero$estimate, 0)
      expect_true(is.na(zero$lower) && is.na(zero$upper))
      expect_gt(nchar(zero$note), 0)
    }
  }
  tied <- jw_effects(data.frame(a = 1:8, b = 1:8 + c(0, 0, 1, 1, 2, 2, 3, 3)),
                     estimator = "median", samples = 500)
  expect_equal(tied$estimate, 1.5)
  expect_true(is.finite(tied$lower) && is.finite(tied$upper))
})

test_that("paired-test p-values preserve the plot's selected test and multiplicity", {
  data <- data.frame(a = c(1, 2, 5, 8, 10, 15), b = c(0, 5, 7, 4, 16, 17),
                     c = c(3, 3, 6, 11, 9, 18))
  expect_null(jw_pvalues(data, "parametric", pairwise = FALSE))
  expect_null(jw_pvalues(data, "robust", pairwise = TRUE))
  parametric <- jw_pvalues(data, "parametric", pairwise = TRUE, adjustment = "holm")
  pair_index <- utils::combn(seq_len(3), 2)
  raw <- apply(pair_index, 2, function(i) stats::t.test(data[[i[1]]], data[[i[2]]],
                                                    paired = TRUE)$p.value)
  expect_equal(parametric$p, stats::p.adjust(raw, "holm"))
  nonparametric <- jw_pvalues(data, "nonparametric", pairwise = TRUE)
  expect_true(all(nonparametric$test == "Durbin-Conover"))
  pair <- data[1:2]
  expect_equal(jw_pvalues(pair, "nonparametric")$p,
    stats::wilcox.test(pair$a, pair$b, paired = TRUE, exact = FALSE, correct = TRUE)$p.value)
})

test_that("paired-change table reuses complete cases and resets when disabled", {
  jw_effect_env$. <- function(x) x
  sys.source(testthat::test_path("..", "..", "R", "jjwithinstats.h.R"), jw_effect_env)
  sys.source(testthat::test_path("..", "..", "R", "jjwithinstats.b.R"), jw_effect_env)
  options_class <- jw_effect_env$jjwithinstatsOptions
  skip_if_not("pairedEffects" %in% names(formals(options_class$public_methods$initialize)),
              "Regenerate the analysis header before integration tests")
  options <- options_class$new(dep1 = "a", dep2 = "b", dep3 = "c", pairedEffects = TRUE,
                                pairedBootstrap = 200, pairedUnits = "seconds")
  data <- data.frame(a = 1:7, b = c(2, 4, 3, 6, 8, 7, 10),
                     c = c(3, 5, NA, 9, 9, 10, 12))
  analysis <- jw_effect_env$jjwithinstatsClass$new(options = options, data = data)
  analysis$init()
  analysis$.__enclos_env__$private$.run()
  rows <- analysis$results$pairedChanges$asDF
  expect_equal(nrow(rows), 3L)
  expect_equal(rows$pairs, rep(6L, 3))
  expect_equal(rows$estimate[1], mean(data$b[-3] - data$a[-3]))
  expect_true(all(rows$units == "seconds"))
  ratio_toggle <- options$option("pairedTimeRatios")
  ratio_toggle$value <- TRUE
  analysis$.__enclos_env__$private$.run()
  expect_equal(analysis$results$pairedChanges$rowCount, 6L)
  ratio_toggle$value <- FALSE
  test_toggle <- options$option("pairwisecomparisons")
  test_toggle$value <- TRUE
  analysis$.__enclos_env__$private$.run()
  expect_true(all(is.finite(analysis$results$pairedChanges$asDF$p)))
  test_toggle$value <- FALSE
  analysis$.__enclos_env__$private$.run()
  expect_true(all(is.na(analysis$results$pairedChanges$asDF$p)))
  toggle <- options$option("pairedEffects")
  toggle$value <- FALSE
  analysis$.__enclos_env__$private$.run()
  expect_equal(analysis$results$pairedChanges$rowCount, 0L)
})

test_that("enabling changes preserves the original plot statistics and exports", {
  env <- new.env(parent = jw_effect_env)
  env$. <- function(x) x
  sys.source(testthat::test_path("..", "..", "R", "utils.R"), env)
  sys.source(testthat::test_path("..", "..", "R", "utils-ggstatsplot.R"), env)
  sys.source(testthat::test_path("..", "..", "R", "jjwithinstats.h.R"), env)
  sys.source(testthat::test_path("..", "..", "R", "jjwithinstats.b.R"), env)
  skip_if_not("pairedEffects" %in% names(formals(
    env$jjwithinstatsOptions$public_methods$initialize)), "Regenerate analysis header")
  data <- data.frame(a = c(20, 30, 28, 40, 18, 12, 30, 15),
                     b = c(15, 32, 23, 33, 19, 9, 25, 11))
  subtitles <- lapply(c(FALSE, TRUE), function(enabled) {
    options <- env$jjwithinstatsOptions$new(dep1 = "a", dep2 = "b", pairedEffects = enabled,
                                           pairedBootstrap = 200, resultssubtitle = TRUE)
    analysis <- env$jjwithinstatsClass$new(options = options, data = data)
    analysis$init()
    analysis$.__enclos_env__$private$.run()
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 550)
    tryCatch(analysis$.__enclos_env__$private$.plot(
      analysis$results$plot, ggtheme = ggplot2::theme_bw(), theme = NULL),
      finally = grDevices::dev.off())
    expect_gt(file.info(f)$size, 1000)
    subtitle <- ggplot2::last_plot()$labels$subtitle
    unlink(f)
    expect_true(is.character(analysis$results$pairedChanges$asString()))
    subtitle
  })
  expect_identical(subtitles[[1]], subtitles[[2]])
})
