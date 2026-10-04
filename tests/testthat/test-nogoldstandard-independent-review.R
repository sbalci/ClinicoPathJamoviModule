# Regressions for the independent review of 2026-10-03.
ngsi_data <- function(counts) {
  patterns <- expand.grid(rep(list(0:1), log2(length(counts))))
  d <- patterns[rep(seq_along(counts), counts), , drop = FALSE]
  d[] <- lapply(d, function(x) factor(ifelse(x == 1, "pos", "neg")))
  names(d) <- paste0("T", seq_len(ncol(d)))
  rownames(d) <- NULL
  d
}

ngsi_args <- function(d, ...) {
  args <- list(data = d, method = "composite", showMethodGuide = TRUE)
  for (j in 1:5) {
    args[paste0("test", j)] <- list(if (j <= ncol(d)) names(d)[j] else NULL)
    args[paste0("test", j, "Positive")] <- list(if (j <= ncol(d)) "pos" else NULL)
  }
  utils::modifyList(args, list(...), keep.null = TRUE)
}

ngsi_run <- function(d, ...) do.call(ClinicoPath::nogoldstandard, ngsi_args(d, ...))

test_that("equal class means do not acquire seed-dependent labels", {
  skip_if_not_installed("poLCA")
  # Exact identifiable mixture: pi=.25, q1=(.75,.5,.25), q0=(.25,.75,.5).
  # Both class means equal .5, so the labeling rule has no answer.
  d <- ngsi_data(c(12, 12, 30, 18, 10, 6, 28, 12))
  fits <- lapply(0:2, function(seed) {
    order <- list(1:3, c(3, 1, 2), c(2, 3, 1))[[seed + 1L]]
    ngsi_run(d[order], method = "latent_class", seed = seed,
             bootstrap = TRUE, nboot = 100)
  })
  for (r in fits) {
    expect_true(all(is.na(unlist(r$prevalence$asDF))))
    expect_true(all(is.na(unlist(r$test_metrics$asDF[-1]))))
    expect_match(r$notices$content, "Class orientation is ambiguous")
    expect_match(r$notices$content, "bootstrap refitting was skipped")
    expect_equal(nrow(r$agreement_stats$asDF), 3L)
    expect_equal(nrow(r$conditional_dependence$asDF), 3L)
    fit <- r$model_fit$asDF
    expect_equal(fit$value[fit$statistic == "Log-Likelihood"],
                 -250.46248726713287, tolerance = 1e-10)
  }
})

test_that("orientation guard distinguishes numerical ties from separated means", {
  orient <- ClinicoPath:::nogoldstandard_orientation
  q1 <- c(.8, .7, .1)
  q0 <- c(.2, .5, .9)
  expect_true(orient(q1, q0)$tied)
  expect_true(orient(q1 + 5e-7, q0)$tied)
  expect_true(orient(q0, q1 + 5e-7)$tied)
  expect_false(orient(q1 + 2e-6, q0)$tied)
  expect_gt(orient(q1 + .01, q0)$contrast, 0)
  expect_lt(orient(q0, q1 + .01)$contrast, 0)
})

test_that("third observed categories are disclosed without changing the selected split", {
  d <- ngsi_data(c(30, 12, 15, 18, 20, 21, 24, 40))
  original <- ngsi_run(d)
  v <- as.character(d$T1)
  v[which(v == "neg")[1:10]] <- "equivocal"
  d$T1 <- factor(v)
  pooled <- ngsi_run(d)
  expect_match(pooled$notices$content, "Test categories collapsed")
  expect_match(pooled$notices$content, "Test 'T1'")
  expect_match(pooled$notices$content, "Only 'pos' is treated as positive")
  expect_match(pooled$notices$content, "equivocal")
  expect_equal(pooled$test_metrics$asDF, original$test_metrics$asDF)
  expect_equal(pooled$agreement_stats$asDF, original$agreement_stats$asDF)

  d$T2[v == "equivocal"] <- NA
  filtered <- ngsi_run(d)
  expect_false(grepl("Test categories collapsed", filtered$notices$content, fixed = TRUE))
  expect_match(filtered$notices$content, "Excluded 10 case")
})

test_that("pair keys remain unique when test names contain separators", {
  skip_if_not_installed("poLCA")
  d <- ngsi_data(c(120, 30, 40, 30, 30, 20, 30, 40,
                  20, 30, 20, 30, 40, 40, 50, 130))
  reference <- ngsi_run(d, method = "latent_class")
  for (nms in list(c("a_b", "c", "a", "b_c"),
                   c("a vs b", "c", "a", "b vs c"))) {
    names(d) <- nms
    r <- ngsi_run(d, method = "latent_class")
    agreement <- r$agreement_stats$asDF
    residuals <- r$conditional_dependence$asDF
    expect_equal(nrow(agreement), choose(4, 2))
    expect_equal(nrow(residuals), choose(4, 2))
    # Compare all numeric results against the same data with ordinary names.
    expect_equal(agreement[-1], reference$agreement_stats$asDF[-1])
    expect_equal(residuals$bvr, reference$conditional_dependence$asDF$bvr,
                 tolerance = 1e-8)
    pairs <- combn(seq_along(nms), 2, simplify = FALSE)
    expected <- vapply(pairs, function(p) mean(d[[p[1]]] == d[[p[2]]]), numeric(1))
    expect_equal(agreement$agreement, expected)
  }
})

test_that("three-test guide acknowledges discrepancies at nominal zero df", {
  skip_if_not_installed("poLCA")
  # All pairwise covariances are negative: incompatible with this two-class model.
  r <- ngsi_run(ngsi_data(c(10, 80, 80, 10, 80, 10, 10, 10)), method = "latent_class")
  expect_match(r$method_guide$content, "probability constraints")
  expect_false(grepl("with exactly three tests its fit cannot be tested",
                     r$method_guide$content, fixed = TRUE))
  fit <- r$model_fit$asDF
  expect_gt(fit$value[fit$statistic == "G-squared"], 0)
  expect_equal(nrow(r$conditional_dependence$asDF), 3L)
})

test_that("heatmap renders with jamovi's palette and without analysis data", {
  skip_if_not_installed("ggplot2")
  d <- ngsi_data(c(30, 12, 15, 18, 20, 21, 24, 40))
  args <- ngsi_args(d)
  args$data <- NULL
  analysis <- ClinicoPath:::nogoldstandardClass$new(
    options = do.call(ClinicoPath:::nogoldstandardOptions$new, args), data = d)
  analysis$run()
  analysis$.__enclos_env__$private$.data <- NULL
  plot <- analysis$results$agreement_plot
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  # saveAs/file existence alone used to pass even when the renderer fell back.
  # Both fallback paths emit an error message, so require a clean actual render.
  expect_message(expect_true(analysis$.createImage(
    plot$.__enclos_env__$private$.renderFun, plot)), NA)
})

test_that("every shipped example supplies the required level arguments", {
  path <- testthat::test_path("..", "..", "inst", "examples", "nogoldstandard_example.R")
  calls <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("nogoldstandard")),
                  as.list(parse(path)))
  expect_length(calls, 10L)
  for (call in calls) {
    args <- as.list(call)[-1]
    expect_true(all(paste0("test", 1:5, "Positive") %in% names(args)))
    if (args$method %in% c("latent_class", "bayesian")) {
      expect_true(all(paste0("test", 1:3) %in% names(args)))
    }
  }
})
