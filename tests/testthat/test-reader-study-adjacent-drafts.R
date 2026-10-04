# Bounded repairs from the thyroid FNA reader-study review. Fit the real backends
# against package references without regenerating or loading the umbrella package.
reader_draft_harness <- function(name, options = list(), data = data.frame()) {
  root <- testthat::test_path("..", "..")
  env <- new.env(parent = globalenv())
  assign(paste0(name, "Base"), R6::R6Class(paste0(name, "TestBase")), env)
  sys.source(file.path(root, "R", paste0(name, ".b.R")), env)
  generator <- get(paste0(name, "Class"), env)
  schema <- yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".a.yaml")))
  defaults <- setNames(lapply(schema$options, function(x) x$default),
                       vapply(schema$options, `[[`, character(1), "name"))
  defaults[names(options)] <- options
  result_schema <- yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".r.yaml")))
  mock_result <- function() {
    e <- new.env(parent = emptyenv())
    e$rows <- list(); e$notes <- list(); e$titles <- list(); e$content <- ""
    e$visible <- TRUE
    e$addRow <- function(rowKey, values) e$rows[[as.character(rowKey)]] <- values
    e$setRow <- function(rowNo, values) e$rows[[as.character(rowNo)]] <- values
    e$deleteRows <- function() e$rows <- list()
    e$setContent <- function(x) e$content <- x
    e$setVisible <- function(x) e$visible <- x
    e$isVisible <- function() e$visible
    e$setState <- function(x) e$state <- x
    e$setNote <- function(key, note) e$notes[[key]] <- note
    e$getColumn <- function(name) list(setTitle = function(x) e$titles[[name]] <- x)
    e
  }
  self <- new.env()
  self$options <- defaults
  self$data <- data
  self$setError <- function(message) stop(message, call. = FALSE)
  self$results <- setNames(lapply(result_schema$items, function(x) mock_result()),
                          vapply(result_schema$items, `[[`, character(1), "name"))
  private <- new.env()
  bound <- list2env(list(self = self, private = private), parent = env)
  for (method in names(generator$private_methods)) {
    fn <- generator$private_methods[[method]]
    environment(fn) <- bound
    private[[method]] <- fn
  }
  for (field in names(generator$private_fields))
    private[[field]] <- generator$private_fields[[field]]
  private$.checkpoint <- function() invisible(NULL)
  list(self = self, private = private)
}

reader_draft_live <- function(name, options = list(), data = data.frame()) {
  root <- testthat::test_path("..", "..")
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "R", paste0(name, ".h.R")), env)
  sys.source(file.path(root, "R", paste0(name, ".b.R")), env)
  opts <- do.call(get(paste0(name, "Options"), env)$new, options)
  analysis <- get(paste0(name, "Class"), env)$new(options = opts, data = data)
  analysis$run()
  analysis
}

reader_draft_change <- function(analysis, name, value) {
  option <- analysis$options$option(name)
  option$value <- value
  analysis$optionsChangedHandler(name)
  analysis$run()
  invisible(analysis)
}

test_that("Gamma GEE uses log link and preserves planned waves after row shuffling", {
  skip_if_not_installed("geepack")
  set.seed(9051)
  d <- expand.grid(visit = c(1L, 2L, 4L), cluster = seq_len(35))
  d$x <- rnorm(nrow(d))
  d$y <- rgamma(nrow(d), shape = 4, scale = exp(1 + .25 * d$x) / 4)
  opts <- list(outcome = "y", predictors = "x", cluster_id = "cluster",
               time_var = "visit", family = "gamma", corstr = "ar1",
               qic = TRUE, posthoc = FALSE, diagnostics = TRUE)
  h <- reader_draft_harness("geemodel", opts, d[sample(nrow(d)), ])
  h$private$.run()
  ref <- geepack::geeglm(y ~ x, data = d, id = cluster, waves = visit,
                        family = Gamma(link = "log"), corstr = "ar1")
  expect_identical(h$private$.model$family$link, "log")
  expect_equal(unname(coef(h$private$.model)), unname(coef(ref)), tolerance = 1e-9)
  expect_equal(h$private$.model$geese$vbeta, ref$geese$vbeta, tolerance = 1e-9)
  criteria <- geepack::QIC(ref)
  expect_equal(h$self$results$qicTable$rows[[1]]$qic, unname(criteria["QIC"]),
               tolerance = 1e-8)
  expect_equal(h$self$results$qicTable$rows[[1]]$qicu, unname(criteria["QICu"]),
               tolerance = 1e-8)
  expect_true(is.na(h$self$results$qicTable$rows[[1]]$aic))
  expect_identical(h$self$results$coefficientsTable$rows[[1]]$df, 1L)
  prepared <- h$private$.prepareClusterOrder(d, "cluster", "visit", "ar1")
  expect_identical(prepared$waves, d$visit)
  expect_error(h$private$.prepareClusterOrder(d, "cluster", NULL, "ar1"), "Select")
  duplicate <- rbind(d, d[1, ])
  expect_error(h$private$.prepareClusterOrder(duplicate, "cluster", "visit", "ar1"),
               "at most one")
  d$visit[1] <- 1.5
  expect_error(h$private$.prepareClusterOrder(d, "cluster", "visit", "ar1"), "integer")
})

test_that("mixed model includes both crossed groups and checked emmeans contrasts", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("emmeans")
  set.seed(9052)
  d <- expand.grid(case = factor(seq_len(24)), reader = factor(seq_len(6)),
                   modality = factor(c("A", "B")))
  d$y <- 10 + 2 * (d$modality == "B") + rnorm(24)[d$case] +
    rnorm(6)[d$reader] + rnorm(nrow(d))
  names(d)[names(d) == "case"] <- "case ID"
  opts <- list(dependent = "y", fixed_factors = "modality",
               random_factors = c("case ID", "reader"), model_type = "crossed",
               show_assumptions = FALSE, show_posthoc = TRUE, show_plots = FALSE,
               show_effect_sizes = FALSE, confidence_level = .90)
  h <- reader_draft_harness("mixedmodelanova", opts, d)
  h$private$.run()
  contrasts(d$modality) <- contr.sum(2)
  ref <- lmerTest::lmer(y ~ modality + (1 | `case ID`) + (1 | reader), data = d)
  expect_equal(lme4::fixef(h$private$.model), lme4::fixef(ref), tolerance = 1e-8)
  expect_setequal(names(h$private$.model@flist), c("case ID", "reader"))
  comparison <- as.data.frame(summary(emmeans::contrast(
    emmeans::emmeans(ref, "modality"), "pairwise"), infer = c(TRUE, TRUE), level = .90))
  row <- h$self$results$posthoc$rows[[1]]
  expect_equal(row$estimate, comparison$estimate, tolerance = 1e-8)
  expect_equal(row$lower_ci, comparison$lower.CL, tolerance = 1e-8)
  expect_equal(row$p_value, comparison$p.value, tolerance = 1e-8)
  expect_length(h$self$results$icc_table$rows, 2)
  h$self$options$model_type <- "random_intercept"
  expect_error(h$private$.buildFormula(), "crossed")
})

test_that("CLMM fits valid data and never advertises unsupported tests as results", {
  skip_if_not_installed("ordinal")
  d <- ordinal::wine
  opts <- list(dep = "rating", fixedFactors = c("temp", "contact"),
               randomTerms = "judge", propOddsTest = TRUE, conditionMeans = TRUE)
  h <- reader_draft_harness("ordinalmixedmodel", opts, d)
  prepared <- h$private$.prepareData()
  expect_equal(nrow(prepared), 72)
  fit <- h$private$.fitModel(prepared)
  ref <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = d)
  expect_equal(unname(fit$beta), unname(ref$beta), tolerance = 1e-8)
  h$private$.run()
  expect_length(h$self$results$propOddsTestTable$rows, 0)
  expect_match(h$self$results$propOddsTestTable$notes$unavailable, "no test was performed")
  expect_length(h$self$results$conditionProbs$rows, 0)
  h$self$options$link <- "probit"
  h$self$results$fixedEffects$deleteRows()
  h$private$.run()
  row <- h$self$results$fixedEffects$rows[[1]]
  expect_null(row$or)
  expect_equal(row$ciLower, row$estimate - qnorm(.975) * row$se)
  expect_match(h$self$results$fixedEffects$notes$ci_scale, "coefficient")
})

test_that("paired t planning counts complete pairs and agrees with noncentral t reference", {
  skip_if_not_installed("pwr")
  opts <- list(test_type = "paired_ttest", effect_size = .5, power = .8,
               alpha = .05, two_sided = TRUE, sample_size = 40,
               dropout_rate = 20)
  h <- reader_draft_harness("clinicaltrialdesign", opts)
  for (calc in c("sample_size", "power", "effect_size")) {
    h$self$options$calculation_type <- calc
    result <- h$private$.calculatePairedTTest()
    reference <- switch(calc,
      sample_size = stats::power.t.test(delta = .5, sd = 1, power = .8,
                                        type = "paired", strict = TRUE)$n,
      power = stats::power.t.test(n = 40, delta = .5, sd = 1,
                                  type = "paired", strict = TRUE)$power,
      effect_size = stats::power.t.test(n = 40, sd = 1, power = .8,
                                        delta = NULL, type = "paired", strict = TRUE)$delta)
    value <- switch(calc, sample_size = result$n, power = result$power,
                     effect_size = result$effect_size)
    expect_equal(value, if (calc == "sample_size") ceiling(reference) else reference,
                 tolerance = 1e-4)
  }
  h$self$options$calculation_type <- "sample_size"
  h$private$.run()
  n <- h$self$results$sample_size_breakdown$rows$paired$total
  expect_equal(n, 34)
  expect_equal(h$self$results$sample_size_breakdown$rows$recruit$total, ceiling(n / .8))
  expect_false(h$self$results$study_protocol_template$visible)
  h$self$options$test_type <- "mcnemar"
  expect_error(h$private$.run(), "discordant-pair")
  expect_length(h$self$results$power_results$rows, 0)
})

test_that("precision planner changes comparator interpretation without changing sample sizes", {
  h <- reader_draft_harness("diagnosticsamplesize", list(
    show_comparison_table = TRUE, show_method_comparison = TRUE, alpha = .10))
  h$private$.run()
  disease <- h$private$.results_cache
  expect_match(h$self$results$sample_size_statement$content, "90.0% Clopper")
  h$self$options$referenceType <- "consensus"
  h$private$.run()
  agreement <- h$private$.results_cache
  expect_identical(agreement$n_required, disease$n_required)
  expect_match(h$self$results$sample_size_summary$rows[[1]]$parameter, "PPA")
  expect_match(h$self$results$prevalence_table$titles$prevalence, "Reference-positive")
  expect_match(h$self$results$methodology$content, "not include comparator uncertainty")
  expect_match(h$self$results$sample_size_statement$content, "do not establish disease")
  expect_length(h$self$results$sample_size_summary$rows, 3)
  h$self$options$comparison_prevalences <- "0, 0.5"
  expect_error(h$private$.generateComparisonTable(), "strictly between")
})

test_that("generated draft interfaces run and refresh comparator and coefficient scales", {
  skip_if_not_installed("ordinal")
  planner <- reader_draft_live("diagnosticsamplesize", list(show_comparison_table = TRUE))
  disease <- planner$results$sample_size_summary$asDF$n_total
  expect_silent(reader_draft_change(planner, "referenceType", "consensus"))
  expect_equal(planner$results$sample_size_summary$asDF$n_total, disease)
  expect_match(planner$results$sample_size_summary$asDF$parameter[1], "PPA")
  if (requireNamespace("RProtoBuf", quietly = TRUE))
    expect_no_error(planner$results$asProtoBuf())

  ordinal <- reader_draft_live("ordinalmixedmodel", list(dep = "rating",
    fixedFactors = c("temp", "contact"), randomTerms = "judge",
    propOddsTest = TRUE, conditionMeans = TRUE), ordinal::wine)
  expect_equal(nrow(ordinal$results$fixedEffects$asDF), 2)
  expect_no_error(reader_draft_change(ordinal, "link", "probit"))
  coefficients <- ordinal$results$fixedEffects$asDF
  expect_equal(coefficients$ciLower, coefficients$estimate - qnorm(.975) * coefficients$se)
  expect_true(all(is.na(coefficients$or)))
  expect_equal(nrow(ordinal$results$propOddsTestTable$asDF), 0)
  expect_equal(nrow(ordinal$results$conditionProbs$asDF), 0)
  if (requireNamespace("RProtoBuf", quietly = TRUE))
    expect_no_error(ordinal$results$asProtoBuf())

  trial <- reader_draft_live("clinicaltrialdesign", list(test_type = "paired_ttest"))
  expect_equal(trial$results$sample_size_breakdown$asDF$total[1], 34)
  expect_no_error(reader_draft_change(trial, "calculation_type", "power"))
  expect_equal(trial$results$sample_size_breakdown$rowCount, 0)
  if (requireNamespace("RProtoBuf", quietly = TRUE))
    expect_no_error(trial$results$asProtoBuf())
  expect_error(reader_draft_change(trial, "test_type", "mcnemar"), "discordant-pair")
  expect_equal(trial$results$power_results$rowCount, 0)
})

test_that("generated GEE and crossed model interfaces produce real fitted tables", {
  skip_if_not_installed("geepack")
  skip_if_not_installed("lmerTest")
  set.seed(9054)
  d <- expand.grid(subject = factor(seq_len(18)), reader = factor(seq_len(5)),
                   wave = seq_len(3))
  d$x <- rnorm(nrow(d))
  d$y <- exp(1 + .2 * d$x + rnorm(18, sd = .25)[d$subject] +
    rnorm(5, sd = .25)[d$reader] + rnorm(nrow(d), sd = .3))
  mixed <- reader_draft_live("mixedmodelanova", list(dependent = "y", covariates = "x",
    random_factors = c("subject", "reader"), model_type = "crossed",
    show_assumptions = FALSE, show_plots = FALSE), d)
  expect_equal(nrow(mixed$results$icc_table$asDF), 2)
  expect_match(mixed$results$model_info$asDF$n_groups, "subject: 18")
  if (requireNamespace("RProtoBuf", quietly = TRUE))
    expect_no_error(mixed$results$asProtoBuf())
  d <- d[d$reader == levels(d$reader)[1], ]
  gee <- reader_draft_live("geemodel", list(outcome = "y", predictors = "x",
    cluster_id = "subject", time_var = "wave", family = "gamma", corstr = "ar1",
    qic = TRUE, diagnostics = TRUE), d[sample(nrow(d)), ])
  expect_match(gee$results$modelInfo$asDF$value[4], "Gamma.*Log")
  expect_equal(nrow(gee$results$coefficientsTable$asDF), 2)
  expect_equal(nrow(gee$results$diagnosticsTable$asDF), 5)
  expect_true(is.finite(gee$results$qicTable$asDF$qic))
  if (requireNamespace("RProtoBuf", quietly = TRUE))
    expect_no_error(gee$results$asProtoBuf())
})
