# mixedcox: every displayed number is checked against an independent coxme/coxph fit.

# Generated and backend classes are namespace-internal: visible under load_all(),
# reached through the namespace for an installed build.
internal_object <- function(name) {
  if (exists(name)) get(name) else utils::getFromNamespace(name, "ClinicoPath")
}

lung_data <- function() {
  d <- survival::lung
  d <- d[!is.na(d$inst), c("time", "status", "age", "sex", "inst")]
  d$inst <- factor(d$inst)
  d$sex <- factor(d$sex, levels = 1:2, labels = c("male", "female"))
  d$status <- factor(d$status, levels = 1:2, labels = c("censored", "dead"))
  d
}

# Log-normal frailty (variance sigma2) on the log hazard, clusters nested in hospitals
sim_data <- function(n_clusters = 30, per_cluster = 20, sigma2 = 0.5, seed = 20260930) {
  withr::local_seed(seed)
  n <- n_clusters * per_cluster
  cluster <- factor(sprintf("C%02d", rep(seq_len(n_clusters), each = per_cluster)))
  b <- rep(rnorm(n_clusters, sd = sqrt(sigma2)), each = per_cluster)
  x <- rnorm(n)
  g <- factor(sample(c("A", "B"), n, replace = TRUE))
  event_time <- rexp(n, rate = 0.05 * exp(0.5 * x + 0.6 * (g == "B") + b))
  censor_time <- rexp(n, rate = 0.02)
  data.frame(
    time = pmin(event_time, censor_time),
    status = factor(ifelse(event_time <= censor_time, "event", "censored"),
                    levels = c("censored", "event")),
    x = x, g = g, cluster = cluster,
    hosp = factor(sprintf("H%d", (as.integer(cluster) - 1) %/% 5))
  )
}

fit_sim <- function(data, ...) {
  mixedcox(data = data, elapsedtime = "time", outcome = "status", outcomeLevel = "event",
           fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster", ...)
}

expect_matches_oracle <- function(table, fit) {
  beta <- unname(coxme::fixef(fit))
  se <- unname(sqrt(diag(as.matrix(stats::vcov(fit)))))
  z <- beta / se
  expect_equal(table$coefficient, beta, tolerance = 1e-8)
  expect_equal(table$se, se, tolerance = 1e-8)
  expect_equal(table$z_statistic, z, tolerance = 1e-8)
  expect_equal(table$p_value, 2 * stats::pnorm(-abs(z)), tolerance = 1e-8)
  expect_equal(table$hazard_ratio, exp(beta), tolerance = 1e-8)
  expect_equal(table$hr_lower, exp(beta - stats::qnorm(0.975) * se), tolerance = 1e-8)
  expect_equal(table$hr_upper, exp(beta + stats::qnorm(0.975) * se), tolerance = 1e-8)
}

test_that("mixedcox yaml: implemented options, no images, jamovi binding grammar, complete clearWith", {
  skip_if_not_installed("yaml")

  analysis <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.a.yaml"))
  results <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.r.yaml"))
  ui_text <- readLines(testthat::test_path("..", "..", "jamovi", "mixedcox.u.yaml"))

  offered <- vapply(analysis$options, `[[`, "", "name")
  expect_setequal(offered, c(
    "data", "elapsedtime", "tint", "dxdate", "fudate", "timetypedata",
    "timetypeoutput", "outcome", "outcomeLevel", "fixed_effects",
    "continuous_effects", "cluster_var", "random_effects", "random_slope_var",
    "nested_clustering", "nested_cluster_var", "sparse_matrix",
    "icc_calculation", "show_fixed_effects", "show_random_effects",
    "show_model_comparison"
  ))
  expect_setequal(vapply(results$items, `[[`, "", "name"),
                  c("todo", "modelSummary", "fixedEffectsTable",
                    "randomEffectsSummary", "modelComparison"))
  # No image can be enabled until its renderer actually creates an image.
  expect_false(any(vapply(results$items, function(item) item$type == "Image", FALSE)))

  # coxme cannot fit a random slope for a factor
  slope <- analysis$options[[which(offered == "random_slope_var")]]
  expect_identical(unlist(slope$permitted), "numeric")

  # jamovi's binding grammar is name, name:value, !, &&, || - an unknown operator
  # makes the binding null and freezes a CheckBox at its default.
  enable <- grep("enable:", ui_text, value = TRUE, fixed = TRUE)
  expect_false(any(grepl("==|!=", enable)))
  expect_true(any(grepl("enable: (!random_effects:slope)", enable, fixed = TRUE)))
  expect_true(any(grepl("enable: (random_effects:slope || random_effects:both)", enable, fixed = TRUE)))
  expect_false(any(grepl("Calculated Time to Data", ui_text, fixed = TRUE)))

  # A model-defining change must clear every output, or a failing re-run shows the old model
  model_options <- c("outcome", "outcomeLevel", "elapsedtime", "tint", "dxdate", "fudate",
                     "timetypedata", "timetypeoutput", "fixed_effects", "continuous_effects",
                     "cluster_var", "random_effects", "random_slope_var", "nested_clustering",
                     "nested_cluster_var", "sparse_matrix")
  for (item in results$items) {
    expect_true(all(model_options %in% unlist(item$clearWith)), info = item$name)
  }
})

test_that("mixedcoxClass inherits the generated mixedcoxBase (hand-edited header regression)", {
  # get_inherit() errors with "object 'mixedcoxBase' not found" when the header lacks it
  base <- internal_object("mixedcoxClass")$get_inherit()
  expect_identical(base$classname, "mixedcoxBase")
  expect_true(R6::is.R6Class(base))
})

test_that("intercept, slope, intercept+slope and nested models all fit (asFormula '|' regression)", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  configs <- list(
    intercept = list(),
    slope = list(random_effects = "slope", random_slope_var = "x"),
    both = list(random_effects = "both", random_slope_var = "x"),
    nested = list(nested_clustering = TRUE, nested_cluster_var = "hosp")
  )
  for (name in names(configs)) {
    res <- do.call(fit_sim, c(list(data = d), configs[[name]]))
    expect_false(grepl("Error", res$todo$content, fixed = TRUE), info = name)
    expect_equal(res$fixedEffectsTable$rowCount, 2L, info = name)
  }
})

test_that("lung (1 | inst): fixed effects equal coxme fixef/vcov and the table is filled (addRow regression)", {
  skip_if_not_installed("coxme")
  d <- lung_data()
  res <- mixedcox(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "dead",
                  fixed_effects = "sex", continuous_effects = "age", cluster_var = "inst")
  fit <- coxme::coxme(survival::Surv(time, status == "dead") ~ sex + age + (1 | inst), data = d)
  cox <- survival::coxph(survival::Surv(time, status == "dead") ~ sex + age, data = d)
  table <- res$fixedEffectsTable$asDF

  expect_identical(table$variable, c("sex: female", "age"))
  expect_matches_oracle(table, fit)
  # z is coef/se, not summary.coxme()'s round(beta/se, 2)
  expect_gt(abs(table$z_statistic[1] - round(table$z_statistic[1], 2)), 1e-4)

  expect_match(res$modelSummary$content, "Observations:</b> 227", fixed = TRUE)
  expect_match(res$modelSummary$content, "Clusters:</b> 18", fixed = TRUE)
  expect_match(res$randomEffectsSummary$content,
               paste0("inst (intercept variance):</b> ", signif(unname(coxme::VarCorr(fit)$inst), 4)),
               fixed = TRUE)

  # Variance at the boundary: the integrated log-likelihood sits just below coxph's, so the
  # statistic is truncated at 0 and the boundary-corrected p is 0.5.
  expect_lt(fit$loglik[["Integrated"]], cox$loglik[2])
  expect_match(res$modelComparison$content, sprintf("%.4f", cox$loglik[2]), fixed = TRUE)
  expect_match(res$modelComparison$content, "Likelihood-ratio statistic:</b> 0.0000", fixed = TRUE)
  expect_match(res$modelComparison$content, "Boundary-corrected p-value:</b> 0.500", fixed = TRUE)
})

test_that("simulated frailty data: coefficients, variance, pi^2/6 fraction and LR test equal the oracle", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  res <- fit_sim(d)
  fit <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 | cluster), data = d)
  cox <- survival::coxph(survival::Surv(time, status == "event") ~ g + x, data = d)

  expect_identical(res$fixedEffectsTable$asDF$variable, c("g: B", "x"))
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)

  s2 <- unname(coxme::VarCorr(fit)$cluster)
  expect_match(res$randomEffectsSummary$content,
               paste0("cluster (intercept variance):</b> ", signif(s2, 4)), fixed = TRUE)
  # Latent residual of a PH model is standard extreme-value (pi^2/6), not logistic (pi^2/3)
  expect_match(res$modelSummary$content,
               sprintf("variance fraction:</b> %.4f", s2 / (s2 + pi^2 / 6)), fixed = TRUE)
  expect_false(grepl(sprintf("%.4f", s2 / (s2 + pi^2 / 3)), res$modelSummary$content, fixed = TRUE))

  lr <- 2 * (fit$loglik[["Integrated"]] - cox$loglik[2])
  expect_gt(lr, 10)
  expect_match(res$modelComparison$content,
               sprintf("Likelihood-ratio statistic:</b> %.4f", lr), fixed = TRUE)
  expect_lt(0.5 * stats::pchisq(lr, 1, lower.tail = FALSE), 0.001)
  expect_match(res$modelComparison$content, "Boundary-corrected p-value:</b> &lt; 0.001", fixed = TRUE)
})

test_that("the random-slope variable joins the fixed effects; slope-only models get a boundary p", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  both <- mixedcox(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "event",
                   fixed_effects = "g", cluster_var = "cluster",
                   random_effects = "both", random_slope_var = "x")
  fit <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 + x | cluster), data = d)
  vc <- coxme::VarCorr(fit)$cluster

  expect_identical(both$fixedEffectsTable$asDF$variable, c("g: B", "x"))
  expect_matches_oracle(both$fixedEffectsTable$asDF, fit)
  expect_match(both$modelSummary$content, "was added to the fixed effects", fixed = TRUE)
  expect_match(both$randomEffectsSummary$content,
               paste0("(slope variance, x):</b> ", signif(vc[2, 2], 4)), fixed = TRUE)
  expect_match(both$randomEffectsSummary$content,
               paste0("(intercept-slope correlation):</b> ", signif(vc[1, 2], 4)), fixed = TRUE)
  expect_match(both$modelSummary$content,
               sprintf("%.4f (at x = 0)", vc[1, 1] / (vc[1, 1] + pi^2 / 6)), fixed = TRUE)
  expect_match(both$modelComparison$content, "No p-value", fixed = TRUE)

  slope <- fit_sim(d, random_effects = "slope", random_slope_var = "x")
  fit_s <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (x | cluster), data = d)
  cox <- survival::coxph(survival::Surv(time, status == "event") ~ g + x, data = d)
  lr <- max(0, 2 * (fit_s$loglik[["Integrated"]] - cox$loglik[2]))
  expect_matches_oracle(slope$fixedEffectsTable$asDF, fit_s)
  expect_match(slope$modelComparison$content,
               sprintf("Boundary-corrected p-value:</b> %.3f", 0.5 * stats::pchisq(lr, 1, lower.tail = FALSE)),
               fixed = TRUE)
  expect_false(grepl("variance fraction", slope$modelSummary$content, fixed = TRUE))
})

test_that("nested (1 | hosp/cluster) shows both variance components and uses both in the fraction", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  res <- fit_sim(d, nested_clustering = TRUE, nested_cluster_var = "hosp")
  fit <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 | hosp/cluster), data = d)
  vc <- coxme::VarCorr(fit)
  inner <- unname(vc[["hosp/cluster"]])
  outer <- unname(vc[["hosp"]])
  total <- inner + outer + pi^2 / 6

  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
  expect_match(res$randomEffectsSummary$content,
               paste0("cluster within hosp (intercept variance):</b> ", signif(inner, 4)), fixed = TRUE)
  expect_match(res$randomEffectsSummary$content,
               paste0("<b>hosp (intercept variance):</b> ", signif(outer, 4)), fixed = TRUE)
  expect_match(res$modelSummary$content,
               sprintf("%.4f for the same cluster; %.4f for the same hosp", (inner + outer) / total, outer / total),
               fixed = TRUE)
  expect_match(res$modelComparison$content, "No p-value", fixed = TRUE)
})

test_that("nested clustering is ignored, with a note, for slope models and drops no rows", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  d$hosp[1:100] <- NA
  res <- fit_sim(d, random_effects = "slope", random_slope_var = "x",
                 nested_clustering = TRUE, nested_cluster_var = "hosp")
  expect_match(res$modelSummary$content, "Observations:</b> 600", fixed = TRUE)
  expect_match(res$modelSummary$content, "(x | cluster)", fixed = TRUE)
  expect_match(res$modelSummary$content, "Nested clustering is fitted only with a random intercept", fixed = TRUE)
})

test_that("the minimum-cluster guard counts clusters left after complete-case removal", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  three <- d[as.integer(d$cluster) <= 3, ]  # 30 declared levels, 3 observed clusters
  res <- fit_sim(three)
  expect_match(res$todo$content, "Insufficient Clusters", fixed = TRUE)
  expect_equal(res$fixedEffectsTable$rowCount, 0L)

  six <- d[as.integer(d$cluster) <= 6, ]
  six$x[as.integer(six$cluster) > 3] <- NA  # three clusters lose every row
  expect_match(fit_sim(six)$todo$content, "(found 3)", fixed = TRUE)
})

test_that("non-syntactic column names are shown as typed, and random slopes on them fit", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  named <- data.frame(`Survival Time` = d$time, `Status (1=event)` = d$status, `Grade 2/3` = d$g,
                      `Age (years)` = d$x, `Hospital ID` = d$cluster, check.names = FALSE)
  res <- mixedcox(data = named, elapsedtime = "Survival Time", outcome = "Status (1=event)",
                  outcomeLevel = "event", fixed_effects = "Grade 2/3",
                  continuous_effects = "Age (years)", cluster_var = "Hospital ID",
                  random_effects = "both", random_slope_var = "Age (years)")
  fit <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 + x | cluster), data = d)

  expect_identical(res$fixedEffectsTable$asDF$variable, c("Grade 2/3: B", "Age (years)"))
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
  expect_match(res$randomEffectsSummary$content, "Hospital ID (intercept variance)", fixed = TRUE)
  expect_false(grepl("Hospital.ID", res$randomEffectsSummary$content, fixed = TRUE))
})

test_that("sparse_matrix switches between coxme's sparse approximation and the full matrix", {
  skip_if_not_installed("coxme")
  d <- sim_data(n_clusters = 120, per_cluster = 5, seed = 7)
  variances <- c()
  for (sparse in c(TRUE, FALSE)) {
    res <- fit_sim(d, sparse_matrix = sparse)
    fit <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 | cluster), data = d,
                        control = coxme::coxme.control(sparse = if (sparse) c(50, 0.02) else c(Inf, 0)))
    variances <- c(variances, unname(coxme::VarCorr(fit)$cluster))
    expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
    expect_match(res$randomEffectsSummary$content,
                 paste0("(intercept variance):</b> ", signif(tail(variances, 1), 4)), fixed = TRUE)
  }
  expect_false(isTRUE(all.equal(variances[1], variances[2])))  # an approximation, not a no-op
})

test_that("dates give the same fit as the equivalent elapsed time", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  d$days <- pmax(1, round(d$time * 30))
  start <- as.Date("2015-01-01") + seq_len(nrow(d)) %% 365
  d$dxdate <- format(start, "%Y-%m-%d")
  d$fudate <- format(start + d$days, "%Y-%m-%d")
  res <- mixedcox(data = d, tint = TRUE, dxdate = "dxdate", fudate = "fudate",
                  timetypedata = "ymd", timetypeoutput = "months",
                  outcome = "status", outcomeLevel = "event",
                  fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster")
  fit <- coxme::coxme(survival::Surv(days, status == "event") ~ g + x + (1 | cluster), data = d)
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
})

test_that("icc_calculation and the show_* switches only hide output", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  expect_false(grepl("variance fraction", fit_sim(d, icc_calculation = FALSE)$modelSummary$content,
                     fixed = TRUE))
  hidden <- fit_sim(d, show_fixed_effects = FALSE, show_random_effects = FALSE,
                    show_model_comparison = FALSE)
  expect_match(hidden$modelSummary$content, "Clusters:</b> 30", fixed = TRUE)
  expect_equal(hidden$fixedEffectsTable$rowCount, 0L)
  expect_identical(hidden$randomEffectsSummary$content, "")
  expect_identical(hidden$modelComparison$content, "")
})

test_that("a re-run that stops before fitting clears the previous model's output", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  analysis <- internal_object("mixedcoxClass")$new(
    options = internal_object("mixedcoxOptions")$new(
      elapsedtime = "time", outcome = "status", outcomeLevel = "event",
      fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster"),
    data = d[as.integer(d$cluster) <= 3, ])
  analysis$init()
  # What jamovi restores from the previous run for items no changed option clears
  analysis$results$modelSummary$setContent("<p>previous model</p>")
  analysis$results$randomEffectsSummary$setContent("<p>previous variances</p>")
  analysis$results$modelComparison$setContent("<p>previous comparison</p>")
  analysis$results$fixedEffectsTable$addRow(rowKey = "old", values = list(variable = "old"))
  analysis$run()

  expect_match(analysis$results$todo$content, "Insufficient Clusters", fixed = TRUE)
  expect_identical(analysis$results$modelSummary$content, "")
  expect_identical(analysis$results$randomEffectsSummary$content, "")
  expect_identical(analysis$results$modelComparison$content, "")
  expect_equal(analysis$results$fixedEffectsTable$rowCount, 0L)
})
