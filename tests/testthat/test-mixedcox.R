# mixedcox: every displayed number is checked against an independent coxme/coxph fit.

# A parked analysis (T/P/D menuGroup suffix) is removed from its submodule by the updater
skip_if_not(exists("mixedcox"), "mixedcox is not in this module (parked analysis)")

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

# sim_data() with whole-day survival times and the two dates as text, as jamovi
# passes a date column. base::format(): jmvcore's format() returns a Date unchanged.
date_data <- function(date_format = "%Y-%m-%d") {
  d <- sim_data()
  d$days <- pmax(1, round(d$time * 30))
  start <- as.Date("2015-01-01") + seq_len(nrow(d)) %% 365
  d$dxdate <- base::format(start, date_format)
  d$fudate <- base::format(start + d$days, date_format)
  d
}

fit_dates <- function(data, ...) {
  data$dxdate <- factor(data$dxdate)
  data$fudate <- factor(data$fudate)
  mixedcox(data = data, tint = TRUE, dxdate = "dxdate", fudate = "fudate",
           outcome = "status", outcomeLevel = "event",
           fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster", ...)
}

# One variance tested at its boundary: 50:50 mixture of chi-square(0) and chi-square(1)
boundary_p <- function(lr) if (lr > 0) 0.5 * stats::pchisq(lr, 1, lower.tail = FALSE) else 1

# The message of the error an expression raises, or "" when it raises none
error_message <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)

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
  skip_if_not(file.exists(testthat::test_path("..", "..", "jamovi", "mixedcox.a.yaml")),
              "mixedcox yaml files are not in this tree (installed build or parked analysis)")

  analysis <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.a.yaml"))
  results <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.r.yaml"))
  ui_text <- readLines(testthat::test_path("..", "..", "jamovi", "mixedcox.u.yaml"))

  offered <- vapply(analysis$options, `[[`, "", "name")
  # timetypeoutput was removed: the Cox partial likelihood depends only on the order
  # of the times and no time is displayed, so an output unit changed nothing
  expect_setequal(offered, c(
    "data", "elapsedtime", "tint", "dxdate", "fudate", "timetypedata",
    "outcome", "outcomeLevel", "fixed_effects",
    "continuous_effects", "cluster_var", "random_effects", "random_slope_var",
    "nested_clustering", "nested_cluster_var", "sparse_matrix",
    "icc_calculation", "show_fixed_effects", "show_random_effects",
    "show_model_comparison"
  ))
  expect_false(any(grepl("timetypeoutput", ui_text, fixed = TRUE)))
  expect_false("timetypeoutput" %in% unlist(lapply(results$items, `[[`, "clearWith")))
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
                     "timetypedata", "fixed_effects", "continuous_effects",
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
  # statistic is truncated at 0. Under the 50:50 chi-square(0)/chi-square(1) mixture
  # P(T >= 0) = 1 (Stata: chibar2(01) = 0.00, Prob >= chibar2 = 1.000), not half of it.
  expect_lt(fit$loglik[["Integrated"]], cox$loglik[2])
  expect_match(res$modelComparison$content, sprintf("%.4f", cox$loglik[2]), fixed = TRUE)
  expect_match(res$modelComparison$content, "Likelihood-ratio statistic:</b> 0.0000", fixed = TRUE)
  expect_match(res$modelComparison$content, "Boundary-corrected p-value:</b> 1.000", fixed = TRUE)
  expect_false(grepl("0.500", res$modelComparison$content, fixed = TRUE))
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
  expect_lt(boundary_p(lr), 0.001)
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
  expect_gt(lr, 0)  # a positive statistic: p is half the chi-square(1) tail
  expect_matches_oracle(slope$fixedEffectsTable$asDF, fit_s)
  expect_match(slope$modelComparison$content,
               sprintf("Boundary-corrected p-value:</b> %.3f", boundary_p(lr)),
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

test_that("dates (read as text in each format) give the same fit as the equivalent elapsed time", {
  skip_if_not_installed("coxme")
  fit <- coxme::coxme(survival::Surv(days, status == "event") ~ g + x + (1 | cluster), data = date_data())
  formats <- c(ymd = "%Y-%m-%d", mdy = "%m/%d/%Y", dmy = "%d/%m/%Y")
  for (type in names(formats)) {
    d <- date_data(formats[[type]])
    expect_type(d$dxdate, "character")
    res <- fit_dates(d, timetypedata = type)
    expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
    expect_match(res$modelSummary$content, "Observations:</b> 600", fixed = TRUE)
    expect_false(grepl("excluded", res$modelSummary$content, fixed = TRUE), info = type)
  }

  # A Date column (an R data frame) is already a date, whatever text format is selected
  d <- date_data()
  d$dxdate <- as.Date(d$dxdate)
  d$fudate <- as.Date(d$fudate)
  res <- mixedcox(data = d, tint = TRUE, dxdate = "dxdate", fudate = "fudate", timetypedata = "dmy",
                  outcome = "status", outcomeLevel = "event",
                  fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster")
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)

  # An elapsed-time variable left assigned is ignored while dates are ticked
  d <- date_data()
  d$reversed <- max(d$time) + 1 - d$time
  res <- fit_dates(d, elapsedtime = "reversed")
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
  expect_match(res$modelSummary$content, "the Time Elapsed variable 'reversed' was not used", fixed = TRUE)
})

test_that("ticking dates with an elapsed time but no date variables asks for the dates", {
  # Previously: "Date Calculation Error: attempt to select less than one element in get1index"
  d <- sim_data()
  res <- fit_sim(d, tint = TRUE)
  expect_match(res$todo$content, "Please specify: Diagnosis Date and Follow-up Date", fixed = TRUE)
  expect_false(grepl("Error", res$todo$content, fixed = TRUE))
  expect_equal(res$fixedEffectsTable$rowCount, 0L)
  expect_identical(res$modelSummary$content, "")

  d$dxdate <- "2015-01-01"
  only_dx <- fit_sim(d, tint = TRUE, dxdate = "dxdate")
  expect_match(only_dx$todo$content, "Diagnosis Date and Follow-up Date", fixed = TRUE)
})

test_that("a blank or NA date is missing: its row is dropped and counted in a note", {
  skip_if_not_installed("coxme")
  d <- date_data()
  d$dxdate[3] <- NA
  d$fudate[7] <- ""
  d$fudate[11] <- "   "
  res <- fit_dates(d, timetypedata = "ymd")
  fit <- coxme::coxme(survival::Surv(days, status == "event") ~ g + x + (1 | cluster),
                      data = d[-c(3, 7, 11), ])
  expect_identical(res$todo$content, "")
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
  expect_match(res$modelSummary$content, "Observations:</b> 597", fixed = TRUE)
  expect_match(res$modelSummary$content,
               "3 row(s) with a missing diagnosis or follow-up date were excluded.", fixed = TRUE)

  # Too few rows left: the stop message still says how many were dropped
  few <- date_data()
  few$fudate[1:560] <- NA
  res_few <- fit_dates(few)
  expect_match(res_few$todo$content, "(found 40; at least 50 are needed)", fixed = TRUE)
  expect_match(res_few$todo$content, "560 row(s) with a missing diagnosis", fixed = TRUE)
})

test_that("a non-blank date that does not read in the selected format stops the analysis", {
  skip_if_not_installed("coxme")
  # Day-Month-Year text read as Month-Day-Year (or the reverse) still reads every date whose
  # day is 12 or less, with day and month swapped. Fitting only the rows that read gave
  # HR 2.44 against a true 1.67, with nothing but a note.
  swapped <- list(mdy = c(text = "%d/%m/%Y", read = "%m/%d/%Y", title = "Month-Day-Year"),
                  dmy = c(text = "%m/%d/%Y", read = "%d/%m/%Y", title = "Day-Month-Year"))
  for (type in names(swapped)) {
    s <- swapped[[type]]
    d <- date_data(s[["text"]])
    dates <- c(d$dxdate, d$fudate)
    unread <- dates[is.na(as.Date(dates, format = s[["read"]]))]
    expect_true(length(unread) > 0 && length(unread) < length(dates), info = type)  # a partial read
    msg <- error_message(fit_dates(d, timetypedata = type))
    expect_match(msg, paste0(
      length(unread), " of 1200 non-blank dates could not be read as ", s[["title"]],
      " (for example '", paste(unique(unread)[1:3], collapse = "', '"), "')"), fixed = TRUE, info = type)
    expect_match(msg, "Check 'Time Type in Data'", fixed = TRUE, info = type)
  }

  # One typo is enough, and a blank cell is not counted as a date
  d <- date_data()
  d$dxdate[3] <- NA
  d$fudate[7] <- "2015-13-45"
  expect_match(error_message(fit_dates(d)),
               "1 of 1199 non-blank dates could not be read as Year-Month-Day (for example '2015-13-45')",
               fixed = TRUE)
  # A text code for a missing date is not a blank
  d <- date_data()
  d$fudate[1:560] <- "unknown"
  expect_match(error_message(fit_dates(d)), "560 of 1200 non-blank dates could not be read", fixed = TRUE)
  # Nothing readable: Year-Month-Day text read as Day-Month-Year
  expect_match(error_message(fit_dates(date_data(), timetypedata = "dmy")),
               "1200 of 1200 non-blank dates could not be read as Day-Month-Year", fixed = TRUE)
})

test_that("a date is read only when the whole value has the selected layout", {
  skip_if_not_installed("coxme")
  # strptime reads a prefix: '13-04-2016' passed as Year-Month-Day (year 13) and a 2-digit
  # year as year 16, so both fitted confident, wrong tables. Both must stop instead.
  msg <- error_message(fit_dates(date_data("%d-%m-%Y"), timetypedata = "ymd"))
  expect_match(msg, "1200 of 1200 non-blank dates could not be read as Year-Month-Day", fixed = TRUE)
  expect_match(msg, "it expects 2016-12-31", fixed = TRUE)
  d <- date_data("%m/%d/%Y")
  d$fudate[1:60] <- base::format(as.Date(d$fudate[1:60], format = "%m/%d/%Y"), "%m/%d/%y")
  expect_match(error_message(fit_dates(d, timetypedata = "mdy")),
               "60 of 1200 non-blank dates could not be read as Month-Day-Year", fixed = TRUE)
  d <- date_data()
  d$fudate[5] <- paste0(d$fudate[5], "7")  # a trailing digit: '2015-...-..7'
  expect_match(error_message(fit_dates(d)), "1 of 1200 non-blank dates could not be read", fixed = TRUE)
})

test_that("'.', '-' and '/' separators and a trailing time read the same dates", {
  skip_if_not_installed("coxme")
  reference <- fit_dates(date_data("%d/%m/%Y"), timetypedata = "dmy")$fixedEffectsTable$asDF
  for (layout in c("%d.%m.%Y", "%d-%m-%Y")) {
    res <- fit_dates(date_data(layout), timetypedata = "dmy")
    expect_identical(res$todo$content, "", info = layout)
    expect_equal(res$fixedEffectsTable$asDF, reference, info = layout)
  }
  # Excel date-time cells arrive as ISO text with a time
  with_time <- date_data()
  with_time$dxdate <- paste(with_time$dxdate, "00:00:00")
  expect_equal(fit_dates(with_time)$fixedEffectsTable$asDF,
               fit_dates(date_data())$fixedEffectsTable$asDF)
})

test_that("numeric date columns stop with their own message; a non-breaking space is blank", {
  skip_if_not_installed("coxme")
  # A date column switched to Continuous, or a numeric column from R (e.g. an Excel serial)
  d <- date_data()
  d$dxdate <- as.numeric(as.Date(d$dxdate))
  msg <- error_message(mixedcox(data = d, tint = TRUE, dxdate = "dxdate", fudate = "fudate",
                                outcome = "status", outcomeLevel = "event", fixed_effects = "g",
                                continuous_effects = "x", cluster_var = "cluster"))
  expect_match(msg, "holds numbers, not dates", fixed = TRUE)

  d <- date_data()
  d$fudate[9] <- " "
  res <- fit_dates(d)
  expect_identical(res$todo$content, "")
  expect_match(res$modelSummary$content,
               "1 row(s) with a missing diagnosis or follow-up date were excluded.", fixed = TRUE)

  # A repeated unreadable value is shown once among the examples
  d <- date_data()
  d$fudate[1:560] <- "unknown"
  expect_match(error_message(fit_dates(d)), "(for example 'unknown')", fixed = TRUE)
})

test_that("an integer Time Elapsed with value labels (a factor carrying 'values') fits", {
  skip_if_not_installed("coxme")
  d <- date_data()
  labelled <- factor(d$days)
  attr(labelled, "values") <- as.integer(levels(labelled))
  d$days_labelled <- labelled
  # The wrapper reads variable arguments as column names, so the names are written out
  res <- mixedcox(data = d, elapsedtime = "days_labelled", outcome = "status", outcomeLevel = "event",
                  fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster")
  plain <- mixedcox(data = d, elapsedtime = "days", outcome = "status", outcomeLevel = "event",
                    fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster")
  expect_identical(res$todo$content, "")
  expect_equal(res$fixedEffectsTable$asDF, plain$fixedEffectsTable$asDF)
})

test_that("a date-format error reaches jamovi's error state and clears the previous model's output", {
  skip_if_not_installed("coxme")
  d <- date_data("%d/%m/%Y")
  d$dxdate <- factor(d$dxdate)
  d$fudate <- factor(d$fudate)
  analysis <- internal_object("mixedcoxClass")$new(
    options = internal_object("mixedcoxOptions")$new(
      tint = TRUE, dxdate = "dxdate", fudate = "fudate", timetypedata = "mdy",
      outcome = "status", outcomeLevel = "event",
      fixed_effects = "g", continuous_effects = "x", cluster_var = "cluster"),
    data = d)
  analysis$init()
  analysis$results$modelSummary$setContent("<p>previous model</p>")
  analysis$results$fixedEffectsTable$addRow(rowKey = "old", values = list(variable = "old"))
  # noThrow = TRUE: the error is recorded in the analysis instead of being thrown
  analysis$run(noThrow = TRUE)
  # reject() inside a catch-all tryCatch would have left the analysis "complete"
  expect_identical(analysis$status, "error")
  expect_identical(analysis$results$todo$content, "")
  expect_identical(analysis$results$modelSummary$content, "")
  expect_equal(analysis$results$fixedEffectsTable$rowCount, 0L)
})

test_that("negative survival times are excluded and counted; a time of 0 is kept", {
  skip_if_not_installed("coxme")
  d <- date_data()
  d$days[1:5] <- -30  # follow-up 30 days before diagnosis
  d$days[6:8] <- 0    # follow-up on the day of diagnosis
  d$fudate <- base::format(as.Date(d$dxdate) + d$days, "%Y-%m-%d")
  res <- fit_dates(d)
  fit <- coxme::coxme(survival::Surv(days, status == "event") ~ g + x + (1 | cluster),
                      data = d[-(1:5), ])
  expect_matches_oracle(res$fixedEffectsTable$asDF, fit)
  expect_match(res$modelSummary$content, "Observations:</b> 595", fixed = TRUE)
  expect_match(res$modelSummary$content, paste0(
    "5 row(s) with a negative survival time (follow-up date before the diagnosis date) ",
    "were excluded."), fixed = TRUE)

  e <- sim_data()
  e$time[1:4] <- -e$time[1:4]
  e$time[5] <- 0
  res_e <- fit_sim(e)
  fit_e <- coxme::coxme(survival::Surv(time, status == "event") ~ g + x + (1 | cluster),
                        data = e[-(1:4), ])
  expect_matches_oracle(res_e$fixedEffectsTable$asDF, fit_e)
  expect_match(res_e$modelSummary$content, "Observations:</b> 596", fixed = TRUE)
  expect_match(res_e$modelSummary$content,
               "4 row(s) with a negative survival time (negative Time Elapsed) were excluded.",
               fixed = TRUE)
})

test_that("the Insufficient Clusters stop says how many rows were dropped for a missing date", {
  skip_if_not_installed("coxme")
  d <- date_data()
  d <- d[as.integer(d$cluster) <= 8, ]
  d$fudate[as.integer(d$cluster) <= 4] <- NA  # 4 of 8 clusters lose every row
  res <- fit_dates(d)
  expect_match(res$todo$content, "Insufficient Clusters", fixed = TRUE)
  expect_match(res$todo$content, "(found 4)", fixed = TRUE)
  expect_match(res$todo$content,
               "80 row(s) with a missing diagnosis or follow-up date were excluded.", fixed = TRUE)
})

test_that("an ordinal fixed effect gives level-vs-first-level hazard ratios, not .L/.Q contrasts", {
  skip_if_not_installed("coxme")
  d <- sim_data()
  # An ordinal stage with a real, increasing effect (x has log-HR 0.5 in sim_data)
  d$stage <- cut(d$x, c(-Inf, -0.5, 0.5, Inf), labels = c("I", "II", "III"), ordered_result = TRUE)
  expect_true(is.ordered(d$stage))
  res <- mixedcox(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "event",
                  fixed_effects = c("g", "stage"), cluster_var = "cluster")
  # Independent oracle: the ordered column itself, coded with treatment contrasts
  fit <- withr::with_options(
    list(contrasts = c("contr.treatment", "contr.treatment")),
    coxme::coxme(survival::Surv(time, status == "event") ~ g + stage + (1 | cluster), data = d))
  expect_identical(names(coxme::fixef(fit)), c("gB", "stageII", "stageIII"))

  table <- res$fixedEffectsTable$asDF
  expect_identical(table$variable, c("g: B", "stage: II", "stage: III"))
  expect_matches_oracle(table, fit)
  expect_true(all(table$hazard_ratio[2:3] > 1) && table$hazard_ratio[3] > table$hazard_ratio[2])
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
