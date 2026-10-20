library(testthat)

sc_daily_fixture <- function() {
  withr::with_seed(20261020, data.frame(
    time = rexp(120, 0.025), status = rbinom(120, 1, .7),
    marker = rnorm(120), site = factor(rep(c('A', 'B', 'C'), 40))
  ))
}
sc_daily_call <- function(d, ...) {
  a <- list(data = d, elapsedtime = 'time', outcome = 'status', contexpl = 'marker',
            outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
  a <- utils::modifyList(a, list(...), keep.null = TRUE)
  do.call(ClinicoPath::survivalcont, a)
}

test_that('SC-D01 row identifier names cannot overwrite selected source columns', {
  d <- sc_daily_fixture()
  rownames(d) <- as.character(seq(7, by = 3, length.out = nrow(d)))
  ref <- sc_daily_call(d, person_time = TRUE, rmst_analysis = TRUE, rmst_tau = 20)
  for (role in c('time', 'marker')) for (label in c('row_names', 'Row Names')) {
    x <- d; names(x)[names(x) == role] <- label
    # A second cleaned-name collision must not steal the reserved internal name.
    x$row_names_2 <- seq_len(nrow(x))
    arg <- if (role == 'time') list(elapsedtime = label) else list(contexpl = label)
    got <- tryCatch(do.call(sc_daily_call, c(list(d = x, person_time = TRUE,
      rmst_analysis = TRUE, rmst_tau = 20), arg)), error = identity)
    expect_false(inherits(got, 'error'), info = paste(role, label))
    if (!inherits(got, 'error')) {
      expect_identical(got$coxTable$asDF$HR_univariable, ref$coxTable$asDF$HR_univariable)
      expect_equal(got$personTimeTable$asDF, ref$personTimeTable$asDF)
      expect_equal(got$rmstTable$asDF, ref$rmstTable$asDF)
    }
  }
})

test_that('SC-D02 explicit NA strata are excluded like genuine missing strata', {
  d <- sc_daily_fixture(); d$site[1:20] <- NA
  ref <- sc_daily_call(d, stratified_cox = TRUE, strata_variable = 'site')
  for (ord in c(FALSE, TRUE)) {
    x <- d; x$site <- addNA(x$site)
    if (ord) x$site <- ordered(x$site, levels = levels(x$site), exclude = NULL)
    attr(x$site, 'values') <- c(10L, 20L, 30L, NA_integer_)
    before <- x
    got <- tryCatch(sc_daily_call(x, stratified_cox = TRUE, strata_variable = 'site'), error = identity)
    expect_false(inherits(got, 'error'))
    if (!inherits(got, 'error')) {
      expect_equal(got$stratifiedCoxTable$asDF, ref$stratifiedCoxTable$asDF)
      expect_identical(got$stratifiedCoxTable$notes$method$note,
                       ref$stratifiedCoxTable$notes$method$note)
    }
    expect_identical(x, before)
  }
  # Literal text "NA" denotes a real fourth stratum, not missing observations.
  literal <- sc_daily_fixture(); z <- as.character(literal$site); z[1:20] <- 'NA'
  literal$site <- factor(z)
  got <- sc_daily_call(literal, stratified_cox = TRUE, strata_variable = 'site')
  oracle <- survival::coxph(survival::Surv(time, status) ~ marker +
                              survival::strata(site), data = literal)
  expect_equal(got$stratifiedCoxTable$asDF$hr, unname(exp(coef(oracle))))
  expect_match(got$stratifiedCoxTable$notes$method$note, '4 levels', fixed = TRUE)
})

test_that('SC-D03 tree no-split fallback is disclosed with the actual method', {
  # With every follow-up/event identical rpart has no improvement/split; the
  # retained fallback is the marker's 1/3 and 2/3 quantiles, not tree split points.
  d <- data.frame(time = rep(1, 120), status = rep(1, 120), marker = seq_len(120))
  got <- sc_daily_call(d, multiple_cutoffs = TRUE, cutoff_method = 'tree')
  expect_equal(got$multipleCutTable$asDF$cutpoint_value,
               round(unname(quantile(d$marker, c(1/3, 2/3))), 2))
  expect_match(got$warnings$content, 'Quantile cut-points are shown instead', fixed = TRUE)
  expect_identical(got$plotMultipleCutoffs$state$method, 'quantile (tree fallback)')
  expect_identical(got$plotMultipleSurvival$state$method, 'quantile (tree fallback)')
})

test_that('SC-D03 quantile supplementation of an insufficient tree is disclosed', {
  d <- data.frame(time = rep(c(1, 10), each = 60), status = rep(1, 120),
                  marker = seq_len(120))
  tree <- rpart::rpart(survival::Surv(time, status) ~ marker, data = d, method = 'exp',
    control = rpart::rpart.control(maxdepth = 4, minsplit = 12, cp = .01))
  expect_equal(nrow(tree$splits), 1L)
  got <- sc_daily_call(d, multiple_cutoffs = TRUE, cutoff_method = 'tree',
                      num_cutoffs = 'three')
  expected <- sort(c(tree$splits[1, 'index'], quantile(d$marker, c(1/3, 2/3))))
  expect_equal(got$multipleCutTable$asDF$cutpoint_value, round(unname(expected), 2))
  expect_match(got$warnings$content, 'Quantile cut-points supplement the tree splits', fixed = TRUE)
  expect_identical(got$plotMultipleCutoffs$state$method, 'tree + quantile')
})

test_that('SC-D01 generated event and RMST names cannot overwrite selected time', {
  d <- sc_daily_fixture()
  ref <- sc_daily_call(d, person_time = TRUE, rmst_analysis = TRUE, rmst_tau = 20)
  for (label in c('status_event', 'rmst_groups')) {
    x <- d; names(x)[1] <- label
    got <- tryCatch(sc_daily_call(x, elapsedtime = label, person_time = TRUE,
      rmst_analysis = TRUE, rmst_tau = 20), error = identity)
    expect_false(inherits(got, 'error'), info = label)
    if (!inherits(got, 'error')) {
      expect_equal(got$personTimeTable$asDF, ref$personTimeTable$asDF)
      expect_equal(got$rmstTable$asDF, ref$rmstTable$asDF)
      expect_identical(got$coxTable$asDF$HR_univariable, ref$coxTable$asDF$HR_univariable)
    }
  }
})

test_that('SC-D01 date work columns cannot overwrite selected numeric epoch dates', {
  d <- sc_daily_fixture()
  ref <- sc_daily_call(d, person_time = TRUE, timetypeoutput = 'days')
  x <- d; x$end <- 1704067200; x$start <- x$end + x$time * 86400
  got <- sc_daily_call(x, tint = TRUE, dxdate = 'end', fudate = 'start',
                      timetypeoutput = 'days', person_time = TRUE)
  expect_equal(got$personTimeTable$asDF, ref$personTimeTable$asDF)
  expect_identical(got$coxTable$asDF$HR_univariable, ref$coxTable$asDF$HR_univariable)
})

test_that('SC-D01 multiple-cutoff work columns cannot overwrite selected time', {
  d <- sc_daily_fixture()
  for (method in c('quantile', 'minpval')) {
    label <- if (method == 'quantile') 'risk_groups' else 'test_groups'
    ref <- sc_daily_call(d, multiple_cutoffs = TRUE, cutoff_method = method)
    x <- d; names(x)[1] <- label
    got <- sc_daily_call(x, elapsedtime = label, multiple_cutoffs = TRUE,
                        cutoff_method = method)
    expect_equal(got$multipleCutTable$asDF, ref$multipleCutTable$asDF)
    expect_identical(got$multipleCutTable$notes$logrank$note,
                     ref$multipleCutTable$notes$logrank$note)
  }
})

test_that('SC-D04 log-rank caveat distinguishes outcome-guided from marker-only cutoffs', {
  got <- sc_daily_call(sc_daily_fixture(), multiple_cutoffs = TRUE,
                       cutoff_method = 'quantile')
  note <- got$multipleCutTable$notes$logrank$note
  expect_false(grepl('largest of many statistics', note, fixed = TRUE))
  expect_match(note, 'When survival outcomes guided the cut-point search', fixed = TRUE)
  expect_match(note, 'unadjusted', fixed = TRUE)
})
