library(testthat)

ti_daily_call <- function(d, ...) {
  do.call(ClinicoPath::timeinterval, c(list(data = d, dx_date = 's', fu_date = 'e',
    time_format = 'dmy', output_unit = 'days'), list(...)))
}

test_that('TI-D01 ambiguity percentages identify their original denominator', {
  # Original date arithmetic: ambiguous 10 of 100 raw person-days, but 0 of 3 after landmark.
  d <- data.frame(s = c('01/02/2020', '13/02/2020', '14/02/2020', '15/02/2020'),
    e = c('11/02/2020', '14/03/2020', '15/03/2020', '16/03/2020'))
  r <- ti_daily_call(d, use_landmark = TRUE, landmark_time = 29,
    include_quality_metrics = TRUE)
  expect_match(r$summary$content, '10% of the raw person-time in days, before filters or landmark',
    fixed = TRUE)
  expect_false(grepl('person-time above', r$summary$content, fixed = TRUE))
  expect_match(r$qualityAssessment$content,
    '10% of the raw person-time in days, before filters or landmark', fixed = TRUE)
  expect_equal(unlist(r$calculated_time$.__enclos_env__$private$.values), c(1, 1, 1))
  expect_match(r$summary$content, '3 person-days', fixed = TRUE)
})

test_that('TI-D02 landmark recovery advice distinguishes summaries and retained cases', {
  # For retained raw x=(30,31,33), y=x-29: sum(x)=94=sum(y)+3*29; sd(x)=sd(y).
  d <- data.frame(s = rep('01/01/2020', 4),
    e = c('11/01/2020', '31/01/2020', '01/02/2020', '03/02/2020'))
  r <- ti_daily_call(d, use_landmark = TRUE, landmark_time = 29,
    include_quality_metrics = TRUE)
  expect_equal(unlist(r$calculated_time$.__enclos_env__$private$.values), c(1, 2, 4))
  expect_match(r$caveatsPanel$content, 'standard deviation is unchanged by subtracting a constant',
    fixed = TRUE)
  expect_match(r$caveatsPanel$content, 'number of retained participants times the landmark', fixed = TRUE)
  expect_match(r$caveatsPanel$content, 'does not restore excluded participants', fixed = TRUE)
  expect_false(grepl('Add the landmark to each value', r$caveatsPanel$content, fixed = TRUE))
  expect_equal(sd(c(1, 2, 4)), sd(c(30, 31, 33)))
  expect_equal(sum(c(1, 2, 4)) + 3 * 29, 94)
})

test_that('TI-D03 regenerated R syntax preserves strings and rejects undeclared arguments', {
  d <- data.frame(s = '2020-01-01', e = '2020-02-01')
  o <- ClinicoPath:::timeintervalOptions$new(dx_date = 's', fu_date = 'e')
  a <- ClinicoPath:::timeintervalClass$new(options = o, data = d)
  a$init()
  normal <- parse(text = a$asSource())[[1]]
  expect_identical(normal$data, quote(data))
  # Replaying benign generated syntax must still use the dataset object.
  env <- new.env(); env$data <- d
  replay <- eval(normal, envir = env)
  expect_s3_class(replay, 'timeintervalResults')
  payload <- '95", output_unit = {stop("SYNTAX_CANARY"); "days"}, timezone = "utc'
  op <- a$options$option('confidence_level'); op$value <- payload
  expr <- parse(text = a$asSource())[[1]]
  expect_identical(expr$confidence_level, payload)
  expect_false('output_unit' %in% names(as.list(expr)))
  # Inspection and argument binding only. Never evaluate the hostile expression.
  expect_silent(match.call(ClinicoPath::timeinterval, expr))
  op$value <- 95
  source_option <- a$.__enclos_env__$private$.sourcifyOption
  opt_names <- setdiff(names(formals(ClinicoPath::timeinterval)), 'data')
  for (nm in opt_names) {
    for (value in c(payload, 'trailing\\', 'line\nbreak')) {
      op <- a$options$option(nm)
      old <- op$value
      op$value <- value
      src <- source_option(op)
      parsed <- tryCatch(parse(text = paste0('list(', src, ')'))[[1]], error = identity)
      expect_false(inherits(parsed, 'error'), info = nm)
      if (!inherits(parsed, 'error')) {
        expect_identical(names(as.list(parsed))[-1], nm, info = nm)
        expect_identical(parsed[[2]], value, info = nm)
      }
      op$value <- old
    }
    op <- a$options$option(nm); old <- op$value
    op$value <- list(untrusted = payload)
    expect_identical(source_option(op), '', info = nm)
    op$value <- c(payload, payload)
    expect_identical(source_option(op), '', info = nm)
    op$value <- old
  }
  rogue <- jmvcore:::Option$new('confidence_level = 95, output_unit = {stop("SYNTAX_CANARY"); "days"}, timezone', 'utc')
  expect_identical(source_option(rogue), '')
})
