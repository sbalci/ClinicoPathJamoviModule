# Framework failures and native result grids from the November module audit.
audit_fix_namespace <- function() {
  for (pkg in c("jsurvival", "ClinicoPath"))
    if (pkg %in% loadedNamespaces()) return(asNamespace(pkg))
  skip("analysis namespace is not loaded")
}
audit_fix_analysis <- function(name, data, args) {
  ns <- audit_fix_namespace()
  options <- do.call(get(paste0(name, "Options"), ns)$new, args)
  a <- get(paste0(name, "Class"), ns)$new(options = options, data = data)
  a$init()
  a
}

test_that("configured fatal inputs raise errors while incomplete input stays quiet", {
  d <- data.frame(time = c(-1, 2, 3, 4), event = c(0, 1, 0, 1), marker = 1:4,
    date = rep(NA_character_, 4))
  cases <- list(
    datetimeconverter = list(datetime_var = "date"),
    outcomeorganizer = list(outcome = "date"),
    singlearm = list(elapsedtime = "time", outcome = "time"),
    survivalcont = list(elapsedtime = "time", outcome = "event", contexpl = "marker"))
  for (name in names(cases)) {
    a <- audit_fix_analysis(name, d, cases[[name]])
    expect_error(a$.__enclos_env__$private$.run(), info = name)
    empty <- audit_fix_analysis(name, d, list())
    expect_error(empty$.__enclos_env__$private$.run(), NA, info = name)
  }
})

test_that("native datetime grids preserve preview values and parsing counts", {
  d <- data.frame(date = c("2024-01-01", "2024-03-02", NA, "bad"))
  a <- audit_fix_analysis("datetimeconverter", d,
    list(datetime_var = "date", datetime_format = "ymd", extract_year = TRUE))
  a$.__enclos_env__$private$.run()
  r <- a$results
  expect_s3_class(r$conversionRows, "Table")
  expect_equal(r$conversionRows$rowCount, 4)
  expect_equal(r$conversionRows$asDF[[2]], d$date)
  expect_equal(r$conversionRows$asDF[[4]], c("OK", "OK", "-", "Failed"))
  expect_equal(r$qualityCounts$asDF[[2]], c(4, 1, 2, 1))
  expect_equal(r$componentRows$asDF[[2]][1:2], c(2024, 2024))
  expect_false(grepl("<table", r$previewTable$content, fixed = TRUE))
  expect_false(grepl("<table", r$qualityMetrics$content, fixed = TRUE))
})

test_that("native grid replacement clears surplus columns and keeps user strings literal", {
  ns <- audit_fix_namespace()
  fill <- get(".populateSurvivalTable", ns)
  table <- jmvcore::Table$new(options = jmvcore::Options$new(), name = "grid",
    title = "Grid", columns = list(), rows = 0)
  d <- data.frame(x = c("<img src=x>", "A&B"), count = c(2, 5), check.names = FALSE)
  names(d) <- c("A+B (years)", "Count")
  fill(table, d)
  expect_equal(unname(table$asDF[[1]]), d[[1]])
  expect_equal(table$asDF[[2]], d[[2]])
  fill(table, d[1, 1, drop = FALSE])
  expect_equal(table$rowCount, 1)
  expect_false(table$columns[[2]]$visible)
  fill(table, data.frame())
  expect_equal(table$rowCount, 0)
  expect_true(all(vapply(table$columns, function(x) !isTRUE(x$visible), logical(1))))
})

test_that("shared bootstrap fallback formula preserves quoted model matrices", {
  ns <- audit_fix_namespace()
  parse_formula <- get(".asSurvivalFormula", ns)
  for (name in c("Age (years)", "A+B", "Grade 2/3", "marker value")) {
    d <- stats::setNames(data.frame(factor(c("low", "high", "low"))), name)
    quoted <- jmvcore::composeTerm(name)
    expected <- stats::model.matrix(stats::reformulate(quoted), data = d)
    actual <- stats::model.matrix(parse_formula(paste("~", quoted)), data = d)
    expect_equal(actual, expected, info = name)
  }
})


.dq_ns <- function() {
  for (p in c('jsurvival', 'ClinicoPath')) if (p %in% loadedNamespaces()) return(asNamespace(p))
  stop('Load the analysis namespace first')
}
testthat::test_that('native datetime percentages use all observations for every row', {
  ns <- .dq_ns()
  for (counts in list(c(4, 1, 2, 1), c(4, 4, 0, 0), c(5, 2, 0, 3))) {
    o <- get('datetimeconverterOptions', ns)$new(datetime_var = 'date')
    a <- get('datetimeconverterClass', ns)$new(options = o,
      data = data.frame(date = c('2020-01-01', 'bad', NA_character_)))
    a$init()
    q <- stats::setNames(as.list(counts), c('total_observations', 'original_missing',
      'successfully_parsed', 'failed_parsing'))
    q$success_rate <- if (counts[1] > counts[2]) 100 * counts[3] / (counts[1] - counts[2]) else NA_real_
    q$min_datetime <- as.POSIXct(NA); q$max_datetime <- as.POSIXct(NA)
    q$failed_samples <- character()
    a$.__enclos_env__$private$.formatQualityMetricsGuide(q, 'not used')
    tab <- a$results$qualityCounts$asDF
    testthat::expect_equal(as.numeric(tab[[2]]), counts)
    pct <- as.numeric(tab[[3]])
    testthat::expect_true(all(is.finite(pct)))
    testthat::expect_equal(pct, 100 * counts / counts[1])
    testthat::expect_equal(sum(pct[-1]), 100)
  }
})
