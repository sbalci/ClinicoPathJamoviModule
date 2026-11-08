# Regression coverage for the module release audit of 2026-11-07.
release_fix_ns <- function() {
  for (pkg in c("jsurvival", "ClinicoPath")) {
    if (pkg %in% loadedNamespaces()) return(asNamespace(pkg))
  }
  testthat::skip("analysis namespace is not loaded")
}

test_that("empty selections with supplied rows return guidance in every survival wrapper", {
  ns <- release_fix_ns()
  analyses <- c("datetimeconverter", "lassocox", "multisurvival", "oddsratio",
                "outcomeorganizer", "singlearm", "survival", "survivalcont", "timeinterval")
  for (name in analyses) {
    fn <- get(name, ns)
    required <- names(formals(fn))[vapply(formals(fn), function(x) {
      identical(x, quote(expr = ))
    }, logical(1))]
    args <- setNames(rep(list(NULL), length(required)), required)
    args$data <- data.frame(unselected = 1:5)
    err <- tryCatch({ suppressMessages(do.call(fn, args)); NULL }, error = identity)
    expect_null(err, info = name)
  }
})

test_that("forest and optimal-cutpoint image states contain no patient frames", {
  ns <- release_fix_ns()
  set.seed(20261107)
  n <- 120
  d <- data.frame(age = rnorm(n, 60, 10), x = rnorm(n), time = rexp(n, .03))
  d$event <- rbinom(n, 1, .7)
  d$binary <- factor(rbinom(n, 1, plogis(.4 * d$x)), levels = 0:1)
  odds <- get("oddsratio", ns)(data = d, explanatory = c("age", "x"),
    outcome = "binary", outcomeLevel = "1", outcomeNegativeLevel = "0",
    predictorLevel = NULL, predictorNegativeLevel = NULL)
  cut <- get("survivalcont", ns)(data = d, elapsedtime = "time", outcome = "event",
    contexpl = "age", outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL,
    awod = NULL, findcut = TRUE)
  contains_frame <- function(x) {
    is.data.frame(x) || (is.list(x) && any(vapply(x, contains_frame, logical(1))))
  }
  for (state in list(odds$plot$state, cut$plot4$state)) {
    expect_false(is.null(state))
    expect_false(contains_frame(state))
  }
})
