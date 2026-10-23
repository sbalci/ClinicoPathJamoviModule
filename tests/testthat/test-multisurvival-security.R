# Exercise the source directly, without loading an installed ClinicoPath copy.
.mss_root <- normalizePath(testthat::test_path("..", ".."))
.mss_env <- new.env(parent = globalenv())
.mss_env$. <- jmvcore::`.`
.mss_env$Surv <- survival::Surv
.mss_env$`%>%` <- magrittr::`%>%`
for (.mss_file in c(
  "00jmv.R", "utils.R", "utils-formula.R", "utils-eventindicator.R",
  "utils-followup.R", "multisurvival-formula.R", "multisurvival-interactions.R",
  "multisurvival-metrics.R", "multisurvival.h.R", "multisurvival.b.R"
)) sys.source(file.path(.mss_root, "R", .mss_file), .mss_env)

.mss_new <- function() {
  .mss_env$multisurvivalClass$new(
    options = .mss_env$multisurvivalOptions$new(), data = data.frame())
}

.mss_source_call <- function(analysis) {
  expression <- parse(text = analysis$asSource())
  stopifnot(length(expression) == 1L)
  match.call(.mss_env$multisurvival, expression[[1]])
}

# Decode only literal c()/list() constructors; never evaluate generated syntax.
.mss_literal <- function(node) {
  if (is.null(node) || is.atomic(node)) return(node)
  if (!is.call(node)) stop("Non-literal syntax")
  values <- lapply(as.list(node)[-1], .mss_literal)
  if (identical(node[[1]], as.name("c"))) return(do.call(c, values))
  if (identical(node[[1]], as.name("list"))) return(values)
  stop("Non-literal call")
}

.mss_unnamed <- function(value) {
  if (is.list(value)) unname(lapply(value, .mss_unnamed)) else unname(value)
}

test_that("every saved option exports hostile strings as literals or is omitted", {
  analysis <- .mss_new()
  options <- analysis$options$options
  baseline_names <- names(as.list(.mss_source_call(analysis)))[-1]
  variants <- c(
    '5", seed = {stop("AUDIT_CANARY"); 123}, timetypeoutput = "days',
    "trailing\\", "first\nsecond", 'a"b\\c\nnext'
  )
  for (option in options) {
    original <- option$value
    for (value in variants) {
      option$value <- value
      call <- .mss_source_call(analysis)
      label <- paste(option$name, encodeString(value))
      expect_true(all(names(as.list(call))[-1] %in%
                        union(baseline_names, option$name)), info = label)
      if (option$name %in% names(formals(.mss_env$multisurvival))) {
        expect_identical(.mss_literal(call[[option$name]]),
                         .mss_unnamed(option$value), info = label)
      } else {
        expect_false(option$name %in% names(as.list(call)), info = label)
      }
    }
    option$value <- original
  }
})

test_that("saved lists cannot introduce executable code or undeclared arguments", {
  analysis <- .mss_new()
  payload <- '5", seed = {stop("AUDIT_CANARY"); 123}, timetypeoutput = "days'
  poisoned_name <- 'x` = {stop("AUDIT_CANARY")}, y'
  for (option in analysis$options$options) {
    original <- option$value
    option$value <- stats::setNames(list(stats::setNames(payload, poisoned_name)),
                                    poisoned_name)
    call <- .mss_source_call(analysis)
    if (identical(option$name, "interactions") || is.character(option$value)) {
      expect_identical(.mss_literal(call[[option$name]]), .mss_unnamed(option$value))
    } else {
      expect_false(option$name %in% names(as.list(call)), info = option$name)
    }
    option$value <- original
  }
  baseline <- analysis$asSource()
  for (name in c("unexpected", 'cutp = "5", seed = {stop("AUDIT_CANARY"); 123}, other')) {
    analysis$options$.addOption(jmvcore:::Option$new(name, payload))
  }
  expect_identical(analysis$asSource(), baseline)
})

test_that("named character vectors export only literal values", {
  analysis <- .mss_new()
  payload <- c('5", seed = {stop("AUDIT_CANARY"); 123}, other = "x', "second\\value")
  names(payload) <- c('x` = {stop("AUDIT_CANARY")}, y', "ordinary")
  for (option in analysis$options$options) {
    original <- option$value
    option$value <- payload
    call <- .mss_source_call(analysis)
    if (option$name %in% names(formals(.mss_env$multisurvival))) {
      expect_identical(.mss_literal(call[[option$name]]), .mss_unnamed(option$value),
                       info = option$name)
    } else {
      expect_false(option$name %in% names(as.list(call)), info = option$name)
    }
    option$value <- original
  }
})

test_that("syntax preserves data, variable vectors and crossed interaction terms", {
  analysis <- .mss_new()
  variables <- c('Treatment "arm"', "age\\value", "line\nbreak")
  interactions <- list(variables[1:2], variables[c(1, 3)])
  for (name in c("explanatory", "contexpl", "stratvar")) {
    option <- analysis$options$option(name)
    option$value <- variables
  }
  option <- analysis$options$option("interactions")
  option$value <- interactions
  call <- .mss_source_call(analysis)
  expect_identical(call$data, as.name("data"))
  expect_identical(.mss_literal(call$explanatory), variables)
  expect_identical(.mss_literal(call$contexpl), variables)
  expect_identical(.mss_literal(call$stratvar), variables)
  expect_identical(.mss_literal(call$interactions), interactions)
})

test_that("a benign exported interaction model refits the same coefficients", {
  set.seed(20261022)
  data <- data.frame(time = stats::rexp(120) + 0.01,
                     status = stats::rbinom(120, 1, 0.7),
                     group = factor(rep(c("A", "B"), 60)), x = stats::rnorm(120))
  names(data)[3:4] <- c('Treatment "arm"', "age\\value")
  args <- list(data = data, elapsedtime = "time", outcome = "status",
               outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
               explanatory = names(data)[3], contexpl = names(data)[4],
               interactions = list(names(data)[3:4]), showSummaries = FALSE)
  log <- tempfile()
  sink(log)
  on.exit({ sink(); unlink(log) }, add = TRUE)
  original <- suppressWarnings(do.call(.mss_env$multisurvival, args))
  analysis <- original$.__enclos_env__$private$.parent
  call <- .mss_source_call(analysis)
  restored_args <- lapply(as.list(call)[-1], function(node) {
    if (identical(node, as.name("data"))) data else .mss_literal(node)
  })
  restored <- suppressWarnings(do.call(.mss_env$multisurvival, restored_args))
  fitted <- function(result) {
    result$.__enclos_env__$private$.parent$.__enclos_env__$private$.coxCache
  }
  expect_equal(stats::coef(fitted(restored)), stats::coef(fitted(original)), tolerance = 1e-12)
  expect_equal(stats::vcov(fitted(restored)), stats::vcov(fitted(original)), tolerance = 1e-12)
  expect_identical(length(stats::coef(fitted(restored))), 3L)
})
