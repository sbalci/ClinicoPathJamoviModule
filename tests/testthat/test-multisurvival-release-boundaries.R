# Exercise the current source in an isolated namespace: load_all() can hide a
# missing helper/import or accidentally use an older installed analysis.
.msb_root <- normalizePath(testthat::test_path("..", ".."))
.msb_env <- new.env(parent = globalenv())
.msb_env$. <- jmvcore::`.`
.msb_env$Surv <- survival::Surv
.msb_env$`%>%` <- magrittr::`%>%`
for (.msb_file in c(
  "00jmv.R", "utils.R", "utils-formula.R", "utils-eventindicator.R",
  "utils-followup.R", "survival-competingrisks.R", "multisurvival-formula.R",
  "multisurvival-interactions.R", "multisurvival-metrics.R", "multisurvival.h.R",
  "multisurvival.b.R"
)) sys.source(file.path(.msb_root, "R", .msb_file), envir = .msb_env)

.msb_data <- function() {
  set.seed(20261022)
  data.frame(time = stats::rexp(120, 0.04) + 0.1,
             status = stats::rbinom(120, 1, 0.7),
             group = factor(sample(c("A", "B"), 120, TRUE)),
             x = stats::rnorm(120))
}

.msb_run <- function(data, ...) {
  args <- utils::modifyList(list(
    data = data, elapsedtime = "time", outcome = "status", outcomeLevel = NULL,
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "group", contexpl = "x"
  ), list(...), keep.null = TRUE)
  output <- tempfile()
  sink(output)
  on.exit({ sink(); unlink(output) }, add = TRUE)
  suppressWarnings(do.call(.msb_env$multisurvival, args))
}

.msb_private <- function(result) {
  result$.__enclos_env__$private$.parent$.__enclos_env__$private
}

test_that("user columns cannot overwrite internal survival variables or row IDs", {
  data <- .msb_data()
  rownames(data) <- as.character(seq(5, by = 2, length.out = nrow(data)))
  reference <- survival::coxph(survival::Surv(time, status) ~ group + x, data = data)
  for (name in c("mytime", "myoutcome", "row_names", "fgid", "fgwt", "fgstatus")) {
    renamed <- data
    names(renamed)[4] <- name
    result <- .msb_run(renamed, contexpl = name)
    private <- .msb_private(result)
    expect_equal(unname(stats::coef(private$.coxCache)),
                 unname(stats::coef(reference)), tolerance = 1e-10, info = name)
    expect_identical(as.character(private$.dataCache$cleanData$row_names),
                     rownames(data), info = name)
  }
})

test_that("non-finite follow-up is rejected before modelling", {
  data <- .msb_data()
  data$time[1] <- Inf
  expect_error(.msb_run(data), "finite follow-up", fixed = TRUE)
})

test_that("saved columns invalidate when date units change", {
  schema <- yaml::read_yaml(file.path(.msb_root, "jamovi", "multisurvival.r.yaml"))
  outputs <- Filter(function(x) identical(x$type, "Output"), schema$items)
  for (item in outputs) {
    expect_true("timetypeoutput" %in% item$clearWith, info = item$name)
  }
})

test_that("a Fine-Gray nomogram reports patients rather than expanded rows", {
  data <- .msb_data()
  data$status <- factor(rep(c("Event", "Competing", "Censored"), 40),
                        levels = c("Censored", "Event", "Competing"))
  result <- .msb_run(data, showNomogram = TRUE, cutp = "5,10")
  summary <- as.character(result$nomogramSummary$content)
  expect_match(summary, "<b>120 patients</b>", fixed = TRUE)
  expect_match(summary, "Fine-Gray", fixed = TRUE)
  expect_match(summary, "Harrell's C-index is not reported", fixed = TRUE)
})

test_that("adjusted-plot text wraps at the current image width", {
  data <- .msb_data()
  result <- .msb_run(data, ac = TRUE, adjexplanatory = "group")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output), add = TRUE)
  result$plot_adj$setSize(480, 450)
  result$plot_adj$saveAs(output)
  plot <- ggplot2::last_plot()
  lines <- strsplit(plot$labels$subtitle, "\n", fixed = TRUE)[[1]]
  expect_gt(length(lines), 1)
  expect_lte(max(nchar(lines)), 60)
  expect_gt(file.info(output)$size, 0)
})

test_that("an unreached adjusted median is described without an NA estimate", {
  data <- .msb_data()
  data$time <- c(rep(1, 10), rep(10, 110))
  data$status <- c(rep(1L, 10), rep(0L, 110))
  result <- .msb_run(data, ac = TRUE, adjexplanatory = "group", ac_summary = TRUE)
  expect_true(all(is.na(as.data.frame(result$adjustedMedianTable)$median)))
  expect_match(as.character(result$adjustedMedianSummary$content), "not reached", fixed = TRUE)
  expect_false(grepl("is NA", as.character(result$adjustedMedianSummary$content), fixed = TRUE))
})

test_that("disease-free analysis accepts recurrence events without recorded deaths", {
  data <- .msb_data()
  reference <- survival::coxph(survival::Surv(time, status) ~ group + x, data = data)
  data$status <- factor(ifelse(data$status == 1, "AWD", "AWOD"))
  result <- .msb_run(data, multievent = TRUE, analysistype = "dfs",
                     awd = "AWD", awod = "AWOD")
  fit <- .msb_private(result)$.coxCache
  expect_s3_class(fit, "coxph")
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(reference)), tolerance = 1e-10)
})
