library(testthat)

# Regressions for the 2026-09-02 /fix-function pass on multisurvival. Each test
# targets a defect the existing suite could not see: its synthetic columns are
# lowercase, every model has two or more predictors, and the competing-risk
# risk-group plot was never rendered.

msfx_df <- function(n = 160, seed = 2026) {
  set.seed(seed)
  data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = stats::rbinom(n, 1, 0.65),
    # Capitalised on purpose: janitor cleans it to `sex` inside the analysis.
    Sex = factor(sample(c("Female", "Male"), n, TRUE)),
    Age = round(stats::rnorm(n, 60, 10), 1),
    cause = factor(sample(c("AWOD", "AWD", "DOD", "DOOC"), n, TRUE,
                          prob = c(0.35, 0.15, 0.30, 0.20)))
  )
}

render_ok <- function(image) {
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  isTRUE(image$.render())
}

test_that("person-time group rows index the cleaned column, not the option label", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex", person_time = TRUE)
  tab <- as.data.frame(res$personTimeTable)
  grp <- tab[grepl("^Sex: ", tab$interval), , drop = FALSE]
  expect_equal(nrow(grp), 2L)

  ref <- stats::aggregate(cbind(events = status, pt = time) ~ Sex, d, sum)
  for (i in seq_len(nrow(ref))) {
    row <- grp[grp$interval == paste0("Sex: ", ref$Sex[i]), , drop = FALSE]
    expect_equal(row$events, ref$events[i])
    expect_equal(row$person_time, round(ref$pt[i], 2))
  }
})

test_that("a single-predictor nomogram is drawn and its scoring guide is not blank", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            contexpl = "Age", showNomogram = TRUE, cutp = "6, 12")
  guide <- paste(as.character(res$nomogram_display$content), collapse = "")
  expect_gt(nchar(guide), 100)
  expect_true(render_ok(res$plot_nomogram))
})

test_that("the competing-risk risk-group plot renders instead of crashing", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "cause",
                            multievent = TRUE, analysistype = "compete",
                            dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                            explanatory = "Sex", contexpl = "Age",
                            calculateRiskScore = TRUE, plotRiskGroups = TRUE,
                            numRiskGroups = "two")
  expect_true(render_ok(res$riskGroupPlot))
})

test_that("model performance metrics carry no fixed-cut-off verdict words", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex", contexpl = "Age",
                            show_survmetrics = TRUE, survmetrics_timepoints = "6, 12")
  tab <- as.data.frame(res$survMetricsTable)
  expect_gt(nrow(tab), 1)
  expect_false(any(grepl("Good|Acceptable|Limited|Excellent|Fair|Poor",
                         tab$interpretation)))
})

test_that("the Cox table header names the user's variables, not mytime/myoutcome", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex")
  # The Cox results are a Table now (fittedTable); its first column title is the header.
  hdr <- vapply(res$fittedTable$columns, function(x) x$title, character(1))
  expect_gt(length(hdr), 0)
  expect_false(any(grepl("Surv(mytime, myoutcome)", hdr, fixed = TRUE)))
  expect_true(any(grepl("Surv(time, status)", hdr, fixed = TRUE)))
})

# check-module 2026-11-28: four catch-all handlers in multisurvival wrapped code that reaches
# private$.checkpoint(), so a restart (an error with code "restart") became "diagnostics
# unavailable" / "Performance metric error" / "Nomogram calculation error" instead of reaching
# jamovi. The callback asks for a restart only from a checkpoint reached INSIDE a tryCatch in
# `owner` (and, when given, under `callee`), so an unguarded checkpoint elsewhere cannot make the
# test pass by itself.
.ms_restart_at <- function(owner, callee = NULL) {
  function(results) {
    calls <- vapply(sys.calls(), function(x) paste(deparse(x[[1]]), collapse = ""), "")
    i <- match(TRUE, endsWith(calls, owner))
    if (is.na(i)) return(NULL)
    rest <- calls[-seq_len(i)]
    j <- match("tryCatch", rest)
    if (is.na(j)) return(NULL)
    if (!is.null(callee) && !any(endsWith(rest[-seq_len(j)], callee))) return(NULL)
    "restart"
  }
}

.ms_restart_run <- function(owner, callee = NULL, ...) {
  set.seed(7)
  n <- 150
  d <- data.frame(age = rnorm(n, 60, 10), arm = factor(sample(c("A", "B"), n, TRUE)))
  t <- rexp(n, 0.03 * exp(0.03 * (d$age - 60))); cen <- runif(n, 10, 80)
  d$time <- pmin(t, cen); d$status <- factor(as.integer(t <= cen))
  ns <- .multisurvival_ns()
  o <- get("multisurvivalOptions", envir = ns)$new(
    elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", contexpl = "age", ...)
  a <- get("multisurvivalClass", envir = ns)$new(options = o, data = d)
  a$init()
  # A flush = TRUE checkpoint serialises the results with RProtoBuf_serialize(), which only the
  # jamovi engine defines. jmvcore's lookup falls through to the global environment, so an inert
  # stub lets the checkpoint reach the callback; its arguments are never evaluated.
  had <- exists("RProtoBuf_serialize", envir = globalenv(), inherits = FALSE)
  if (!had) {
    assign("RProtoBuf_serialize", function(...) NULL, envir = globalenv())
    on.exit(rm("RProtoBuf_serialize", envir = globalenv()), add = TRUE)
  }
  a$.__enclos_env__$private$.checkpointCB <- .ms_restart_at(owner, callee)
  tryCatch(suppressWarnings(a$.__enclos_env__$private$.run()), error = function(e) e)
}

test_that("multisurvival lets a checkpoint restart escape its optional-output handlers", {
  for (case in list(
    list(owner = ".performSurvivalAnalysis", callee = ".cox_ph", args = list(ph_cox = TRUE)),
    list(owner = ".calculate_survmetrics", callee = NULL, args = list(show_survmetrics = TRUE)),
    list(owner = ".calculate_nomogram", callee = ".nomogram", args = list(showNomogram = TRUE))
  )) {
    res <- do.call(.ms_restart_run, c(list(case$owner, case$callee), case$args))
    code <- if (inherits(res, "error")) res$code else NA_character_
    expect_identical(code, "restart", info = case$owner)
  }
})

# The risk-group fallback used jmvcore::reject() as control flow inside a catch-all. A predictor
# whose linear predictor has two values cannot be cut into four quantile groups, so it must fall
# back to two groups (one binary predictor), with the fallback disclosed.
test_that("multisurvival risk groups fall back to fewer groups without reject()", {
  set.seed(8)
  n <- 160
  d <- data.frame(arm = factor(sample(c("A", "B"), n, TRUE)))
  t <- rexp(n, ifelse(d$arm == "B", 0.05, 0.02)); cen <- runif(n, 10, 80)
  d$time <- pmin(t, cen); d$status <- factor(as.integer(t <= cen))
  r <- suppressWarnings(multisurvival(data = d, elapsedtime = "time", outcome = "status",
    outcomeLevel = "1", explanatory = "arm", calculateRiskScore = TRUE, numRiskGroups = "four",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
  txt <- paste(r$warnings$content, r$infoMessages$content, collapse = " ")
  expect_true(grepl("Fell back to 2 groups|Risk groups could not be formed", txt))
})
