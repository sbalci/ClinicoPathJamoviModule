library(testthat)

ct_daily_data <- function() {
  data.frame(grp = factor(rep(c("A", "B"), each = 20)),
             x = c(1:20, 3:22),
             cat = factor(c(rep(c("yes", "no"), c(14, 6)),
                            rep(c("yes", "no"), c(6, 14)))))
}
ct_daily_run <- function(d, ...) {
  do.call(crosstable, c(list(data = d, vars = c("x", "cat"), group = "grp",
                            showSMD = TRUE, showSummary = TRUE), list(...)))
}

test_that("VAL-crosstable-01 genuine NA levels use the selected missing-data policy", {
  d <- ct_daily_data()
  reference <- ct_daily_run(d, excl = TRUE)
  for (ordered in c(FALSE, TRUE)) {
    dirty <- rbind(d, data.frame(grp = "A", x = 100, cat = NA))
    dirty$cat <- factor(dirty$cat, exclude = NULL, ordered = ordered)
    attr(dirty$cat, "label") <- "Category"
    attr(dirty$cat, "values") <- seq_along(levels(dirty$cat))
    before <- dirty
    got <- ct_daily_run(dirty, excl = TRUE)
    expect_equal(got$smdTable$asDF$smd, reference$smdTable$asDF$smd)
    expect_match(got$summary$content, "Cross table of 40 rows", fixed = TRUE)
    expect_match(got$summary$content, "1 of 41 rows", fixed = TRUE)
    expect_false(got$errorNotice$visible)
    expect_identical(dirty, before)
    expect_match(paste(got$notes$content, got$notices$content),
                 "removed 1 of 41 rows", fixed = TRUE)
    # Without listwise exclusion, x keeps the extra observation, but cat does not.
    kept <- ct_daily_run(dirty, excl = FALSE)
    expect_equal(kept$smdTable$asDF$smd[2], reference$smdTable$asDF$smd[2])
    expect_false(kept$errorNotice$visible)
    expect_match(kept$summary$content, "Cross table of 41 rows", fixed = TRUE)
  }
  literal <- d
  literal$cat <- factor(c("NA", as.character(d$cat)[-1]),
                        levels = c("no", "yes", "NA"))
  res <- ct_daily_run(literal, excl = TRUE)
  expect_match(res$summary$content, "Cross table of 40 rows", fixed = TRUE)
  expect_false(grepl("excluded for missing", res$summary$content, fixed = TRUE))
  # Genuine missing groups must not become an additional comparison arm.
  dirty <- rbind(d, data.frame(grp = NA, x = 100, cat = "yes"))
  dirty$grp <- factor(dirty$grp, exclude = NULL)
  got <- ct_daily_run(dirty, excl = TRUE)
  expect_equal(got$smdTable$asDF$smd, reference$smdTable$asDF$smd)
  expect_match(got$summary$content, "Cross table of 40 rows", fixed = TRUE)
})

test_that("VAL-crosstable-02 normal returns and option changes clear old results", {
  d <- ct_daily_data()
  o <- crosstableOptions$new(vars = c("x", "cat"), group = "grp",
                             showSMD = TRUE, showSummary = TRUE, p_adjust = "BH")
  a <- crosstableClass$new(options = o, data = d)
  p <- a$.__enclos_env__$private
  run <- function() { p$.init(); p$.run() }
  set <- function(name, value) { opt <- o$option(name); opt$value <- value }
  run()
  expect_true(all(is.finite(a$results$smdTable$asDF$smd)))
  expect_gt(nchar(a$results$summary$content), 20)
  set("vars", NULL); run()
  for (item in c("tablestyle1", "tablestyle2", "tablestyle3", "tablestyle4",
                 "summary", "qvalueExplanation", "testInformation", "todo2")) {
    expect_false(grepl("[[:alnum:]]", a$results$get(item)$content), info = item)
  }
  expect_equal(a$results$smdTable$rowCount, 0)
  expect_match(a$results$todo$content, "Welcome to Cross Table Analysis")
  expect_false(a$results$qvalueExplanation$visible)
  set("vars", c("x", "cat")); run()
  fresh <- ct_daily_run(d, p_adjust = "BH")
  expect_equal(a$results$smdTable$asDF, fresh$smdTable$asDF)
  expect_identical(a$results$summary$content, fresh$summary$content)
  set("showSummary", FALSE); set("showSMD", FALSE); run()
  expect_false(grepl("[[:alnum:]]", a$results$summary$content))
  expect_equal(a$results$smdTable$rowCount, 0)
  set("showSummary", TRUE); set("showSMD", TRUE); run()
  # A normal validation return after a valid table must not retain its claims.
  p$.data$grp <- factor(rep("A", nrow(d)))
  run()
  expect_false(grepl("[[:alnum:]]", a$results$summary$content))
  expect_false(grepl("[[:alnum:]]", a$results$tablestyle3$content))
  expect_true(all(is.na(a$results$smdTable$asDF$smd)))
  expect_match(a$results$notices$content, "Only one group")
})

test_that("VAL-crosstable-03 restored options cannot insert executable syntax", {
  d <- ct_daily_data()
  payloads <- c('gtsummary", showSummary = {stop("SYNTAX_CANARY"); TRUE}, cont = "mean',
                "trailing\\", "line\nbreak")
  for (name in setdiff(names(formals(crosstable)), c("data", "vars", "group"))) {
    for (value in payloads) {
      o <- crosstableOptions$new(vars = "x", group = "grp")
      a <- crosstableClass$new(options = o, data = d)
      opt <- o$option(name); opt$value <- value
      parsed <- tryCatch(parse(text = a$asSource())[[1]], error = function(e) NULL)
      expect_false(is.null(parsed), info = name)
      if (!is.null(parsed)) {
        expect_identical(parsed[[name]], value, info = name)
        expect_false(any(vapply(as.list(parsed)[-1], is.call, logical(1))), info = name)
      }
    }
  }
  o <- crosstableOptions$new(vars = "x", group = "grp")
  a <- crosstableClass$new(options = o, data = d)
  evil_name <- 'sty = "gtsummary", showSummary = {stop("SYNTAX_CANARY"); TRUE}, cont'
  o$.addOption(jmvcore:::Option$new(evil_name, "mean"))
  parsed <- parse(text = a$asSource())[[1]]
  expect_setequal(names(as.list(parsed))[-1], c("data", "vars", "group"))
  opt <- o$option("sty"); opt$value <- list(payloads)
  parsed <- parse(text = a$asSource())[[1]]
  expect_null(parsed$sty)
})

test_that("VAL-crosstable-04 FDR prose states an expected upper bound and BY dependence", {
  for (method in c("BH", "BY")) {
    r <- ct_daily_run(ct_daily_data(), p_adjust = method)
    text <- paste(r$qvalueExplanation$content, r$testInformation$content)
    expect_match(text, "at most", fixed = TRUE)
    expect_match(text, "repeated", fixed = TRUE)
    expect_false(grepl("expect ~5%", text, fixed = TRUE))
    expect_false(grepl("expect 5% of discoveries", text, fixed = TRUE))
    expect_false(grepl("Less conservative than Bonferroni/Holm", text, fixed = TRUE))
    if (method == "BY") {
      expect_match(text, "arbitrary dependence", fixed = TRUE)
      expect_false(grepl("FDR methods assume independence", text, fixed = TRUE))
    }
  }
})
