# Regressions for the 2026-10-12 security audit of lassologistic (all LOW, display/integrity).
# SEC-1 labels in text cells, SEC-2 alias note, SEC-3 braces in refusals, SEC-4 unused manual cuts.
# Table cells and notes are parsed as HTML by the jamovi client: "<b>x" is not XSS but renders
# as "x", and "<img ...>" or "<LOD" as nothing. .rocLabel() breaks the "<" so the name survives.

sec_fit <- function(d, ...) {
  skip_if_not_installed("glmnet")
  suppressWarnings(ClinicoPath::lassologistic(data = d, outcome = "y", lambda = "lambda.min", ...))
}
sec_note <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note

sec_data <- function(n = 150) {
  set.seed(11)
  d <- data.frame(y = factor(sample(c("<b>evt", "ref"), n, TRUE)), check.names = FALSE)
  ev <- d$y == "<b>evt"
  d[["<img src=x>"]] <- rnorm(n) + 1.5 * ev
  d[["Age, years"]] <- rnorm(n, 60, 10) + 8 * ev
  d$z <- rnorm(n)
  d
}

test_that("SEC-1 a label that looks like a tag stays visible in every text cell", {
  res <- sec_fit(sec_data(), outcomeLevel = "<b>evt",
                 explanatory = c("<img src=x>", "Age, years", "z"),
                 scoringSystem = TRUE, showVariableImportance = TRUE)
  expect_true("< img src=x>" %in% res$coefficients$asDF$variable)
  expect_true("< img src=x>" %in% res$scoringTable$asDF$variable)
  expect_true("< img src=x>" %in% res$variableImportance$asDF$variable)
  ms <- res$modelSummary$asDF
  expect_match(ms$value[ms$statistic == "Event class (positive)"], "^< b>evt \\(n=")
})

test_that("SEC-2 the overlapping-names note breaks tags in names and levels", {
  set.seed(12)
  n <- 150
  d <- data.frame(y = factor(sample(c("e", "r"), n, TRUE)), check.names = FALSE)
  ev <- d$y == "e"
  d[["<b>"]] <- factor(ifelse(runif(n) < .3 + .4 * ev, "x", "a"))  # dummy term "<b>x"
  d[["<b>x"]] <- rnorm(n) + ev                                       # numeric "<b>x": a collision
  d$z <- rnorm(n)
  res <- sec_fit(d, outcomeLevel = "e", explanatory = c("<b>", "<b>x", "z"))
  note <- sec_note(res$coefficients, "source_names")
  expect_match(note, "< b>x denotes < b> = x", fixed = TRUE)
  expect_false(grepl("<b>", note, fixed = TRUE))
})

test_that("SEC-3 a level containing {name} is shown in the refusal, not as an ellipsis", {
  set.seed(13)
  # levels pinned: in the C locale "{" sorts after letters
  d <- data.frame(y = factor(sample(c("{grade}", "ref"), 100, TRUE), levels = c("{grade}", "ref")),
                  a = rnorm(100), b = rnorm(100))
  expect_error(sec_fit(d, outcomeLevel = "{missing}", explanatory = c("a", "b")),
               "'(missing)' not found in the outcome variable. Observed levels: (grade), ref.",
               fixed = TRUE)
  d$y <- factor(sample(c("{a}", "{b}", "c"), 100, TRUE), levels = c("{a}", "{b}", "c"))
  expect_error(sec_fit(d, outcomeLevel = "{a}", explanatory = c("a", "b")),
               "3 observed levels ((a), (b), c)", fixed = TRUE)
})

test_that("SEC-4 a manual cut that was not used is reported, not silently replaced", {
  d <- sec_data()
  res <- sec_fit(d, outcomeLevel = "<b>evt", explanatory = c("<img src=x>", "Age, years", "z"),
                 scoringSystem = TRUE, scoreCutMethod = "manual",
                 scoreCutPoints = "Age, years=65; z=0; nosuchvar=3; junk")
  note <- sec_note(res$scoringTable, "manual_ignored")
  # the comma splits "Age, years=65" into "Age" (malformed) and "years=65" (no such variable)
  for (piece in c("Age", "years", "nosuchvar", "junk"))
    expect_match(note, piece, fixed = TRUE)
  expect_false(grepl("z=0", note, fixed = TRUE))
  # every entry usable: no note
  ok <- sec_fit(d, outcomeLevel = "<b>evt", explanatory = c("<img src=x>", "Age, years", "z"),
                scoringSystem = TRUE, scoreCutMethod = "manual", scoreCutPoints = "z=0")
  expect_null(sec_note(ok$scoringTable, "manual_ignored"))
})
