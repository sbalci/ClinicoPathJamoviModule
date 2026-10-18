# A `type: Level` option without `allowNone: true` is filled by the jamovi client with
# the variable's FIRST level, so in lassocox, oddsratio, outcomeorganizer and survivalcont
# the first level - not the one the user means - silently became the event. R tests pass
# the level, so only the schema shows the defect. With allowNone the GUI sends NULL, and
# each analysis must then ask for the level rather than infer it.

library(testthat)

allownone_yaml <- function(analysis) {
  path <- testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml"))
  skip_if_not(file.exists(path), "source schemas are not part of the installed package")
  opts <- yaml::read_yaml(path)$options
  stats::setNames(opts, vapply(opts, function(o) o$name, ""))
}

test_that("every event, censored and positive Level option can be left empty", {
  targets <- list(lassocox = c("outcomeLevel", "censorLevel"),
                  oddsratio = c("outcomeLevel", "predictorLevel"),
                  outcomeorganizer = c("outcomeLevel", "recurrenceLevel"),
                  survivalcont = "outcomeLevel")
  for (analysis in names(targets)) {
    opts <- allownone_yaml(analysis)
    for (name in targets[[analysis]]) {
      expect_identical(opts[[name]]$type, "Level", info = paste(analysis, name))
      expect_true(isTRUE(opts[[name]]$allowNone), info = paste(analysis, name))
      expect_null(opts[[name]]$default, info = paste(analysis, name))
    }
  }
})

html_text <- function(res) {
  paste(unlist(lapply(res$items, function(i)
    tryCatch(as.character(i$content), error = function(e) ""))), collapse = "\n")
}

surv_df <- function(n = 60, seed = 3) {
  set.seed(seed)
  data.frame(time = round(rexp(n, 1 / 24), 1),
             status = factor(sample(c("Alive", "Dead"), n, TRUE), levels = c("Alive", "Dead")),
             marker = round(rnorm(n, 50, 10), 1))
}

test_that("survivalcont asks for the event level instead of taking the first level", {
  res <- run_survivalcont(data = surv_df(), elapsedtime = "time", outcome = "status",
                          contexpl = "marker", outcomeLevel = NULL)
  text <- html_text(res)
  expect_match(text, "Event Level is not selected", fixed = TRUE)
  expect_match(text, "Alive, Dead", fixed = TRUE)
})

test_that("outcomeorganizer asks for the event and the recurrence level", {
  expect_error(run_outcomeorganizer(data = outcomeorganizer_rfs, outcome = "vital",
                                    outcomeLevel = NULL),
               "Event Level is not selected")
  expect_error(run_outcomeorganizer(data = outcomeorganizer_rfs, outcome = "vital",
                                    outcomeLevel = "Dead", recurrence = "recurrence",
                                    recurrenceLevel = NULL, analysistype = "rfs"),
               "no recurrence event level was chosen")
})

or_df <- function(n = 240, seed = 11) {
  set.seed(seed)
  d <- data.frame(grade = factor(sample(c("Low", "High"), n, TRUE)),
                  age = round(rnorm(n, 60, 10), 1))
  lp <- -1 + 0.9 * (d$grade == "High") + 0.02 * (d$age - 60)
  d$dead <- factor(ifelse(rbinom(n, 1, plogis(lp)) == 1, "Dead", "Alive"),
                   levels = c("Alive", "Dead"))
  d
}

test_that("oddsratio waits for the positive outcome level", {
  res <- suppressWarnings(oddsratio(data = or_df(), explanatory = "grade", outcome = "dead",
                                    outcomeLevel = NULL, predictorLevel = NULL))
  expect_match(as.character(res$todo$content), "Positive Outcome Level Required", fixed = TRUE)
})

# An empty predictor level used to be guessed (keyword match, else the second
# level) and flagged "automatically detected". The positive level is never
# inferred now: the diagnostic metrics wait for the choice and list the levels.
test_that("an empty predictor level leaves the diagnostic metrics waiting, never guessed", {
  res <- suppressWarnings(oddsratio(data = or_df(), explanatory = "grade", outcome = "dead",
                                    outcomeLevel = "Dead", diagnosticPredictor = "grade",
                                    predictorLevel = NULL, showNomogram = TRUE))
  text <- html_text(res)
  expect_match(text, "Diagnostic metrics are waiting for the Predictor Positive Level", fixed = TRUE)
  expect_match(text, "High (n = ", fixed = TRUE)
  expect_false(grepl("automatically detected", text, fixed = TRUE))
  expect_false(grepl("as specified", text, fixed = TRUE))
  expect_false(grepl("Sensitivity:", text, fixed = TRUE))
})
