# Regressions for the /check-function-full lassologistic findings fixed on 2026-10-12:
# M1 default score scale, M2 notice order, M3 bootstrap reuse, L7/L8 column titles,
# L9 calibration-slope subset. (L4/L5, the refused outcomes, are in test-lassologistic.R.)

fx_pannen <- function(...) {
  skip_if_not_installed("glmnet")
  args <- list(data = ClinicoPath::lassologistic_pannen, outcome = "diagnosis", outcomeLevel = "PanNEC",
               explanatory = c("p53_aberrant", "rb1_loss", "sstr2a_3plus", "ki67_pct", "daxx_atrx_loss",
                               "organoid_pattern", "plasmacytoid_cells", "interstitial_reaction",
                               "coexist_carcinoma", "mitotic_count", "age_years", "tumor_size_cm"))
  suppressWarnings(do.call(ClinicoPath::lassologistic, utils::modifyList(args, list(...))))
}
fx_note <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note
fx_text <- function(res) gsub("\\s+", " ", gsub("<[^>]+>", " ", res$notices$content))

test_that("M1 the default score is Max-scaled and stays inside its 20-point ceiling", {
  expect_identical(formals(ClinicoPath::lassologistic)$scoringMethod, "maxscaled")
  res <- fx_pannen(scoringSystem = TRUE)
  st <- res$scoringTable$asDF
  expect_equal(st$points, st$points_maxscaled)
  expect_lte(max(abs(st$points)), 10)                      # scoringMaxPoints default
  expect_null(fx_note(res$scoringTable, "wide_scale"))
  expect_false(grepl("Wide Scoring Scale", fx_text(res), fixed = TRUE))
  # Smallest-unit on the same data gave -23 to 120 with no warning under the old 100-point rule
  sm <- fx_pannen(scoringSystem = TRUE, scoringMethod = "smallest")
  expect_gt(max(abs(sm$scoringTable$asDF$points)), 20)
  expect_match(fx_note(sm$scoringTable, "wide_scale"), "more than the 20 the Max-scaled method allows")
  expect_match(fx_text(sm), "Wide Scoring Scale", fixed = TRUE)
})

test_that("M1 compare mode uses Max-scaled for the selected points and says so", {
  res <- fx_pannen(scoringSystem = TRUE, scoringMethod = "compare")
  st <- res$scoringTable$asDF
  expect_equal(st$points, st$points_maxscaled)
  sp <- res$scoringPerformance$asDF
  expect_equal(sp$value[sp$metric == "Scoring method"], "Max-scaled (primary)")
  expect_match(fx_note(res$scoringTable, "method"), "use Max-scaled", fixed = TRUE)
})

test_that("M2 notices render most severe first", {
  # 60 rows of the bundled data: EPV < 5 raises a Serious warning beside the INFO notices. (The
  # full data set raised one by itself until it was regenerated to overlap, review R2 2026-10-12.)
  # (Not fx_pannen(data = ): modifyList() merges a data frame column by column.)
  skip_if_not_installed("glmnet")
  res <- suppressWarnings(ClinicoPath::lassologistic(
    data = ClinicoPath::lassologistic_pannen[c(1:30, 101:130), ], outcome = "diagnosis", outcomeLevel = "PanNEC",
    explanatory = c("p53_aberrant", "rb1_loss", "sstr2a_3plus", "ki67_pct", "daxx_atrx_loss", "organoid_pattern",
                    "plasmacytoid_cells", "interstitial_reaction", "coexist_carcinoma", "mitotic_count",
                    "age_years", "tumor_size_cm")))
  txt <- fx_text(res)
  sw <- regexpr("Serious warning:", txt, fixed = TRUE)
  info <- regexpr("Information:", txt, fixed = TRUE)
  expect_true(sw > 0 && info > 0)
  expect_lt(sw, info)        # "Analysis unit" INFO used to come first
  sev <- regmatches(txt, gregexpr("(Error|Serious warning|Warning|Information):", txt))[[1]]
  rank <- match(sub(":$", "", sev), c("Error", "Serious warning", "Warning", "Information"))
  expect_false(is.unsorted(rank))
})

test_that("M3 a filled bootstrap summary is reused with its notes and recomputed when the inputs change", {
  skip_if_not_installed("glmnet")
  set.seed(5); n <- 200
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$y <- rbinom(n, 1, plogis(0.8 * d$a))
  opts <- ClinicoPath:::lassologisticOptions$new(outcome = "y", outcomeLevel = NULL,
    explanatory = c("a", "b", "c"), bootstrapValidation = TRUE, bootstrapN = 50)
  an <- ClinicoPath:::lassologisticClass$new(options = opts, data = d)
  an$init(); suppressWarnings(an$run())
  vt <- an$results$validationTable
  first <- vt$asDF
  st <- vt$state
  expect_false(is.null(st$key))
  expect_false(any(vapply(st, function(x) is.data.frame(x) || length(x) > 50, logical(1))))  # no row-level data

  # a reused summary is what the next run writes: plant a sentinel optimism
  st$mean_optimism_auc <- 0.25
  vt$setState(st)
  suppressWarnings(an$run())
  again <- vt$asDF
  expect_equal(again$optimism[1], 0.25)
  expect_equal(again$corrected[1], st$apparent_auc - 0.25)
  expect_match(fx_note(vt, "optimism_warning"), "Optimism = 0.250", fixed = TRUE)  # notes rebuilt
  expect_match(fx_note(vt, "seed"), "Random seed")

  # a summary whose key no longer matches the inputs is recomputed, not shown
  st$key$data <- paste0(st$key$data, "0")   # the data hash of some other input
  vt$setState(st)
  suppressWarnings(an$run())
  expect_equal(vt$asDF$optimism, first$optimism)
})

test_that("L7/L8 the comparison columns name what they hold", {
  res <- fx_pannen(scoringSystem = TRUE, scoringMethod = "compare", showModelComparison = TRUE)
  expect_equal(res$methodComparison$getColumn("info_loss")$title, "Relative AUC Loss vs Full Model (%)")
  expect_equal(res$modelComparison$getColumn("n_variables")$title, "N Terms")
})

test_that("L9 a calibration-slope correction resting on a subset of replicates says so", {
  skip_if_not_installed("glmnet")
  d <- withr::with_seed(2, {
    d <- data.frame(x1 = rnorm(40), x2 = rnorm(40), x3 = rnorm(40))
    d$y <- rbinom(40, 1, plogis(3 * d$x1)); d
  })
  res <- suppressWarnings(ClinicoPath::lassologistic(data = d, outcome = "y", outcomeLevel = NULL,
    explanatory = c("x1", "x2", "x3"), lambda = "lambda.min", nfolds = 5,
    bootstrapValidation = TRUE, bootstrapN = 50))
  note <- fx_note(res$validationTable, "slope_subset")
  expect_match(note, "rests on \\d+ of the 50 completed replicates")
  k <- as.integer(sub(".*rests on (\\d+) of.*", "\\1", note))
  expect_lt(k, 50)
  expect_true(is.finite(res$validationTable$asDF$corrected[3]))
})

# /fix-function 2026-10-12: refusals caused by listwise deletion now give the cause and the counts,
# and glmnet's per-call small-fold warnings become one notice each instead of raw Analysis Notes.
ff_preds <- c("p53_aberrant", "rb1_loss", "sstr2a_3plus", "ki67_pct", "daxx_atrx_loss", "organoid_pattern",
              "mitotic_count", "age_years", "tumor_size_cm", "institution")
# do.call: the wrapper reads a bare `explanatory = ff_preds` as a column NAMED ff_preds.
ff_run <- function(...) do.call(ClinicoPath::lassologistic, list(...))

test_that("FF-1 a refusal caused by missing predictors says how many cases were excluded", {
  skip_if_not_installed("glmnet")
  d <- ClinicoPath::lassologistic_pannen
  d$ki67_pct[d$diagnosis == "PanNET_G3"] <- NA       # the marker recorded in one group only
  expect_error(ff_run(data = d, outcome = "diagnosis", outcomeLevel = "PanNEC",
                                          explanatory = ff_preds),
               "Only cases with outcome 'PanNEC' remain\\. \\d+ of 200 cases have the outcome and every predictor recorded; \\d+ were excluded because a predictor was missing\\.")
  set.seed(21)
  small <- data.frame(y = factor(rep(c("e", "r"), c(4, 20))), x1 = rnorm(24), x2 = rnorm(24))
  expect_error(ff_run(data = small, outcome = "y", outcomeLevel = "e",
                                          explanatory = c("x1", "x2")),
               "The complete cases include 4 events and 20 non-events.", fixed = TRUE)
})

test_that("FF-2 small folds raise one notice, not a warning per cv.glmnet call", {
  skip_if_not_installed("glmnet")
  d <- withr::with_seed(5, ClinicoPath::lassologistic_pannen[sample(200, 30), ])   # 27 complete cases
  expect_no_warning(res <- ff_run(data = d, outcome = "diagnosis",
    outcomeLevel = "PanNEC", explanatory = ff_preds, bootstrapValidation = TRUE, bootstrapN = 50))
  expect_match(res$notices$content, "Cross-validation error computed per case", fixed = TRUE)
  pn <- ClinicoPath::lassologistic_pannen
  few <- rbind(pn[pn$diagnosis == "PanNEC", ][1:6, ], pn[pn$diagnosis == "PanNET_G3", ][1:60, ])
  few <- few[complete.cases(few[c("diagnosis", ff_preds)]), ]
  expect_no_warning(res <- ff_run(data = few, outcome = "diagnosis",
    outcomeLevel = "PanNEC", explanatory = ff_preds))
  n_hits <- lengths(gregexpr("Few cases of one class per fold", res$notices$content, fixed = TRUE))
  expect_equal(n_hits, 1L)
})
