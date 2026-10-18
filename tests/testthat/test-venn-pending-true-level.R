# var1true..var7true are Level options. Without `allowNone: true` the jamovi client
# pre-fills the FIRST level, so a marker coded Absent/Present silently became the set
# "Absent" (inverted membership). With allowNone the option arrives as NULL until the
# user picks a level, and the analysis must WAIT for it - an INFO notice that lists the
# observed levels, nothing counted or drawn - instead of raising a validation error.

vennClass <- getFromNamespace("vennClass", "ClinicoPath")
vennOptions <- getFromNamespace("vennOptions", "ClinicoPath")

library(testthat)

# A: 52 Absent, 28 Present, 2 missing, plus an unused level that must not be listed
pending_data <- function() {
  data.frame(
    A = factor(c(rep("Absent", 52), rep("Present", 28), NA, NA),
               levels = c("Absent", "Present", "Unknown")),
    B = factor(rep(c("Neg", "Pos"), c(32, 50))),
    C = factor(rep(c("No", "Yes"), c(62, 20))))
}

vn_pending <- function(d, ...) {
  args <- list(data = d, var1 = "A", var1true = "Present", var2 = "B", var2true = "Pos",
               var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
               var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
               var7 = NULL, var7true = NULL)
  # modifyList() drops list elements set to NULL, so assign them one by one
  dots <- list(...)
  for (nm in names(dots)) args[nm] <- list(dots[[nm]])
  do.call(venn, args)
}

plot_items <- c("plotGgvenn", "plotGgVennDiagram", "plotUpsetR", "plotComplexUpset")
count_cols <- c("trueCount", "falseCount", "totalCount", "truePercentage")

expect_waiting <- function(res, var, levels_text) {
  notices <- as.character(res$notices$content)
  expect_match(notices, "Waiting for a True Level", fixed = TRUE)
  expect_match(notices, paste0("'", var, "'"), fixed = TRUE)
  expect_match(notices, levels_text, fixed = TRUE)
  expect_match(notices, "Nothing is drawn or counted", fixed = TRUE)
  # INFO, not an error or a warning
  expect_false(grepl("ERROR", notices, fixed = TRUE))
  expect_false(isTRUE(res$validationErrors$visible))
  # nothing counted, nothing to draw
  sm <- res$summary$asDF
  expect_true(all(is.na(sm[, count_cols])))
  for (img in plot_items) expect_null(res[[img]]$state)
}

test_that("all seven true-level options allow None and carry no default", {
  path <- testthat::test_path("..", "..", "jamovi", "venn.a.yaml")
  skip_if_not(file.exists(path), "venn.a.yaml not reachable from the test directory")
  opts <- yaml::read_yaml(path)$options
  by_name <- stats::setNames(opts, vapply(opts, `[[`, character(1), "name"))
  for (nm in paste0("var", 1:7, "true")) {
    o <- by_name[[nm]]
    expect_identical(o$type, "Level", info = nm)
    expect_true(isTRUE(o$allowNone), info = nm)
    expect_false("default" %in% names(o), info = nm)
  }
})

test_that("an unset true level for var1, var2 or var3 waits with the observed levels", {
  d <- pending_data()
  for (unset in list(NULL, "", NA, NA_character_)) {
    label <- if (is.null(unset)) "NULL" else deparse(unset)

    res <- expect_no_error(vn_pending(d, var1true = unset))
    expect_waiting(res, "A", "Absent (n = 52), Present (n = 28)")
    # missing values and the unused level are not offered as levels
    expect_false(grepl("Unknown", as.character(res$notices$content), fixed = TRUE),
                 info = label)

    res <- expect_no_error(vn_pending(d, var2true = unset))
    expect_waiting(res, "B", "Neg (n = 32), Pos (n = 50)")
    # A and B are both chosen here, so only B is waiting
    expect_false(grepl("'A'", as.character(res$notices$content), fixed = TRUE),
                 info = label)

    res <- expect_no_error(vn_pending(d, var3 = "C", var3true = unset))
    expect_waiting(res, "C", "No (n = 62), Yes (n = 20)")
    expect_equal(nrow(res$summary$asDF), 3L)
  }
})

test_that("every selected variable without a true level gets its own waiting notice", {
  res <- vn_pending(pending_data(), var1true = NULL, var3 = "C", var3true = NULL)
  notices <- as.character(res$notices$content)
  expect_match(notices, "'A'", fixed = TRUE)
  expect_match(notices, "'C'", fixed = TRUE)
  expect_false(grepl("'B'", notices, fixed = TRUE))
})

test_that("ready -> waiting -> ready on one analysis object leaves no stale result", {
  d <- pending_data()
  a <- vennClass$new(
    options = vennOptions$new(
      var1 = "A", var1true = "Present", var2 = "B", var2true = "Pos",
      var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
      var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
      var7 = NULL, var7true = NULL,
      show_ggVennDiagram = TRUE, show_upsetR = TRUE, show_complexUpset = TRUE,
      showSetCalculations = TRUE, clinicalSummary = TRUE, reportSentences = TRUE,
      showMembershipTable = TRUE),
    data = d)
  a$init()
  level1 <- a$options$option("var1true")   # an R6 reference: setting it changes `a`

  # ready: complete cases are the 80 rows with A observed (B: 48 Pos among them)
  a$run()
  expect_equal(a$results$summary$asDF$trueCount, c(28L, 48L))
  for (img in plot_items) expect_false(is.null(a$results[[img]]$state), info = img)
  expect_match(as.character(a$results$setCalculations$content), "cases")
  expect_gt(a$results$membershipTable$rowCount, 0L)
  expect_true("levels" %in% names(a$results$summary$notes))

  # waiting: the user clears var1's true level
  level1$value <- NULL
  expect_no_error(a$run())
  expect_waiting(a$results, "A", "Absent (n = 52), Present (n = 28)")
  for (nm in c("setCalculations", "clinicalSummary", "reportSentences"))
    expect_false(grepl("[0-9]", as.character(a$results[[nm]]$content)), info = nm)
  expect_equal(a$results$membershipTable$rowCount, 0L)
  expect_false("levels" %in% names(a$results$summary$notes))

  # ready again, with the opposite level: the numbers follow the choice
  level1$value <- "Absent"
  a$run()
  expect_equal(a$results$summary$asDF$trueCount, c(52L, 48L))
  expect_false(grepl("Waiting for a True Level",
                     as.character(a$results$notices$content), fixed = TRUE))
  for (img in plot_items) expect_false(is.null(a$results[[img]]$state), info = img)
})

test_that("a true level that is not a level of its variable is an honest error", {
  res <- vn_pending(pending_data(), var1true = "Maybe")
  expect_true(res$validationErrors$visible)
  msg <- as.character(res$validationErrors$content)
  expect_match(msg, "Maybe", fixed = TRUE)
  expect_match(msg, "'A'", fixed = TRUE)
  expect_match(msg, "Available levels: Absent, Present", fixed = TRUE)
  expect_false(grepl("backticks", msg, fixed = TRUE))
  expect_false(grepl("Waiting for a True Level",
                     as.character(res$notices$content), fixed = TRUE))
  expect_true(all(is.na(res$summary$asDF[, count_cols])))
})

test_that("with var2 still missing, an unset var1 level keeps the welcome panel", {
  # The validator used to raise "true level not specified" here, ahead of the
  # onboarding panel. The wait only starts once var1 and var2 are both selected.
  res <- expect_no_error(vn_pending(pending_data(), var1true = NULL,
                                    var2 = NULL, var2true = NULL))
  expect_true(res$welcome$visible)
  expect_false(isTRUE(res$validationErrors$visible))
  expect_false(grepl("Waiting for a True Level",
                     as.character(res$notices$content), fixed = TRUE))
})
