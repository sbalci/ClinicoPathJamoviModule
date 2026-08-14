# Regression cover for the defects found during the dataquality release review.
# Each block fails against the pre-review backend.

library(testthat)

dq_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

test_that("Little's MCAR test is actually reachable", {
  # naniar is called at runtime via requireNamespace()/naniar::mcar_test(), but
  # it sat in Suggests. jamovi installs Imports on first run and cannot install a
  # missing package on demand, so every jamovi user got "unavailable because the
  # optional naniar package is not installed" with no way to fix it. Moved to
  # Imports (missingdataexplorer.b.R calls it at runtime too).
  desc <- read.dcf("../../DESCRIPTION")
  imports <- trimws(strsplit(desc[1, "Imports"], ",")[[1]])
  suggests <- trimws(strsplit(desc[1, "Suggests"], ",")[[1]])
  expect_true("naniar" %in% imports)
  expect_false("naniar" %in% suggests)

  set.seed(3)
  d <- data.frame(x = c(rnorm(50), rep(NA, 10)),
                  y = c(rnorm(55), rep(NA, 5)),
                  z = rnorm(60))
  out <- dq_txt(dataquality(data = d, vars = c("x", "y", "z"),
                            check_missing = TRUE)$text$content)

  expect_match(out, "Little's MCAR test (naniar)", fixed = TRUE)
  expect_false(grepl("naniar package is not installed", out, fixed = TRUE))
  # the reported p must match naniar directly
  p <- as.data.frame(naniar::mcar_test(d))[1, "p.value"]
  expect_match(out, sprintf("p = %.4f", p), fixed = TRUE)
})

test_that("MCAR is skipped, with a reason, when it cannot be computed", {
  set.seed(4)
  one_numeric <- data.frame(x = c(rnorm(50), rep(NA, 10)),
                            g = factor(sample(c("a", "b"), 60, TRUE)))
  out1 <- dq_txt(dataquality(data = one_numeric, vars = c("x", "g"),
                             check_missing = TRUE)$text$content)
  expect_match(out1, "requires at least two numeric variables")

  complete <- data.frame(x = rnorm(40), y = rnorm(40))
  out2 <- dq_txt(dataquality(data = complete, vars = c("x", "y"),
                             check_missing = TRUE)$text$content)
  expect_match(out2, "no missing values")
})

test_that("the variance flag is labelled as what it detects", {
  # sd < .Machine$double.eps detects EXACTLY constant variables. It was labelled
  # "Near-zero variance" everywhere, which claims a screen it does not perform:
  # a variable with sd = 1e-9 is genuinely degenerate and is not flagged.
  d <- data.frame(const = rep(5, 50),
                  tiny  = 5 + rnorm(50, 0, 1e-9),
                  normal = rnorm(50))
  r <- dataquality(data = d, vars = c("const", "tiny", "normal"),
                   showExplanations = TRUE, showRecommendations = TRUE)

  panel <- dq_txt(r$text$content)
  expect_match(panel, "Constant (zero variance): const", fixed = TRUE)
  # only `const` may be listed on that flag line - `tiny` (sd = 1e-9) is not
  # detected, which is exactly the limitation the label now admits to
  flagged <- regmatches(panel, regexpr("Constant \\(zero variance\\): [^A-Z]+", panel))
  expect_false(grepl("tiny", flagged))
  expect_false(grepl("normal", flagged))

  # no output may still call this a near-zero-variance screen
  for (item in list(r$text, r$summary, r$recommendations, r$explanations))
    expect_false(grepl("Near-Zero Variance", as.character(item$content), fixed = TRUE))

  # and the limitation is stated, pointing at the right tool
  expl <- dq_txt(r$explanations$content)
  expect_match(expl, "does NOT cover")
  expect_match(expl, "nearZeroVar")
})

test_that("row-mode duplicates with one variable explain the fallback", {
  # Ticking "Duplicate rows" with a single variable fell through to value-level
  # analysis under a heading that contradicted the checkbox, with no message.
  set.seed(1)
  d <- data.frame(a = sample(letters[1:3], 60, TRUE))
  out <- dq_txt(dataquality(data = d, vars = "a", check_duplicates = TRUE,
                            complete_cases_only = TRUE)$text$content)
  expect_match(out, "needs at least two variables")

  # two variables: real row analysis, no note
  d2 <- cbind(d, b = sample(letters[1:3], 60, TRUE))
  out2 <- dq_txt(dataquality(data = d2, vars = c("a", "b"), check_duplicates = TRUE,
                             complete_cases_only = TRUE)$text$content)
  expect_match(out2, "Duplicate Row Analysis")
  expect_false(grepl("needs at least two variables", out2))
})

test_that("duplicate counts match hand computation in both modes", {
  set.seed(1)
  d <- data.frame(a = sample(letters[1:3], 60, TRUE),
                  b = sample(letters[1:3], 60, TRUE),
                  c = sample(1:4, 60, TRUE),
                  e = sample(1:4, 60, TRUE))
  vars <- c("a", "b", "c", "e")

  dup_rows <- nrow(d) - nrow(unique(d))
  dup_vals <- sum(vapply(d, function(x) sum(!is.na(x)) - length(unique(na.omit(x))), numeric(1)))

  # !! is required: jmvcore resolves a bare symbol to its own NAME, so a plain
  # `vars = vars` asks for a column literally called "vars".
  row_mode <- dq_txt(dataquality(data = d, vars = !!vars, check_duplicates = TRUE,
                                 complete_cases_only = TRUE)$summary$content)
  val_mode <- dq_txt(dataquality(data = d, vars = !!vars, check_duplicates = TRUE,
                                 complete_cases_only = FALSE)$summary$content)

  # the two modes count different things and must SAY which
  expect_match(row_mode, sprintf("%d duplicate rows", dup_rows))
  expect_match(val_mode, sprintf("%d duplicate values", dup_vals))
  expect_false(grepl("duplicate values", row_mode))
  expect_false(grepl("duplicate rows", val_mode))
})

test_that("per-variable missing and duplicate percentages are correct", {
  set.seed(9)
  x <- c(rnorm(80), rep(NA, 20))
  g <- c(rep(c("a", "b"), 40), rep(NA, 20))
  d <- data.frame(x = x, g = g)

  out <- dq_txt(dataquality(data = d, vars = c("x", "g"), check_missing = TRUE)$text$content)

  # the analysis pastes round(pct, 1), so 20.0 prints as "20" not "20.0"
  expect_match(out, sprintf("Missing: %d/%d (%s%%)", sum(is.na(x)), length(x),
                            round(100 * sum(is.na(x)) / length(x), 1)), fixed = TRUE)

  # duplicate % is over NON-MISSING values
  nm <- sum(!is.na(g)); uq <- length(unique(na.omit(g)))
  val <- dq_txt(dataquality(data = d, vars = c("x", "g"), check_duplicates = TRUE,
                            complete_cases_only = FALSE)$text$content)
  expect_match(val, sprintf("Unique: %d, Duplicates: %d (%s%% of non-missing)",
                            uq, nm - uq, round(100 * (nm - uq) / nm, 1)), fixed = TRUE)
})
