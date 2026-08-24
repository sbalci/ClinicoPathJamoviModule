# ── .stripBackticks: shared design-matrix colname cleaner ─────────────────────
#
# model.matrix() builds colnames from the formula's TERM LABELS, and terms()
# deparses a non-syntactic data-frame column name with backticks. jamovi
# variable names routinely contain spaces, hyphens, parentheses and percent
# signs, and jmvcore deliberately restores those raw names into self$data, so
# this is the normal case rather than an exotic one.
#
# Nine analyses build a design matrix this way and printed the backticks
# verbatim in results tables (or matched column names against the original
# variable name and silently missed). They now share this one helper.

# .stripBackticks is package-internal (no @export), so it is not in scope for
# tests under load_all(export_all = FALSE); reach it through the namespace.
.stripBackticks <- ClinicoPath:::.stripBackticks

test_that("backticks are stripped from both numeric and factor-dummy colnames", {
  d <- data.frame(
    check.names = FALSE,
    `Ki-67 (%)`   = c(10, 20, 30, 40),
    `Tumor Grade` = factor(c("Low", "High", "Low", "High")),
    plain         = c(1, 2, 3, 4)
  )
  mm <- model.matrix(~ ., data = d)[, -1, drop = FALSE]

  # the defect: a numeric gets backticks at BOTH ends, a factor dummy gets the
  # closing backtick in the MIDDLE - which is why an anchored ^`|`$ is not enough
  expect_true(any(grepl("^`.*`$", colnames(mm))))
  expect_true(any(grepl("^`.*`.+$", colnames(mm))))

  cleaned <- colnames(.stripBackticks(mm))
  expect_false(any(grepl("`", cleaned, fixed = TRUE)))
  expect_true("Ki-67 (%)" %in% cleaned)
  expect_true("Tumor GradeLow" %in% cleaned)
  # the ORIGINAL name must survive intact - make.names would give "X.Ki.67...."
  expect_false(any(grepl("^X\\.", cleaned)))
})

test_that("an anchored strip would not have been enough", {
  # guards against a regression to the first, incomplete version of this fix
  nms <- c("`Ki-67 (%)`", "`Tumor Grade`Low")
  anchored <- gsub("^`|`$", "", nms)
  expect_true(any(grepl("`", anchored, fixed = TRUE)))   # the factor case survives
  expect_false(any(grepl("`", .stripBackticks(nms), fixed = TRUE)))
})

test_that("stripping cannot leave two columns sharing a name", {
  # a numeric "Tumor GradeLow" beside the factor dummy above collides once the
  # quoting is removed; every downstream consumer looks columns up BY NAME
  d <- data.frame(
    check.names = FALSE,
    `Tumor Grade`    = factor(c("Low", "High", "Low", "High")),
    `Tumor GradeLow` = c(1, 2, 3, 4)
  )
  mm <- model.matrix(~ ., data = d)[, -1, drop = FALSE]
  cleaned <- colnames(.stripBackticks(mm))

  expect_equal(anyDuplicated(cleaned), 0L)
  expect_equal(length(cleaned), ncol(mm))
})

test_that("it is a no-op for ordinary syntactic names", {
  d <- data.frame(age = c(1, 2, 3, 4), grade = factor(c("a", "b", "a", "b")))
  mm <- model.matrix(~ ., data = d)[, -1, drop = FALSE]
  expect_identical(.stripBackticks(mm), mm)

  nms <- c("age", "gradeb")
  expect_identical(.stripBackticks(nms), nms)
})

test_that("it handles the shapes callers actually pass", {
  expect_null(.stripBackticks(NULL))
  # a matrix with no colnames is returned untouched
  m <- matrix(1:4, 2)
  expect_identical(.stripBackticks(m), m)
  # character vector form (used where only the names are carried forward)
  expect_identical(.stripBackticks(c("`a b`", "c")), c("a b", "c"))
  expect_identical(.stripBackticks(character(0)), character(0))
})

test_that("every analysis that builds a design matrix from user data uses it", {
  # Nine sites were confirmed to leak backticks into display or name lookups.
  # If one of them is refactored back to a bare model.matrix(), this fails.
  fixed <- c(
    "adaptivelasso", "grouplasso", "highdimcox", "lassocox", "lassologistic",
    "ncvregcox", "pcacox", "survivalbart", "firthregression", "modelbuilder"
  )
  for (fn in fixed) {
    path <- test_path("..", "..", "R", paste0(fn, ".b.R"))
    skip_if_not(file.exists(path), paste("missing", path))
    src <- readLines(path, warn = FALSE)
    expect_true(any(grepl(".stripBackticks", src, fixed = TRUE)),
                info = paste(fn, "no longer strips design-matrix backticks"))
  }
})

test_that("the second backtick source - fitted-model coefficient names - is cleaned", {
  # composeTerms() backtick-quotes non-syntactic names when it builds the model
  # FORMULA (deliberately, for safety), so the FITTED MODEL's own coefficient
  # names come back quoted too. That is independent of the design matrix, and
  # it is what pcacox / firthregression / modelbuilder actually PRINT.
  set.seed(4)
  n <- 80
  d <- data.frame(
    check.names = FALSE,
    y             = rbinom(n, 1, 0.5),
    `Ki-67 (%)`   = rnorm(n, 30, 10),
    `Tumor Grade` = factor(sample(c("Low", "High"), n, TRUE), levels = c("Low", "High"))
  )
  terms_str <- paste(jmvcore::composeTerms(list("Ki-67 (%)", "Tumor Grade")), collapse = " + ")
  fit <- stats::glm(stats::as.formula(paste("y ~", terms_str)), data = d, family = stats::binomial())

  raw <- rownames(summary(fit)$coefficients)
  expect_true(any(grepl("`", raw, fixed = TRUE)))          # the defect exists
  cleaned <- .stripBackticks(raw)
  expect_false(any(grepl("`", cleaned, fixed = TRUE)))     # and is removed
  expect_true("Ki-67 (%)" %in% cleaned)
  expect_true("Tumor GradeHigh" %in% cleaned)
  expect_equal(length(cleaned), length(raw))               # nothing lost
})

test_that("analyses that print fitted-model coefficient names clean them", {
  # Guards the three sites where the design-matrix fix alone was NOT enough.
  for (fn in c("pcacox", "firthregression", "modelbuilder")) {
    path <- test_path("..", "..", "R", paste0(fn, ".b.R"))
    skip_if_not(file.exists(path), paste("missing", path))
    src <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_true(
      grepl("rownames(coef", src, fixed = TRUE) || grepl("names(firth_fit$coefficients)", src, fixed = TRUE),
      info = paste(fn, "should still read model coefficient names")
    )
    expect_true(
      grepl(".stripBackticks(rownames(", src, fixed = TRUE) ||
      grepl(".stripBackticks(names(", src, fixed = TRUE),
      info = paste(fn, "no longer cleans fitted-model coefficient names")
    )
  }
})
