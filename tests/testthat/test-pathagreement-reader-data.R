test_that("reader preparation retains pair-specific and reference-specific cases", {
  d <- data.frame(a = factor(c("yes", "no", "unknown", NA)),
                  b = factor(c("yes", "yes", "no", "no")),
                  c = factor(c(NA, "no", "no", "yes")),
                  gold = factor(c("yes", "no", "yes", "no")),
                  id = c("same", "same", "C3", "C4"))
  p <- .paReaderPrepare(d, list(readerWorkflow = "binary", vars = c("a", "b", "c"),
    readerPositive = "yes", readerNegative = "no", referenceStandard = "gold",
    referencePositive = "yes", referenceNegative = "no", caseID = "id"))
  expect_true(p$ready)
  expect_equal(p$rows, 1:4)
  expect_equal(p$ids, d$id)
  expect_equal(which(!is.na(p$calls[, 1]) & !is.na(p$calls[, 2])), 1:2)
  expect_equal(which(p$shared), 2L)
  expect_true(is.na(p$calls[3, 1]))
  r <- .paReaderReport(p)
  expect_equal(r$readerDataSummary$indeterminate[1], 1)
  expect_equal(r$readerDataSummary$missing[1], 1)
  expect_equal(r$readerAccuracyTable$n[r$readerAccuracyTable$reader == "a"], rep(2, 3))
})

test_that("numeric rules and inclusive bands classify boundary ties correctly", {
  d <- data.frame(a = c(4, 5, 6, NA), b = c(5, 5, 7, 4))
  expected <- list(ge = c(FALSE, TRUE, TRUE, NA), gt = c(FALSE, FALSE, TRUE, NA),
                   le = c(TRUE, TRUE, FALSE, NA), lt = c(TRUE, FALSE, FALSE, NA))
  for (rule in names(expected)) {
    p <- .paReaderPrepare(d, list(readerWorkflow = "numeric_cutoff", readerScores = c("a", "b"),
      clinicalCutoff = "5", nearCutoffWidth = "0", positiveRule = rule))
    expect_equal(unname(p$calls[, 1]), expected[[rule]])
    expect_equal(unname(p$near[, 1]), c(FALSE, TRUE, FALSE, FALSE))
  }
  expect_false(.paReaderPrepare(d, list(readerWorkflow = "numeric_cutoff",
    readerScores = c("a", "b")))$ready)
  expect_error(.paReaderPrepare(d, list(readerWorkflow = "numeric_cutoff",
    readerScores = c("a", "b"), clinicalCutoff = "system('id')")), "finite number")
})

test_that("ordinal boundaries respect common declared order and metadata mapping", {
  lv <- c("Absent", "Weak", "Strong")
  d <- data.frame(a = ordered(c("Weak", "Strong", NA), lv),
                  b = ordered(c("Absent", "Strong", "Weak"), lv),
                  id = c("META_site", "C1", "C2"), gold = c("X", "yes", "no"))
  o <- list(readerWorkflow = "ordinal_cutoff", vars = c("a", "b"),
    ordinalCutoff = "Weak", nearCutoffLevels = "Weak\nStrong", caseID = "id",
    useMetadataRows = TRUE)
  p <- .paReaderPrepare(d, o)
  expect_equal(p$rows, 2:3)
  expect_equal(p$metadata_n, 1L)
  expect_equal(p$ids, c("C1", "C2"))
  # Removing metadata rows never redefines the declared clinical ordering.
  expect_equal(p$scale, lv)
  o$nearCutoffLevels <- "Weak; Strong"
  expect_equal(.paReaderPrepare(d, o)$near, p$near)
  o$ordinalCutoff <- "Absent"
  expect_true(.paReaderPrepare(d, o)$ready)
  d$b <- factor(d$b, levels = lv, ordered = FALSE)
  expect_error(.paReaderPrepare(d, o), "ordered factors")
})

test_that("unknown labels, incompatible ordinal scales and missing clusters fail clearly", {
  d <- data.frame(a = factor(c("Y", "N")), b = factor(c("Y", "N")), id = c("p1", NA))
  o <- list(readerWorkflow = "binary", vars = c("a", "b"), readerPositive = "Y",
            readerNegative = "N", readerCluster = "id")
  expect_error(.paReaderPrepare(d, o), "cluster ID")
  o$readerCluster <- NULL; o$readerPositive <- "typo"
  expect_error(.paReaderPrepare(d, o), "available levels")
  d$a <- ordered(d$a, c("Y", "N")); d$b <- ordered(d$b, c("N", "Y"))
  expect_error(.paReaderPrepare(d, list(readerWorkflow = "ordinal_cutoff", vars = c("a", "b"),
    ordinalCutoff = "Y")), "same declared")
})
