reader_fixture <- function() {
  gold <- rep(c("Yes", "No"), each = 10)
  data.frame(a = factor(c(rep("Yes", 9), "No", rep("No", 6), rep("Yes", 4))),
             b = factor(gold), gold = factor(gold), id = paste0("Case", 1:20),
             patient = rep(1:10, each = 2))
}

reader_run <- function(data, ...) {
  defaults <- list(readerWorkflow = "binary", vars = c("a", "b"),
    readerPositive = "Yes", readerNegative = "No", referenceStandard = "gold",
    referencePositive = "Yes", referenceNegative = "No")
  do.call(pathagreement, c(list(data = data), utils::modifyList(defaults, list(...))))
}

test_that("the exported wrapper reports explicit disease-positive accuracy and code", {
  d <- reader_fixture()
  r <- reader_run(d, readerCode = TRUE, readerConfidence = .9)
  x <- r$readerAccuracyTable$asDF
  a <- x[x$reader == "a", ]
  expect_equal(a$estimate, c(.9, .6, .75))
  expect_equal(a$tp, rep(9, 3))
  expect_equal(a$fn, rep(1, 3))
  expect_equal(a$tn, rep(6, 3))
  expect_equal(a$fp, rep(4, 3))
  expect_equal(c(a$lower[1], a$upper[1]), as.numeric(binom.test(9, 10, conf.level = .9)$conf.int))
  expect_equal(a$status, c("Exact binomial", "Exact binomial", "Bootstrap not requested"))
  expect_equal(r$kappaTable$rowCount, 0)
  expect_match(r$readerRCode$content, "readerConfidence = 0.9", fixed = TRUE)
  your_data <- d
  rerun <- eval(parse(text = r$readerRCode$content))
  expect_equal(rerun$readerAccuracyTable$asDF, x)
})

test_that("missing third readings do not suppress estimable pairs or accuracy", {
  d <- data.frame(a = factor(c("Yes", "No", NA)),
    b = factor(c("Yes", NA, "No")), c = factor(c(NA, "Yes", "No")),
    gold = factor(c("Yes", "No", "No")))
  r <- reader_run(d, vars = c("a", "b", "c"), readerPopulation = "both")
  a <- r$readerAccuracyTable$asDF
  expect_equal(a$n[a$population == "available"], rep(2, 9))
  expect_equal(a$n[a$population == "shared"], rep(0, 9))
  expect_equal(r$cutoffAgreementTable$asDF$n, rep(1, 15))
  expect_equal(r$cutoffDisagreementCases$asDF$row, 2)
})

test_that("numeric mode preserves raw readings and cap does not change summaries", {
  d <- data.frame(a = c(10, 9, 11, 2), b = c(9, 10, 11, 10), id = c("same", "same", "C3", "C4"))
  args <- list(data = d, readerWorkflow = "numeric_cutoff", readerScores = c("a", "b"),
    clinicalCutoff = "10", nearCutoffWidth = "0", caseID = "id", referenceAccuracy = FALSE)
  a <- do.call(pathagreement, c(args, list(readerCaseLimit = 1)))
  b <- do.call(pathagreement, c(args, list(readerCaseLimit = 50)))
  expect_equal(a$cutoffAgreementTable$asDF, b$cutoffAgreementTable$asDF)
  cases <- b$cutoffDisagreementCases$asDF
  expect_equal(cases$row, c(1, 2, 4))
  expect_equal(cases$score1, c("10", "9", "2"))
  expect_equal(cases$case, c("same", "same", "C4"))
  expect_equal(a$cutoffDisagreementCases$rowCount, 1)
  expect_equal(b$cutoffAgreementTable$asDF$n, rep(c(4, 3), each = 5))
  expect_equal(d$a, c(10, 9, 11, 2))
})

test_that("ordinal cutoff keeps the original scale estimate", {
  lv <- c("Absent", "Weak", "Strong")
  d <- data.frame(a = ordered(rep(lv, 4), lv), b = ordered(rep(c("Absent", "Strong", "Strong"), 4), lv))
  r <- pathagreement(d, vars = c("a", "b"), readerWorkflow = "ordinal_cutoff",
    ordinalCutoff = "Strong", nearCutoffLevels = "Weak\nStrong", wght = "equal")
  expect_equal(r$readerOriginalAgreement$asDF$kappa,
    irr::kappa2(data.frame(a = as.integer(d$a), b = as.integer(d$b)), weight = "equal")$value)
  expect_equal(r$cutoffAgreementTable$asDF$n, rep(c(12, 8), each = 5))
})

test_that("cluster inference is explicit and composite uses the same pair/reference mask", {
  d <- reader_fixture()
  d$b[1] <- NA
  r <- reader_run(d, readerCluster = "patient", readerBootstrap = TRUE,
    bootstrapSamples = 100, readerPairs = "selected", readerPair1 = "a", readerPair2 = "b",
    showKappaBalanced = TRUE, compositeReader = "a")
  a <- r$readerAccuracyTable$asDF
  expect_false(any(a$status == "Exact binomial"))
  expect_equal(a$units, rep(10, 6))
  expect_equal(a$attempted, rep(100, 6))
  k <- r$kappaBalancedTable$asDF
  expect_equal(k$n, 19)
  expect_equal(k$sensitivity, 8/9)
  expect_equal(k$specificity, .6)
  expect_equal(k$estimate, sqrt(k$ba * k$kappa))
  no_boot <- reader_run(d, readerCluster = "patient")$readerAccuracyTable$asDF
  expect_true(all(is.nan(no_boot$lower) | is.na(no_boot$lower)))
  expect_true(all(no_boot$status == "Cluster intervals require bootstrap"))
})

test_that("partial input and option changes clear stale rows", {
  ns <- asNamespace("ClinicoPath")
  o <- ns$pathagreementOptions$new(readerWorkflow = "numeric_cutoff", readerScores = c("a", "b"))
  a <- ns$pathagreementClass$new(data = data.frame(a = c(1, 2), b = c(2, 1)), options = o)
  a$run()
  expect_match(a$results$readerSettings$asDF$value[1], "cutoff")
  cutoff <- o$option("clinicalCutoff")
  cutoff$value <- "2"
  a$.__enclos_env__$private$.status <- "none"
  a$init(); a$run()
  expect_equal(a$results$cutoffDisagreementCases$rowCount, 2)
  cutoff$value <- "3"
  a$.__enclos_env__$private$.status <- "none"
  a$init(); a$run()
  expect_equal(a$results$cutoffDisagreementCases$rowCount, 0)
  expect_true(is.na(a$results$cutoffAgreementTable$asDF$estimate[3]))
  workflow <- o$option("readerWorkflow")
  workflow$value <- "off"
  a$.__enclos_env__$private$.status <- "none"
  a$init(); a$run()
  expect_equal(a$results$cutoffAgreementTable$rowCount, 0)
})

test_that("reader bootstrap cancellation propagates jamovi's restart signal", {
  ns <- asNamespace("ClinicoPath")
  o <- ns$pathagreementOptions$new(readerWorkflow = "binary", vars = c("a", "b"),
    readerPositive = "Yes", readerNegative = "No", readerBootstrap = TRUE,
    bootstrapSamples = 100, referenceAccuracy = FALSE)
  a <- ns$pathagreementClass$new(data = reader_fixture(), options = o)
  a$init()
  p <- a$.__enclos_env__$private
  unlockBinding(".checkpoint", p)
  p$.checkpoint <- function(...) stop(structure(list(message = "restart", call = NULL,
    code = "restart"), class = c("error", "condition")))
  result <- tryCatch(p$.readerRun(), error = identity)
  expect_identical(result$code, "restart")
})
