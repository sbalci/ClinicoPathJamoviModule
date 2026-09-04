# Regression cover for the defects found during the alluvial release review.
# The first block fails against the pre-review backend; the rest guard behaviour
# that was already correct and that the review depended on.

library(testthat)

al <- function(d, ...) {
  args <- list(data = d, condensationvar = NULL, fillGgalluvial = NULL, weight = NULL)
  do.call(alluvial, utils::modifyList(args, list(...)))
}
al_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

al_priv <- function(d, ...) {
  ns <- asNamespace("ClinicoPath")
  get("alluvialClass", ns)$new(
    options = get("alluvialOptions", ns)$new(...), data = d)$.__enclos_env__$private
}

test_that("variables dropped by the maximum-variables cap are named", {
  # plot_vars <- head(vars, maxvars) discarded the rest silently. A user who
  # selected twelve time points saw a diagram of eight with no way to tell which
  # stages were missing; the only hint was the flow-count notice, whose
  # arithmetic (3^8 = 6561) quietly reflected eight.
  set.seed(4); n <- 120
  d <- as.data.frame(setNames(
    lapply(1:12, function(i) factor(sample(c("A", "B", "C"), n, TRUE))),
    paste0("V", 1:12)))

  notices <- al_txt(al(d, vars = paste0("V", 1:12))$notices$content)

  expect_match(notices, "Variables not shown")
  expect_match(notices, "first 8 of 12")
  expect_match(notices, "V9, V10, V11, V12")
  expect_match(notices, "Maximum variables")
})

test_that("no truncation notice appears when every variable fits", {
  set.seed(4); n <- 60
  d <- as.data.frame(setNames(
    lapply(1:3, function(i) factor(sample(c("A", "B"), n, TRUE))), paste0("V", 1:3)))

  expect_false(grepl("Variables not shown",
                     al_txt(al(d, vars = paste0("V", 1:3))$notices$content)))

  # and raising the cap includes them
  d12 <- as.data.frame(setNames(
    lapply(1:10, function(i) factor(sample(c("A", "B"), n, TRUE))), paste0("V", 1:10)))
  expect_false(grepl("Variables not shown",
                     al_txt(al(d12, vars = paste0("V", 1:10), maxvars = 10)$notices$content)))
})

test_that("weighted flows sum exactly to a hand aggregation", {
  set.seed(8); n <- 60
  d <- data.frame(T1 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  T2 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  w  = sample(1:5, n, TRUE))
  d$w[1:9] <- NA

  agg <- al_priv(d, vars = c("T1", "T2"), weight = "w")$.aggregateDataForGgalluvial(
    d, c("T1", "T2"), "w")
  hand <- stats::aggregate(w ~ T1 + T2, data = d[!is.na(d$w), ], FUN = sum)

  expect_equal(nrow(agg), nrow(hand))
  key <- function(x) paste(x$T1, x$T2)
  expect_equal(agg$w[order(key(agg))], hand$w[order(key(hand))])
  expect_equal(sum(agg$w), sum(d$w, na.rm = TRUE))
})

test_that("cases with a missing weight are disclosed", {
  set.seed(8); n <- 60
  d <- data.frame(T1 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  T2 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  w  = sample(1:5, n, TRUE))
  d$w[1:9] <- NA

  notices <- al_txt(al(d, vars = c("T1", "T2"), weight = "w",
                       engine = "ggalluvial")$notices$content)
  expect_match(notices, "Missing Weights")
  expect_match(notices, "9 observations")
})

test_that("missing values become a visible stratum by default, not a silent drop", {
  # excl = FALSE is the default. All rows must survive, with NA shown as its own
  # category rather than quietly removed.
  set.seed(6); n <- 100
  d <- data.frame(T1 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  T2 = factor(sample(c("Rem", "Prog"), n, TRUE)))
  d$T2[1:12] <- NA

  kept <- al_priv(d, vars = c("T1", "T2"))$.handleMissingValues(
    d, c("T1", "T2"), exclude = FALSE)

  expect_equal(nrow(kept), n)
  expect_equal(sum(is.na(kept$T2)), 0L)
  expect_true("(Missing)" %in% levels(kept$T2))
})

test_that("excluding missing values reports the exact case loss", {
  set.seed(6); n <- 100
  d <- data.frame(T1 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  T2 = factor(sample(c("Rem", "Prog"), n, TRUE)))
  d$T2[1:12] <- NA

  # Reported once, in the "How to read this diagram" notice (the former HTML
  # "Data Validation" panel duplicated it and is gone).
  warn <- al_txt(al(d, vars = c("T1", "T2"), excl = TRUE)$notices$content)
  expect_match(warn, "12 of 100 rows had a missing value")
  expect_match(warn, "remaining 88 rows")
  expect_equal(sum(complete.cases(d)), 88L)
})

test_that("a continuous variable is refused with an actionable message", {
  # Alluvial strata must be categorical; silently binning a lab value would
  # change what the diagram means.
  set.seed(4); n <- 120
  d <- data.frame(grp = factor(sample(c("X", "Y"), n, TRUE)), lab = rnorm(n, 100, 20))

  res <- al(d, vars = c("grp", "lab"))
  warn <- al_txt(res$notices$content)
  expect_match(warn, "ERROR: Continuous Variable Not Allowed")
  expect_null(res$plot$state)
  expect_match(warn, "lab")
  expect_match(warn, "categorize")
})

test_that("the condensation plot uses tidy-eval injection", {
  # easyalluvial::plot_condensation tidy-evaluates `first`; passing a character
  # column name errors with "<varname> is not a column in df".
  src <- readLines("../../R/alluvial.b.R")
  i <- grep("plot_condensation", src)
  i <- i[length(i)]                       # the call, not the importFrom
  window <- paste(src[max(1, i - 4):(i + 4)], collapse = " ")
  expect_match(window, "rlang::inject")
  expect_match(window, "!!rlang::sym")
})

# ---- /check-function pass (2026-09-03) ---------------------------------------

test_that("variable names with spaces and punctuation reach both engines, the weight formula and the condensation panel", {
  set.seed(11); n <- 60
  d <- data.frame(
    "tumour grade"  = factor(sample(c("G1", "G2", "G3"), n, TRUE)),
    "stage/2020"    = factor(sample(c("I", "II", "III"), n, TRUE)),
    "resp (RECIST)" = factor(sample(c("CR", "PR", "SD"), n, TRUE)),
    "wt%"           = runif(n, 1, 3),
    check.names = FALSE)
  vars <- c("tumour grade", "stage/2020", "resp (RECIST)")
  ns <- asNamespace("ClinicoPath")
  obj <- function(...) {
    get("alluvialClass", ns)$new(options = get("alluvialOptions", ns)$new(...), data = d)
  }
  # The renderers catch every error and draw an explanation into the image, so a
  # plain "returned TRUE" proves nothing. Turn that fallback into an error.
  draw <- function(a, item, fn) {
    priv <- a$.__enclos_env__$private
    unlockBinding(".messagePlot", priv)
    priv$.messagePlot <- function(text) stop("render fell back to a message plot: ", text)
    lockBinding(".messagePlot", priv)
    f <- tempfile(fileext = ".png"); grDevices::png(f)
    on.exit(grDevices::dev.off(), add = TRUE)
    priv[[fn]](a$results[[item]], ggtheme = ggplot2::theme_gray(), theme = list())
  }

  a <- obj(vars = vars, condensationvar = "stage/2020", fillGgalluvial = NULL, weight = NULL)
  a$.__enclos_env__$private$.run()
  expect_false(is.null(a$results$plot$state))
  expect_false(is.null(a$results$plot2$state))
  expect_no_error(draw(a, "plot", ".plot"))
  expect_no_error(draw(a, "plot2", ".plot2"))

  # GG Alluvial: the weight goes through constructFormula()/asFormula() into
  # aggregate(), the fill and axes through rlang::sym()
  b <- obj(vars = vars, engine = "ggalluvial", weight = "wt%", fillGgalluvial = "resp (RECIST)",
           condensationvar = NULL, showCounts = TRUE, labelNodes = TRUE, colorPalette = "dark2")
  b$.__enclos_env__$private$.run()
  expect_false(is.null(b$results$plot$state))
  expect_false(grepl("ERROR", b$results$notices$content))
  expect_no_error(draw(b, "plot", ".plot"))
  agg <- b$results$plot$state$data
  expect_equal(sum(agg[["wt%"]]), sum(d[["wt%"]]))
})

test_that("the flow table lists every path with counts that match table(), commonest first", {
  set.seed(21); n <- 90
  d <- data.frame(A = factor(sample(c("a1", "a2"), n, TRUE)), B = factor(sample(c("b1", "b2", "b3"), n, TRUE)))
  res <- al(d, vars = c("A", "B"), showFlowTable = TRUE)
  ft <- res$flowTable$asDF
  ref <- as.data.frame(table(paste(d$A, d$B, sep = " \u{2192} ")), stringsAsFactors = FALSE)
  expect_equal(nrow(ft), nrow(ref))
  expect_equal(sum(ft$n), n)
  expect_equal(ft$n, sort(ref$Freq, decreasing = TRUE))
  expect_equal(ft$n[match(ref$Var1, ft$path)], ref$Freq)
  expect_equal(sum(ft$pct), 1)
  expect_match(al_txt(res$notices$content), "Commonest path: ")
  expect_match(al_txt(res$notices$content), paste0("\\(", max(ref$Freq), " of ", n, " cases"))
  # off by default: no rows are built
  expect_equal(nrow(al(d, vars = c("A", "B"))$flowTable$asDF), 0L)
})

test_that("a weighted flow table carries exact weight totals and orders by weight", {
  set.seed(22); n <- 60
  d <- data.frame(A = factor(sample(c("x", "y"), n, TRUE)), B = factor(sample(c("p", "q"), n, TRUE)),
                  w = sample(1:9, n, TRUE))
  d$w[1:5] <- NA
  res <- al(d, vars = c("A", "B"), engine = "ggalluvial", weight = "w", showFlowTable = TRUE)
  ft <- res$flowTable$asDF
  ref <- tapply(d$w, paste(d$A, d$B, sep = " \u{2192} "), sum, na.rm = TRUE)
  expect_equal(ft$w[match(names(ref), ft$path)], as.numeric(ref))
  expect_equal(ft$w, sort(as.numeric(ref), decreasing = TRUE))
  expect_equal(sum(ft$n), n - 5L)   # cases are the rows the ribbons are drawn from: no-weight rows excluded
  expect_true(res$flowTable$getColumn("w")$visible)
  # under the Easy engine the weight is ignored, so the column stays hidden
  res_easy <- al(d, vars = c("A", "B"), weight = "w", showFlowTable = TRUE)
  expect_false(res_easy$flowTable$getColumn("w")$visible)
  expect_match(al_txt(res$notices$content), "weight total")
})
