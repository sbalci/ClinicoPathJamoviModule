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
    d, c("T1", "T2"), exclude = FALSE, report = TRUE)

  expect_equal(nrow(kept), n)
  expect_equal(sum(is.na(kept$T2)), 0L)
  expect_true("(Missing)" %in% levels(kept$T2))
})

test_that("excluding missing values reports the exact case loss", {
  set.seed(6); n <- 100
  d <- data.frame(T1 = factor(sample(c("Rem", "Prog"), n, TRUE)),
                  T2 = factor(sample(c("Rem", "Prog"), n, TRUE)))
  d$T2[1:12] <- NA

  warn <- al_txt(al(d, vars = c("T1", "T2"), excl = TRUE)$dataWarning$content)
  expect_match(warn, "12 of 100")
  expect_match(warn, "88 complete cases")
  expect_equal(sum(complete.cases(d)), 88L)
})

test_that("a continuous variable is refused with an actionable message", {
  # Alluvial strata must be categorical; silently binning a lab value would
  # change what the diagram means.
  set.seed(4); n <- 120
  d <- data.frame(grp = factor(sample(c("X", "Y"), n, TRUE)), lab = rnorm(n, 100, 20))

  warn <- al_txt(al(d, vars = c("grp", "lab"))$dataWarning$content)
  expect_match(warn, "Continuous Variable Not Allowed")
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
