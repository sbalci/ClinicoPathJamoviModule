# Regression cover for the defect found during the categorize release review.

library(testthat)

ca <- function(d, ...) categorize(data = d, ...)
ca_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

test_that("manual break points are never overwritten", {
  # `breaks[1] <- min(x)` and `breaks[length] <- max(x)` replaced the user's own
  # outermost cut-points instead of extending the range. Measured on eGFR data
  # spanning 12.3-109.2 with the standard CKD cut-points "30,60,90": the breaks
  # used were 12.26, 60, 109.21 - the 30 and 90 thresholds deleted, four CKD
  # stages collapsed into two bins, and the 30 patients below 30 (stage 4/5)
  # merged with stage 3, with nothing said.
  set.seed(3)
  egfr <- c(runif(30, 12, 29), runif(60, 31, 59), runif(60, 61, 89), runif(30, 91, 110))
  d <- data.frame(eGFR = egfr)

  res <- ca(d, var = "eGFR", method = "manual", breaks = "30,60,90")
  brks <- res$breakpointsTable$asDF$value

  # every requested threshold survives
  for (b in c(30, 60, 90)) expect_true(any(abs(brks - b) < 1e-8), info = b)
  # and the data range is still covered
  expect_lte(min(brks), min(egfr))
  expect_gte(max(brks), max(egfr))
  expect_equal(length(brks), 5L)

  # four bins, matching a hand tabulation
  freq <- res$freqTable$asDF
  expect_equal(nrow(freq), 4L)
  expect_equal(freq$n, c(sum(egfr < 30), sum(egfr >= 30 & egfr < 60),
                         sum(egfr >= 60 & egfr < 90), sum(egfr >= 90)))
})

test_that("manual breaks inside the data range are all kept", {
  set.seed(5); y <- rnorm(200, 50, 12)
  brks <- ca(data.frame(v = y), var = "v", method = "manual",
             breaks = "20,40,60,80")$breakpointsTable$asDF$value
  for (b in c(20, 40, 60, 80)) expect_true(any(abs(brks - b) < 1e-8), info = b)
  expect_equal(length(brks), 6L)   # 4 requested + min + max
})

test_that("manual breaks already spanning the data are left exactly alone", {
  y <- c(5, 25, 45, 65, 85)
  brks <- ca(data.frame(v = y), var = "v", method = "manual",
             breaks = "0,30,60,100")$breakpointsTable$asDF$value
  expect_equal(brks, c(0, 30, 60, 100))
})

test_that("computed methods reproduce their published definitions", {
  set.seed(5); y <- rnorm(200, 50, 12); d <- data.frame(v = y)
  brk <- function(m, ...) ca(d, var = "v", method = m, ...)$breakpointsTable$asDF$value

  expect_equal(brk("equal", nbins = 4),
               seq(min(y), max(y), length.out = 5), tolerance = 1e-3)
  expect_equal(brk("quantile", nbins = 4),
               unname(unique(quantile(y, seq(0, 1, length.out = 5)))), tolerance = 1e-3)
  expect_equal(brk("median"), c(min(y), median(y), max(y)), tolerance = 1e-3)
  expect_equal(brk("meansd", sdmult = 1),
               c(min(y), mean(y) - sd(y), mean(y), mean(y) + sd(y), max(y)), tolerance = 1e-3)
  skip_if_not_installed("classInt")
  # The natural-breaks method now asks classInt for style = "fisher", the exact
  # Fisher-Jenks dynamic-programming optimum on the full data, instead of
  # style = "jenks", which is the Jenks-Caspall approximation AND draws an
  # unseeded random 10% subsample above largeN, so it returned different
  # cut-points on consecutive runs of the same data. Measured here they differ
  # by up to 0.25 (38.90 vs 39.13), well outside the tolerance - so this
  # assertion tracks the definition the analysis actually implements.
  expect_equal(brk("jenks", nbins = 4),
               classInt::classIntervals(y, n = 4, style = "fisher",
                                        largeN = Inf)$brks, tolerance = 1e-3)
})

test_that("interval closure options are honoured", {
  d <- data.frame(v = c(0, 5, 10))
  rc <- ca(d, var = "v", method = "manual", breaks = "0,5,10",
           rightclosed = TRUE, includelowest = TRUE)$freqTable$asDF
  lc <- ca(d, var = "v", method = "manual", breaks = "0,5,10",
           rightclosed = FALSE, includelowest = TRUE)$freqTable$asDF

  expect_equal(rc$n, c(2, 1))    # (0,5] holds 0 and 5
  expect_equal(lc$n, c(1, 2))    # [0,5) holds 0 only
  expect_false(identical(rc$range, lc$range))
})

test_that("collapsed quantile bins are reported, not silently accepted", {
  # 60% ties make three of the five quartile breaks identical
  x <- c(rep(0, 60), 1:40)
  res <- ca(data.frame(lab = x), var = "lab", method = "quantile", nbins = 4)
  msg <- ca_txt(res$notices$content)

  expect_match(msg, "Bin collapse")
  expect_match(msg, "requested 4 categories but only 2")
  expect_equal(nrow(res$freqTable$asDF), 2L)
})

test_that("a constant variable is refused with a clear message", {
  res <- ca(data.frame(v = rep(50, 10)), var = "v", method = "equal", nbins = 3)
  msg <- ca_txt(res$notices$content)
  expect_match(msg, "zero variability")
  expect_equal(nrow(res$freqTable$asDF), 0L)
})

# ---------------------------------------------------------------------------
# Out-of-range exclusion.
#
# With break points preserved (the fix above), manual breaks that sit inside the
# data range produce extra bins below the first and above the last threshold.
# That is right for CKD staging but wrong for a user who wants those values
# dropped, so the choice is now explicit. The wrapper cannot accept the new
# option until jmvtools::prepare() regenerates it, so the behaviour is exercised
# through the private path and the declaration is checked in the yaml.
# ---------------------------------------------------------------------------

ca_priv <- function(d, ...) {
  ns <- asNamespace("ClinicoPath")
  get("categorizeClass", ns)$new(
    options = get("categorizeOptions", ns)$new(...), data = d)$.__enclos_env__$private
}

test_that("excludeoutofrange is declared and defaults to the existing behaviour", {
  opts <- yaml::read_yaml("../../jamovi/categorize.a.yaml")$options
  o <- Filter(function(x) identical(x$name, "excludeoutofrange"), opts)
  expect_length(o, 1)
  expect_equal(o[[1]]$type, "Bool")
  expect_false(o[[1]]$default)     # off = extend = previous behaviour

  ui <- readLines("../../jamovi/categorize.u.yaml")
  i <- grep("name: excludeoutofrange", ui)
  expect_length(i, 1)
  # only meaningful for manual breaks, so the control is gated
  expect_match(paste(ui[i:(i + 1)], collapse = " "), "method:manual")
})

# The private .optionOr() shim this block used to exercise existed only while
# the option was declared in the yaml but not yet compiled into the .h.R.
# jmvtools::prepare() has since generated it (R/categorize.h.R declares
# excludeoutofrange = FALSE), so .run() reads self$options directly and the
# shim is gone. What still needs covering is that the compiled option exists
# and keeps the previous behaviour as its default.
test_that("the compiled option exists and defaults to the previous behaviour", {
  ns <- asNamespace("ClinicoPath")
  opts <- get("categorizeOptions", ns)$new(var = "v")
  expect_false(opts$excludeoutofrange)
})

test_that("excluding keeps the break points exactly as entered", {
  set.seed(3)
  egfr <- c(runif(30, 12, 29), runif(60, 31, 59), runif(60, 61, 89), runif(30, 91, 110))
  priv <- ca_priv(data.frame(eGFR = egfr), var = "eGFR")

  kept <- priv$.calculateBreaks(egfr, "manual", 4, "30,60,90", 1, extend_to_data = FALSE)
  expect_equal(kept, c(30, 60, 90))

  xc <- cut(egfr, breaks = kept, include.lowest = TRUE, right = TRUE)
  expect_equal(nlevels(xc), 2L)
  expect_equal(sum(is.na(xc)), sum(egfr < 30) + sum(egfr > 90))
  expect_equal(sum(!is.na(xc)), sum(egfr >= 30 & egfr <= 90))
})

test_that("extending remains the default and loses no cases", {
  set.seed(3)
  egfr <- c(runif(30, 12, 29), runif(60, 31, 59), runif(60, 61, 89), runif(30, 91, 110))
  d <- data.frame(eGFR = egfr)

  res <- ca(d, var = "eGFR", method = "manual", breaks = "30,60,90")
  freq <- res$freqTable$asDF
  expect_equal(nrow(freq), 4L)
  expect_equal(sum(freq$n), nrow(d))          # nothing dropped by default
})

test_that("the switch is ignored for computed methods", {
  # Their break points are built from min(x)/max(x), so nothing can fall outside.
  set.seed(5); y <- rnorm(100, 50, 12)
  priv <- ca_priv(data.frame(v = y), var = "v")
  for (m in c("equal", "quantile", "median", "meansd")) {
    b <- priv$.calculateBreaks(y, m, 4, "", 1, extend_to_data = FALSE)
    expect_lte(min(b), min(y), label = m)
    expect_gte(max(b), max(y), label = m)
  }
})
