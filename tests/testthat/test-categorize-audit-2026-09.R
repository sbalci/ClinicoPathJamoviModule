# Regression cover for the 2026-09 audit fixes in categorize.

library(testthat)

ca_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

test_that("median split names the tied extreme instead of 'Insufficient break points'", {
  skip_if_not_installed("jmvReadWrite")
  # zero-inflated count: 70% at the minimum
  d <- data.frame(nodes = c(rep(0, 70), 1:30))
  r <- categorize(data = d, var = "nodes", method = "median")
  n <- ca_txt(r$notices$content)
  expect_match(n, "Median split is not possible")
  expect_match(n, "equals the minimum")
  expect_match(n, "70% of the observations are tied")
  expect_false(grepl("Insufficient break points", n))
  expect_equal(r$freqTable$rowCount, 0)

  # ceiling effect: 60% at the maximum
  d <- data.frame(score = c(1:40, rep(100, 60)))
  r <- categorize(data = d, var = "score", method = "median")
  expect_match(ca_txt(r$notices$content), "equals the maximum")

  # an ordinary median split is untouched
  r <- categorize(data = data.frame(x = 1:100), var = "x", method = "median")
  expect_equal(r$freqTable$asDF$n[1:2], c(50, 50))
})

test_that("a decimal-comma manual entry is echoed back as the break points it parsed to", {
  skip_if_not_installed("jmvReadWrite")
  d <- data.frame(x = seq(0, 10, by = 0.5))
  r <- categorize(data = d, var = "x", method = "manual", breaks = "2,5")
  n <- ca_txt(r$notices$content)
  expect_match(n, "were read as 2, 5")
  expect_match(n, "decimal separator")
  # ... and they really were used as two break points (min, 2, 5, max)
  expect_equal(r$breakpointsTable$asDF$value, c(0, 2, 5, 10))

  # the documented "0, 25, 50" style gets no such note, nor does "2.5"
  r <- categorize(data = d, var = "x", method = "manual", breaks = "2, 5")
  expect_false(grepl("were read as", r$notices$content))
  r <- categorize(data = d, var = "x", method = "manual", breaks = "2.5")
  expect_false(grepl("were read as", r$notices$content))
  expect_equal(r$breakpointsTable$asDF$value, c(0, 2.5, 10))

  # the unparsable-entry error carries the same hint
  r <- categorize(data = d, var = "x", method = "manual", breaks = "2,5 7,5")
  expect_match(ca_txt(r$notices$content), "Invalid manual break points.*decimal separator")
})

test_that("plot state is set only after every validation check has passed", {
  skip_if_not_installed("jmvReadWrite")
  d <- data.frame(x = c(0, 0.25, 0.5, 0.75, 1))
  # near-coincident break points are rejected by .validateBreaks()
  r <- categorize(data = d, var = "x", method = "manual", breaks = "0, 1e-12, 1")
  expect_match(ca_txt(r$notices$content), "too close together")
  expect_null(r$plot$state)
  expect_equal(r$freqTable$rowCount, 0)

  # a valid run stores the breaks the tables use
  r <- categorize(data = d, var = "x", method = "manual", breaks = "0.5")
  expect_equal(r$plot$state$breaks, c(0, 0.5, 1))
  expect_equal(r$plot$state$varname, "x")
  expect_equal(r$plot$state$method, "manual")
})

test_that("the out-of-range and mean+/-SD notices survive the translator's bracket rule", {
  skip_if_not_installed("jmvReadWrite")
  # jmvcore's translate() drops everything from a " [" onwards, so the text
  # after the break-point range must still be present in the rendered notice.
  d <- data.frame(x = 1:100)
  r <- categorize(data = d, var = "x", method = "manual", breaks = "20, 80",
                  excludeoutofrange = TRUE)
  n <- ca_txt(r$notices$content)
  expect_match(n, "outside the break points \\(20 to 80\\): 19 below and 20 above")

  # mean 6.1, sd 21: mean - 1 SD falls below the minimum, mean + 1 SD stays inside
  r <- categorize(data = data.frame(x = c(rep(0, 20), 1:5, 50, 100)), var = "x", method = "meansd")
  n <- ca_txt(r$notices$content)
  expect_match(n, "the lower boundary \\(mean - 1 SD\\) lies beyond the observed range")
  expect_false(grepl("\\blower and upper\\b", n))
})

test_that("notice text shows the method title, never the option key", {
  skip_if_not_installed("jmvReadWrite")
  r <- categorize(data = data.frame(x = 1:100), var = "x", method = "meansd")
  n <- ca_txt(r$notices$content)
  expect_match(n, "using the Mean \\+/- SD method")
  expect_false(grepl("using the meansd method", n))
})

test_that("the notices item clears on every option it reports on", {
  root <- if (file.exists("../../jamovi/categorize.r.yaml")) "../.." else "."
  items <- yaml::read_yaml(file.path(root, "jamovi/categorize.r.yaml"))$items
  notices <- Filter(function(i) i$name == "notices", items)[[1]]
  expect_true(all(c("newvarname", "addtodata", "ordered") %in% notices$clearWith))
})
