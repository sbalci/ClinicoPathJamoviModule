# Regression cover for the defects found during the benford release review.
# Each block fails against the pre-review backend.

library(testthat)

bf_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

# The narrative verdict lives in the summary table's Assessment row; the
# reportSentence panel carries only the headline plus the recommendation.
bf_assessment <- function(res) {
  df <- res$summary$asDF
  df$interpretation[df$statistic == "Assessment"]
}
bf_madrow <- function(res) {
  df <- res$summary$asDF
  df$interpretation[df$statistic == "MAD (Mean Absolute Deviation)"]
}

# Exactly-Benford data: the first digits of 10^U are Benford-distributed by
# construction, so anything the analysis flags here is a false positive.
benford_data <- function(n, seed) { set.seed(seed); 10^runif(n, 0, 4) }

# Private methods must be reached through a live instance: pulling them off
# R6ClassGenerator$private_methods gives an unbound closure with no `private`.
bf_private <- function() {
  ns <- asNamespace("ClinicoPath")
  obj <- get("benfordClass", ns)$new(
    options = get("benfordOptions", ns)$new(var = "v", digits = 2),
    data = data.frame(v = benford_data(200, 1)))
  obj$.__enclos_env__$private
}

test_that("clean Benford data is never accused of manipulation", {
  # With the default 2-digit setting, Nigrini's MAD cut-offs sit BELOW the
  # sampling-noise floor until n > 1301, so exactly-Benford data was labelled
  # "Nonconformity" in 20/20 simulated runs at n = 100, 300 and 1000 - and the
  # analysis then reported "potential manipulation" and "IMMEDIATE REVIEW
  # REQUIRED". These are precisely the sample sizes the module's own guidance
  # recommends ("100-1000 observations").
  skip_if_not_installed("benford.analysis")

  for (n in c(150, 500, 1000)) {
    res <- benford(data = data.frame(v = benford_data(n, 100 + n)),
                   var = "v", digits = 2)
    txt <- paste(bf_txt(res$reportSentence$content), bf_assessment(res))

    expect_false(grepl("manipulation", txt), info = paste("n =", n))
    expect_false(grepl("IMMEDIATE REVIEW", txt), info = paste("n =", n))
    # and the output must say why the conformity label is being set aside
    expect_match(bf_assessment(res), "sampling noise", info = paste("n =", n))
    expect_match(bf_madrow(res), "not reliable", info = paste("n =", n))
  }
})

test_that("a genuine departure from Benford's Law is still detected", {
  # Uniform 100-999 has uniform leading digits - as far from Benford as it gets.
  skip_if_not_installed("benford.analysis")

  set.seed(7)
  res <- benford(data = data.frame(v = round(runif(500, 100, 999))),
                 var = "v", digits = 2)
  expect_match(bf_txt(res$reportSentence$content), "High concern")
  expect_match(bf_assessment(res), "departure from Benford")
  # the verdict must show the basis: chi-square plus size relative to noise
  expect_match(bf_assessment(res), "Chi-square goodness-of-fit")
  expect_match(bf_assessment(res), "times the deviation expected from sampling noise")
})

test_that("the noise floor matches simulation", {
  # E|p_hat - p| = sqrt(2 p (1-p) / (pi n)) per bin, averaged over bins.
  skip_if_not_installed("benford.analysis")

  emad <- bf_private()$.expectedMadUnderNull

  for (dg in c(1, 2, 3)) {
    for (n in c(1000, 5000)) {
      set.seed(9)
      sim <- median(replicate(15, benford.analysis::benford(
        10^runif(n, 0, 4), number.of.digits = dg)$MAD))
      analytic <- emad(n, dg)
      # The per-bin normal approximation is loosest for 1-digit analysis, where
      # there are only 9 bins and p is large; 25% is comfortably tight enough to
      # catch a wrong formula while tolerating that.
      expect_lt(abs(analytic - sim) / sim, 0.25,
                label = sprintf("digits=%d n=%d analytic=%.6f sim=%.6f",
                                dg, n, analytic, sim))
    }
  }
})

test_that("the minimum n for a trustworthy MAD label is digit-specific", {
  priv <- bf_private()
  minN <- priv$.minNForMadLabel
  reliable <- priv$.madLabelIsReliable

  # cut-offs 0.015 / 0.0022 / 0.0005 against the noise floor
  expect_equal(ceiling(minN(1)), 246)
  expect_equal(ceiling(minN(2)), 1301)
  expect_equal(ceiling(minN(3)), 2550)

  # 2-digit analysis (the default) is unreliable at the sizes the module suggests
  expect_false(reliable(1000, 2))
  expect_true(reliable(2000, 2))
  # 1-digit analysis needs far fewer observations
  expect_true(reliable(500, 1))
})

test_that("the suspicious-values panel lists the flagged rows, not the first rows", {
  # suspect_indices came from as.numeric(rownames(getSuspects(...))), but
  # getSuspects returns a data.table whose rownames are reset to 1..nrow, so the
  # indices were always 1, 2, 3, ... and the panel listed the FIRST n rows of the
  # dataset under the heading "Suspicious Data Points".
  skip_if_not_installed("benford.analysis")

  x <- benford_data(2000, 11)
  res <- benford(data = data.frame(v = x), var = "v", digits = 2)
  panel <- as.character(res$text2$content)

  b <- benford.analysis::benford(x, number.of.digits = 2)
  flagged <- as.numeric(benford.analysis::getSuspects(
    bfd = b, data = data.frame(value = x))$value)
  expect_gt(length(flagged), 0)

  # The panel prints "row  value" pairs. Parse them and check the invariant that
  # was broken: the printed row number must actually hold the printed value.
  pairs <- regmatches(panel, gregexpr("(?m)^\\s*([0-9]+)\\s+([0-9.]+)\\s*$", panel, perl = TRUE))[[1]]
  expect_gt(length(pairs), 5)

  rows <- as.integer(sub("^\\s*([0-9]+).*", "\\1", pairs))
  vals <- as.numeric(sub("^\\s*[0-9]+\\s+", "", pairs))

  expect_true(all(abs(x[rows] - vals) < 1e-4))          # row really holds value
  expect_true(all(vapply(vals, function(v)              # and value really flagged
    any(abs(flagged - v) < 1e-6), logical(1))))
  expect_false(identical(rows, seq_along(rows)))        # not just 1, 2, 3, ...
})

test_that("the MAD matches a hand-computed first-two-digit deviation", {
  skip_if_not_installed("benford.analysis")

  x <- benford_data(2000, 11)
  b <- benford.analysis::benford(x, number.of.digits = 2)

  d2 <- as.numeric(substr(gsub("\\.", "", formatC(x, format = "e", digits = 10)), 1, 2))
  obs <- as.numeric(table(factor(d2, levels = 10:99))) / length(x)
  expected <- log10(1 + 1 / (10:99))

  expect_equal(b$MAD, mean(abs(obs - expected)), tolerance = 1e-9)
})

test_that("the welcome panel is hidden once a variable is selected", {
  # `visible: (!var)` never worked: jmvcore only treats a visible string as an
  # expression when it starts with "(" plus a letter, so a leading "!" was
  # returned as a raw truthy string and the (empty) "Getting Started" box stayed
  # on screen for every run.
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = benford_data(300, 1)), var = "v", digits = 2)
  expect_false(res$welcome$visible)
})
