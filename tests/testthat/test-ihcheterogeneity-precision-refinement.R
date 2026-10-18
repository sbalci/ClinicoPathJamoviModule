ihc_precision_private <- function() {
  ihcheterogeneityClass$new(
    data = data.frame(), options = ihcheterogeneityOptions$new()
  )$.__enclos_env__$private
}

# Independent integration over the Beta density, rather than the production
# integration over uniform probability and F quantiles.
ihc_width_oracle <- function(n, k, rho) {
  v1 <- n - 1
  v2 <- v1 * (k - 1)
  scale <- (1 + (k - 1) * rho) / (1 - rho)
  stats::integrate(function(b) {
    f <- scale * (v2 / v1) * b / (1 - b)
    lower <- f / stats::qf(0.975, v1, v2)
    upper <- f * stats::qf(0.975, v2, v1)
    width <- (upper - 1) / (upper + k - 1) - (lower - 1) / (lower + k - 1)
    width * stats::dbeta(b, v1 / 2, v2 / 2)
  }, 0, 1, rel.tol = 1e-8)$value
}

test_that("two-measurement high-ICC planning meets the expected-width target", {
  private <- ihc_precision_private()
  n <- private$.iccPrecisionN(2, 0.9, 0.2)
  expect_identical(n, 21L)
  expect_gt(ihc_width_oracle(15, 2, 0.9), 0.25)
  expect_lte(ihc_width_oracle(n, 2, 0.9), 0.2)
  expect_gt(ihc_width_oracle(n - 1, 2, 0.9), 0.2)
  expect_equal(private$.iccExpectedWidth(n, 2, 0.9),
                ihc_width_oracle(n, 2, 0.9), tolerance = 1e-6)
})

test_that("precision sizes are minimal and respect the analysis case minimum", {
  private <- ihc_precision_private()
  for (k in c(2, 3, 5)) {
    for (rho in c(0.75, 0.9, 0.99)) {
      sizes <- vapply(c(0.2, 0.1), function(width) {
        n <- private$.iccPrecisionN(k, rho, width)
        expect_gte(n, 5)
        expect_lte(ihc_width_oracle(n, k, rho), width + 1e-7)
        if (n > 5) expect_gt(ihc_width_oracle(n - 1, k, rho), width)
        n
      }, integer(1))
      expect_gte(sizes[2], sizes[1])
    }
  }
})

test_that("infinite reference and regional measurements give actionable errors", {
  d <- data.frame(ref = seq(10, 100, 10), core = seq(10, 100, 10) + rep(c(-1, 1), 5))
  for (column in c("ref", "core")) {
    for (value in c(Inf, -Inf)) {
      bad <- d
      bad[[column]][2] <- value
      analysis <- ihcheterogeneityClass$new(
        data = bad, options = ihcheterogeneityOptions$new(wholesection = "ref", biopsy1 = "core")
      )
      expect_error(analysis$run(), "Infinite biomarker measurements", fixed = TRUE)
      expect_error(analysis$run(), column, fixed = TRUE)
      analysis$run(noThrow = TRUE)
      expect_identical(analysis$status, "error")
    }
  }
  bad <- d
  bad$core[2] <- Inf
  expect_error(ihcheterogeneity(bad, biopsy1 = "ref", biopsy2 = "core"),
               "Infinite biomarker measurements", fixed = TRUE)
})
