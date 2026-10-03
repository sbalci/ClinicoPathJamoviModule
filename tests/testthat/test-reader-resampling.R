test_that("resampling is deterministic, preserves RNG and keeps clusters whole", {
  withr::local_seed(21)
  before <- .Random.seed
  d <- data.frame(x = c(0, 1, 1, 0, 0, 1), id = rep(letters[1:3], each = 2))
  stat <- function(z) {
    expect_true(all(table(z$id) %% 2L == 0L))
    c(mean = mean(z$x))
  }
  a <- .readerResample(d, stat, clusters = d$id, samples = 20, seed = 8)
  expect_identical(.Random.seed, before)
  b <- .readerResample(d, stat, clusters = d$id, samples = 20, seed = 8)
  expect_equal(a, b)
  expect_true(a$constant)
  expect_true(is.na(a$lower))
})

test_that("undefined draws are counted and never silently filtered", {
  d <- data.frame(x = c(1, rep(0, 19)))
  r <- .readerResample(d, function(z) c(rate = if (sum(z$x) == 0) NA_real_ else mean(z$x)),
                       samples = 100, seed = 81)
  expect_gt(r$undefined, 0)
  expect_equal(r$valid + r$undefined, r$attempted)
  expect_true(is.na(r$lower))
  expect_match(r$status, "Undefined")
})

test_that("confidence level affects percentile intervals and negative kappa is valid", {
  d <- data.frame(x = seq_len(30))
  stat <- function(z) c(mean = mean(z$x), kappa = -mean(z$x) / 30)
  a <- .readerResample(d, stat, samples = 100, confidence = .8, seed = 19)
  b <- .readerResample(d, stat, samples = 100, confidence = .95, seed = 19)
  expect_true(all(a$lower >= b$lower))
  expect_true(all(a$upper <= b$upper))
  expect_true(all(a$undefined == 0))
  expect_true(all(is.finite(a$lower)))
})

test_that("unused factor cluster levels are not sampling units", {
  d <- data.frame(x = 1:4)
  ids <- factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused"))
  r <- .readerResample(d, function(z) c(n = nrow(z)), clusters = ids, samples = 20)
  expect_true(r$constant)
  expect_equal(r$estimate, 4)
})
