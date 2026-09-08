# ═══════════════════════════════════════════════════════════
# Separation heuristic, outlier-rule scope, and small-group reporting
#
# Regression cover for the /review-function findings of 2026-09-07:
# the separation descriptor was the mean of ALL pairwise centroid distances,
# so one distant group could mask a fully overlapping pair and the copy-ready
# manuscript sentence called it "well-separated".
# ═══════════════════════════════════════════════════════════

library(testthat)

strip_html <- function(x) gsub("[ ]+", " ", gsub("<[^>]*>", " ", x))

test_that("a fully overlapping pair is not called well-separated", {
  # Responder and Non-responder are drawn from the SAME distribution; Reference
  # sits 30 SD away. Under mean-of-all-pairs the index was 20.5 (verdict
  # "well-separated") while the closest pair was 0.10.
  set.seed(9)
  n <- 60
  d <- data.frame(
    x = c(rnorm(n), rnorm(n), rnorm(n) + 30),
    y = c(rnorm(n), rnorm(n), rnorm(n)),
    g = factor(rep(c("Responder", "Non-responder", "Reference"), each = n)))

  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  show_summary = TRUE)
  txt <- strip_html(res$summary$content)

  expect_match(txt, "described as overlapping")
  expect_false(grepl("described as well-separated", txt, fixed = TRUE))
  # and the heuristic must describe itself honestly
  expect_match(txt, "smallest centroid distance between any two groups")
  expect_match(txt, "worst-separated pair, not the average pair")
})

test_that("the separation descriptor does not drift with the number of groups", {
  # Adjacent groups are 4 SD apart in every case. The old mean-of-all-pairs
  # index climbed 4.47 -> 9.70 across k = 2..6 purely from group count.
  verdict <- function(k) {
    set.seed(1)
    m <- 40 * k
    d <- data.frame(
      x = rnorm(m) + rep(seq_len(k) * 4, each = 40),
      y = rnorm(m),
      g = factor(rep(paste0("G", seq_len(k)), each = 40)))
    res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                    show_summary = TRUE)
    grepl("described as well-separated", strip_html(res$summary$content), fixed = TRUE)
  }
  # same adjacent gap -> same verdict at every k
  expect_true(verdict(2))
  expect_true(verdict(4))
  expect_true(verdict(6))
})

test_that("two groups are unaffected: min and mean coincide at k = 2", {
  set.seed(4)
  n <- 50
  d <- data.frame(x = c(rnorm(n), rnorm(n) + 6), y = rnorm(2 * n),
                  g = factor(rep(c("A", "B"), each = n)))
  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  show_summary = TRUE)
  expect_match(strip_html(res$summary$content), "described as well-separated")
})

test_that("the outlier panel states that the rule is per-axis, not bivariate", {
  # A point at marginal z = (1.19, -1.23) on a perfectly correlated cloud is
  # grossly off the structure yet passes both marginal IQR tests. The panel must
  # not let a reader assume the rule is bivariate.
  set.seed(3)
  n <- 200
  x <- rnorm(n); y <- x
  x[1] <- 1.2; y[1] <- -1.2
  res <- hullplot(data = data.frame(x = x, y = y, g = factor(rep("A", n))),
                  x_var = "x", y_var = "y", group_var = "g",
                  outlier_detection = TRUE)
  txt <- strip_html(res$outliers$content)

  expect_match(txt, "each axis separately")
  expect_match(txt, "does not test the X-Y combination")
  # the documented limitation is real: this point is genuinely not flagged
  expect_match(txt, "A: 0 potential outliers")
})

test_that("a group too small for an ellipse is explained, not left to ggplot2", {
  # stat_ellipse() needs >= 4 points; below that it emitted "Too few points to
  # calculate an ellipse" plus a "Removed 1 row" warning into jamovi's
  # Analysis Notes as unexplained package chatter.
  set.seed(5)
  d <- data.frame(x = c(rnorm(30), 1, 2), y = c(rnorm(30), 1, 2),
                  g = factor(c(rep("A", 30), "B", "B")))
  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  confidence_ellipses = TRUE)

  expect_match(res$notices$content, "Some groups have no data ellipse")
  expect_match(strip_html(res$notices$content), "no ellipse is drawn for: B")
  expect_match(strip_html(res$notices$content), "at least 4 points")
})

test_that("no ellipse notice when every group is large enough", {
  set.seed(6)
  d <- data.frame(x = rnorm(60), y = rnorm(60),
                  g = factor(rep(c("A", "B"), each = 30)))
  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  confidence_ellipses = TRUE)
  expect_false(grepl("no data ellipse", res$notices$content, fixed = TRUE))
})

test_that("a one-point group reports that SD is not estimable", {
  set.seed(7)
  d <- data.frame(x = c(rnorm(30), 5), y = c(rnorm(30), 5),
                  g = factor(c(rep("A", 30), "B")))
  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  show_statistics = TRUE)
  txt <- strip_html(res$statistics$content)

  expect_match(txt, "SD not estimable, n = 1")
  expect_false(grepl("± NA", txt, fixed = TRUE))
})

test_that("emitted HTML contains no bare ampersand", {
  set.seed(8)
  d <- data.frame(x = rnorm(40), y = rnorm(40),
                  g = factor(rep(c("A", "B"), each = 20)))
  res <- hullplot(data = d, x_var = "x", y_var = "y", group_var = "g",
                  show_assumptions = TRUE, show_statistics = TRUE,
                  show_summary = TRUE, outlier_detection = TRUE)

  for (panel in c("assumptions", "statistics", "summary", "outliers", "interpretation")) {
    html <- res[[panel]]$content
    bare <- regmatches(html, gregexpr("&(?![a-zA-Z]{2,12};)", html, perl = TRUE))[[1]]
    expect_length(bare, 0)
  }
})
