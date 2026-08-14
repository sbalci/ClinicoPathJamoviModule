# Regression cover for the defects found during the venn release review.
# Each block fails against the pre-review backend.

library(testthat)

vn <- function(d, ...) {
  args <- list(data = d, var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
               var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
               var7 = NULL, var7true = NULL)
  do.call(venn, utils::modifyList(args, list(...)))
}

test_that("variables whose names sanitise alike stay separate sets", {
  # Each block called make.names() on its own variable, so "Tumor Grade" and
  # "Tumor.Grade" both became "Tumor.Grade" and the second set silently
  # overwrote the first in the working frame. The summary table still showed
  # both original names, so two genuinely different sets - 30 positive and 5
  # positive - were both reported as 5, and the diagram drew them identically.
  d <- data.frame(check.names = FALSE,
    `Tumor Grade` = factor(rep(c("Yes", "No"), c(30, 10))),
    `Tumor.Grade` = factor(rep(c("Yes", "No"), c(5, 35))))

  df <- vn(d, var1 = "Tumor Grade", var1true = "Yes",
              var2 = "Tumor.Grade", var2true = "Yes")$summary$asDF

  expect_equal(nrow(df), 2L)
  counts <- stats::setNames(df$trueCount, as.character(df$variable))
  expect_equal(counts[["Tumor Grade"]], 30)
  expect_equal(counts[["Tumor.Grade"]], 5)
  expect_false(counts[["Tumor Grade"]] == counts[["Tumor.Grade"]])
})

test_that("selecting the same variable twice is refused", {
  # A Venn diagram of a set against itself is degenerate; it used to be accepted
  # silently and drawn as two identical circles.
  d <- data.frame(A = factor(rep(c("Yes", "No"), c(30, 10))),
                  B = factor(rep(c("Yes", "No"), c(5, 35))))
  expect_error(
    vn(d, var1 = "A", var1true = "Yes", var2 = "A", var2true = "Yes"),
    "more than once")

  # two different variables still work
  expect_equal(nrow(vn(d, var1 = "A", var1true = "Yes",
                          var2 = "B", var2true = "Yes")$summary$asDF), 2L)
})

test_that("set counts reproduce a hand-computed tabulation", {
  set.seed(11); n <- 200
  A <- rbinom(n, 1, .5); B <- rbinom(n, 1, .4); C <- rbinom(n, 1, .3)
  d <- data.frame(A = factor(ifelse(A == 1, "Pos", "Neg")),
                  B = factor(ifelse(B == 1, "Pos", "Neg")),
                  C = factor(ifelse(C == 1, "Pos", "Neg")))

  df <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              var3 = "C", var3true = "Pos")$summary$asDF

  counts <- stats::setNames(df$trueCount, as.character(df$variable))
  expect_equal(counts[["A"]], sum(A))
  expect_equal(counts[["B"]], sum(B))
  expect_equal(counts[["C"]], sum(C))
  expect_true(all(df$totalCount == n))
  expect_equal(unname(df$truePercentage), unname(df$trueCount / n))
})

test_that("the membership table is bounded and says when it truncates", {
  # jmvcore's addRow is called once per case and the cost grew quadratically:
  # 3.2s at n=200, 8.3s at n=400, 34.0s at n=800, and a 20000-row dataset did
  # not finish in ten minutes. Rendering is now capped.
  set.seed(2); n <- 900
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  tb <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable

  expect_equal(nrow(tb$asDF), 500L)
  note <- get("note", tb$.__enclos_env__$private$.notes[["truncated"]])
  expect_match(note, "first 500 of 900")
  expect_match(note, "Add membership groups to data")
})

test_that("a small membership table is not truncated and carries no note", {
  set.seed(3); n <- 40
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  tb <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable

  expect_equal(nrow(tb$asDF), n)
  nt <- tb$.__enclos_env__$private$.notes[["truncated"]]
  expect_true(is.null(nt) || is.null(get("note", nt)))
})

test_that("membership groups match a hand-computed set combination", {
  d <- data.frame(
    A = factor(c("Pos", "Pos", "Neg", "Neg")),
    B = factor(c("Pos", "Neg", "Pos", "Neg")))

  mt <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable$asDF

  expect_equal(nrow(mt), 4L)
  grp <- mt[[grep("Group", names(mt), ignore.case = TRUE)[1]]]
  expect_equal(as.character(grp), c("A & B", "A", "B", "None"))
})

test_that("enabling the membership table does not slow the rest of the analysis", {
  # The whole cost belongs to the table; everything else must stay instant.
  set.seed(2); n <- 2000
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  t0 <- Sys.time()
  vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")
  without <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(without, 5)
})

test_that("an absent true level is reported, not thrown", {
  d <- data.frame(a = factor(c("Yes", "Yes")), b = factor(c("No", "No")))
  res <- vn(d, var1 = "a", var1true = "Maybe", var2 = "b", var2true = "No")

  expect_true(res$validationErrors$visible)
  msg <- as.character(res$validationErrors$content)
  expect_match(msg, "Maybe")
  expect_match(msg, "Available levels")
  expect_equal(nrow(res$summary$asDF), 0L)
})

test_that("excluded incomplete cases are disclosed", {
  d <- data.frame(A = factor(c(rep("Pos", 8), rep("Neg", 2))),
                  B = factor(c(rep("Pos", 5), rep("Neg", 3), NA, NA)))
  res <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")

  txt <- paste(as.character(res$validationWarnings$content), collapse = " ")
  expect_match(txt, "CASE EXCLUSION")
  expect_match(txt, "Original N=10")
  expect_match(txt, "Final N=8")
  # and the reported totals reflect the complete cases only
  expect_true(all(res$summary$asDF$totalCount == 8))
})
