# Regression cover for the defects found during the crosstable release review.
# Each block fails against the pre-review backend.

library(testthat)

# each style writes to its own result item
ct_item <- c(nejm = "tablestyle4", lancet = "tablestyle4", hmisc = "tablestyle4",
             arsenal = "tablestyle1", finalfit = "tablestyle2", gtsummary = "tablestyle3")

ct_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))
ct_tab <- function(res, sty) ct_txt(res[[ct_item[[sty]]]]$content)

# a sparse 2x2 where chi-square and Fisher fall on opposite sides of 0.05
sparse_2x2 <- function() {
  data.frame(grp = factor(rep(c("A", "B"), c(20, 20))),
             cat = factor(c(rep("Yes", 2), rep("No", 18),
                            rep("Yes", 8), rep("No", 12))))
}

test_that("the reference table really does separate the two tests", {
  d <- sparse_2x2()
  tab <- table(d$cat, d$grp)
  expect_lt(chisq.test(tab, correct = FALSE)$p.value, 0.05)   # ~0.0285
  expect_gt(fisher.test(tab)$p.value, 0.05)                   # ~0.0648
})

test_that("gtsummary honours an explicit Fisher request", {
  # It ignored `pcat` entirely and always used its own default, so a user asking
  # for Fisher on a sparse 2x2 got chi-square: 0.028 rather than 0.065 - which is
  # the difference between reporting a significant association and not.
  skip_if_not_installed("gtsummary")
  d <- sparse_2x2()

  chi <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                           sty = "gtsummary", pcat = "chisq"), "gtsummary")
  fis <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                           sty = "gtsummary", pcat = "fisher"), "gtsummary")

  expect_false(identical(chi, fis))
  expect_match(fis, sprintf("%.3f", round(fisher.test(table(d$cat, d$grp))$p.value, 3)))
  expect_false(grepl("0.028", fis, fixed = TRUE))
})

test_that("arsenal and finalfit still honour the test choice", {
  d <- sparse_2x2()
  for (sty in c("arsenal", "finalfit")) {
    chi <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                             sty = sty, pcat = "chisq"), sty)
    fis <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                             sty = sty, pcat = "fisher"), sty)
    expect_false(identical(chi, fis), info = sty)
  }
})

test_that("styles that ignore a setting say so", {
  # The tangram styles (NEJM was the default until 1.0.9) apply none of the statistical
  # options, and p-value adjustment exists only in gtsummary. Nothing said so, so
  # a user could set Fisher + Benjamini-Hochberg, see a table, and reasonably
  # believe both had been applied.
  d <- sparse_2x2()

  n <- crosstable(data = d, vars = "cat", group = "grp", sty = "nejm", pcat = "fisher")
  expect_true(n$dataQualityNotice$visible)
  msg <- ct_txt(n$dataQualityNotice$content)
  expect_match(msg, "NEJM style does not apply")
  expect_match(msg, "Test for categorical variables")
  expect_match(msg, "built-in tests")
})

test_that("p-value adjustment is flagged outside gtsummary", {
  set.seed(2)
  d <- data.frame(grp = factor(sample(c("A", "B"), 100, TRUE)))
  for (i in 1:6) d[[paste0("v", i)]] <- factor(sample(c("Y", "N"), 100, TRUE))
  vars <- paste0("v", 1:6)

  for (sty in c("nejm", "arsenal", "finalfit")) {
    msg <- ct_txt(crosstable(data = d, vars = !!vars, group = "grp",
                             sty = sty, p_adjust = "BH")$dataQualityNotice$content)
    expect_match(msg, "P-value adjustment", info = sty)
  }

  # gtsummary supports it, so no warning there
  g <- crosstable(data = d, vars = !!vars, group = "grp",
                  sty = "gtsummary", p_adjust = "BH")
  expect_false(grepl("does not apply", ct_txt(g$dataQualityNotice$content)))
})

test_that("no notice appears when every selected setting is honoured", {
  d <- sparse_2x2()
  # defaults: pcat = chisq, cont = mean, p_adjust = none -> nothing requested
  for (sty in c("nejm", "arsenal", "finalfit", "gtsummary")) {
    res <- crosstable(data = d, vars = "cat", group = "grp", sty = sty)
    expect_false(grepl("does not apply", ct_txt(res$dataQualityNotice$content)),
                 info = sty)
  }
  # and a setting a style DOES honour raises nothing
  expect_false(grepl("does not apply", ct_txt(
    crosstable(data = d, vars = "cat", group = "grp",
               sty = "arsenal", pcat = "fisher")$dataQualityNotice$content)))
})

test_that("arsenal p-values match the tests it claims to run", {
  d <- sparse_2x2()
  tab <- table(d$cat, d$grp)

  chi <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                           sty = "arsenal", pcat = "chisq"), "arsenal")
  fis <- ct_tab(crosstable(data = d, vars = "cat", group = "grp",
                           sty = "arsenal", pcat = "fisher"), "arsenal")

  expect_match(chi, sprintf("%.3f", round(chisq.test(tab, correct = FALSE)$p.value, 3)))
  expect_match(fis, sprintf("%.3f", round(fisher.test(tab)$p.value, 3)))
})

test_that("supplying only one of vars/group shows the welcome panel", {
  data(histopathology, package = "ClinicoPath")
  h <- as.data.frame(histopathology)

  expect_match(as.character(crosstable(data = h, vars = "Sex")$todo$content),
               "Welcome to Cross Table Analysis")
  expect_match(as.character(crosstable(data = h, group = "Group")$todo$content),
               "Welcome to Cross Table Analysis")
})

# --- Notices written to dataQualityNotice must compose, not overwrite ---------
# The per-style "honours" notice added during this review used setContent(),
# which REPLACED the data-quality warnings the same .run() had already written
# to that identical output element. The lost warnings were not cosmetic: the
# earlier block escalates for small samples, so exactly the caution a
# pathologist most needs disappeared whenever they also picked a style that
# ignores one of their statistical settings.
test_that("style-honours notice does not erase the data-quality warnings", {
    set.seed(7)
    d <- data.frame(
        grp = factor(rep(c("A", "B"), each = 6)),
        v1  = factor(c("x","x","y","y","z","z","x","y","y","z","z","z")),
        v2  = factor(c("p","q","p","q","p","q","p","p","q","q","p","q"))
    )
    # sty = "nejm" ignores pcat -> emits the honours notice;
    # n = 12 -> also emits the small-sample data-quality warning.
    res <- crosstable(data = d, vars = c("v1", "v2"), group = "grp",
                      sty = "nejm", pcat = "fisher")
    notice <- ct_txt(res$dataQualityNotice$content)

    expect_match(notice, "does not apply the following setting")   # honours
    expect_match(notice, "small sample size")                      # data quality
})

# --- release review 2026-09-05: every engine pinned to base R -----------------
ct_ref <- function() {
  set.seed(42); n <- 120
  d <- data.frame(grp2 = factor(rep(c("Ctrl", "Trt"), each = n / 2)),
                  grp3 = factor(rep(c("G1", "G2", "G3"), length.out = n)))
  d$bin  <- factor(ifelse(runif(n) < ifelse(d$grp2 == "Trt", 0.55, 0.35), "Pos", "Neg"))
  d$cat3 <- factor(sample(c("Low", "Mid", "High"), n, TRUE, c(.4, .35, .25)))
  d$age  <- rnorm(n, 60, 10) + ifelse(d$grp2 == "Trt", 3, 0)
  d
}
f3 <- function(p) sprintf("%.3f", round(p, 3))

test_that("finalfit p-values match base R: Yates on 2x2, Pearson otherwise, aov", {
  d <- ct_ref()
  t <- ct_tab(crosstable(data = d, vars = c("bin", "cat3", "age"), group = "grp2", sty = "finalfit"), "finalfit")
  expect_match(t, f3(chisq.test(table(d$bin, d$grp2), correct = TRUE)$p.value), fixed = TRUE)
  expect_match(t, f3(chisq.test(table(d$cat3, d$grp2))$p.value), fixed = TRUE)
  expect_match(t, f3(anova(lm(age ~ grp2, d))$`Pr(>F)`[1]), fixed = TRUE)
})

test_that("gtsummary p- and q-values match base R: Pearson, Wilcoxon/Kruskal-Wallis, BH", {
  d <- ct_ref()
  t <- ct_tab(crosstable(data = d, vars = c("bin", "cat3", "age"), group = "grp2",
                         sty = "gtsummary", p_adjust = "BH"), "gtsummary")
  p <- c(chisq.test(table(d$bin, d$grp2), correct = FALSE)$p.value,
         chisq.test(table(d$cat3, d$grp2))$p.value,
         wilcox.test(age ~ grp2, d)$p.value)
  for (v in c(f3(p), f3(p.adjust(p, "BH")))) expect_match(t, v, fixed = TRUE)
  t3 <- ct_tab(crosstable(data = d, vars = "age", group = "grp3", sty = "gtsummary"), "gtsummary")
  expect_match(t3, f3(kruskal.test(age ~ grp3, d)$p.value), fixed = TRUE)
})

test_that("tangram layouts print Pearson and the rank-based F test, and the summary reproduces them", {
  d <- ct_ref()
  res <- crosstable(data = d, vars = c("bin", "age"), group = "grp2", sty = "nejm", showSummary = TRUE)
  t <- ct_tab(res, "nejm")
  expect_match(t, sprintf("=%.2f", chisq.test(table(d$bin, d$grp2), correct = FALSE)$statistic), fixed = TRUE)
  a <- anova(lm(rank(age) ~ grp2, d))
  expect_match(t, sprintf("=%.2f, P=%.2f", a$`F value`[1], a$`Pr(>F)`[1]), fixed = TRUE)
  expect_match(ct_txt(res$summary$content), sprintf("age (p = %s)", f3(a$`Pr(>F)`[1])), fixed = TRUE)
})
