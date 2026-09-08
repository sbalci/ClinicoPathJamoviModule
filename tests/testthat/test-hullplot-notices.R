# ═══════════════════════════════════════════════════════════
# Notice panel: routing, severity, accumulation, early returns
#
# hullplot surfaces errors and data-quality findings through the `notices`
# result item, declared FIRST in hullplot.r.yaml so they render ABOVE the plot
# they qualify. They used to live inside the interpretation guide, which sits
# below the figure and below two optional tables.
# ═══════════════════════════════════════════════════════════

library(testthat)

hullplot_notice_data <- function(n = 90) {
  set.seed(42)
  data.frame(
    Biomarker = rnorm(n, 20, 5),
    Ki67      = rnorm(n, 30, 10),
    Grade     = factor(rep(c("G1", "G2", "G3"), length.out = n)),
    Age       = runif(n, 30, 80)
  )
}

test_that("a clean run produces no notices", {
  res <- hullplot(
    data = hullplot_notice_data(), x_var = "Biomarker", y_var = "Ki67",
    group_var = "Grade")
  expect_equal(res$notices$content, "")
})

test_that("excluded rows are reported as a notice, not buried in the guide", {
  d <- hullplot_notice_data()
  d$Ki67[1:20] <- NA
  d$Biomarker[21] <- Inf
  res <- hullplot(
    data = d, x_var = "Biomarker", y_var = "Ki67", group_var = "Grade")

  expect_match(res$notices$content, "Rows excluded from the plot")
  expect_match(res$notices$content, "were excluded because a selected variable was missing")
  expect_match(res$notices$content, "infinite/undefined value")
  # and no longer duplicated into the interpretation guide
  expect_false(grepl("were excluded because", res$interpretation$content, fixed = TRUE))
})

test_that("a high-cardinality colour variable is flagged", {
  # color_var is cast to a factor and drawn with a discrete palette, so a
  # continuous variable yields one legend key per distinct value.
  res <- hullplot(
    data = hullplot_notice_data(), x_var = "Biomarker", y_var = "Ki67",
    group_var = "Grade", color_var = "Age")
  expect_match(res$notices$content, "Colour variable has many distinct values")
})

test_that("groups too small for a hull are flagged", {
  d <- hullplot_notice_data()
  d$Grade <- as.character(d$Grade)
  d$Grade[1:2] <- "tiny"          # a 2-point group cannot support a hull
  d$Grade <- factor(d$Grade)
  expect_lt(min(table(d$Grade)), 3)
  res <- hullplot(
    data = d, x_var = "Biomarker", y_var = "Ki67", group_var = "Grade")
  expect_match(res$notices$content, "Groups too small for a meaningful hull")
})

test_that("an empty dataset yields an ERROR notice, not a silent blank pane", {
  res <- hullplot(
    data = hullplot_notice_data()[0, ], x_var = "Biomarker", y_var = "Ki67",
    group_var = "Grade")
  expect_match(res$notices$content, "No data to plot")
  # `todo` carries onboarding only
  expect_equal(res$todo$content, "")
})

test_that("notices render on every early return", {
  # .run() has three early returns; .renderNotices() is wired through on.exit()
  # rather than as a trailing statement, which used to drop them silently.
  res <- hullplot(
    data = hullplot_notice_data()[0, ], x_var = "Biomarker", y_var = "Ki67",
    group_var = "Grade")
  expect_gt(nchar(res$notices$content), 0)

  # the no-variables-selected path shows the welcome panel and no notices
  res2 <- hullplot(
    data = hullplot_notice_data(), x_var = "Biomarker", y_var = "Ki67")
  expect_match(res2$todo$content, "Welcome to Hull Plot")
  expect_equal(res2$notices$content, "")
})

test_that("notices do not accumulate across repeated runs", {
  # .addNotice() appends, so .run() must reset the list each cycle.
  d <- hullplot_notice_data()
  d$Ki67[1:20] <- NA
  # hullplotOptions/hullplotClass are namespace-internal (only `hullplot` is
  # exported), so a bare name or a `::` prefix would fail against an installed
  # build and silently take the whole test with it.
  hullplotOptions <- getFromNamespace("hullplotOptions", "ClinicoPath")
  hullplotClass   <- getFromNamespace("hullplotClass", "ClinicoPath")
  opts <- hullplotOptions$new(
    x_var = "Biomarker", y_var = "Ki67", group_var = "Grade")
  analysis <- hullplotClass$new(options = opts, data = d)

  analysis$run()
  once <- lengths(gregexpr("border-left", analysis$results$notices$content))
  analysis$run(); analysis$run()
  thrice <- lengths(gregexpr("border-left", analysis$results$notices$content))

  expect_equal(once, thrice)
})

test_that("the notice panel is readable in both jamovi themes", {
  d <- hullplot_notice_data()
  d$Ki67[1:20] <- NA
  res <- hullplot(
    data = d, x_var = "Biomarker", y_var = "Ki67", group_var = "Grade")
  html <- res$notices$content

  # translucent tints only; an opaque hex fill with no foreground is unreadable
  # against jamovi's dark theme
  expect_false(grepl("background-color: #", html, fixed = TRUE))
  expect_match(html, "color: inherit")
  # only the five structural HTML entities are safe
  entities <- unique(unlist(regmatches(html, gregexpr("&[a-zA-Z]+;", html))))
  expect_true(all(entities %in% c("&lt;", "&gt;", "&amp;", "&quot;", "&apos;")))
})

test_that("hull_concavity is bounded to the range concaveman honours", {
  d <- hullplot_notice_data()
  # below 1 concaveman clamps, so 0 and 0.5 drew byte-identical hulls
  expect_error(
    hullplot(data = d, x_var = "Biomarker", y_var = "Ki67",
                          group_var = "Grade", hull_concavity = 0.5),
    regexp = "between", ignore.case = TRUE)
  # the old max of 2 was ggforce's DEFAULT, not a ceiling; 10 is a documented
  # upstream example value and must now be reachable
  wide_concavity <- hullplot(
    data = d, x_var = "Biomarker", y_var = "Ki67", group_var = "Grade",
    hull_concavity = 10)
  expect_true(inherits(wide_concavity, "hullplotResults"))
})
