# Runtime regression for the tables whose row STRUCTURE was moved from .run()
# into .init() (jamovi library audit, 2026-08-17).
#
# Why this file exists, and why it must actually RUN the analyses:
#
#   Moving rows to .init() means .run() switches from addRow() to setRow(), and
#   jmvcore's Table$setRow() matches rowKeys with identical() -- it is strict
#   about type, and about ATTRIBUTES. A rowKey of c(num = "num") is not
#   identical to "num", so setRow() aborts the whole analysis with
#   "rowKey 'num' not found".
#
#   That is invisible to a parse check, to a static grep, and to every
#   schema-consistency test in this repo. It shipped once already: crosstable's
#   SMD table crashed on the happy path because .crosstableDisplayName() was
#   silently shadowed by an identically-named helper in R/survival.b.R that
#   returned a NAMED character. Only running the analysis end to end catches it.
#
# Keep every analysis whose rows live in .init() represented here.

skip_if_not_installed("jmvcore")

make_data <- function(n = 60, special_names = FALSE) {
  set.seed(1)
  d <- data.frame(
    grp  = factor(rep(c("A", "B"), each = n / 2)),
    num  = rnorm(n, 50, 10),
    num2 = rnorm(n, 5, 1),
    cat  = factor(sample(c("x", "y"), n, TRUE)),
    check.names = FALSE
  )
  if (special_names)
    names(d) <- c("grp", "Age (yrs)", "Tumor-Size", "Path Cat")
  d
}

analysis_available <- function(fn) exists(fn, mode = "function")


test_that("crosstable SMD table fills its .init() rows without a rowKey error", {
  skip_if_not(analysis_available("crosstable"), "package not loaded")

  d <- make_data()
  res <- crosstable(data = d, vars = c("num", "cat"), group = "grp",
                    sty = "nejm", showSMD = TRUE)
  expect_equal(res$smdTable$rowCount, 2L)

  # special characters in variable names exercise the cleaned<->original mapping
  # that the rowKeys depend on
  d2 <- make_data(special_names = TRUE)
  res2 <- crosstable(data = d2, vars = c("Age (yrs)", "Path Cat"), group = "grp",
                     sty = "nejm", showSMD = TRUE)
  expect_equal(res2$smdTable$rowCount, 2L)

  # a grouping variable with != 2 levels takes the early-return branch; the
  # pre-built rows must simply stay empty rather than throwing
  d3 <- make_data()
  d3$grp <- factor(sample(c("A", "B", "C"), nrow(d3), TRUE))
  expect_no_error(crosstable(data = d3, vars = "num", group = "grp",
                             sty = "nejm", showSMD = TRUE))

  # showSMD off must not build or fill anything
  expect_no_error(crosstable(data = d, vars = c("num", "cat"), group = "grp",
                             sty = "nejm", showSMD = FALSE))
})


test_that("checkdata fills both fixed row sets for numeric and categorical vars", {
  skip_if_not(analysis_available("checkdata"), "package not loaded")

  d <- make_data()

  # showDistribution defaults to FALSE, so the distribution rows must be built
  # only when it is switched on -- .init() gates on the option for that reason
  off <- checkdata(data = d, var = "num")
  expect_equal(off$missingVals$rowCount, 4L)
  expect_equal(off$distribution$rowCount, 0L)

  num <- checkdata(data = d, var = "num", showDistribution = TRUE)
  expect_equal(num$missingVals$rowCount, 4L)
  expect_gt(num$distribution$rowCount, 0L)

  # the categorical branch uses the OTHER fixed row set
  cat_res <- checkdata(data = d, var = "cat", showDistribution = TRUE)
  expect_equal(cat_res$missingVals$rowCount, 4L)
  expect_gt(cat_res$distribution$rowCount, 0L)
})


test_that("benford fills its six .init() summary rows", {
  skip_if_not(analysis_available("benford"), "package not loaded")

  res <- benford(data = make_data(), var = "num")
  expect_equal(res$summary$rowCount, 6L)
})


test_that("venn builds one summary row per selected variable", {
  skip_if_not(analysis_available("venn"), "package not loaded")

  d <- make_data()
  d$cat3 <- factor(sample(c("p", "q"), nrow(d), TRUE))

  # every `type: Level` option is a REQUIRED argument of the generated wrapper
  # (the compiler forbids `default:` on Level), so all of them must be passed
  two <- venn(data = d, var1 = "cat", var1true = "x", var2 = "grp", var2true = "A",
              var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
              var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
              var7 = NULL, var7true = NULL)
  expect_equal(two$summary$rowCount, 2L)

  three <- venn(data = d, var1 = "cat", var1true = "x", var2 = "grp", var2true = "A",
                var3 = "cat3", var3true = "p", var4 = NULL, var4true = NULL,
                var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
                var7 = NULL, var7true = NULL)
  expect_equal(three$summary$rowCount, 3L)
})


# --- tables moved into .init() in the MEDIUM-findings pass (2026-08-20) --------
# Same class as the four above: .run() now uses setRow(), which aborts the whole
# analysis if .init() did not create the key. Only running the analysis catches it.

test_that("diagnosticmeta bivariateresults keeps its five .init() rows", {
  skip_if_not(analysis_available("diagnosticmeta"), "package not loaded")

  dm <- data.frame(
    study = paste0("S", 1:8),
    tp = c(45, 38, 52, 41, 60, 33, 47, 55), fp = c(8, 12, 6, 10, 5, 14, 9, 7),
    fn = c(7, 9, 5, 8, 4, 11, 6, 5), tn = c(90, 85, 95, 88, 100, 80, 92, 97),
    stringsAsFactors = FALSE)

  res <- diagnosticmeta(data = dm, study = "study", true_positives = "tp",
                        false_positives = "fp", false_negatives = "fn",
                        true_negatives = "tn")
  expect_equal(res$bivariateresults$rowCount, 5L)

  # fewer than three studies is fatal and must reach jamovi as an error, not as
  # red HTML in the onboarding panel
  expect_error(
    diagnosticmeta(data = dm[1:2, ], study = "study", true_positives = "tp",
                   false_positives = "fp", false_negatives = "fn",
                   true_negatives = "tn"),
    "At least 3 studies")
})


test_that("ihcheterogeneity variancetable keeps its four .init() rows", {
  skip_if_not(analysis_available("ihcheterogeneity"), "package not loaded")

  set.seed(11)
  ihc <- data.frame(whole = rnorm(30, 50, 12), b1 = rnorm(30, 50, 12),
                    b2 = rnorm(30, 50, 12), b3 = rnorm(30, 50, 12))

  with_ref <- ihcheterogeneity(data = ihc, wholesection = "whole", biopsy1 = "b1",
                               biopsy2 = "b2", biopsy3 = "b3",
                               variance_components = TRUE)
  expect_equal(with_ref$variancetable$rowCount, 4L)

  # the two has_reference-dependent labels are an OPTION-shape branch, so .init()
  # must build four rows on that path too
  no_ref <- ihcheterogeneity(data = ihc, wholesection = NULL, biopsy1 = "b1",
                             biopsy2 = "b2", biopsy3 = "b3",
                             variance_components = TRUE)
  expect_equal(no_ref$variancetable$rowCount, 4L)

  # option off: rows still present, just unfilled
  off <- ihcheterogeneity(data = ihc, wholesection = "whole", biopsy1 = "b1",
                          biopsy2 = "b2", biopsy3 = "b3",
                          variance_components = FALSE)
  expect_equal(off$variancetable$rowCount, 4L)
})


test_that("swimmerplot summary keeps five fixed rows plus conditional response rows", {
  skip_if_not(analysis_available("swimmerplot"), "package not loaded")

  set.seed(11)
  sw <- data.frame(id = paste0("P", 1:12), start = rep(0, 12),
                   end = sample(4:20, 12, TRUE),
                   resp = factor(sample(c("CR", "PR", "SD", "PD"), 12, TRUE)),
                   stringsAsFactors = FALSE)

  base <- swimmerplot(data = sw, patientID = "id", startTime = "start", endTime = "end")
  expect_equal(base$summary$rowCount, 5L)

  # the conditional response rows are keyed by level and must not duplicate
  withresp <- swimmerplot(data = sw, patientID = "id", startTime = "start",
                          endTime = "end", responseVar = "resp")
  expect_gt(withresp$summary$rowCount, 5L)
})


test_that("sequentialtests and decisioncalculator fill their fixed .init() rows", {
  skip_if_not(analysis_available("sequentialtests"), "package not loaded")

  on <- sequentialtests(test1_sens = 0.90, test1_spec = 0.80, test2_sens = 0.85,
                        test2_spec = 0.95, prevalence = 0.10,
                        show_cost_analysis = TRUE)
  expect_equal(on$cost_analysis_table$rowCount, 3L)

  # option off: the rows exist but stay unfilled -- and must not throw
  off <- sequentialtests(test1_sens = 0.90, test1_spec = 0.80, test2_sens = 0.85,
                         test2_spec = 0.95, prevalence = 0.10,
                         show_cost_analysis = FALSE)
  expect_equal(off$cost_analysis_table$rowCount, 3L)

  skip_if_not(analysis_available("decisioncalculator"), "package not loaded")
  dc <- decisioncalculator(TP = 90, FP = 30, TN = 170, FN = 10, multiplecuts = TRUE)
  expect_equal(dc$multipleCutoffTable$rowCount, 3L)
  dc_off <- decisioncalculator(TP = 90, FP = 30, TN = 170, FN = 10, multiplecuts = FALSE)
  expect_equal(dc_off$multipleCutoffTable$rowCount, 3L)
})


test_that("linechart and lollipop summary tables fill their eight .init() rows", {
  skip_if_not(analysis_available("linechart"), "package not loaded")

  set.seed(11)
  lc <- data.frame(x = rep(1:10, 3), y = rnorm(30, 10, 2),
                   g = factor(rep(c("a", "b", "c"), each = 10)))
  # the "Group Names" row is gated on the groupby OPTION in .init() -- the same
  # condition .populateSummary() uses -- so the row set is 8 grouped, 7 ungrouped
  # and every key setRow() touches exists on both paths
  grouped <- linechart(data = lc, xvar = "x", yvar = "y", groupby = "g")
  expect_equal(grouped$summary$rowCount, 8L)

  ungrouped <- linechart(data = lc, xvar = "x", yvar = "y")
  expect_equal(ungrouped$summary$rowCount, 7L)

  skip_if_not(analysis_available("lollipop"), "package not loaded")
  lp <- data.frame(lbl = factor(paste0("i", 1:8)), val = runif(8, 1, 10))
  # `highlight` is a type: Level option, so it is a REQUIRED wrapper argument
  lol <- lollipop(data = lp, dep = "val", group = "lbl", highlight = NULL)
  expect_equal(lol$summary$rowCount, 8L)
})
