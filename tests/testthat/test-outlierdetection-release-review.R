# Regression cover for the defects found during the outlierdetection release
# review. Each block fails against the pre-review backend.

library(testthat)

od_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

# Pull the "Row" column out of the rendered per-observation table.
od_rows <- function(raw) {
  raw <- as.character(raw)
  body <- sub(".*<tbody>", "", raw)
  # take the FIRST <td> of each <tr>; a fixed column stride is fragile because
  # the column count varies with the detection method
  trs <- regmatches(body, gregexpr("<tr[^>]*>.*?</tr>", body))[[1]]
  if (!length(trs)) return(numeric(0))
  first <- vapply(trs, function(tr) {
    td <- regmatches(tr, regexpr("<td[^>]*>[^<]*</td>", tr))
    if (!length(td)) return(NA_character_)
    gsub("<[^>]*>", "", td)
  }, character(1), USE.NAMES = FALSE)
  as.numeric(first[!is.na(first)])
}

test_that("the per-observation table renders for datasets larger than 100 rows", {
  # The listing was wrapped in `if (nrow(outlier_df) <= 100)`, so it vanished for
  # any dataset with more than 100 rows - essentially every clinical dataset -
  # while show_outlier_table defaults to TRUE, its description promises
  # "classification for each observation", and the summary directly above says
  # "review the N flagged observation(s) below". There was nothing below.
  set.seed(1)
  x <- rnorm(200, 5, 1)
  planted <- c(7, 88, 195)
  x[planted] <- 40

  raw <- as.character(outlierdetection(
    data = data.frame(lab = x), vars = "lab",
    method_category = "univariate", univariate_methods = "zscore_robust"
  )$outlier_table$content)

  expect_match(raw, "<table", fixed = TRUE)
  expect_true(all(planted %in% od_rows(raw)))
})

test_that("reported row numbers point at the original rows after subsampling", {
  # Datasets over 10000 rows are subsampled to 5000. The listed row numbers must
  # still address the user's own file, or "verify against source records" sends
  # them to the wrong patient.
  set.seed(1)
  n <- 12000
  y <- rnorm(n, 5, 1)
  y[c(37, 5001, 9999, 11500)] <- 40

  raw <- as.character(outlierdetection(
    data = data.frame(lab = y), vars = "lab",
    method_category = "univariate", univariate_methods = "zscore_robust"
  )$outlier_table$content)

  rows <- od_rows(raw)
  expect_gt(length(rows), 0)
  # original numbering runs past the 5000-row subsample
  expect_true(any(rows > 5000))
  # and every listed row really is an extreme value in the ORIGINAL data
  expect_true(all(abs(y[rows] - median(y)) > 3 * mad(y)))
})

test_that("the plain-language summary states the real dataset size", {
  # It described the SUBSAMPLE as "your dataset" ("In your dataset of 5000
  # observations" for a 12000-row file), and the copy-ready report sentence
  # repeated the wrong N.
  set.seed(1)
  y <- rnorm(12000, 5, 1); y[c(37, 5001, 9999, 11500)] <- 40
  txt <- od_txt(outlierdetection(data = data.frame(lab = y), vars = "lab",
                                 method_category = "univariate",
                                 univariate_methods = "zscore_robust")$outlier_table$content)

  expect_false(grepl("In your dataset of 5000 observations", txt, fixed = TRUE))
  expect_match(txt, "12000-observation dataset")
  # sampling must be presented as a LOWER BOUND, not a complete answer
  expect_match(txt, "lower bound")
  expect_match(txt, "cannot be detected")
})

test_that("an unsampled dataset gets no sampling language", {
  set.seed(1)
  x <- rnorm(200, 5, 1); x[c(7, 88)] <- 40
  txt <- od_txt(outlierdetection(data = data.frame(lab = x), vars = "lab",
                                 method_category = "univariate",
                                 univariate_methods = "zscore_robust")$outlier_table$content)
  expect_match(txt, "In your dataset of 200 observations")
  expect_false(grepl("subsample", txt))
  expect_false(grepl("lower bound", txt))
})

test_that("reject() messages substitute their placeholder", {
  # jmvcore::reject()'s second POSITIONAL argument is `code`, not a substitution
  # value, so `reject("... {}", method)` left a literal "{}" on screen.
  d <- data.frame(a = rep(5, 50), b = rnorm(50))
  warn <- as.character(outlierdetection(data = d, vars = c("a", "b"))$warnings$content)

  expect_match(warn, "constant values (no variation): a", fixed = TRUE)
  expect_false(grepl("{}", warn, fixed = TRUE))

  # and the all-missing branch, reached by calling the method directly
  ns <- asNamespace("ClinicoPath")
  obj <- get("outlierdetectionClass", ns)$new(
    options = get("outlierdetectionOptions", ns)$new(vars = c("a", "b")),
    data = data.frame(a = as.numeric(rep(NA, 50)), b = rnorm(50)))
  msg <- tryCatch(
    obj$.__enclos_env__$private$.perform_outlier_detection(
      data.frame(a = as.numeric(rep(NA, 50)), b = rnorm(50))),
    error = conditionMessage)
  expect_match(msg, "all missing values: a", fixed = TRUE)
  expect_false(grepl("{}", msg, fixed = TRUE))
})

test_that("package-missing messages do not tell jamovi users to install packages", {
  # dbscan and robustbase are both in Imports, so a jamovi user cannot act on
  # "install.packages(...)" advice even if the branch were reached.
  src <- readLines("../../R/outlierdetection.b.R")
  expect_false(any(grepl("install.packages(", src, fixed = TRUE)))
})

test_that("outlier flags match performance::check_outliers directly", {
  set.seed(11)
  x <- c(rnorm(300, 10, 2), 40, 45, -15)
  d <- data.frame(lab = x)

  raw <- as.character(outlierdetection(
    data = d, vars = "lab", method_category = "univariate",
    univariate_methods = "zscore_robust")$outlier_table$content)
  got <- sort(od_rows(raw))

  ref <- performance::check_outliers(d, method = "zscore_robust", threshold = 3.29)
  expected <- sort(which(as.logical(ref)))

  expect_equal(got, as.numeric(expected))
})

test_that("the module's thresholds are the performance package defaults", {
  # 3.29 / 1.7 / 0.999 are not invented conventions - they are what
  # performance::check_outliers uses. Guard against silent drift.
  defaults <- performance:::.check_outliers_thresholds(data.frame(x = rnorm(10)))
  expect_equal(round(defaults$zscore_robust, 2), 3.29)
  expect_equal(defaults$iqr, 1.7)
  expect_equal(defaults$eti, 0.999)

  opts <- yaml::read_yaml("../../jamovi/outlierdetection.a.yaml")$options
  get_default <- function(nm) Filter(function(o) identical(o$name, nm), opts)[[1]]$default
  expect_equal(get_default("zscore_threshold"), 3.29)
  expect_equal(get_default("iqr_multiplier"), 1.7)
  expect_equal(get_default("confidence_level"), 0.999)
})

# ---------------------------------------------------------------------------
# Limitations closed after the first review pass.
# ---------------------------------------------------------------------------

test_that("subsampling threshold and size are exposed as options", {
  # They were hard-coded at 10000 / 5000, so a user needing a complete outlier
  # list on a large file had no way to get one.
  opts <- yaml::read_yaml("../../jamovi/outlierdetection.a.yaml")$options
  by_name <- function(nm) Filter(function(o) identical(o$name, nm), opts)

  for (nm in c("sampleThreshold", "sampleSize")) {
    o <- by_name(nm)
    expect_length(o, 1)
    expect_equal(o[[1]]$type, "Integer")
  }
  expect_equal(by_name("sampleThreshold")[[1]]$default, 10000)  # previous behaviour
  expect_equal(by_name("sampleSize")[[1]]$default, 5000)

  # and both must be reachable from the UI, or they exist only for R callers
  ui <- readLines("../../jamovi/outlierdetection.u.yaml")
  expect_true(any(grepl("name: sampleThreshold", ui)))
  expect_true(any(grepl("name: sampleSize", ui)))
})

test_that("reading a not-yet-compiled option falls back instead of crashing", {
  # jmvcore's `$` ERRORS on an option the compiled .h.R does not declare, so a
  # newly added option would crash every run until jmvtools::prepare() is run.
  ns <- asNamespace("ClinicoPath")
  obj <- get("outlierdetectionClass", ns)$new(
    options = get("outlierdetectionOptions", ns)$new(vars = "lab"),
    data = data.frame(lab = rnorm(50)))
  oo <- obj$.__enclos_env__$private$.optionOr

  expect_equal(oo("sampleThreshold", 10000), 10000)
  expect_equal(oo("sampleSize", 5000), 5000)
  expect_equal(oo("definitely_not_an_option", "fallback"), "fallback")
  # an option that DOES exist must come through unchanged
  expect_equal(oo("zscore_threshold", -1), 3.29)
})

test_that("the default sampling behaviour is unchanged", {
  # The new options must reproduce the previously hard-coded 10000/5000 exactly.
  set.seed(1)
  y <- rnorm(12000, 5, 1); y[c(37, 5001, 9999, 11500)] <- 40
  res <- outlierdetection(data = data.frame(lab = y), vars = "lab",
                          method_category = "univariate",
                          univariate_methods = "zscore_robust")
  txt <- od_txt(res$outlier_table$content)
  expect_match(txt, "5000 observations")
  expect_match(txt, "12000")

  # the performance notice lives in the warnings panel; it must name the
  # threshold and tell the user which control changes it
  warn <- od_txt(res$warnings$content)
  expect_match(warn, "subsampling threshold of 10000")
  expect_match(warn, "Subsample above (rows)", fixed = TRUE)
})

test_that("multivariate methods reproduce their reference implementations", {
  set.seed(7)
  X <- MASS::mvrnorm(300, mu = c(0, 0), Sigma = matrix(c(1, .6, .6, 1), 2))
  X <- rbind(X, cbind(c(6, -6, 5.5), c(-6, 6, 5.5)))   # 3 joint outliers
  d <- as.data.frame(X)
  names(d) <- c("m1", "m2")

  got <- function(m) sort(which(as.logical(performance::check_outliers(d, method = m))))

  # Mahalanobis: chi-square cutoff on the classical distance
  md <- stats::mahalanobis(d, colMeans(d), stats::cov(d))
  expect_equal(got("mahalanobis"),
               sort(which(md > stats::qchisq(1 - 0.001, df = ncol(d)))))

  # MCD: same cutoff on the robust distance
  cv <- MASS::cov.rob(d, method = "mcd", nsamp = "best")
  md_r <- stats::mahalanobis(d, cv$center, cv$cov)
  expect_equal(got("mcd"),
               sort(which(md_r > stats::qchisq(1 - 0.001, df = ncol(d)))))

  # LOF: log(lof) with minPts = ncol(x) against a mean-free sd cutoff
  loglof <- log(dbscan::lof(d, minPts = ncol(d)))
  expect_equal(got("lof"),
               sort(which(loglof > stats::qnorm(1 - 0.001) * stats::sd(loglof))))

  # every method must find the three planted joint outliers
  for (m in c("mahalanobis", "mcd", "lof"))
    expect_true(all(301:303 %in% got(m)), info = m)
})

test_that("choosing LOF warns that it flags more liberally", {
  # performance hard-codes minPts = ncol(x). Measured on this data: minPts = 2
  # flags 11 points where minPts = 10 or 20 flag 4, with all 3 real outliers
  # caught either way - so the extra 7 are false positives the user cannot tune
  # away from inside jamovi.
  set.seed(7)
  X <- MASS::mvrnorm(300, mu = c(0, 0), Sigma = matrix(c(1, .6, .6, 1), 2))
  X <- rbind(X, cbind(c(6, -6, 5.5), c(-6, 6, 5.5)))
  d <- as.data.frame(X); names(d) <- c("m1", "m2")

  lof_warn <- od_txt(outlierdetection(data = d, vars = c("m1", "m2"),
                                      method_category = "multivariate",
                                      multivariate_methods = "lof")$warnings$content)
  expect_match(lof_warn, "About Local Outlier Factor")
  expect_match(lof_warn, "Cross-check")

  # and it must NOT appear for methods it does not describe
  mah_warn <- od_txt(outlierdetection(data = d, vars = c("m1", "m2"),
                                      method_category = "multivariate",
                                      multivariate_methods = "mahalanobis")$warnings$content)
  expect_false(grepl("About Local Outlier Factor", mah_warn))

  # the liberality claim must stay true of the underlying package
  n_default <- sum(log(dbscan::lof(d, minPts = ncol(d))) >
                     stats::qnorm(0.999) * stats::sd(log(dbscan::lof(d, minPts = ncol(d)))))
  l20 <- log(dbscan::lof(d, minPts = 20))
  n_20 <- sum(l20 > stats::qnorm(0.999) * stats::sd(l20))
  expect_gt(n_default, n_20)
})

# --- sampleSize must not be clamped to sampleThreshold ------------------------
# The two options are independent: sampleThreshold decides WHEN to subsample,
# sampleSize decides HOW MANY rows to keep. An earlier clamp
# (min(sampleSize, sampleThreshold)) silently overrode an explicit user choice,
# contradicting the option's own help text "larger values recover more of them
# at the cost of speed".
test_that("sampleSize larger than sampleThreshold is honoured, not clamped", {
    set.seed(11)
    d <- data.frame(v = c(stats::rnorm(3000), 999))

    res <- outlierdetection(data = d, vars = "v",
                            sampleThreshold = 1000, sampleSize = 5000)
    msg <- od_txt(res$warnings$content)

    # n = 3001 > threshold 1000 -> subsampling happens and is disclosed
    expect_match(msg, "sampled")
    # It must retain min(sampleSize, n) = 3001 rows, NOT the clamped 1000.
    expect_false(grepl("sampled 1,?000 observations", msg))
    expect_match(msg, "3,?001|3001")
})

# ---- 2026-09-03 release review: method-comparison counts are the per-method flag sums ----

test_that("the method comparison table reports each method's own flag count", {
  data("histopathology", package = "ClinicoPath")
  d <- as.data.frame(histopathology)
  vars <- c("Age", "OverallTime", "MeasurementA")
  # do.call: a bare `vars = vars` is resolved by jmvcore as a column called "vars"
  r <- do.call(outlierdetection, list(data = d, vars = vars, method_category = "composite",
                                      show_method_comparison = TRUE))
  cc <- d[complete.cases(d[, vars]), vars]
  ref <- attr(performance::check_outliers(cc, method = c("zscore_robust", "iqr", "mahalanobis"),
                                          threshold = list(zscore_robust = 3.29, iqr = 1.7)), "data")
  expected <- colSums(ref[, grep("^Outlier_", names(ref)), drop = FALSE])
  html <- r$method_comparison$content
  for (col in names(expected)) {
    cell <- regmatches(html, regexpr(paste0(col, "</strong></td>\\s*<td[^>]*>\\s*[0-9]+"), html))
    expect_length(cell, 1)
    expect_equal(as.numeric(sub(".*>\\s*", "", cell)), unname(expected[[col]]), label = col)
  }
  # the agreement sentence counts observations, not flags
  expect_match(html, sprintf("Of the %d observations analysed", nrow(cc)))
})
