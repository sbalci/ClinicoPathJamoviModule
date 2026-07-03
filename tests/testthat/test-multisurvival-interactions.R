# Fast, harness-free unit tests for multisurvival interaction helpers.
# Source only the two self-contained helper files (no devtools::load_all,
# no R6/jamovi harness needed) — see feedback_testing_strategy.
suppressMessages({
  library(testthat)
  library(survival)
  library(jmvcore)
})

# Resolve the repo root robustly regardless of how this file is executed
# (testthat::test_file() chdirs into the test file's directory before
# running the tests, so getwd() is normally ".../tests/testthat"; other
# runners such as testthat::test_dir()/devtools::test() may leave the
# working directory at the package root instead). Walk up from both
# candidate starting points looking for the marker file R/utils.R.
.find_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "utils.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}

.root <- .find_root(file.path(dirname(dirname(getwd()))))
if (is.na(.root)) .root <- .find_root(getwd())
if (is.na(.root)) stop("Could not locate repo root (marker file R/utils.R not found)")

source(file.path(.root, "R", "utils.R"))
source(file.path(.root, "R", "multisurvival-interactions.R"))

# Local-only verification shim (does NOT touch production code): the CRAN
# release of jmvcore (2.7.7, what a plain system R installs) does not export
# asFormula() — it ships only in jamovi's own bundled jmvcore (>= 2.7.12),
# which cannot be loaded into a newer system R. See project memory
# "feedback_phantom_private_method_nonfunction" -> "Local-verification
# caveat". `.asSurvivalFormula()` in R/utils.R is left exactly as written
# (still calls jmvcore::asFormula for real inside jamovi); here we only
# swap the lookup used by THIS TEST RUN so `.buildSurvivalFormula()` can be
# exercised outside the jamovi app. This is faithful for well-formed
# formulas: asFormula() returns the same parsed formula as as.formula(), it
# only adds a security allow-list on top.
if (!exists("asFormula", envir = asNamespace("jmvcore"), inherits = FALSE)) {
  assign(".asSurvivalFormula", function(x) stats::as.formula(x), envir = .GlobalEnv)
}

test_that(".buildSurvivalFormula appends escaped interaction terms", {
  f <- .buildSurvivalFormula(
    time_var = "mytime", outcome_var = "myoutcome",
    predictors = c("arm", "bio"),
    interaction_terms = "`arm`:`bio`"
  )
  rhs <- as.character(f)[3]
  expect_true(grepl("arm", rhs))
  expect_true(grepl("bio", rhs))
  expect_true(grepl(":", rhs))
})

test_that(".buildSurvivalFormula still works with no interactions", {
  f <- .buildSurvivalFormula("mytime", "myoutcome", c("arm", "bio"))
  rhs <- as.character(f)[3]
  expect_false(grepl(":", rhs))
  expect_true(grepl("arm", rhs) && grepl("bio", rhs))
})

test_that(".buildSurvivalFormula keeps strata after interactions", {
  f <- .buildSurvivalFormula("mytime", "myoutcome", c("arm"),
                             strata_vars = "site",
                             interaction_terms = "`arm`:`bio`")
  rhs <- as.character(f)[3]
  expect_true(grepl("strata\\(", rhs))
  expect_true(grepl(":", rhs))
})

test_that(".buildSurvivalFormula applies strata even when predictors are empty", {
  f <- .buildSurvivalFormula("mytime", "myoutcome",
                             predictors = character(0),
                             strata_vars = "site")
  rhs <- as.character(f)[3]
  expect_true(grepl("strata\\(", rhs))
  expect_false(grepl(":", rhs))
})

test_that(".mapInteractionTerms maps display labels to real names", {
  all_labels <- list(arm = "Treatment Arm", bio = "Biomarker", age = "Age")
  interactions <- list(c("Treatment Arm", "Biomarker"))
  out <- .mapInteractionTerms(interactions, all_labels)
  expect_equal(out, list(c("arm", "bio")))
})

test_that(".mapInteractionTerms passes through names already real / unlabelled", {
  all_labels <- list(arm = "Treatment Arm")
  out <- .mapInteractionTerms(list(c("arm", "unlabelled_col")), all_labels)
  expect_equal(out, list(c("arm", "unlabelled_col")))
})

test_that(".mapInteractionTerms returns empty list for NULL/empty", {
  expect_equal(.mapInteractionTerms(NULL, list(a = "A")), list())
  expect_equal(.mapInteractionTerms(list(), list(a = "A")), list())
})

test_that(".interactionTermsForFormula escapes and joins with colon", {
  out <- .interactionTermsForFormula(list(c("arm", "weird name")))
  expect_equal(out, "arm:`weird name`")
})

test_that(".interactionTermsForFinalfit joins raw with colon", {
  out <- .interactionTermsForFinalfit(list(c("arm", "bio"), c("arm", "age")))
  expect_equal(out, c("arm:bio", "arm:age"))
})

test_that(".interactionModeratorInfo identifies focal, moderator, categorical", {
  d <- data.frame(arm = factor(c("a","b")), bio = factor(c("x","y")), age = c(1.0, 2.0))
  info <- .interactionModeratorInfo(c("arm", "bio"), d)
  expect_equal(info$focal, "arm")
  expect_equal(info$moderator, "bio")
  expect_true(info$twoway)
  expect_true(info$categorical_moderator)

  info2 <- .interactionModeratorInfo(c("arm", "age"), d)   # continuous moderator
  expect_false(info2$categorical_moderator)

  info3 <- .interactionModeratorInfo(c("arm", "bio", "age"), d)  # 3-way
  expect_false(info3$twoway)
})

test_that(".interactionTestTable extracts interaction-coefficient HRs matching coxph", {
  set.seed(1)
  n <- 400
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE))
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(bio=="pos", 0.1, 0) +
         ifelse(arm=="trt" & bio=="pos", -0.9, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  fit <- survival::coxph(survival::Surv(time, status) ~ arm * bio, data = d)

  tab <- .interactionTestTable(fit, conf_level = 0.95)
  expect_s3_class(tab, "data.frame")
  expect_true(any(grepl(":", tab$term)))

  # HR of the interaction row equals exp(coef) from coxph
  sm <- summary(fit)$coefficients
  int_name <- rownames(sm)[grepl(":", rownames(sm))][1]
  expect_equal(tab$hr[tab$term == int_name], unname(exp(sm[int_name, "coef"])),
               tolerance = 1e-8)
  expect_equal(tab$p[tab$term == int_name], unname(sm[int_name, "Pr(>|z|)"]),
               tolerance = 1e-8)
})

test_that(".interactionTestTable returns NULL when no interaction present", {
  set.seed(2); n <- 100
  d <- data.frame(time = rexp(n), status = rbinom(n,1,.7),
                  arm = factor(sample(c("a","b"), n, TRUE)))
  fit <- survival::coxph(survival::Surv(time, status) ~ arm, data = d)
  expect_null(.interactionTestTable(fit))
})

test_that(".interactionTestTable CI columns equal exp(confint) bounds", {
  set.seed(11)
  n <- 400
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE))
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="pos", -0.9, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  fit <- survival::coxph(survival::Surv(time, status) ~ arm * bio, data = d)
  tab <- .interactionTestTable(fit, conf_level = 0.95)
  int_name <- rownames(summary(fit)$coefficients)[grepl(":", rownames(summary(fit)$coefficients))][1]
  ci <- suppressWarnings(stats::confint(fit, level = 0.95))
  expect_equal(tab$ci_lower[tab$term == int_name], unname(exp(ci[int_name, 1])), tolerance = 1e-8)
  expect_equal(tab$ci_upper[tab$term == int_name], unname(exp(ci[int_name, 2])), tolerance = 1e-8)
})

test_that(".computeSubgroupHRs matches manual relevel-and-refit", {
  set.seed(3)
  n <- 600
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE))
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="pos", -0.9, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  f <- survival::Surv(time, status) ~ arm * bio

  sub <- .computeSubgroupHRs(f, d, focal = "arm", moderator = "bio",
                             conf_level = 0.95)
  expect_s3_class(sub, "data.frame")
  expect_setequal(sub$moderator_level, c("neg","pos"))

  # Ground truth: relevel bio to "pos", refit, read arm coefficient.
  d2 <- d; d2$bio <- relevel(d2$bio, ref = "pos")
  fit2 <- survival::coxph(f, data = d2)
  hr_pos <- unname(exp(coef(fit2)["armtrt"]))
  got <- sub$hr[sub$moderator_level == "pos" & sub$focal_effect == "trt"]
  expect_equal(got, hr_pos, tolerance = 1e-6)
})

test_that(".computeSubgroupHRs returns NULL for non-factor moderator", {
  set.seed(4); n <- 200
  d <- data.frame(time = rexp(n), status = rbinom(n,1,.7),
                  arm = factor(sample(c("a","b"), n, TRUE)),
                  age = rnorm(n))
  f <- survival::Surv(time, status) ~ arm * age
  expect_null(.computeSubgroupHRs(f, d, focal = "arm", moderator = "age"))
})

# Fix 1 regression: a focal name with a space is backticked by the formula
# builder, so coxph's coefficient carries the backticks. The helper must
# escape the focal name too, otherwise the coef lookup silently misses.
test_that(".computeSubgroupHRs handles spaced (backticked) focal name", {
  set.seed(7)
  n <- 600
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE))
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="pos", -0.9, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, bio, `treatment arm` = arm,
                  check.names = FALSE, stringsAsFactors = FALSE)
  d[["treatment arm"]] <- factor(d[["treatment arm"]])
  f <- stats::as.formula(
    "survival::Surv(time, status) ~ `treatment arm` * bio")

  sub <- .computeSubgroupHRs(f, d, focal = "treatment arm", moderator = "bio")
  expect_s3_class(sub, "data.frame")
  expect_setequal(sub$moderator_level, c("neg","pos"))

  # Ground truth: relevel bio to "pos", refit, read the backticked focal coef.
  d2 <- d; d2$bio <- relevel(d2$bio, ref = "pos")
  fit2 <- survival::coxph(f, data = d2)
  hr_pos <- unname(exp(coef(fit2)["`treatment arm`trt"]))
  got <- sub$hr[sub$moderator_level == "pos" & sub$focal_effect == "trt"]
  expect_equal(got, hr_pos, tolerance = 1e-6)
})

# Fix 2a regression: an ORDERED-factor moderator must not crash relevel().
test_that(".computeSubgroupHRs handles ordered-factor moderator", {
  set.seed(8)
  n <- 500
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("low","high"), n, TRUE),
                levels = c("low","high"), ordered = TRUE)
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="high", -0.7, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  f <- survival::Surv(time, status) ~ arm * bio

  expect_error(
    sub <- .computeSubgroupHRs(f, d, focal = "arm", moderator = "bio"),
    NA)
  expect_s3_class(sub, "data.frame")
  expect_setequal(sub$moderator_level, c("low","high"))
})

# Fix 2b regression: a moderator with a declared-but-unobserved level must not
# crash relevel() (a zero-count ref is rejected) — droplevels handles it.
test_that(".computeSubgroupHRs handles unobserved moderator level", {
  set.seed(9)
  n <- 500
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE),
                levels = c("neg","pos","unknown"))   # "unknown" never sampled
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="pos", -0.8, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  f <- survival::Surv(time, status) ~ arm * bio

  expect_error(
    sub <- .computeSubgroupHRs(f, d, focal = "arm", moderator = "bio"),
    NA)
  expect_s3_class(sub, "data.frame")
  expect_setequal(sub$moderator_level, c("neg","pos"))  # no "unknown" row
})

# Fix 3 regression: a normal, convergent fit is flagged converged == TRUE.
test_that(".computeSubgroupHRs reports converged column for a normal fit", {
  set.seed(10)
  n <- 600
  arm <- factor(sample(c("ctrl","trt"), n, TRUE))
  bio <- factor(sample(c("neg","pos"), n, TRUE))
  lp  <- ifelse(arm=="trt", -0.2, 0) + ifelse(arm=="trt" & bio=="pos", -0.9, 0)
  time <- rexp(n, exp(lp)); status <- rbinom(n, 1, 0.7)
  d <- data.frame(time, status, arm, bio)
  f <- survival::Surv(time, status) ~ arm * bio

  sub <- .computeSubgroupHRs(f, d, focal = "arm", moderator = "bio")
  expect_true("converged" %in% names(sub))
  expect_true(all(sub$converged))
})
