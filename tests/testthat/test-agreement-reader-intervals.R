test_that("specific agreement bootstrap uses complete reader rows and whole clusters", {
  d <- data.frame(a = factor(rep(c("N", "N", "Y", "Y", "Y"), 8)),
    b = factor(rep(c("N", "Y", "Y", "Y", "N"), 8)), id = rep(1:20, each = 2))
  d$a[1] <- NA
  r <- agreement_run(d, vars = c("a", "b"), specificAgreement = TRUE,
    specificPositiveCategory = "Y", specificAllCategories = FALSE,
    specificCluster = "id", specificCIMethod = "bootstrap", nBoot = 100, seed = 81)$results$specificAgreementTable
  x <- r$asDF
  expect_equal(x$n_cases, c(39, 39))
  expect_equal(x$units, c(20, 20))
  expect_equal(x$attempted, c(100, 100))
  expect_equal(x$valid + x$undefined, x$attempted)
  keep <- complete.cases(d)
  stat <- function(z) c(specific_agreement = 2 * sum(z$a == "Y" & z$b == "Y") / (sum(z$a == "Y") + sum(z$b == "Y")))
  expected <- .readerResample(d[keep, ], stat, as.character(d$id[keep]), samples = 100, seed = 81)
  pos <- grepl("Positive", x$category)
  expect_equal(x$specific_agreement[pos], expected$estimate)
  expect_equal(x$ci_lower[pos], expected$lower)
  expect_match(agreement_notes(r)[["ci_bootstrap"]], "fixed readers")
  legacy <- agreement_run(d, vars = c("a", "b"), specificAgreement = TRUE,
    specificCIMethod = "legacy")$results$specificAgreementTable
  expect_true(all(legacy$asDF$ci_method == "Approximate Wilson (legacy)"))
  expect_match(agreement_notes(legacy)[["ci_approx"]], "approx", ignore.case = TRUE)
})

test_that("weighted draft kappa uses explicit weights and their matching variance", {
  lv <- c("Low", "Middle", "High")
  d <- data.frame(a = ordered(rep(lv, c(20, 10, 10)), lv),
    b = ordered(c(rep("Low", 15), rep("Middle", 10), rep("High", 15)), lv))
  r <- cohenskappa(d, rater1 = "a", rater2 = "b", kappa_type = "all")$kappaTable$asDF
  for (kind in c("Linear", "Quadratic")) {
    power <- if (kind == "Linear") 1 else 2
    weights <- 1 - (abs(outer(1:3, 1:3, "-")) / 2)^power
    expected <- psych::cohen.kappa(table(d), w = weights)
    idx <- grepl(kind, r$method)
    expect_equal(sum(idx), 1)
    expect_equal(r$kappa[idx], expected$weighted.kappa)
    expect_equal(r$se[idx], sqrt(expected$var.weighted))
    expect_equal(r$kappa[idx], irr::kappa2(data.frame(a = as.integer(d$a), b = as.integer(d$b)),
      weight = if (power == 1) "equal" else "squared")$value)
  }
})

test_that("specific agreement bootstrap preserves cancellation control flow", {
  ns <- asNamespace("ClinicoPath")
  d <- data.frame(a = factor(rep(c("N", "Y"), 20)), b = factor(rep(c("N", "Y", "Y", "N"), 10)))
  o <- ns$agreementOptions$new(vars = c("a", "b"), specificAgreement = TRUE,
    specificCIMethod = "bootstrap", nBoot = 100)
  a <- ns$agreementClass$new(data = d, options = o)
  a$init()
  p <- a$.__enclos_env__$private
  unlockBinding(".checkpoint", p)
  p$.checkpoint <- function(...) stop(agreement_restart_error())
  result <- tryCatch(p$.calculateSpecificAgreement(d), error = identity)
  expect_identical(result$code, "restart")
})
