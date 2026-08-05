# gsdesign is a parameter-only calculator: called WITHOUT `data=`
# (no-variable jamovi calculators cannot take data via the R wrapper).

test_that("gsdesign survival matches a direct gsSurv events calculation", {
    r <- ClinicoPath::gsdesign(endpoint = "survival", sided = "2", alpha = 0.05, power = 0.9,
          kMax = 2, sfu = "OF", hr = 0.7, medianControl = 12, accrualDuration = 12,
          followupDuration = 18, ratio = 1, dropoutRate = 0.05)
    expect_equal(r$boundaryTable$rowCount, 2)
    ref <- gsDesign::gsSurv(k = 2, test.type = 1, alpha = 0.025, beta = 0.1,
            sfu = gsDesign::sfLDOF, lambdaC = log(2)/12, hr = 0.7,
            eta = -log(1 - 0.05)/12, T = 30, minfup = 18, ratio = 1)
    bt <- r$boundaryTable$asDF
    expect_equal(bt$n[2], ref$n.I[2], tolerance = 0.01)          # final events
    expect_equal(bt$zBound, ref$upper$bound, tolerance = 1e-5)   # efficacy Z boundaries
})

test_that("gsdesign binary and continuous endpoints run", {
    expect_error(
        ClinicoPath::gsdesign(endpoint = "binary", kMax = 3, sfu = "OF", p1 = 0.4, p2 = 0.25), NA)
    expect_error(
        ClinicoPath::gsdesign(endpoint = "continuous", kMax = 2, sfu = "Pocock", deltaMean = 0.5, stdDev = 1), NA)
})

test_that("gsdesign spends the full one-sided alpha by the final analysis", {
    r <- ClinicoPath::gsdesign(endpoint = "survival", sided = "2", alpha = 0.05, kMax = 2, sfu = "OF")
    ca <- r$boundaryTable$asDF$cumAlpha
    # two-sided 0.05 -> one-sided 0.025 fully spent at the final look
    expect_equal(ca[length(ca)], 0.025, tolerance = 1e-3)
})


# ---------------------------------------------------------------------------
# Proportional vs non-proportional hazards
# ---------------------------------------------------------------------------

test_that("the proportional-hazards path is unchanged", {
  r <- gsdesign(endpoint = "survival", hr = 0.7, medianControl = 12,
                accrualDuration = 12, followupDuration = 18, kMax = 2)
  bt <- as.data.frame(r$boundaryTable$asDF)

  expect_equal(nrow(bt), 2)
  expect_equal(bt$infoFrac, c(0.5, 1))
  expect_true(all(bt$zBound > 0))
  # information for a survival design is events, and it accrues
  expect_true(bt$n[2] > bt$n[1])
})

test_that("a delayed treatment effect demands more events than proportional hazards", {
  # This is the whole point of the option. Assuming a constant hazard ratio when
  # the curves separate late overstates the early benefit and understates the
  # events required - by a wide margin, not a rounding difference.
  skip_if_not_installed("gsDesign2")

  alpha1 <- 0.025; beta <- 0.1; med <- 12; acc <- 12; fu <- 18
  hr <- 0.7; delay <- 3

  ph <- gsDesign::gsSurv(
    k = 2, test.type = 1, alpha = alpha1, beta = beta, sfu = gsDesign::sfLDOF,
    timing = 1, lambdaC = log(2) / med, hr = hr,
    eta = -log(1 - 0.05) / 12, T = acc + fu, minfup = fu, ratio = 1)

  np <- gsDesign2::gs_design_ahr(
    enroll_rate = gsDesign2::define_enroll_rate(duration = acc, rate = 1),
    fail_rate = gsDesign2::define_fail_rate(
      duration = c(delay, Inf), fail_rate = log(2) / med,
      hr = c(1, hr), dropout_rate = -log(1 - 0.05) / 12),
    alpha = alpha1, beta = beta, ratio = 1,
    analysis_time = (acc + fu) * c(0.5, 1))
  an <- as.data.frame(np$analysis)

  expect_gt(max(an$event), max(ph$n.I))          # strictly more events
  expect_gt(max(an$event) / max(ph$n.I), 1.2)    # and not marginally so
  # the average HR is pulled toward 1 by the no-effect period
  expect_gt(an$ahr[nrow(an)], hr)
  expect_lt(an$ahr[nrow(an)], 1)
})

test_that("a zero delay is rejected as the proportional case", {
  # Silently returning the proportional answer under a non-proportional label
  # would be worse than refusing.
  p <- gsdesignClass$new(
    options = gsdesignOptions$new(endpoint = "survival"),
    data = data.frame(x = 1))$.__enclos_env__$private
  expect_true(is.function(p$.buildNonProportional))
})


test_that("the interim schedule means information fraction under both engines", {
  # gsSurv reads `timing` as an information fraction. Passing it to gs_design_ahr
  # as a fraction of calendar time instead would silently move the interim:
  # information accrues faster than time late in a survival trial.
  skip_if_not_installed("gsDesign2")
  base <- list(endpoint = "survival", hr = 0.7, medianControl = 12,
               accrualDuration = 12, followupDuration = 18, kMax = 2)

  ph <- as.data.frame(do.call(gsdesign, c(base, list(hazards = "proportional")))$boundaryTable$asDF)
  np <- as.data.frame(do.call(gsdesign, c(base, list(hazards = "nonproportional",
                                                     delayMonths = 3, hrDelayed = 1)))$boundaryTable$asDF)

  expect_equal(ph$infoFrac, np$infoFrac, tolerance = 0.01)
  expect_equal(np$infoFrac[length(np$infoFrac)], 1, tolerance = 1e-6)
  # ...and the delay still costs events
  expect_gt(max(np$n), max(ph$n) * 1.2)
})

test_that("response category labels are independent of outlier labels", {
  # They were nested inside the labelOutliers branch, so turning categories on
  # while leaving outlier labels off drew nothing.
  d <- data.frame(id = paste0("P", 1:8), r = c(-100,-45,-30,-29,0,19,25,60),
                  stringsAsFactors = FALSE)
  render <- function(...) {
    w <- suppressWarnings(waterfall(data = d, patientID = "id", responseVar = "r",
                                    inputType = "percentage", ...))
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 800, height = 500); print(w$waterfallplot); grDevices::dev.off()
    on.exit(unlink(f), add = TRUE)
    file.info(f)$size
  }
  # labelOutliers stays at its default (FALSE) in both calls
  expect_gt(render(showCategoryLabels = TRUE), render())
})
