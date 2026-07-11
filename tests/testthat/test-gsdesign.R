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
