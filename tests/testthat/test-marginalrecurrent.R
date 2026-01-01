test_that("baseline hazard yields valid survival estimates", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("reReg")

    data("simDat", package = "reReg")

    fit <- reReg::reReg(
        reReg::Recur(t.stop, id, event, terminal = status) ~ x1 + x2,
        data = simDat,
        model = "cox|cox",
        se = "sand",
        B = 50
    )

    haz_fun <- fit[["Haz0"]]
    lam_fun <- fit[["Lam0"]]

    expect_true(is.function(haz_fun))
    expect_true(is.function(lam_fun))

    time_grid <- seq(0, max(simDat$t.stop), length.out = 25)

    # Population-level hazard scaled by the mean frailty component
    scaled_hazard <- haz_fun(time_grid) * exp(fit$log.muZ)
    survival <- exp(-scaled_hazard)
    cumulative_rate <- lam_fun(time_grid)

    expect_equal(survival[1], 1, tolerance = 2e-2)
    expect_true(all(diff(survival) <= 1e-8))
    expect_true(all(survival >= 0 & survival <= 1 + 1e-8))

    expect_true(min(diff(cumulative_rate)) >= -1e-8)
    expect_true(all(cumulative_rate >= -1e-8))
})
