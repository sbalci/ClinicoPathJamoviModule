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

