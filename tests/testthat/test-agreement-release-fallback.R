test_that("null-SE fallback gives a neutral coverage caveat even when it is wider", {
  d <- data.frame(a = factor(rep(c("N", "N", "P", "P"), c(45, 5, 5, 45))),
                  b = factor(rep(c("N", "P", "N", "P"), c(45, 5, 5, 45))))
  k <- irr::kappa2(d)
  null_se <- unname(k$value / k$statistic)
  non_null_se <- unname(vcd::Kappa(table(d))$Unweighted[2])
  expect_equal(k$value, .8)
  expect_equal(non_null_se, .06)
  expect_equal(null_se, .10)
  expect_gt(null_se, non_null_se)
  local_mocked_bindings(Kappa = function(...) stop("injected vcd failure"), .package = "vcd")
  r <- suppressWarnings(agreement(data = d, vars = c("a", "b")))$irrtable
  expect_equal(r$asDF$ci_lower[1], .8 - qnorm(.975) * .1)
  expect_equal(r$asDF$ci_upper[1], .8 + qnorm(.975) * .1)
  note <- paste(vapply(r$notes, function(n) n$note, ""), collapse = " ")
  expect_match(note, "coverage at nonzero kappa is uncertain", fixed = TRUE)
  expect_false(grepl("optimistic|too narrow when kappa is high", note))
})
