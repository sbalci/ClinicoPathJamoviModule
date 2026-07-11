
test_that("multifocalconcordance computes per-marker concordance and kappa (paired)", {
  skip_if_not_installed("jmvcore")

  # 4 cases, 2 foci each, one marker. Cases 1-3 concordant, case 4 discordant -> 75%.
  data <- data.frame(
    case_id = c("A","A","B","B","C","C","D","D"),
    sample  = rep(c("Primary","Met"), 4),
    HER2    = c("Positive","Positive", "Negative","Negative",
                "Positive","Positive", "Positive","Negative"))
  expect_no_error({
    model <- multifocalconcordance(
      data = data, caseId = "case_id", focusId = "sample", markers = "HER2",
      showPerMarker = TRUE, showKappa = TRUE, showCaseLevel = TRUE, showPlot = TRUE)
  })
  expect_true(inherits(model, "jmvcoreClass"))

  pm <- model$results$perMarkerTable$asDF
  expect_equal(pm$nCases[pm$marker == "HER2"], 4)
  expect_equal(pm$concordant[pm$marker == "HER2"], 3)
  expect_equal(round(pm$rate[pm$marker == "HER2"], 2), 0.75)
  # kappa reported (paired design) - non-empty numeric string
  expect_false(pm$kappa[pm$marker == "HER2"] == "\u2014")
})

test_that("multifocalconcordance recovers built-in concordance rates", {
  skip_if_not_installed("jmvcore")

  set.seed(2026); ncase <- 90
  mk_pair <- function(cp) {
    prim <- sample(c("Positive","Negative"), ncase, TRUE)
    met  <- ifelse(rbinom(ncase, 1, cp) == 1, prim,
                   ifelse(prim == "Positive","Negative","Positive"))
    list(prim = prim, met = met)
  }
  her2 <- mk_pair(0.92); er <- mk_pair(0.80); pdl1 <- mk_pair(0.68)
  data <- data.frame(
    case_id = rep(sprintf("C%03d", seq_len(ncase)), each = 2),
    sample = rep(c("Primary","Met"), ncase),
    HER2 = as.vector(rbind(her2$prim, her2$met)),
    ER   = as.vector(rbind(er$prim, er$met)),
    PDL1 = as.vector(rbind(pdl1$prim, pdl1$met)))

  model <- multifocalconcordance(
    data = data, caseId = "case_id", focusId = "sample",
    markers = c("HER2","ER","PDL1"), showPerMarker = TRUE)
  pm <- model$results$perMarkerTable$asDF
  # HER2 should be the most concordant, PDL1 the least
  expect_true(pm$rate[pm$marker == "HER2"] > pm$rate[pm$marker == "PDL1"])
  expect_true(all(pm$nCases == 90))
})
