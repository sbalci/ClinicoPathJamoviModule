
test_that("tumorbudding assigns ITBCC grades at the correct cutoffs", {
  skip_if_not_installed("jmvcore")

  data <- data.frame(bud_count = c(0, 4, 5, 9, 10, 25))
  expect_no_error({
    model <- tumorbudding(
      data = data, budCount = "bud_count", fieldArea = 0.785,
      showGrading = TRUE, showPerCase = TRUE, showSurvival = FALSE, showPlot = TRUE)
  })
  expect_true(inherits(model, "jmvcoreClass"))

  gd <- model$results$gradingTable$asDF
  # 0,4 -> Bd1 (2); 5,9 -> Bd2 (2); 10,25 -> Bd3 (2)
  expect_equal(gd$n[gd$grade == "Bd1 (low)"], 2)
  expect_equal(gd$n[gd$grade == "Bd2 (intermediate)"], 2)
  expect_equal(gd$n[gd$grade == "Bd3 (high)"], 2)
})

test_that("tumorbudding normalizes non-standard field area and takes the hotspot", {
  skip_if_not_installed("jmvcore")

  # 6 buds in a 0.5 mm2 field -> 9.4 per 0.785 -> Bd2; multiple fields per case -> max
  data <- data.frame(
    case_id = c("A", "A", "B", "B"),
    bud_count = c(3, 6, 12, 2))
  model <- tumorbudding(
    data = data, budCount = "bud_count", caseId = "case_id", fieldArea = 0.5,
    showGrading = TRUE, showPerCase = TRUE, showSurvival = FALSE)
  pc <- model$results$perCaseTable$asDF
  expect_equal(nrow(pc), 2)                      # two cases
  # case A hotspot = 6 raw -> 9.4 normalized -> Bd2
  expect_equal(pc$grade[pc$case == "A"], "Bd2 (intermediate)")
  # case B hotspot = 12 raw -> 18.8 -> Bd3
  expect_equal(pc$grade[pc$case == "B"], "Bd3 (high)")
})

test_that("tumorbudding links grade to survival", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("survival")

  set.seed(2026); n <- 180
  buds <- rpois(n, sample(c(2, 7, 13), n, TRUE, c(.45, .30, .25)))
  gi <- ifelse(buds <= 4, 1L, ifelse(buds <= 9, 2L, 3L))
  data <- data.frame(
    bud_count = buds,
    os = round(rexp(n, 0.04 * exp(0.5 * (gi - 1))), 1),
    event = rbinom(n, 1, 0.7))
  model <- tumorbudding(
    data = data, budCount = "bud_count", fieldArea = 0.785,
    survivalTime = "os", survivalStatus = "event", eventLevel = "1",
    showSurvival = TRUE)
  sv <- model$results$survivalTable$asDF
  expect_true(any(grepl("Log-rank", sv$statistic)))
})
