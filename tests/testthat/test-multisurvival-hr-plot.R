test_that("multisurvival HR plot renders (finalfit)", {
  skip_if_not_installed("survival")
  skip_if_not_installed("finalfit")

  colon <- survival::colon

  result <- suppressWarnings(multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1",
    explanatory = "sex",
    contexpl = "age",
    hr = TRUE,
    sty = "t1",
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  ))

  tmp <- tempfile(fileext = ".png")
  expect_silent(suppressWarnings(result$plot$saveAs(tmp)))
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
})
