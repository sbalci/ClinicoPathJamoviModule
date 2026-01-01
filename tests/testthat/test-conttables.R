# Load required libraries and data
library(ClinicoPath)
data(histopathology, package = "ClinicoPath")

test_that("contTables module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("contTablesClass"))
  expect_true(is.function(contTables))
})

test_that("contTables handles basic input validation", {
  # Test with missing required variables
  expect_error(
    contTables(data = histopathology, rows = NULL, cols = "Sex"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    contTables(data = histopathology, rows = "Sex", cols = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("contTables works with valid inputs", {
  # Test basic functionality with categorical variables
  result <- contTables(
    data = histopathology,
    rows = "Sex",
    cols = "Mortality5yr"
  )
  
  expect_s3_class(result, "contTablesClass")
  expect_true("Sex" %in% names(histopathology))
  expect_true("Mortality5yr" %in% names(histopathology))
})

test_that("contTables handles 2x2 table with appropriate measures", {
  # Create a binary subset for testing 2x2 measures
  test_data <- histopathology[histopathology$Sex %in% c("Male", "Female") & 
                             histopathology$Mortality5yr %in% c("Alive", "Dead"), ]
  
  result <- contTables(
    data = test_data,
    rows = "Sex",
    cols = "Mortality5yr",
    chiSq = TRUE,
    fisher = TRUE,
    odds = TRUE,
    logOdds = TRUE,
    relRisk = TRUE,
    ci = TRUE,
    ciWidth = 95
  )
  
  expect_s3_class(result, "contTablesClass")
  
  # Check that results contain expected tables
  expect_true("freqs" %in% names(result$results))
  expect_true("chiSq" %in% names(result$results))
  expect_true("odds" %in% names(result$results))
})

test_that("contTables handles larger contingency tables", {
  # Test with variables that have more than 2 levels
  result <- contTables(
    data = histopathology,
    rows = "Grade",
    cols = "TStage",
    chiSq = TRUE,
    likeRat = TRUE,
    contCoef = TRUE,
    phiCra = TRUE,
    gamma = TRUE
  )
  
  expect_s3_class(result, "contTablesClass")
})

test_that("contTables handles layered analysis", {
  # Test with layer variables
  result <- contTables(
    data = histopathology,
    rows = "Sex",
    cols = "Mortality5yr",
    layers = "Group",
    chiSq = TRUE
  )
  
  expect_s3_class(result, "contTablesClass")
})

test_that("contTables handles count data", {
  # Test using HairEyeColor data as mentioned in documentation
  if (require(datasets, quietly = TRUE)) {
    data(HairEyeColor)
    dat <- as.data.frame(HairEyeColor)
    
    result <- contTables(
      data = dat,
      rows = "Hair",
      cols = "Eye",
      counts = "Freq",
      chiSq = TRUE,
      fisher = FALSE
    )
    
    expect_s3_class(result, "contTablesClass")
  }
})

test_that("contTables formula interface works", {
  # Test using formula interface
  if (require(datasets, quietly = TRUE)) {
    data(HairEyeColor)
    dat <- as.data.frame(HairEyeColor)
    
    result <- contTables(
      formula = Freq ~ Hair:Eye,
      data = dat
    )
    
    expect_s3_class(result, "contTablesClass")
  }
})

test_that("contTables handles percentages correctly", {
  # Test different percentage options
  result <- contTables(
    data = histopathology,
    rows = "Sex",
    cols = "Mortality5yr",
    obs = TRUE,
    exp = TRUE,
    pcRow = TRUE,
    pcCol = TRUE,
    pcTot = TRUE
  )
  
  expect_s3_class(result, "contTablesClass")
})

test_that("contTables handles tau-b calculation", {
  # Test Kendall's tau-b (can be slow)
  result <- contTables(
    data = histopathology,
    rows = "Grade",
    cols = "TStage",
    taub = TRUE
  )
  
  expect_s3_class(result, "contTablesClass")
})

test_that("contTables handles edge cases", {
  # Test with very small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- contTables(
      data = small_data,
      rows = "Sex",
      cols = "Mortality5yr"
    )
  }, NA)  # Should not error during initialization
})

test_that("contTables confidence interval options work", {
  # Test different confidence interval widths
  ci_widths <- c(90, 95, 99)
  
  for (ci_width in ci_widths) {
    expect_error({
      result <- contTables(
        data = histopathology,
        rows = "Sex",
        cols = "Mortality5yr",
        odds = TRUE,
        ci = TRUE,
        ciWidth = ci_width
      )
    }, NA)
  }
})

test_that("contTables handles missing data appropriately", {
  # Create dataset with some missing values
  test_data <- histopathology
  test_data$Sex[1:5] <- NA
  
  expect_error({
    result <- contTables(
      data = test_data,
      rows = "Sex",
      cols = "Mortality5yr"
    )
  }, NA)  # Should handle missing data gracefully
})
