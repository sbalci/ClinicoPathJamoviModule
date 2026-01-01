context("test-crosstable")

# Load required library
library(ClinicoPath)

test_that("crosstable works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic functionality with histopathology data
  expect_error(
    crosstable(
      data = histopathology,
      vars = c("Sex", "Grade"),
      group = "Group",
      sty = "nejm"
    ),
    NA
  )
  
  # Test different styles
  styles <- c("arsenal", "finalfit", "gtsummary", "nejm", "lancet", "hmisc")
  
  for (style in styles) {
    test_that(paste("crosstable works with", style, "style"), {
      expect_error(
        crosstable(
          data = histopathology,
          vars = c("Sex", "Grade"),
          group = "Group",
          sty = style
        ),
        NA
      )
    })
  }
  
  # Test with missing value exclusion
  test_that("crosstable works with missing value exclusion", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "nejm",
        excl = TRUE
      ),
      NA
    )
  })
  
  # Test with different continuous variable summary methods
  test_that("crosstable works with different continuous methods", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex"),
        group = "Group",
        sty = "finalfit",
        cont = "mean"
      ),
      NA
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex"),
        group = "Group",
        sty = "finalfit",
        cont = "median"
      ),
      NA
    )
  })
  
  # Test with different categorical test methods
  test_that("crosstable works with different categorical tests", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "finalfit",
        pcat = "chisq"
      ),
      NA
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "finalfit",
        pcat = "fisher"
      ),
      NA
    )
  })
  
  # Test with single variable
  test_that("crosstable works with single variable", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = "Sex",
        group = "Group",
        sty = "nejm"
      ),
      NA
    )
  })
  
  # Test with multiple variables
  test_that("crosstable works with multiple variables", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade", "Race"),
        group = "Group",
        sty = "gtsummary"
      ),
      NA
    )
  })
  
  # Test with continuous and categorical variables mixed
  test_that("crosstable works with mixed variable types", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex", "Grade"),
        group = "Group",
        sty = "gtsummary"
      ),
      NA
    )
  })
  
  # Test error conditions
  test_that("crosstable handles errors appropriately", {
    # Test with missing required parameters
    expect_error(
      crosstable(
        data = histopathology,
        vars = "Sex"
        # Missing group parameter
      )
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        group = "Group"
        # Missing vars parameter
      )
    )
    
    # Test with non-existent variables
    expect_error(
      crosstable(
        data = histopathology,
        vars = "NonExistentVar",
        group = "Group",
        sty = "nejm"
      )
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        vars = "Sex",
        group = "NonExistentGroup",
        sty = "nejm"
      )
    )
  })
  
  # Test return structure
  test_that("crosstable returns correct structure", {
    result <- crosstable(
      data = histopathology,
      vars = c("Sex", "Grade"),
      group = "Group",
      sty = "nejm"
    )
    
    # Test that it returns a crosstableResults object (jamovi results object)
    expect_s3_class(result, "crosstableResults")
    expect_true("tablestyle4" %in% names(result))
  })
  
})
