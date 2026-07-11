context("test-crosstable")

# Load required library

test_that("crosstable works", {
  skip_if_not_installed('jmvReadWrite')
  
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


test_that("crosstable SMD balance column computes correctly", {
  skip_if_not_installed("jmvcore")

  set.seed(1)
  df <- data.frame(
    grp  = factor(rep(c("A", "B"), c(200, 180))),
    xcon = c(rnorm(200, 10, 2), rnorm(180, 11, 2.2)),
    xbin = factor(c(rbinom(200, 1, 0.3), rbinom(180, 1, 0.45))),
    xcat = factor(c(sample(c("L1","L2","L3"), 200, TRUE, c(.5,.3,.2)),
                    sample(c("L1","L2","L3"), 180, TRUE, c(.4,.3,.3)))))

  expect_no_error({
    model <- crosstable(
      data = df, vars = c("xcon", "xbin", "xcat"), group = "grp",
      sty = "gtsummary", showSMD = TRUE)
  })
  expect_true(inherits(model, "jmvcoreClass"))

  smd <- model$results$smdTable$asDF
  expect_equal(nrow(smd), 3)
  # continuous SMD ~ -0.49 (magnitude ~0.49)
  expect_equal(round(abs(smd$absSMD[smd$variable == "xcon"]), 1), 0.5)
  # types classified
  expect_equal(smd$vtype[smd$variable == "xcon"], "continuous")
  expect_equal(smd$vtype[smd$variable == "xcat"], "categorical")
  # all |SMD| finite and >= 0
  expect_true(all(smd$absSMD >= 0 & is.finite(smd$absSMD)))
})

test_that("crosstable SMD requires exactly two groups", {
  skip_if_not_installed("jmvcore")
  set.seed(2)
  df <- data.frame(
    grp = factor(sample(c("A", "B", "C"), 150, TRUE)),
    x   = rnorm(150))
  model <- crosstable(data = df, vars = "x", group = "grp",
                      sty = "gtsummary", showSMD = TRUE)
  smd <- model$results$smdTable$asDF
  expect_equal(nrow(smd), 0)   # no rows for 3-group data; note explains why
})
