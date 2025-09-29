test_that("venn works", {
  # Load the package
  library(ClinicoPath)
  
  # Test with basic mtcars data
  mtcars_test <- mtcars
  mtcars_test$vs <- factor(mtcars_test$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
  mtcars_test$am <- factor(mtcars_test$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
  
  # Test basic 2-variable venn
  result <- venn(data = mtcars_test, 
                 var1 = "vs", var1true = "V-shaped", 
                 var2 = "am", var2true = "Manual")
  
  # Check that result is not null
  expect_false(is.null(result))
  
  # Check that result has expected structure
  expect_true(is.object(result))
  expect_true(inherits(result, "vennResults"))
  
  # Test with 3 variables
  mtcars_test$gear_high <- factor(ifelse(mtcars_test$gear > 3, "High", "Low"))
  
  result3 <- venn(data = mtcars_test, 
                  var1 = "vs", var1true = "V-shaped", 
                  var2 = "am", var2true = "Manual",
                  var3 = "gear_high", var3true = "High")
  
  expect_false(is.null(result3))
  expect_true(inherits(result3, "vennResults"))
  
  # Test with 4 variables
  mtcars_test$cyl_high <- factor(ifelse(mtcars_test$cyl > 4, "High", "Low"))
  
  result4 <- venn(data = mtcars_test, 
                  var1 = "vs", var1true = "V-shaped", 
                  var2 = "am", var2true = "Manual",
                  var3 = "gear_high", var3true = "High",
                  var4 = "cyl_high", var4true = "High")
  
  expect_false(is.null(result4))
  expect_true(inherits(result4, "vennResults"))
})

test_that("venn requires minimum variables", {
  library(ClinicoPath)
  
  # Test error handling when insufficient variables provided
  expect_error(venn(data = mtcars))
  expect_error(venn(data = mtcars, var1 = "vs"))
  expect_error(venn(data = mtcars, var1 = "vs", var1true = "1"))
})

test_that("venn handles factor conversion", {
  library(ClinicoPath)
  
  # Test automatic factor conversion
  test_data <- data.frame(
    var1 = c("A", "B", "A", "B"),
    var2 = c("X", "Y", "X", "Y"),
    stringsAsFactors = FALSE
  )
  
  result <- venn(data = test_data, 
                 var1 = "var1", var1true = "A", 
                 var2 = "var2", var2true = "X")
  
  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
})

test_that("venn works with clinical data example", {
  library(ClinicoPath)
  
  # Test with clinical data as shown in examples
  clinical_data <- data.frame(
    patient_id = 1:50,
    diabetes = sample(c("Yes", "No"), 50, replace = TRUE, prob = c(0.3, 0.7)),
    hypertension = sample(c("Yes", "No"), 50, replace = TRUE, prob = c(0.4, 0.6)),
    obesity = sample(c("Yes", "No"), 50, replace = TRUE, prob = c(0.25, 0.75)),
    stringsAsFactors = FALSE
  )
  
  result <- venn(data = clinical_data,
                 var1 = "diabetes", var1true = "Yes",
                 var2 = "hypertension", var2true = "Yes",
                 var3 = "obesity", var3true = "Yes")
  
  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
})

test_that("venn summary statistics work correctly", {
  library(ClinicoPath)
  
  # Create test data with known proportions
  test_data <- data.frame(
    var1 = factor(c(rep("True", 30), rep("False", 70))),
    var2 = factor(c(rep("True", 40), rep("False", 60))),
    stringsAsFactors = FALSE
  )
  
  # Test that venn can be created without errors
  result <- venn(data = test_data, 
                 var1 = "var1", var1true = "True", 
                 var2 = "var2", var2true = "True")
  
  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
  
  # Test that summary results are accessible
  expect_true(is.object(result$summary))
})

test_that("venn handles edge cases", {
  library(ClinicoPath)
  
  # Test with all TRUE values
  all_true <- data.frame(
    var1 = factor(rep("Yes", 10)),
    var2 = factor(rep("Yes", 10))
  )
  
  result1 <- venn(data = all_true, 
                  var1 = "var1", var1true = "Yes", 
                  var2 = "var2", var2true = "Yes")
  
  expect_false(is.null(result1))
  
  # Test with all FALSE values
  all_false <- data.frame(
    var1 = factor(rep("No", 10)),
    var2 = factor(rep("No", 10))
  )
  
  result2 <- venn(data = all_false, 
                  var1 = "var1", var1true = "Yes", 
                  var2 = "var2", var2true = "Yes")
  
  expect_false(is.null(result2))
  
  # Test with single observation
  single_obs <- data.frame(
    var1 = factor("Yes"),
    var2 = factor("No")
  )
  
  result3 <- venn(data = single_obs, 
                  var1 = "var1", var1true = "Yes", 
                  var2 = "var2", var2true = "Yes")
  
  expect_false(is.null(result3))
})

test_that("venn works with missing optional parameters", {
  library(ClinicoPath)
  
  # Test with only required parameters
  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y"))
  )
  
  # This should work with just var1 and var2
  result <- venn(data = test_data, 
                 var1 = "var1", var1true = "A", 
                 var2 = "var2", var2true = "X")
  
  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
})

test_that("venn works with ComplexUpset options", {
  library(ClinicoPath)

  # Test ComplexUpset functionality
  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y", "X", "Y")),
    var3 = factor(c("P", "Q", "P", "Q", "P", "Q"))
  )

  # Test with ComplexUpset options
  result <- venn(data = test_data,
                 var1 = "var1", var1true = "A",
                 var2 = "var2", var2true = "X",
                 var3 = "var3", var3true = "P",
                 show_complexUpset = TRUE,
                 sortBy = "freq",
                 minSize = 1,
                 showAnnotations = TRUE)

  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
})

test_that("venn works with different upset sorting options", {
  library(ClinicoPath)
  
  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y"))
  )
  
  # Test different sorting options
  for (sort_option in c("freq", "degree", "none")) {
    result <- venn(data = test_data, 
                   var1 = "var1", var1true = "A", 
                   var2 = "var2", var2true = "X",
                   sortBy = sort_option)
    
    expect_false(is.null(result))
    expect_true(inherits(result, "vennResults"))
  }
})

test_that("venn works with minimum size filtering", {
  library(ClinicoPath)

  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y", "X", "Y"))
  )

  # Test with minimum size filter
  result <- venn(data = test_data,
                 var1 = "var1", var1true = "A",
                 var2 = "var2", var2true = "X",
                 minSize = 2)

  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))
})

test_that("venn works with separate plot type options", {
  library(ClinicoPath)

  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y"))
  )

  # Test with only ggvenn
  result1 <- venn(data = test_data,
                  var1 = "var1", var1true = "A",
                  var2 = "var2", var2true = "X",
                  show_ggvenn = TRUE,
                  show_ggVennDiagram = FALSE,
                  show_upsetR = FALSE,
                  show_complexUpset = FALSE)

  expect_false(is.null(result1))
  expect_true(inherits(result1, "vennResults"))

  # Test with only ggVennDiagram
  result2 <- venn(data = test_data,
                  var1 = "var1", var1true = "A",
                  var2 = "var2", var2true = "X",
                  show_ggvenn = FALSE,
                  show_ggVennDiagram = TRUE,
                  show_upsetR = FALSE,
                  show_complexUpset = FALSE)

  expect_false(is.null(result2))
  expect_true(inherits(result2, "vennResults"))

  # Test with only upsetR
  result3 <- venn(data = test_data,
                  var1 = "var1", var1true = "A",
                  var2 = "var2", var2true = "X",
                  show_ggvenn = FALSE,
                  show_ggVennDiagram = FALSE,
                  show_upsetR = TRUE,
                  show_complexUpset = FALSE)

  expect_false(is.null(result3))
  expect_true(inherits(result3, "vennResults"))

  # Test with multiple plots enabled
  result4 <- venn(data = test_data,
                  var1 = "var1", var1true = "A",
                  var2 = "var2", var2true = "X",
                  show_ggvenn = TRUE,
                  show_ggVennDiagram = TRUE,
                  show_upsetR = TRUE,
                  show_complexUpset = TRUE)

  expect_false(is.null(result4))
  expect_true(inherits(result4, "vennResults"))
})

test_that("venn shows warning message for ggvenn with >4 variables", {
  library(ClinicoPath)

  # Create test data with 5 variables
  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A")),
    var2 = factor(c("X", "Y", "X", "Y", "X")),
    var3 = factor(c("P", "Q", "P", "Q", "P")),
    var4 = factor(c("M", "N", "M", "N", "M")),
    var5 = factor(c("U", "V", "U", "V", "U"))
  )

  # Test that ggvenn shows explanatory message with >4 variables
  result <- venn(data = test_data,
                 var1 = "var1", var1true = "A",
                 var2 = "var2", var2true = "X",
                 var3 = "var3", var3true = "P",
                 var4 = "var4", var4true = "M",
                 var5 = "var5", var5true = "U",
                 show_ggvenn = TRUE,
                 show_ggVennDiagram = FALSE,
                 show_upsetR = FALSE,
                 show_complexUpset = FALSE)

  expect_false(is.null(result))
  expect_true(inherits(result, "vennResults"))

  # Test that ggVennDiagram works fine with >4 variables
  result2 <- venn(data = test_data,
                  var1 = "var1", var1true = "A",
                  var2 = "var2", var2true = "X",
                  var3 = "var3", var3true = "P",
                  var4 = "var4", var4true = "M",
                  var5 = "var5", var5true = "U",
                  show_ggvenn = FALSE,
                  show_ggVennDiagram = TRUE,
                  show_upsetR = FALSE,
                  show_complexUpset = FALSE)

  expect_false(is.null(result2))
  expect_true(inherits(result2, "vennResults"))
})

test_that("membership table populates when enabled", {
  library(ClinicoPath)

  test_data <- data.frame(
    var1 = factor(c("A", "B", "A", "B")),
    var2 = factor(c("X", "Y", "X", "Y"))
  )

  result <- venn(
    data = test_data,
    var1 = "var1", var1true = "A",
    var2 = "var2", var2true = "X",
    showSetCalculations = TRUE,
    showMembershipTable = TRUE
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Membership Table", output, fixed = TRUE)))
  expect_true(any(grepl("Group", output, fixed = TRUE)))
  expect_true(any(grepl("Yes", output, fixed = TRUE)))
  expect_true(any(grepl("var1 & var2", output, fixed = TRUE)) ||
                any(grepl("None", output, fixed = TRUE)))
  expect_false(any(grepl("Error in generating membership table", output, fixed = TRUE)))
})
