test_that("jjtreemap works with basic treemap", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create test data for treemap
  test_data <- data.frame(
    category = factor(rep(c("A", "B", "C", "D"), each = 5)),
    value = c(runif(5, 10, 20), runif(5, 15, 25), runif(5, 8, 18), runif(5, 12, 22))
  )
  
  # Aggregate data for treemap
  agg_data <- aggregate(value ~ category, data = test_data, FUN = sum)
  
  # Test basic functionality
  result <- jjtreemap(
    data = agg_data,
    group = "category",
    size = "value",
    aspectRatio = 1.67,
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjtreemap works with color variable", {
  # Create test data with color variable
  test_data <- data.frame(
    product = factor(c("Product A", "Product B", "Product C", "Product D", "Product E")),
    sales = c(120, 80, 150, 90, 110),
    region = factor(c("North", "South", "East", "West", "Central"))
  )
  
  result <- jjtreemap(
    data = test_data,
    group = "product",
    size = "sales",
    color = "region",
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap handles different label options", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test different font faces
  font_faces <- c("normal", "bold", "italic", "bolditalic")
  
  for (font_face in font_faces) {
    result <- jjtreemap(
      data = test_data,
      group = "category",
      size = "value",
      labelFontFace = font_face,
      showLabels = TRUE
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjtreemap handles label alignment options", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test horizontal alignments
  h_aligns <- c("left", "center", "right")
  v_aligns <- c("top", "center", "bottom")
  
  for (h_align in h_aligns) {
    for (v_align in v_aligns) {
      result <- jjtreemap(
        data = test_data,
        group = "category",
        size = "value",
        labelAlignH = h_align,
        labelAlignV = v_align,
        showLabels = TRUE
      )
      
      expect_s3_class(result, "Group")
    }
  }
})

test_that("jjtreemap handles custom styling", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test with custom styling
  result <- jjtreemap(
    data = test_data,
    group = "category",
    size = "value",
    borderWidth = 1.2,
    borderLevel1Width = 2,
    borderLevel2Width = 0.8,
    borderLevel1Color = "darkblue",
    borderLevel2Color = "lightgray",
    labelLevel1Size = 18,
    labelLevel2Size = 14,
    labelLevel1Color = "black",
    labelLevel2Color = "gray",
    labelBackground = "white",
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap handles edge cases", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test with missing group (should return early)
  result_no_group <- jjtreemap(
    data = test_data,
    group = NULL,
    size = "value"
  )
  
  expect_s3_class(result_no_group, "Group")
  
  # Test with missing size (should return early)
  result_no_size <- jjtreemap(
    data = test_data,
    group = "category",
    size = NULL
  )
  
  expect_s3_class(result_no_size, "Group")
})

test_that("jjtreemap handles missing values", {
  # Create data with missing values
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D", NA, "E")),
    value = c(100, 80, NA, 40, 60, 90)
  )
  
  result <- jjtreemap(
    data = test_data,
    group = "category",
    size = "value",
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap validates input parameters", {
  # Test with empty data
  expect_error(
    jjtreemap(
      data = data.frame(),
      group = "category",
      size = "value"
    ),
    "Argument 'group' contains 'category' which is not present in the dataset"
  )
})

test_that("jjtreemap works with plot labels", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test with titles and captions
  result <- jjtreemap(
    data = test_data,
    group = "category",
    size = "value",
    title = "Sales by Category",
    subtitle = "Q4 2023 Results",
    caption = "Source: Sales Database",
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap performance optimization works", {
  # Test that cached data is being used efficiently
  test_data <- data.frame(
    category = factor(rep(LETTERS[1:10], each = 10)),
    value = runif(100, 10, 100)
  )
  
  # Aggregate for treemap
  agg_data <- aggregate(value ~ category, data = test_data, FUN = sum)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjtreemapOptions$new(
    group = "category",
    size = "value"
  )
  
  analysis <- ClinicoPath:::jjtreemapClass$new(
    options = options,
    data = agg_data
  )
  
  # Test that optimization methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjtreemap works with hierarchical data", {
  # Create hierarchical test data
  test_data <- data.frame(
    main_category = factor(rep(c("Electronics", "Clothing", "Food"), each = 4)),
    sub_category = factor(c(
      "Phones", "Laptops", "TVs", "Tablets",
      "Shirts", "Pants", "Shoes", "Hats",
      "Fruits", "Vegetables", "Dairy", "Meat"
    )),
    sales = c(
      150, 200, 180, 120,
      80, 90, 110, 60,
      70, 65, 85, 95
    )
  )
  
  result <- jjtreemap(
    data = test_data,
    group = "sub_category",
    size = "sales",
    color = "main_category",
    showLabels = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap handles aspect ratio options", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(100, 80, 60, 40)
  )
  
  # Test different aspect ratios
  aspect_ratios <- c(1, 1.33, 1.67, 2)
  
  for (ratio in aspect_ratios) {
    result <- jjtreemap(
      data = test_data,
      group = "category",
      size = "value",
      aspectRatio = ratio,
      showLabels = TRUE
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjtreemap handles label size and overlap", {
  test_data <- data.frame(
    category = factor(rep(LETTERS[1:20], each = 1)),
    value = runif(20, 5, 50)
  )
  
  # Test with small rectangles and label settings
  result <- jjtreemap(
    data = test_data,
    group = "category",
    size = "value",
    showLabels = TRUE,
    labelSize = 8,
    labelOverlap = 0.2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjtreemap handles large datasets efficiently", {
  # Test with larger dataset to check performance optimizations
  large_data <- data.frame(
    category = factor(rep(paste0("Category_", 1:50), each = 2)),
    value = runif(100, 10, 1000)
  )
  
  # Aggregate for better performance
  agg_data <- aggregate(value ~ category, data = large_data, FUN = sum)
  
  # This should complete without significant delay due to optimizations
  start_time <- Sys.time()
  result <- jjtreemap(
    data = agg_data,
    group = "category",
    size = "value",
    showLabels = TRUE
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "Group")
  # Performance test - should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 30) # Less than 30 seconds
})
