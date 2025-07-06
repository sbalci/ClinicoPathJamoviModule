test_that("jcomplexupset works", {
  
  # Load test data
  data("jcomplexupset_test_data")
  
  # Check that the test data exists and has the right structure
  expect_true(exists("jcomplexupset_test_data"))
  expect_true(is.data.frame(jcomplexupset_test_data))
  expect_true(nrow(jcomplexupset_test_data) > 0)
  
  # Check that we have binary variables suitable for UpSet plots
  binary_vars <- c("Surgery", "Chemotherapy", "Radiotherapy", "Immunotherapy", "TargetedTherapy")
  expect_true(all(binary_vars %in% names(jcomplexupset_test_data)))
  
  # Check data types
  for (var in binary_vars) {
    expect_true(is.logical(jcomplexupset_test_data[[var]]))
  }
  
  # Test the jcomplexupset function
  skip_if_not_installed("ComplexUpset")
  
  # Basic function call test
  result <- jcomplexupset(
    data = jcomplexupset_test_data,
    set_vars = binary_vars,
    min_size = 1,
    max_degree = 3,
    sort_by = "size",
    sort_order = "descending"
  )
  
  expect_true(is.list(result))
  
  # Test with additional parameters
  result2 <- jcomplexupset(
    data = jcomplexupset_test_data,
    set_vars = binary_vars,
    min_size = 5,
    max_degree = 2,
    show_percentages = TRUE,
    annotations = "intersection_size"
  )
  
  expect_true(is.list(result2))
  
  # Test with biomarker data
  biomarker_vars <- c("HER2_Positive", "ER_Positive", "PR_Positive", "PDL1_Positive")
  
  result3 <- jcomplexupset(
    data = jcomplexupset_test_data,
    set_vars = biomarker_vars,
    theme_style = "theme_minimal",
    color_palette = "viridis"
  )
  
  expect_true(is.list(result3))
  
  # Test error handling
  expect_error(
    jcomplexupset(
      data = jcomplexupset_test_data,
      set_vars = c("Surgery"),  # Only one variable - should fail
      min_size = 1
    ),
    "At least 2 set variables are required"
  )
  
  # Test with non-existent variables
  expect_error(
    jcomplexupset(
      data = jcomplexupset_test_data,
      set_vars = c("NonExistent1", "NonExistent2"),
      min_size = 1
    ),
    "not found in data"
  )
  
  # Test with too many variables
  too_many_vars <- paste0("Var", 1:15)
  expect_error(
    jcomplexupset(
      data = data.frame(matrix(TRUE, nrow = 10, ncol = 15, dimnames = list(NULL, too_many_vars))),
      set_vars = too_many_vars,
      min_size = 1
    ),
    "Maximum of 10 set variables supported"
  )
})

test_that("jcomplexupset with molecular data works", {
  
  # Load molecular test data
  data("molecular_subtype_data")
  
  # Check data structure
  expect_true(exists("molecular_subtype_data"))
  expect_true(is.data.frame(molecular_subtype_data))
  
  # Test with mutation data
  mutation_vars <- c("BRCA1_Mutation", "BRCA2_Mutation", "TP53_Mutation", "PIK3CA_Mutation")
  
  skip_if_not_installed("ComplexUpset")
  
  result <- jcomplexupset(
    data = molecular_subtype_data,
    set_vars = mutation_vars,
    min_size = 1,
    sort_by = "size",
    show_statistics = TRUE,
    show_interpretation = TRUE
  )
  
  expect_true(is.list(result))
  
  # Test with pathway data
  pathway_vars <- c("PI3K_Pathway", "WNT_Pathway", "RB_Pathway", "DNA_Repair")
  
  result2 <- jcomplexupset(
    data = molecular_subtype_data,
    set_vars = pathway_vars,
    annotations = "intersection_ratio",
    theme_style = "theme_classic"
  )
  
  expect_true(is.list(result2))
})

test_that("jcomplexupset with diagnostic data works", {
  
  # Load diagnostic test data
  data("diagnostic_test_data")
  
  # Check data structure
  expect_true(exists("diagnostic_test_data"))
  expect_true(is.data.frame(diagnostic_test_data))
  
  # Test with imaging modalities
  imaging_vars <- c("CT_Scan", "MRI", "PET_Scan", "Ultrasound")
  
  skip_if_not_installed("ComplexUpset")
  
  result <- jcomplexupset(
    data = diagnostic_test_data,
    set_vars = imaging_vars,
    min_size = 10,
    keep_empty_groups = FALSE,
    color_palette = "Set1"
  )
  
  expect_true(is.list(result))
  
  # Test with lab tests
  lab_vars <- c("CBC", "Chemistry_Panel", "Tumor_Markers", "Genetic_Testing")
  
  result2 <- jcomplexupset(
    data = diagnostic_test_data,
    set_vars = lab_vars,
    sort_by = "degree",
    sort_order = "ascending",
    show_percentages = TRUE
  )
  
  expect_true(is.list(result2))
  
  # Test with consultation data
  consult_vars <- c("Oncology", "Surgery", "Radiology", "Pathology")
  
  result3 <- jcomplexupset(
    data = diagnostic_test_data,
    set_vars = consult_vars,
    annotations = "union_size",
    text_size = 14,
    plot_title = "Specialist Consultations",
    plot_subtitle = "Pattern Analysis"
  )
  
  expect_true(is.list(result3))
})

test_that("jcomplexupset parameter validation works", {
  
  data("jcomplexupset_test_data")
  binary_vars <- c("Surgery", "Chemotherapy", "Radiotherapy")
  
  skip_if_not_installed("ComplexUpset")
  
  # Test various parameter combinations
  result1 <- jcomplexupset(
    data = jcomplexupset_test_data,
    set_vars = binary_vars,
    min_size = 0,
    max_degree = 10,
    width_ratio = 0.5,
    height_ratio = 0.8
  )
  expect_true(is.list(result1))
  
  # Test theme variations
  themes <- c("theme_minimal", "theme_classic", "theme_gray", "theme_bw", "theme_void")
  
  for (theme in themes) {
    result <- jcomplexupset(
      data = jcomplexupset_test_data,
      set_vars = binary_vars,
      theme_style = theme,
      text_size = 10
    )
    expect_true(is.list(result))
  }
  
  # Test color palettes
  palettes <- c("viridis", "plasma", "inferno", "magma", "Set1", "Set2", "Dark2", "Paired")
  
  for (palette in palettes) {
    result <- jcomplexupset(
      data = jcomplexupset_test_data,
      set_vars = binary_vars,
      color_palette = palette
    )
    expect_true(is.list(result))
  }
  
  # Test annotation types
  annotations <- c("none", "intersection_size", "intersection_ratio", "union_size")
  
  for (annotation in annotations) {
    result <- jcomplexupset(
      data = jcomplexupset_test_data,
      set_vars = binary_vars,
      annotations = annotation,
      base_annotations_height = 1.5
    )
    expect_true(is.list(result))
  }
})

test_that("jcomplexupset data conversion works", {
  
  # Test with character data that should be converted to logical
  char_data <- data.frame(
    ID = 1:50,
    Var1 = sample(c("Yes", "No"), 50, replace = TRUE),
    Var2 = sample(c("Present", "Absent"), 50, replace = TRUE),
    Var3 = sample(c("1", "0"), 50, replace = TRUE),
    Var4 = sample(c("TRUE", "FALSE"), 50, replace = TRUE)
  )
  
  skip_if_not_installed("ComplexUpset")
  
  result <- jcomplexupset(
    data = char_data,
    set_vars = c("Var1", "Var2", "Var3", "Var4"),
    min_size = 1
  )
  
  expect_true(is.list(result))
  
  # Test with numeric data
  numeric_data <- data.frame(
    ID = 1:50,
    Num1 = sample(c(0, 1), 50, replace = TRUE),
    Num2 = sample(c(0, 1, 2), 50, replace = TRUE),
    Num3 = sample(c(-1, 0, 1), 50, replace = TRUE)
  )
  
  result2 <- jcomplexupset(
    data = numeric_data,
    set_vars = c("Num1", "Num2", "Num3"),
    min_size = 1
  )
  
  expect_true(is.list(result2))
})