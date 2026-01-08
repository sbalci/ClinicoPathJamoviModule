# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: agepyramid
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(agepyramid_test)
data(agepyramid_pediatric)
data(agepyramid_geriatric)
data(agepyramid_cancer)

test_that("agepyramid respects all bin width options", {
  # Test various bin widths
  for (bin_w in c(1, 5, 10, 15, 20)) {
    result <- agepyramid(
      data = agepyramid_test,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male",
      bin_width = bin_w
    )
    expect_no_error(result)
  }
})

test_that("agepyramid respects all color palette options", {
  # Test standard palette
  result1 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    color_palette = "standard"
  )
  expect_no_error(result1)

  # Test accessible palette
  result2 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    color_palette = "colorblind"
  )
  expect_no_error(result2)

  # Test custom palette with custom colors
  result3 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    color_palette = "custom",
    female_color = "#FF0000",
    male_color = "#0000FF"
  )
  expect_no_error(result3)
})

test_that("agepyramid works with all age group presets", {
  # Custom bins
  result1 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "custom",
    bin_width = 5
  )
  expect_no_error(result1)

  # Pediatric preset with pediatric data
  result2 <- agepyramid(
    data = agepyramid_pediatric,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "pediatric"
  )
  expect_no_error(result2)

  # Reproductive preset
  result3 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "reproductive"
  )
  expect_no_error(result3)

  # Geriatric preset with geriatric data
  result4 <- agepyramid(
    data = agepyramid_geriatric,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "geriatric"
  )
  expect_no_error(result4)

  # Life course preset
  result5 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "lifecourse"
  )
  expect_no_error(result5)
})



test_that("agepyramid respects custom plot titles", {
  custom_titles <- c(
    "Age Pyramid",
    "Population Age Distribution",
    "Patient Demographics",
    "Age Structure by Gender"
  )

  for (title in custom_titles) {
    result <- agepyramid(
      data = agepyramid_test,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male",
      plot_title = title
    )
    expect_no_error(result)
  }
})

test_that("agepyramid handles factor vs character gender variables", {
  # Convert to factor
  test_data_factor <- agepyramid_test
  test_data_factor$gender <- as.factor(test_data_factor$gender)

  result_factor <- agepyramid(
    data = test_data_factor,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Convert to character
  test_data_char <- agepyramid_test
  test_data_char$gender <- as.character(test_data_char$gender)

  result_char <- agepyramid(
    data = test_data_char,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Both should work
  expect_no_error(result_factor)
  expect_no_error(result_char)
})

test_that("agepyramid works with different population structures", {
  # Cancer population (older, more males)
  result1 <- agepyramid(
    data = agepyramid_cancer,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    plot_title = "Cancer Patient Age Distribution"
  )
  expect_no_error(result1)

  # Pediatric population
  result2 <- agepyramid(
    data = agepyramid_pediatric,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "pediatric",
    plot_title = "Pediatric Age Distribution"
  )
  expect_no_error(result2)

  # Geriatric population (more females)
  result3 <- agepyramid(
    data = agepyramid_geriatric,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "geriatric",
    plot_title = "Geriatric Age Distribution"
  )
  expect_no_error(result3)
})

test_that("agepyramid handles all argument combinations", {
  # Test with all options specified
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 10,
    plot_title = "Complete Test",
    female_color = "#E69F00",
    male_color = "#56B4E9",
    color_palette = "custom",
    age_groups = "custom"
  )

  expect_no_error(result)
  expect_s3_class(result, "agepyramidResults")
})
