# Tests for pcaloadingheatmap functionality

library(testthat)

mtcars_small <- mtcars[, c("mpg", "disp", "hp", "drat")]

test_that("pcaloadingheatmap rejects non-numeric variables", {
  data <- data.frame(
    cont1 = rnorm(10),
    cont2 = rnorm(10),
    category = factor(sample(letters[1:3], 10, replace = TRUE))
  )

  expect_error(
    pcaloadingheatmap(data = data, vars = c("cont1", "cont2", "category")),
    "requires a numeric variable",
    fixed = FALSE
  )
})

# Helper used for loadings normalization
calc_loadings <- ClinicoPath:::pcaloadingheatmap_normalized_loadings

scaled_pca <- stats::prcomp(mtcars_small, center = TRUE, scale. = TRUE)
unscaled_pca <- stats::prcomp(mtcars_small, center = TRUE, scale. = FALSE)

scaled_data <- as.matrix(mtcars_small)

unscaled_loadings_expected <- {
  load_cov <- sweep(unscaled_pca$rotation, 2, unscaled_pca$sdev, `*`)
  sweep(load_cov, 1, apply(mtcars_small, 2, stats::sd), `/`)
}

scaled_loadings_expected <- sweep(scaled_pca$rotation, 2, scaled_pca$sdev, `*`)

scaled_loadings_expected <- pmax(pmin(scaled_loadings_expected, 1), -1)
unscaled_loadings_expected <- pmax(pmin(unscaled_loadings_expected, 1), -1)

scaled_loadings <- calc_loadings(scaled_pca, scaled_data, TRUE)
unscaled_loadings <- calc_loadings(unscaled_pca, scaled_data, FALSE)

test_that("loadings are consistent with scale settings", {
  expect_equal(scaled_loadings, scaled_loadings_expected, tolerance = 1e-8)
  expect_equal(unscaled_loadings, unscaled_loadings_expected, tolerance = 1e-8)
})
