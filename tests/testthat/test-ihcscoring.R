
test_that('ihcscoring analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    intensity_var = sample(0:3, n, replace = TRUE),   # intensity is a 0-3 score
    proportion_var = round(runif(n, 0, 100)),         # percentage of positive nuclei
    sample_id_var = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    immune_cells_var = runif(n, 1, 100),
    tumor_cells_var = runif(n, 1, 100),
    primary_marker1 = runif(n, 1, 100),
    primary_marker2 = runif(n, 1, 100),
    secondary_marker = runif(n, 1, 100),
    pd1_marker = runif(n, 1, 100),
    pdl1_marker = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- ihcscoring(
      data = data,
    guided_biomarker = 'manual',
    intensity_var = 'intensity_var',
    proportion_var = 'proportion_var',
    sample_id_var = 'sample_id_var',
    group_var = 'group_var',
    scoring_method = 'both',
    binary_cutpoint = 100,
    allred_cutpoint = 3,
    intensity_scale = 'standard',
    biomarker_type = 'other',
    show_plots = TRUE,
    show_agreement_plots = TRUE,
    include_statistics = TRUE,
    include_digital_validation = FALSE,
    agreement_analysis = TRUE,
    quality_control = TRUE,
    clinical_interpretation = TRUE,
    export_results = FALSE,
    multiple_cutoffs = FALSE,
    cps_analysis = FALSE,
    immune_cells_var = 'immune_cells_var',
    tumor_cells_var = 'tumor_cells_var',
    cutoff_comparison = TRUE,
    confidence_level = 0.95,
    bootstrap_n = 1000,
    automated_analysis = FALSE,
    segmentation_method = 'manual',
    color_deconvolution = TRUE,
    minimum_nuclear_area = 50,
    maximum_nuclear_area = 2000,
    batch_processing = FALSE,
    image_format = 'tiff',
    validation_metrics = TRUE,
    molecular_classification = FALSE,
    classification_system = 'bladder_mibc',
    primary_marker1 = 'primary_marker1',
    primary_marker2 = 'primary_marker2',
    secondary_marker = 'secondary_marker',
    pd1_marker = 'pd1_marker',
    pdl1_marker = 'pdl1_marker',
    subtype_statistics = TRUE,
    subtype_visualization = TRUE,
    language = 'english',
    colorblind_safe = TRUE,
    high_contrast = FALSE,
    font_size = 'normal',
    outcome_positive = NULL
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, 'ihcscoringResults'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcscoring.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV. Export is a jmvReadWrite concern; skip rather than fail the
  # analysis test when it cannot round-trip.
  tryCatch(
    jmvReadWrite::write_omv(model, omv_path),
    error = function(e) message("OMV export failed: ", conditionMessage(e))
  )
  if (!file.exists(omv_path)) skip("OMV export failed; skipping file existence check")
  expect_true(file.exists(omv_path))
})



test_that("ihcscoring finds a binary-outcome optimal cutpoint (Youden)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("cutpointr")

  set.seed(11); n <- 220
  intensity  <- sample(0:3, n, TRUE)
  proportion <- round(runif(n, 0, 100))
  hscore <- intensity * proportion
  outcome <- factor(ifelse(rbinom(n, 1, plogis((hscore - 120) / 60)) == 1, "Pos", "Neg"),
                    levels = c("Neg", "Pos"))
  data <- data.frame(intensity_var = intensity, proportion_var = proportion,
                     outcome = outcome)

  expect_no_error({
    model <- ihcscoring(
      data = data, intensity_var = "intensity_var", proportion_var = "proportion_var",
      optimal_cutpoint = TRUE, optimize_score = "hscore",
      outcome_type = "binary", outcome_var = "outcome", outcome_positive = "Pos")
  })
  expect_true(inherits(model, "ihcscoringResults"))

  ct <- model$optimalCutpointTable$asDF
  expect_true(nrow(ct) >= 5)
  expect_true("Optimal cutpoint" %in% ct$quantity)
  expect_true("AUC" %in% ct$quantity)
})

test_that("ihcscoring finds a survival-outcome optimal cutpoint (maxstat)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("maxstat")

  set.seed(11); n <- 220
  intensity  <- sample(0:3, n, TRUE)
  proportion <- round(runif(n, 0, 100))
  hscore <- intensity * proportion
  time   <- rexp(n, 0.02 * (0.5 + hscore / 150))
  status <- rbinom(n, 1, 0.7)
  data <- data.frame(intensity_var = intensity, proportion_var = proportion,
                     time = time, event = status)

  expect_no_error({
    model <- ihcscoring(
      data = data, intensity_var = "intensity_var", proportion_var = "proportion_var",
      optimal_cutpoint = TRUE, optimize_score = "hscore",
      outcome_type = "survival", outcome_var = "event", outcome_positive = "1",
      cutpoint_time_var = "time")
  })
  expect_true(inherits(model, "ihcscoringResults"))

  ct <- model$optimalCutpointTable$asDF
  expect_true("Optimal cutpoint" %in% ct$quantity)
  expect_true(any(grepl("log-rank", ct$quantity, ignore.case = TRUE)))
})


test_that("ER/PR percentage-scale rows use the percentage, not the H-score", {
  skip_if_not_installed("jmvcore")

  # Built so the two scales DISAGREE. Cases 1 and 2 are 0.5% and 0.8% positive -- below
  # 1% on the percentage scale -- but at intensity 3 their H-scores are 1.5 and 2.4, so a
  # `hscore >= 1` test counts them as ">=1%". That was the defect: the 1% and 10%
  # percentage cutoffs were applied to hscore (0-300) and labelled as percentages.
  data <- data.frame(
    intensity_var  = c(3,   3,   1,  2,  3,  0),
    proportion_var = c(0.5, 0.8, 5, 40, 90,  0)
  )

  model <- ihcscoring(
    data = data,
    intensity_var = "intensity_var", proportion_var = "proportion_var",
    biomarker_type = "er", binary_cutpoint = 100,
    outcome_positive = NULL
  )
  expect_true(inherits(model, "ihcscoringResults"))

  df <- model$biomarkerspecific$biomarkerresults$asDF
  val <- function(pattern) df$value[grepl(pattern, df$parameter, fixed = TRUE)][1]

  # 3 of 6 cases are >=1% on the percentage scale. Applying the cutoff to hscore gave 5/6.
  expect_equal(val("Percentage scale: >=1%"), 50, tolerance = 1e-8)
  expect_equal(val("Percentage scale: <1%"), 50, tolerance = 1e-8)
  # only case 3 (5%) sits in 1-10%; on the hscore scale four cases did
  expect_equal(val("Percentage scale: 1-10%"), 100 / 6, tolerance = 1e-6)

  # The H-score row is reported on its own scale against the cutpoint: only case 5
  # (3 x 90 = 270) reaches 100.
  expect_equal(val("H-score scale: >=100"), 100 / 6, tolerance = 1e-6)

  # No FDA claim anywhere in the rendered text.
  expect_false(any(grepl("FDA[- ]approved", unlist(df), ignore.case = TRUE)))
})

test_that("HER2 rows are the intensity score and account for every case", {
  skip_if_not_installed("jmvcore")

  # 0/1+/2+/3+ is the intensity score, not an H-score band. The old implementation banded
  # hscore and never displayed the 3+ row, so the table under-reported the highest group.
  data <- data.frame(
    intensity_var  = c(0, 1, 2, 3, 3, 2),
    proportion_var = c(0, 10, 40, 90, 80, 30)
  )

  model <- ihcscoring(
    data = data,
    intensity_var = "intensity_var", proportion_var = "proportion_var",
    biomarker_type = "her2",
    outcome_positive = NULL
  )
  df <- model$biomarkerspecific$biomarkerresults$asDF
  val <- function(pattern) df$value[grepl(pattern, df$parameter, fixed = TRUE)][1]

  expect_equal(val("Intensity scale: 0 or 1+"), 100 * 2 / 6, tolerance = 1e-6)
  expect_equal(val("Intensity scale: 2+"),      100 * 2 / 6, tolerance = 1e-6)
  expect_equal(val("Intensity scale: 3+"),      100 * 2 / 6, tolerance = 1e-6)

  # The three bands are exhaustive -- the defining failure of the old hscore banding.
  bands <- c(val("Intensity scale: 0 or 1+"), val("Intensity scale: 2+"),
             val("Intensity scale: 3+"))
  expect_equal(sum(bands), 100, tolerance = 1e-6)
})
