# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjdotplotstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(jjdotplotstats_test)
data(jjdotplotstats_twogroup)
data(jjdotplotstats_fourgroup)
data(jjdotplotstats_reference)

test_that("jjdotplotstats respects all statistical type options", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for typestatistics:", stat_type))
  }
})

test_that("jjdotplotstats respects all effect size type options", {
  devtools::load_all()

  effect_types <- c("biased", "unbiased", "eta", "omega")

  for (eff_type in effect_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      typestatistics = "parametric",
      effsizetype = eff_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for effsizetype:", eff_type))
  }
})

test_that("jjdotplotstats respects all centrality type options", {
  devtools::load_all()

  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (cent_type in centrality_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      centralityplotting = TRUE,
      centralitytype = cent_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for centralitytype:", cent_type))
  }
})

test_that("jjdotplotstats respects centrality parameter options", {
  devtools::load_all()

  centrality_params <- c("mean", "median", "none")

  for (param in centrality_params) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      centralityplotting = TRUE,
      centralityparameter = param
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for centralityparameter:", param))
  }
})

test_that("jjdotplotstats respects centrality plotting option", {
  devtools::load_all()

  # Without centrality plotting
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With centrality plotting
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats respects results subtitle option", {
  devtools::load_all()

  # Without subtitle
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    resultssubtitle = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With subtitle
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats respects Bayes Factor message option", {
  devtools::load_all()

  # Without BF message
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes",
    bfmessage = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With BF message
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes",
    bfmessage = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats respects test value line option", {
  devtools::load_all()

  # Without reference line
  result1 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With reference line
  result2 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats respects confidence level parameter", {
  devtools::load_all()

  conf_levels <- c(0.80, 0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      conflevel = conf
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for conflevel:", conf))
  }
})

test_that("jjdotplotstats respects decimal places parameter", {
  devtools::load_all()

  k_values <- c(0, 1, 2, 3, 4, 5)

  for (k in k_values) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      k = k
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for k =", k))
  }
})

test_that("jjdotplotstats respects centrality k parameter", {
  devtools::load_all()

  centrality_k_values <- c(0, 1, 2, 3)

  for (cent_k in centrality_k_values) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      centralityplotting = TRUE,
      centralityk = cent_k
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for centralityk =", cent_k))
  }
})

test_that("jjdotplotstats respects custom title parameters", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    mytitle = "Custom Main Title",
    xtitle = "Custom X-axis Label",
    ytitle = "Custom Y-axis Label"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats respects plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 300, height = 300),
    list(width = 600, height = 450),
    list(width = 800, height = 600),
    list(width = 1200, height = 800)
  )

  for (dim in dimensions) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      plotwidth = dim$width,
      plotheight = dim$height
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("jjdotplotstats handles different group counts", {
  devtools::load_all()

  # Two groups
  result1 <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Three groups
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Four groups
  result3 <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose"
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles combinations of statistical type and effect size", {
  devtools::load_all()

  combinations <- expand.grid(
    typestatistics = c("parametric", "nonparametric", "robust"),
    effsizetype = c("biased", "unbiased", "eta", "omega"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      typestatistics = combinations$typestatistics[i],
      effsizetype = combinations$effsizetype[i]
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for typestatistics =", combinations$typestatistics[i],
                               "effsizetype =", combinations$effsizetype[i]))
  }
})

test_that("jjdotplotstats handles combinations of centrality options", {
  devtools::load_all()

  combinations <- expand.grid(
    centralitytype = c("parametric", "nonparametric", "robust"),
    centralityparameter = c("mean", "median", "none"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      centralityplotting = TRUE,
      centralitytype = combinations$centralitytype[i],
      centralityparameter = combinations$centralityparameter[i]
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for centralitytype =", combinations$centralitytype[i],
                               "centralityparameter =", combinations$centralityparameter[i]))
  }
})

test_that("jjdotplotstats handles comprehensive argument combinations", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "unbiased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    centralityparameter = "mean",
    resultssubtitle = TRUE,
    conflevel = 0.95,
    k = 2,
    centralityk = 2,
    mytitle = "Comprehensive Test",
    xtitle = "Tumor Reduction (%)",
    ytitle = "Treatment Group",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles grouped analysis with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      grvar = "hospital",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for grouped analysis with typestatistics:", stat_type))
  }
})

test_that("jjdotplotstats handles test value with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust")

  for (stat_type in stat_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_reference,
      dep = "bp_reduction",
      group = "drug",
      testvalue = 10,
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for test value with typestatistics:", stat_type))
  }
})

test_that("jjdotplotstats handles test value with centrality plotting", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE,
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles grouped analysis with centrality plotting", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles all display options combined", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    resultssubtitle = TRUE,
    mytitle = "Complete Display Test",
    xtitle = "X-axis",
    ytitle = "Y-axis"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles Bayesian analysis with all options", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes",
    bfmessage = TRUE,
    centralityplotting = TRUE,
    centralitytype = "bayes",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles high precision display", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    k = 3,
    centralityk = 3,
    centralityplotting = TRUE,
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles custom confidence with centrality", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    conflevel = 0.99,
    centralityplotting = TRUE,
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles publication-ready configuration", {
  devtools::load_all()

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "biased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    centralityparameter = "mean",
    resultssubtitle = TRUE,
    mytitle = "Tumor Response Across Treatment Arms",
    xtitle = "Tumor Reduction (%)",
    ytitle = "Treatment Group",
    conflevel = 0.95,
    k = 2,
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})
