
if (requireNamespace('jmvReadWrite') && requireNamespace('PMCMRplus') && requireNamespace('multcomp') && requireNamespace('pwr')) {
  devtools::load_all()
}

test_that('advancedanova analysis works: Tukey & Assumptions', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 60
  data <- data.frame(
    dependent = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2)),
    group = factor(rep(c('A', 'B', 'C'), each = 20))
  )

  # Run analysis
  expect_no_error({
    model <- advancedanova(
      data = data,
      dependent = 'dependent',
      fixed = 'group',
      covariates = NULL,
      wls = NULL,
      posthoc_method = 'tukey',
      assumptions = TRUE,
      effect_sizes = TRUE,
      confidence_level = 0.95
    )
  })

  # Check structure
  expect_true(inherits(model, 'jmvcoreClass'))
  
  # Check ANOVA table
  anova_table <- model$anova$asDF
  expect_equal(nrow(anova_table), 4) # Between, Within, Total, Effect Sizes
  expect_true('Effect Sizes' %in% anova_table$source)
  
  # Check Assumption tables
  expect_true(nrow(model$assumptions$asDF) > 0)
  
})

test_that('advancedanova: Games-Howell', {
    skip_if_not_installed('PMCMRplus')

    set.seed(123)
    data <- data.frame(
        dependent = c(rnorm(20, 10, 1), rnorm(20, 12, 5), rnorm(20, 15, 1)),
        group = factor(rep(c('A', 'B', 'C'), each = 20))
    )

    expect_no_error({
        model <- advancedanova(
            data = data,
            dependent = 'dependent',
            fixed = 'group',
            covariates = NULL,
            wls = NULL,
            posthoc_method = 'games_howell'
        )
    })
    
    # Check Games-Howell table existence
    expect_true(!is.null(model$gameshowell))
})

test_that('advancedanova: Dunnett', {
    skip_if_not_installed('multcomp')

    set.seed(123)
    data <- data.frame(
        dependent = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2)),
        group = factor(rep(c('Control', 'Treat1', 'Treat2'), each = 20))
    )

    expect_no_error({
        model <- advancedanova(
            data = data,
            dependent = 'dependent',
            fixed = 'group',
            covariates = NULL,
            wls = NULL,
            posthoc_method = 'dunnett',
            control_group = 'Control'
        )
    })
    
    # Check Dunnett table existence
    expect_true(!is.null(model$dunnett))
})

test_that('advancedanova: Power Analysis', {
    skip_if_not_installed('pwr')

    set.seed(123)
    data <- data.frame(
        dependent = c(rnorm(20, 10, 2), rnorm(20, 15, 2)),
        group = factor(rep(c('A', 'B'), each = 20))
    )

    expect_no_error({
        model <- advancedanova(
            data = data,
            dependent = 'dependent',
            fixed = 'group',
            covariates = NULL,
            wls = NULL,
            effect_sizes = TRUE
        )
    })
    
    # Verify Power Analysis row is populated
    anova_table <- model$anova$asDF
    power_row <- anova_table[anova_table$source == 'Observed Power', ]
    
    # If pwr is installed and the feature is enabled, this should exist
    if(requireNamespace('pwr', quietly=TRUE)) {
         expect_equal(nrow(power_row), 1)
         expect_true(as.numeric(power_row$eta_squared) > 0) # Power is stored in eta_squared col for convenience/hack
    }
})

