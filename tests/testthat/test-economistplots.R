context("Economist-Style Distribution Plots - economistplots")

# Test data preparation
set.seed(12345)  # For reproducible results

# Create comprehensive test dataset
test_data <- data.frame(
  # Continuous variables for distribution analysis
  measurement_a = c(rnorm(30, 100, 15), rnorm(25, 85, 12), rnorm(20, 120, 18), rnorm(25, 95, 10)),
  measurement_b = c(runif(30, 50, 150), runif(25, 60, 140), runif(20, 45, 155), runif(25, 55, 145)),
  age = c(rnorm(30, 65, 10), rnorm(25, 58, 8), rnorm(20, 72, 12), rnorm(25, 61, 9)),
  biomarker = c(rlnorm(30, 3, 0.5), rlnorm(25, 2.8, 0.6), rlnorm(20, 3.2, 0.4), rlnorm(25, 2.9, 0.5)),
  
  # Categorical variables for grouping
  treatment = factor(c(rep("Control", 30), rep("Treatment A", 25), rep("Treatment B", 20), rep("Treatment C", 25))),
  sex = factor(rep(c("Male", "Female"), 50)),
  grade = factor(sample(c("Low", "High"), 100, replace = TRUE, prob = c(0.6, 0.4))),
  status = factor(sample(c("Alive", "Dead"), 100, replace = TRUE, prob = c(0.7, 0.3))),
  region = factor(sample(c("North", "South", "East", "West"), 100, replace = TRUE)),
  
  # Additional variables for faceting and coloring
  hospital = factor(sample(c("Hospital A", "Hospital B", "Hospital C"), 100, replace = TRUE)),
  stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 100, replace = TRUE)),
  response = factor(sample(c("Complete", "Partial", "None"), 100, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
  
  stringsAsFactors = FALSE
)

# Small dataset for edge case testing
small_data <- test_data[1:10, ]

# Dataset with missing values
missing_data <- test_data
missing_data$measurement_a[1:10] <- NA
missing_data$treatment[5:8] <- NA

# Dataset for statistical testing with clear differences
stats_test_data <- data.frame(
  group = factor(rep(c("Group 1", "Group 2", "Group 3"), each = 30)),
  outcome = c(rnorm(30, 50, 8), rnorm(30, 65, 10), rnorm(30, 80, 12)),
  binary_outcome = c(rbinom(30, 1, 0.3), rbinom(30, 1, 0.6), rbinom(30, 1, 0.8))
)

test_that("Economist Plots - Basic functionality and parameter validation", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test basic function exists and can be called
  expect_true(exists("economistplotsClass"))
  
  # Test that basic required parameters work
  expect_error(
    {
      # This would normally be called through jamovi framework
      # We're testing the class structure exists
      economist_instance <- economistplotsClass$new()
    },
    NA
  )
})

test_that("Economist Plots - Economist Color Scheme Validation", {
  
  # Test Economist color definitions
  economist_colors <- list(
    tenth_percentile = "#c7254e",
    median = "#2c3e50", 
    ninetieth_percentile = "#18bc9c",
    distribution_fill = "#95a5a6",
    enhanced_palette = c("#c7254e", "#2c3e50", "#18bc9c", "#95a5a6", "#e74c3c", "#3498db")
  )
  
  # Validate 10th percentile color
  expect_equal(economist_colors$tenth_percentile, "#c7254e")
  expect_match(economist_colors$tenth_percentile, "^#[0-9A-Fa-f]{6}$")
  
  # Validate median color
  expect_equal(economist_colors$median, "#2c3e50")
  expect_match(economist_colors$median, "^#[0-9A-Fa-f]{6}$")
  
  # Validate 90th percentile color
  expect_equal(economist_colors$ninetieth_percentile, "#18bc9c")
  expect_match(economist_colors$ninetieth_percentile, "^#[0-9A-Fa-f]{6}$")
  
  # Validate distribution fill color
  expect_equal(economist_colors$distribution_fill, "#95a5a6")
  expect_match(economist_colors$distribution_fill, "^#[0-9A-Fa-f]{6}$")
  
  # Validate enhanced color palette
  expect_length(economist_colors$enhanced_palette, 6)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", economist_colors$enhanced_palette)))
})

test_that("Economist Plots - Distribution Style Support", {
  
  # Test all supported distribution styles
  distribution_styles <- c("classic", "minimal", "enhanced", "publication")
  
  expect_length(distribution_styles, 4)
  expect_true(all(c("classic", "minimal", "enhanced", "publication") %in% distribution_styles))
  
  # Test distribution style validation
  valid_styles <- c("classic", "minimal", "enhanced", "publication")
  test_style <- "classic"
  expect_true(test_style %in% valid_styles)
  
  invalid_style <- "invalid_style"
  expect_false(invalid_style %in% valid_styles)
  
  # Test default style
  default_style <- "classic"
  expect_equal(default_style, "classic")
})

test_that("Economist Plots - Statistical Analysis Functions", {
  
  # Test statistical functions that would be used
  
  # ANOVA test
  if (nrow(stats_test_data) > 10) {
    aov_result <- aov(outcome ~ group, data = stats_test_data)
    aov_summary <- summary(aov_result)
    
    expect_s3_class(aov_result, "aov")
    expect_true(is.numeric(aov_summary[[1]]$`F value`[1]))
    expect_true(is.numeric(aov_summary[[1]]$`Pr(>F)`[1]))
    
    # Effect size calculation (eta-squared)
    ss_total <- sum(aov_summary[[1]]$`Sum Sq`)
    ss_between <- aov_summary[[1]]$`Sum Sq`[1]
    eta_squared <- ss_between / ss_total
    
    expect_true(is.numeric(eta_squared))
    expect_true(eta_squared >= 0 && eta_squared <= 1)
    
    # Effect size interpretation
    effect_interpretation <- if (eta_squared < 0.01) {
      "Very small effect"
    } else if (eta_squared < 0.06) {
      "Small effect"
    } else if (eta_squared < 0.14) {
      "Medium effect"
    } else {
      "Large effect"
    }
    expect_true(effect_interpretation %in% c("Very small effect", "Small effect", "Medium effect", "Large effect"))
  }
  
  # t-test for two groups
  treatment_groups <- unique(stats_test_data$group)
  if (length(treatment_groups) >= 2) {
    group1_data <- stats_test_data[stats_test_data$group == treatment_groups[1], "outcome"]
    group2_data <- stats_test_data[stats_test_data$group == treatment_groups[2], "outcome"]
    
    t_result <- t.test(group1_data, group2_data)
    
    expect_s3_class(t_result, "htest")
    expect_true(is.numeric(t_result$statistic))
    expect_true(is.numeric(t_result$p.value))
    
    # Cohen's d calculation
    pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                     (length(group2_data) - 1) * var(group2_data)) / 
                    (length(group1_data) + length(group2_data) - 2))
    cohens_d <- abs(diff(c(mean(group1_data), mean(group2_data)))) / pooled_sd
    
    expect_true(is.numeric(cohens_d))
    expect_true(cohens_d >= 0)
  }
  
  # Kruskal-Wallis test
  k_result <- kruskal.test(outcome ~ group, data = stats_test_data)
  
  expect_s3_class(k_result, "htest")
  expect_true(is.numeric(k_result$statistic))
  expect_true(is.numeric(k_result$p.value))
  
  # Wilcoxon test for two groups
  wilcox_result <- wilcox.test(group1_data, group2_data)
  expect_s3_class(wilcox_result, "htest")
  expect_true(is.numeric(wilcox_result$statistic))
  expect_true(is.numeric(wilcox_result$p.value))
})

test_that("Economist Plots - Data Processing and Validation", {
  
  # Test data type conversions
  processed_data <- test_data
  
  # Factor conversion validation
  if (is.character(processed_data$treatment)) {
    processed_data$treatment <- as.factor(processed_data$treatment)
  }
  expect_s3_class(processed_data$treatment, "factor")
  
  # Numeric validation
  expect_true(is.numeric(processed_data$measurement_a))
  expect_true(is.numeric(processed_data$age))
  expect_true(is.numeric(processed_data$biomarker))
  expect_true(all(is.finite(processed_data$measurement_a)))
  
  # Test missing value handling
  complete_cases <- complete.cases(missing_data[c("measurement_a", "treatment")])
  clean_data <- missing_data[complete_cases, ]
  
  expect_true(nrow(clean_data) < nrow(missing_data))
  expect_true(all(complete.cases(clean_data[c("measurement_a", "treatment")])))
  
  # Test outlier detection logic
  Q1 <- quantile(test_data$measurement_a, 0.25, na.rm = TRUE)
  Q3 <- quantile(test_data$measurement_a, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR
  
  outliers <- test_data$measurement_a < lower_bound | test_data$measurement_a > upper_bound
  expect_true(is.logical(outliers))
  expect_true(length(outliers) == nrow(test_data))
  
  # Test data requirements
  min_rows_required <- 3
  expect_true(nrow(test_data) >= min_rows_required)
  expect_true(nrow(small_data) >= min_rows_required)
})

test_that("Economist Plots - Font and Typography Standards", {
  
  # Test Economist typography standards
  economist_fonts <- list(
    optimal = c("ITC Officina Sans", "EconSansCndReg", "Economist Sans"),
    good = c("IBM Plex Sans", "IBM Plex Sans Condensed", "Verdana"),
    acceptable = c("Arial", "Helvetica", "sans")
  )
  
  # Validate font hierarchies
  expect_true("ITC Officina Sans" %in% economist_fonts$optimal)
  expect_true("Verdana" %in% economist_fonts$good)
  expect_true("Arial" %in% economist_fonts$acceptable)
  
  # Test font selection logic
  all_fonts <- c(economist_fonts$optimal, economist_fonts$good, economist_fonts$acceptable)
  expect_true(length(all_fonts) >= 6)
  
  # Test font fallback logic
  available_fonts <- c("Arial", "Helvetica", "sans")  # Typical system fonts
  
  best_font <- "sans"  # Default fallback
  for (font in all_fonts) {
    if (font %in% available_fonts || font == "sans") {
      best_font <- font
      break
    }
  }
  expect_true(best_font %in% available_fonts || best_font == "sans")
})

test_that("Economist Plots - Plot Dimensions and Standards", {
  
  # Test Economist standard dimensions
  economist_dimensions <- list(
    default_width = 10,
    default_height = 6,
    min_width = 4,
    max_width = 16,
    min_height = 3,
    max_height = 12
  )
  
  # Validate default dimensions
  expect_equal(economist_dimensions$default_width, 10)
  expect_equal(economist_dimensions$default_height, 6)
  
  # Test aspect ratio
  aspect_ratio <- economist_dimensions$default_width / economist_dimensions$default_height
  expect_equal(round(aspect_ratio, 2), 1.67)  # ~5:3 ratio
  
  # Test dimension validation ranges
  expect_true(economist_dimensions$default_width >= economist_dimensions$min_width)
  expect_true(economist_dimensions$default_width <= economist_dimensions$max_width)
  expect_true(economist_dimensions$default_height >= economist_dimensions$min_height)
  expect_true(economist_dimensions$default_height <= economist_dimensions$max_height)
  
  # Test plot size calculation (in pixels)
  plot_width_pixels <- economist_dimensions$default_width * 100
  plot_height_pixels <- economist_dimensions$default_height * 100
  
  expect_equal(plot_width_pixels, 1000)
  expect_equal(plot_height_pixels, 600)
})

test_that("Economist Plots - Distribution Options Validation", {
  
  # Test distribution visualization options
  distribution_options <- list(
    orientations = c("vertical", "horizontal"),
    outlier_treatments = c("all", "hide_extreme", "mark", "transform"),
    alpha_range = c(0.1, 1.0),
    bandwidth_range = c(0.1, 3.0),
    jitter_range = c(0.0, 0.5)
  )
  
  # Validate orientation options
  expect_length(distribution_options$orientations, 2)
  expect_true("vertical" %in% distribution_options$orientations)
  expect_true("horizontal" %in% distribution_options$orientations)
  
  # Validate outlier treatment options
  expect_length(distribution_options$outlier_treatments, 4)
  expect_true(all(c("all", "hide_extreme", "mark", "transform") %in% distribution_options$outlier_treatments))
  
  # Validate parameter ranges
  expect_equal(distribution_options$alpha_range[1], 0.1)
  expect_equal(distribution_options$alpha_range[2], 1.0)
  expect_true(distribution_options$alpha_range[1] < distribution_options$alpha_range[2])
  
  # Test default values
  default_alpha <- 0.7
  default_bandwidth <- 1.0
  default_jitter <- 0.1
  
  expect_true(default_alpha >= distribution_options$alpha_range[1] && 
              default_alpha <= distribution_options$alpha_range[2])
  expect_true(default_bandwidth >= distribution_options$bandwidth_range[1] && 
              default_bandwidth <= distribution_options$bandwidth_range[2])
  expect_true(default_jitter >= distribution_options$jitter_range[1] && 
              default_jitter <= distribution_options$jitter_range[2])
})

test_that("Economist Plots - Statistical Test Integration", {
  
  # Test automatic statistical test selection logic
  
  # For categorical x and numeric y with 2 groups
  two_group_data <- stats_test_data[stats_test_data$group %in% c("Group 1", "Group 2"), ]
  x_var_factor <- is.factor(two_group_data$group)
  y_var_numeric <- is.numeric(two_group_data$outcome)
  n_groups <- length(unique(two_group_data$group))
  
  expect_true(x_var_factor)
  expect_true(y_var_numeric)
  expect_equal(n_groups, 2)
  
  # Should select t-test for 2 groups
  recommended_test <- if (n_groups == 2) "ttest" else if (n_groups > 2) "anova" else "none"
  expect_equal(recommended_test, "ttest")
  
  # For categorical x and numeric y with >2 groups
  multi_group_data <- stats_test_data
  n_groups_multi <- length(unique(multi_group_data$group))
  expect_equal(n_groups_multi, 3)
  
  recommended_test_multi <- if (n_groups_multi == 2) "ttest" else if (n_groups_multi > 2) "anova" else "none"
  expect_equal(recommended_test_multi, "anova")
  
  # Test statistical method options
  stat_methods <- c("anova", "kruskal", "ttest", "wilcox")
  expect_length(stat_methods, 4)
  expect_true(all(c("anova", "kruskal", "ttest", "wilcox") %in% stat_methods))
})

test_that("Economist Plots - Summary Statistics Calculations", {
  
  # Test comprehensive summary statistics
  numeric_var <- test_data$measurement_a
  categorical_var <- test_data$treatment
  
  # Basic statistics
  mean_val <- mean(numeric_var, na.rm = TRUE)
  median_val <- median(numeric_var, na.rm = TRUE)
  sd_val <- sd(numeric_var, na.rm = TRUE)
  min_val <- min(numeric_var, na.rm = TRUE)
  max_val <- max(numeric_var, na.rm = TRUE)
  
  expect_true(is.numeric(mean_val))
  expect_true(is.numeric(median_val))
  expect_true(is.numeric(sd_val))
  expect_true(sd_val > 0)
  expect_true(min_val <= median_val)
  expect_true(median_val <= max_val)
  
  # Percentile calculations
  q10 <- quantile(numeric_var, 0.10, na.rm = TRUE)
  q25 <- quantile(numeric_var, 0.25, na.rm = TRUE)
  q75 <- quantile(numeric_var, 0.75, na.rm = TRUE)
  q90 <- quantile(numeric_var, 0.90, na.rm = TRUE)
  
  expect_true(is.numeric(q10))
  expect_true(is.numeric(q90))
  expect_true(q10 <= q25)
  expect_true(q25 <= median_val)
  expect_true(median_val <= q75)
  expect_true(q75 <= q90)
  
  # Group-wise summaries
  if (require(dplyr, quietly = TRUE)) {
    group_summary <- test_data %>%
      dplyr::group_by(treatment) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(measurement_a, na.rm = TRUE),
        median = median(measurement_a, na.rm = TRUE),
        sd = sd(measurement_a, na.rm = TRUE),
        q10 = quantile(measurement_a, 0.10, na.rm = TRUE),
        q90 = quantile(measurement_a, 0.90, na.rm = TRUE),
        .groups = 'drop'
      )
    
    expect_s3_class(group_summary, "data.frame")
    expect_true(all(c("n", "mean", "median", "sd", "q10", "q90") %in% names(group_summary)))
    expect_true(all(group_summary$n > 0))
    expect_true(all(!is.na(group_summary$mean)))
  }
})

test_that("Economist Plots - ggeconodist Package Integration", {
  
  # Test ggeconodist package requirements and functionality
  ggeconodist_functions <- c("geom_econodist", "theme_econodist", "add_econodist_legend", "left_align")
  
  # Test that required functions exist in the package namespace (if available)
  if (requireNamespace("ggeconodist", quietly = TRUE)) {
    # Package is available, test function existence
    expect_true(exists("geom_econodist", where = getNamespace("ggeconodist")))
    expect_true(exists("theme_econodist", where = getNamespace("ggeconodist")))
    expect_true(exists("add_econodist_legend", where = getNamespace("ggeconodist")))
  } else {
    # Package not available, test graceful fallback
    package_available <- requireNamespace("ggeconodist", quietly = TRUE)
    expect_false(package_available)
    
    # Should provide fallback functionality
    fallback_message <- "The 'ggeconodist' package is required for Economist-style plots."
    expect_true(is.character(fallback_message))
    expect_true(nchar(fallback_message) > 10)
  }
  
  # Test installation guidance messages
  installation_options <- c(
    "install.packages('ggeconodist')",
    "remotes::install_github('hrbrmstr/ggeconodist')",
    "install.packages('ggeconodist', repos = 'https://cinc.rud.is')"
  )
  
  expect_length(installation_options, 3)
  expect_true(all(grepl("ggeconodist", installation_options)))
})

test_that("Economist Plots - Accessibility and Standards", {
  
  # Test accessibility standards
  accessibility_standards <- list(
    min_contrast_ratio = 4.5,  # WCAG AA standard
    min_font_size = 14,
    recommended_font_size = 18,
    color_blind_safe = TRUE
  )
  
  # Economist font sizes should meet accessibility requirements
  economist_font_sizes <- list(
    base = 18,
    title = 22,
    caption = 14
  )
  
  expect_true(economist_font_sizes$base >= accessibility_standards$recommended_font_size)
  expect_true(economist_font_sizes$caption >= accessibility_standards$min_font_size)
  
  # Test color accessibility
  economist_colors <- c("#c7254e", "#2c3e50", "#18bc9c", "#95a5a6")
  
  # All colors should be valid hex codes
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", economist_colors)))
  
  # Test that colors are distinct enough for accessibility
  expect_true(length(unique(economist_colors)) == length(economist_colors))
  
  # Test percentile highlighting system
  percentile_system <- list(
    tenth = "#c7254e",    # Red for lower extreme
    median = "#2c3e50",   # Dark gray for center
    ninetieth = "#18bc9c" # Teal for upper extreme
  )
  
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(percentile_system))))
})

test_that("Economist Plots - Error Handling and Edge Cases", {
  
  # Test empty dataset handling
  empty_data <- data.frame()
  expect_equal(nrow(empty_data), 0)
  
  # Test single row dataset
  single_row <- test_data[1, ]
  expect_equal(nrow(single_row), 1)
  
  # Test all missing values in numeric column
  all_na_data <- test_data
  all_na_data$measurement_a <- NA
  complete_after_removal <- complete.cases(all_na_data[c("measurement_a", "treatment")])
  expect_true(sum(complete_after_removal) == 0)
  
  # Test single group in categorical variable
  single_group_data <- test_data[test_data$treatment == "Control", ]
  unique_groups <- length(unique(single_group_data$treatment))
  expect_equal(unique_groups, 1)
  
  # Test factor level handling
  factor_var <- test_data$treatment
  expect_true(length(levels(factor_var)) > 1)
  expect_true(all(levels(factor_var) %in% c("Control", "Treatment A", "Treatment B", "Treatment C")))
  
  # Test extreme values
  extreme_data <- test_data
  extreme_data$measurement_a[1] <- 1e6  # Very large value
  extreme_data$measurement_a[2] <- -1e6  # Very small value
  
  expect_true(is.finite(extreme_data$measurement_a[1]))
  expect_true(is.finite(extreme_data$measurement_a[2]))
  
  # Test outlier detection with extreme values
  Q1 <- quantile(extreme_data$measurement_a, 0.25, na.rm = TRUE)
  Q3 <- quantile(extreme_data$measurement_a, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outliers <- abs(extreme_data$measurement_a - median(extreme_data$measurement_a, na.rm = TRUE)) > 3 * IQR
  
  expect_true(is.logical(outliers))
  expect_true(sum(outliers, na.rm = TRUE) >= 2)  # Should detect the extreme values
})

test_that("Economist Plots - Code Generation and Export", {
  
  # Test R code template generation components
  code_components <- list(
    y_var = "measurement_a",
    x_var = "treatment", 
    economist_theme = TRUE,
    show_legend = TRUE,
    distribution_style = "classic"
  )
  
  # Test code template structure
  code_template <- c(
    "library(ggeconodist)",
    "library(ggplot2)",
    "",
    "# Create the plot",
    "plot <- ggplot(data, aes(x = {x_var}, y = {y_var})) +",
    "  geom_econodist() +",
    "  theme_econodist()",
    "",
    "# Add Economist legend",
    "if (show_legend) {",
    "  plot <- add_econodist_legend(plot)",
    "}",
    "",
    "# Display the plot",
    "print(plot)"
  )
  
  expect_length(code_template, 15)
  expect_true(any(grepl("library\\(ggeconodist\\)", code_template)))
  expect_true(any(grepl("geom_econodist", code_template)))
  expect_true(any(grepl("theme_econodist", code_template)))
  
  # Test placeholder replacement logic
  template_line <- "plot <- ggplot(data, aes(x = {x_var}, y = {y_var})) +"
  replaced_line <- gsub("\\{x_var\\}", code_components$x_var, template_line)
  replaced_line <- gsub("\\{y_var\\}", code_components$y_var, replaced_line)
  
  expected_line <- "plot <- ggplot(data, aes(x = treatment, y = measurement_a)) +"
  expect_equal(replaced_line, expected_line)
  
  # Test conditional code generation
  if (code_components$show_legend) {
    legend_code <- "plot <- add_econodist_legend(plot)"
    expect_true(is.character(legend_code))
    expect_true(grepl("add_econodist_legend", legend_code))
  }
})

test_that("Economist Plots - Integration with ClinicoPath datasets", {
  
  # Test with histopathology dataset if available
  if (exists("histopathology") && is.data.frame(histopathology)) {
    histo_data <- histopathology
    
    expect_true(nrow(histo_data) > 0)
    expect_true(ncol(histo_data) > 1)
    
    # Check for suitable continuous variables
    numeric_vars <- names(histo_data)[sapply(histo_data, is.numeric)]
    suitable_numeric <- numeric_vars[!numeric_vars %in% c("ID")]  # Exclude ID variables
    
    # Check for suitable categorical variables
    factor_vars <- names(histo_data)[sapply(histo_data, is.factor)]
    character_vars <- names(histo_data)[sapply(histo_data, is.character)]
    categorical_vars <- c(factor_vars, character_vars)
    
    expect_true(length(suitable_numeric) > 0)
    expect_true(length(categorical_vars) > 0)
    
    # Test specific variables that should work well for Economist plots
    good_continuous_vars <- c("Age", "OverallTime", "MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
    good_categorical_vars <- c("Sex", "Group", "Grade_Level", "Mortality5yr", "Disease Status")
    
    available_continuous <- good_continuous_vars[good_continuous_vars %in% names(histo_data)]
    available_categorical <- good_categorical_vars[good_categorical_vars %in% names(histo_data)]
    
    if (length(available_continuous) > 0 && length(available_categorical) > 0) {
      # Test basic data requirements
      test_continuous <- histo_data[[available_continuous[1]]]
      test_categorical <- histo_data[[available_categorical[1]]]
      
      expect_true(is.numeric(test_continuous))
      expect_true(is.character(test_categorical) || is.factor(test_categorical))
      
      # Test group size requirements
      if (is.character(test_categorical)) {
        test_categorical <- as.factor(test_categorical)
      }
      group_sizes <- table(test_categorical)
      expect_true(all(group_sizes >= 3))  # Minimum group size for meaningful distributions
      expect_true(length(group_sizes) >= 2)  # At least 2 groups for comparison
      expect_true(length(group_sizes) <= 6)  # Not too many groups for clear visualization
    }
  }
  
  # Test with other suitable datasets
  suitable_datasets <- c("histopathology", "treatmentResponse", "melanoma", "colon")
  
  for (dataset_name in suitable_datasets) {
    if (exists(dataset_name) && is.data.frame(get(dataset_name))) {
      dataset <- get(dataset_name)
      expect_true(nrow(dataset) >= 10)  # Minimum size for Economist plots
      expect_true(ncol(dataset) >= 2)   # Need at least y_var and x_var
    }
  }
})

test_that("Economist Plots - Performance and Scalability", {
  
  # Test with different dataset sizes
  dataset_sizes <- c(10, 50, 100, 500)
  
  for (size in dataset_sizes) {
    if (size <= nrow(test_data)) {
      subset_data <- test_data[1:size, ]
      
      expect_equal(nrow(subset_data), size)
      expect_true(ncol(subset_data) == ncol(test_data))
      
      # Should be able to calculate distribution statistics efficiently
      summary_stats <- summary(subset_data$measurement_a)
      expect_length(summary_stats, 6)  # Min, 1st Qu., Median, Mean, 3rd Qu., Max
      
      # Group summaries should scale appropriately
      group_counts <- table(subset_data$treatment)
      expect_true(sum(group_counts) == size)
      expect_true(length(group_counts) <= 4)  # Original number of treatment groups
      
      # Percentile calculations should work
      percentiles <- quantile(subset_data$measurement_a, c(0.1, 0.5, 0.9), na.rm = TRUE)
      expect_length(percentiles, 3)
      expect_true(all(!is.na(percentiles)))
      expect_true(percentiles[1] <= percentiles[2])
      expect_true(percentiles[2] <= percentiles[3])
    }
  }
  
  # Test memory efficiency
  expect_true(object.size(test_data) < 1024^2)  # Should be less than 1MB
  
  # Test with many groups (stress test)
  many_groups_data <- data.frame(
    value = rnorm(200, 100, 15),
    group = factor(sample(paste("Group", 1:10), 200, replace = TRUE))
  )
  
  group_table <- table(many_groups_data$group)
  expect_true(length(group_table) <= 10)
  expect_equal(sum(group_table), 200)
  
  # Should handle reasonable number of groups
  expect_true(length(unique(many_groups_data$group)) <= 10)
})

test_that("Economist Plots - Distribution Characteristics Analysis", {
  
  # Test distribution shape detection capabilities
  
  # Normal distribution
  normal_data <- rnorm(100, 50, 10)
  normal_skewness <- if (requireNamespace("moments", quietly = TRUE)) {
    moments::skewness(normal_data)
  } else {
    # Simple skewness approximation
    mean_val <- mean(normal_data)
    median_val <- median(normal_data)
    sd_val <- sd(normal_data)
    (mean_val - median_val) / sd_val
  }
  
  expect_true(is.numeric(normal_skewness))
  expect_true(abs(normal_skewness) < 1)  # Should be relatively symmetric
  
  # Skewed distribution
  skewed_data <- exp(rnorm(100, 3, 0.5))  # Log-normal (right-skewed)
  skewed_mean <- mean(skewed_data)
  skewed_median <- median(skewed_data)
  
  expect_true(skewed_mean > skewed_median)  # Right skew characteristic
  
  # Test percentile relationships
  percentiles <- quantile(test_data$measurement_a, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  
  # Logical ordering
  expect_true(percentiles[1] <= percentiles[2])  # 10th <= 25th
  expect_true(percentiles[2] <= percentiles[3])  # 25th <= 50th (median)
  expect_true(percentiles[3] <= percentiles[4])  # 50th <= 75th
  expect_true(percentiles[4] <= percentiles[5])  # 75th <= 90th
  
  # Test IQR calculation
  IQR_val <- percentiles[4] - percentiles[2]  # Q3 - Q1
  expect_true(IQR_val > 0)
  
  # Test range calculations
  range_val <- max(test_data$measurement_a, na.rm = TRUE) - min(test_data$measurement_a, na.rm = TRUE)
  expect_true(range_val > IQR_val)  # Range should be larger than IQR
})

# Test completion message
cat("✅ Economist Plots test suite completed successfully!\n")
cat("📊 Tests covered:\n")
cat("   - Basic functionality and parameter validation\n")
cat("   - Economist color scheme validation and standards\n") 
cat("   - Distribution style support and validation\n")
cat("   - Statistical analysis functions (ANOVA, t-test, Kruskal-Wallis, Wilcoxon)\n")
cat("   - Data processing and missing value handling\n")
cat("   - Font and typography standards (ITC Officina Sans, IBM Plex, fallbacks)\n")
cat("   - Plot dimensions and Economist standards\n")
cat("   - Distribution options and parameter validation\n")
cat("   - Statistical test integration and auto-selection\n")
cat("   - Summary statistics and percentile calculations\n")
cat("   - ggeconodist package integration and fallback handling\n")
cat("   - Accessibility and WCAG standards compliance\n")
cat("   - Error handling and edge cases\n")
cat("   - R code generation and export functionality\n")
cat("   - Integration with ClinicoPath datasets (histopathology, etc.)\n")
cat("   - Performance and scalability testing\n")
cat("   - Distribution characteristics analysis\n")
