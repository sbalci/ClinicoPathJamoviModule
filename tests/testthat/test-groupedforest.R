test_that("groupedforest module files exist", {
  
  # Test that required files exist
  expect_true(file.exists("R/groupedforest.b.R"))
  expect_true(file.exists("jamovi/groupedforest.a.yaml"))
  expect_true(file.exists("jamovi/groupedforest.u.yaml"))
  expect_true(file.exists("jamovi/groupedforest.r.yaml"))
  expect_true(file.exists("R/groupedforest.h.R"))
})

test_that("groupedforest class and function availability", {
  
  # Load the package functions quietly
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  # Test that groupedforest class exists after loading
  expect_true(exists("groupedforestClass"))
  expect_true(exists("groupedforest"))
  
  # Test class inheritance
  if (exists("groupedforestClass")) {
    expect_true(inherits(groupedforestClass, "R6ClassGenerator"))
  }
})

test_that("groupedforest backend implementation structure", {
  
  # Read the backend file and check for key elements
  backend_content <- readLines("R/groupedforest.b.R", warn = FALSE)
  backend_text <- paste(backend_content, collapse = "\n")
  
  # Check for essential methods
  expect_true(grepl(".run\\s*=\\s*function", backend_text))
  expect_true(grepl(".plot_forest\\s*=\\s*function", backend_text))
  
  # Check for survival analysis integration
  expect_true(grepl("survival::", backend_text))
  expect_true(grepl("coxph", backend_text))
  expect_true(grepl("Surv", backend_text))
  
  # Check for grouped analysis functionality
  expect_true(grepl(".perform_grouped_analysis", backend_text))
  expect_true(grepl("forest.*plot", backend_text))
  expect_true(grepl("hazard.*ratio", backend_text))
  
  # Check for error handling
  expect_true(grepl("tryCatch", backend_text))
})

test_that("groupedforest YAML configurations are valid", {
  
  # Test analysis configuration
  if (requireNamespace("yaml", quietly = TRUE)) {
    analysis_config <- yaml::read_yaml("jamovi/groupedforest.a.yaml")
    
    expect_equal(analysis_config$name, "groupedforest")
    expect_true("title" %in% names(analysis_config))
    expect_true("options" %in% names(analysis_config))
    
    # Check for key options
    option_names <- sapply(analysis_config$options, function(x) x$name)
    expected_options <- c("time_var", "event_var", "treatment_var", "grouping_var")
    expect_true(all(expected_options %in% option_names))
    
    # Test results configuration
    results_config <- yaml::read_yaml("jamovi/groupedforest.r.yaml")
    item_names <- sapply(results_config$items, function(x) x$name)
    expect_true("forest_plot" %in% item_names)
    expect_true("statistics_table" %in% item_names)
  }
})

test_that("groupedforest test datasets exist and are properly structured", {
  
  # Test that datasets were created
  expect_true(file.exists("data/groupedforest_comprehensive_data.rda"))
  expect_true(file.exists("data/groupedforest_simple_data.rda"))
  expect_true(file.exists("data/groupedforest_multi_subgroups.rda"))
  expect_true(file.exists("data/groupedforest_precision_medicine.rda"))
  expect_true(file.exists("data/groupedforest_biomarker_data.rda"))
  expect_true(file.exists("data/groupedforest_interaction_data.rda"))
  
  # Load and test the main dataset
  load("data/groupedforest_comprehensive_data.rda")
  
  expect_s3_class(groupedforest_comprehensive_data, "data.frame")
  expect_gt(nrow(groupedforest_comprehensive_data), 150)
  expect_gt(ncol(groupedforest_comprehensive_data), 8)
  
  # Check essential columns for grouped forest analysis
  essential_cols <- c("survival_months", "death_event", "treatment", "biomarker_status")
  expect_true(all(essential_cols %in% names(groupedforest_comprehensive_data)))
  
  # Check data types
  expect_true(is.numeric(groupedforest_comprehensive_data$survival_months))
  expect_true(is.numeric(groupedforest_comprehensive_data$death_event))
  expect_true(is.factor(groupedforest_comprehensive_data$treatment))
  expect_true(is.factor(groupedforest_comprehensive_data$biomarker_status))
})

test_that("groupedforest handles basic survival data structure", {
  
  # Load basic survival data for testing
  load("data/basic_survival_data.rda")
  
  # Test that the basic survival data has required columns
  required_cols <- c("survival_months", "death_event", "treatment", "sex")
  expect_true(all(required_cols %in% names(basic_survival_data)))
  
  # Test data types
  expect_true(is.numeric(basic_survival_data$survival_months))
  expect_true(is.numeric(basic_survival_data$death_event))
  expect_true(is.factor(basic_survival_data$treatment))
})

test_that("groupedforest parameter combinations work", {
  
  # Load package quietly
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load test data
    load("data/basic_survival_data.rda")
    
    # Test basic groupedforest instantiation
    expect_error({
      result <- groupedforest(
        data = basic_survival_data,
        time_var = "survival_months",
        event_var = "death_event", 
        treatment_var = "treatment",
        grouping_var = "sex"
      )
    }, NA)  # Should not error during instantiation
  }
})

test_that("groupedforest with comprehensive dataset", {
  
  # Load package quietly
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load comprehensive test data
    load("data/groupedforest_comprehensive_data.rda")
    
    # Test with biomarker grouping
    expect_error({
      result <- groupedforest(
        data = groupedforest_comprehensive_data,
        time_var = "survival_months",
        event_var = "death_event",
        treatment_var = "treatment", 
        grouping_var = "biomarker_status",
        show_statistics = TRUE,
        show_overall = TRUE
      )
    }, NA)
    
    # Test with tumor stage grouping
    expect_error({
      result <- groupedforest(
        data = groupedforest_comprehensive_data,
        time_var = "survival_months",
        event_var = "death_event",
        treatment_var = "treatment",
        grouping_var = "tumor_stage",
        confidence_level = 0.95,
        sort_by_hr = TRUE
      )
    }, NA)
  }
})

test_that("groupedforest multiple subgroups dataset", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load multi-subgroups data
    load("data/groupedforest_multi_subgroups.rda")
    
    # Test with molecular subtype
    expect_error({
      result <- groupedforest(
        data = groupedforest_multi_subgroups,
        time_var = "time_to_event",
        event_var = "event_occurred",
        treatment_var = "intervention",
        grouping_var = "molecular_subtype",
        covariates = "patient_age"
      )
    }, NA)
    
    # Test with risk category
    expect_error({
      result <- groupedforest(
        data = groupedforest_multi_subgroups,
        time_var = "time_to_event", 
        event_var = "event_occurred",
        treatment_var = "intervention",
        grouping_var = "risk_category",
        show_counts = TRUE,
        interaction_test = TRUE
      )
    }, NA)
  }
})

test_that("groupedforest precision medicine dataset", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load precision medicine data
    load("data/groupedforest_precision_medicine.rda")
    
    # Test with genomic variants
    expect_error({
      result <- groupedforest(
        data = groupedforest_precision_medicine,
        time_var = "progression_free_months",
        event_var = "progression_event",
        treatment_var = "therapy_type",
        grouping_var = "genomic_variant",
        reference_treatment = "Chemotherapy"
      )
    }, NA)
    
    # Test with expression levels
    expect_error({
      result <- groupedforest(
        data = groupedforest_precision_medicine,
        time_var = "progression_free_months",
        event_var = "progression_event", 
        treatment_var = "therapy_type",
        grouping_var = "expression_level",
        covariates = c("age_at_diagnosis", "tumor_size")
      )
    }, NA)
  }
})

test_that("groupedforest biomarker stratification", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load biomarker data
    load("data/groupedforest_biomarker_data.rda")
    
    # Test with biomarker levels
    expect_error({
      result <- groupedforest(
        data = groupedforest_biomarker_data,
        time_var = "overall_survival_months",
        event_var = "death_indicator",
        treatment_var = "treatment_arm",
        grouping_var = "biomarker_level",
        plot_title = "Biomarker Stratified Analysis"
      )
    }, NA)
    
    # Test with pathway status
    expect_error({
      result <- groupedforest(
        data = groupedforest_biomarker_data,
        time_var = "overall_survival_months",
        event_var = "death_indicator",
        treatment_var = "treatment_arm",
        grouping_var = "pathway_status",
        hr_range = "wide",
        plot_theme = "publication"
      )
    }, NA)
  }
})

test_that("groupedforest clinical trial interaction data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Load interaction data
    load("data/groupedforest_interaction_data.rda")
    
    # Test with genetic profiles
    expect_error({
      result <- groupedforest(
        data = groupedforest_interaction_data,
        time_var = "event_free_survival",
        event_var = "event_status",
        treatment_var = "randomized_treatment",
        grouping_var = "genetic_profile",
        interaction_test = TRUE,
        export_data = TRUE
      )
    }, NA)
    
    # Test with disease severity
    expect_error({
      result <- groupedforest(
        data = groupedforest_interaction_data,
        time_var = "event_free_survival",
        event_var = "event_status", 
        treatment_var = "randomized_treatment",
        grouping_var = "disease_severity",
        covariates = c("age_years", "baseline_severity_score"),
        custom_hr_min = 0.2,
        custom_hr_max = 5.0,
        hr_range = "custom"
      )
    }, NA)
  }
})

test_that("groupedforest plot theme options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    load("data/groupedforest_simple_data.rda")
    
    # Test different plot themes
    themes <- c("clinical", "minimal", "classic", "publication")
    
    for (theme in themes) {
      expect_error({
        result <- groupedforest(
          data = groupedforest_simple_data,
          time_var = "time",
          event_var = "event",
          treatment_var = "treatment",
          grouping_var = "subgroup",
          plot_theme = theme
        )
      }, NA, info = paste("Theme:", theme))
    }
  }
})

test_that("groupedforest hazard ratio range options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    load("data/groupedforest_simple_data.rda")
    
    # Test different HR range options
    hr_ranges <- c("auto", "wide", "narrow", "custom")
    
    for (hr_range in hr_ranges) {
      expect_error({
        result <- groupedforest(
          data = groupedforest_simple_data,
          time_var = "time",
          event_var = "event",
          treatment_var = "treatment", 
          grouping_var = "subgroup",
          hr_range = hr_range,
          custom_hr_min = 0.1,
          custom_hr_max = 10
        )
      }, NA, info = paste("HR Range:", hr_range))
    }
  }
})

test_that("groupedforest confidence level options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    load("data/groupedforest_simple_data.rda")
    
    # Test different confidence levels
    conf_levels <- c(0.80, 0.90, 0.95, 0.99)
    
    for (conf_level in conf_levels) {
      expect_error({
        result <- groupedforest(
          data = groupedforest_simple_data,
          time_var = "time",
          event_var = "event",
          treatment_var = "treatment",
          grouping_var = "subgroup",
          confidence_level = conf_level
        )
      }, NA, info = paste("Confidence Level:", conf_level))
    }
  }
})

test_that("groupedforest display options combinations", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    load("data/groupedforest_comprehensive_data.rda")
    
    # Test comprehensive parameter combinations
    expect_error({
      result <- groupedforest(
        data = groupedforest_comprehensive_data,
        time_var = "survival_months",
        event_var = "death_event",
        treatment_var = "treatment",
        grouping_var = "biomarker_status",
        reference_treatment = "Control",
        show_overall = TRUE,
        show_statistics = TRUE,
        show_counts = TRUE,
        sort_by_hr = TRUE,
        interaction_test = TRUE,
        confidence_level = 0.95,
        plot_theme = "clinical",
        hr_range = "auto",
        plot_title = "Comprehensive Grouped Forest Analysis"
      )
    }, NA)
  }
})

test_that("groupedforest dependency handling", {
  
  # Test function behavior when required packages are available
  required_packages <- c("survival", "survminer", "ggplot2", "dplyr", "broom")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        suppressMessages(suppressWarnings(devtools::load_all()))
        if (exists("groupedforest")) {
          load("data/basic_survival_data.rda")
          result <- groupedforest(
            data = basic_survival_data,
            time_var = "survival_months",
            event_var = "death_event",
            treatment_var = "treatment",
            grouping_var = "sex"
          )
        }
      }, NA, info = paste("Package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("groupedforest with missing data handling", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    # Create data with missing values
    load("data/groupedforest_simple_data.rda")
    test_data <- groupedforest_simple_data
    
    # Introduce some missing values
    test_data$time[1:5] <- NA
    test_data$treatment[6:10] <- NA
    
    expect_error({
      result <- groupedforest(
        data = test_data,
        time_var = "time",
        event_var = "event",
        treatment_var = "treatment",
        grouping_var = "subgroup"
      )
    }, NA)  # Should handle missing data gracefully
  }
})

test_that("groupedforest required methods exist", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupedforest")) {
    load("data/basic_survival_data.rda")
    
    result <- groupedforest(
      data = basic_survival_data,
      time_var = "survival_months",
      event_var = "death_event",
      treatment_var = "treatment",
      grouping_var = "sex"
    )
    
    # Check that required methods exist
    expect_true(exists(".plot_forest", envir = result$.__enclos_env__$private))
    expect_true(exists(".run", envir = result$.__enclos_env__$private))
    expect_true(exists(".perform_grouped_analysis", envir = result$.__enclos_env__$private))
  }
})