test_that("groupsummary module files exist", {
  
  # Test that required files exist
  expect_true(file.exists("R/groupsummary.b.R"))
  expect_true(file.exists("jamovi/groupsummary.a.yaml"))
  expect_true(file.exists("jamovi/groupsummary.u.yaml"))
  expect_true(file.exists("jamovi/groupsummary.r.yaml"))
  expect_true(file.exists("R/groupsummary.h.R"))
})

test_that("groupsummary class and function availability", {
  
  # Load the package functions quietly
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  # Test that groupsummary class exists after loading
  expect_true(exists("groupsummaryClass"))
  expect_true(exists("groupsummary"))
  
  # Test class inheritance
  if (exists("groupsummaryClass")) {
    expect_true(inherits(groupsummaryClass, "R6ClassGenerator"))
  }
})

test_that("groupsummary backend implementation structure", {
  
  # Read the backend file and check for key elements
  backend_content <- readLines("R/groupsummary.b.R", warn = FALSE)
  backend_text <- paste(backend_content, collapse = "\n")
  
  # Check for essential methods
  expect_true(grepl(".run\\s*=\\s*function", backend_text))
  expect_true(grepl(".plot\\s*=\\s*function", backend_text))
  
  # Check for data aggregation functionality
  expect_true(grepl("dplyr::group_by", backend_text))
  expect_true(grepl("dplyr::summarise", backend_text))
  expect_true(grepl("summary_funs", backend_text))
  
  # Check for date handling
  expect_true(grepl("lubridate::", backend_text))
  expect_true(grepl("dateVar", backend_text))
  expect_true(grepl("timeAggregation", backend_text))
  
  # Check for statistics options
  expect_true(grepl("sum\\(", backend_text))
  expect_true(grepl("mean\\(", backend_text))
  expect_true(grepl("median\\(", backend_text))
})

test_that("groupsummary YAML configurations are valid", {
  
  # Test analysis configuration
  if (requireNamespace("yaml", quietly = TRUE)) {
    analysis_config <- yaml::read_yaml("jamovi/groupsummary.a.yaml")
    
    expect_equal(analysis_config$name, "groupsummary")
    expect_true("title" %in% names(analysis_config))
    expect_true("options" %in% names(analysis_config))
    
    # Check for key options
    option_names <- sapply(analysis_config$options, function(x) x$name)
    expected_options <- c("groupVars", "sumVars", "statistics", "dateVar")
    expect_true(all(expected_options %in% option_names))
    
    # Test results configuration
    results_config <- yaml::read_yaml("jamovi/groupsummary.r.yaml")
    item_names <- sapply(results_config$items, function(x) x$name)
    expect_true("summaryTable" %in% item_names)
    expect_true("plot" %in% item_names)
  }
})

test_that("groupsummary test datasets exist and are properly structured", {
  
  # Test that datasets were created
  expect_true(file.exists("data/groupsummary_simple.rda"))
  expect_true(file.exists("data/groupsummary_sales_data.rda"))
  expect_true(file.exists("data/groupsummary_survey_data.rda"))
  expect_true(file.exists("data/groupsummary_financial_data.rda"))
  expect_true(file.exists("data/groupsummary_manufacturing_data.rda"))
  expect_true(file.exists("data/groupsummary_web_analytics.rda"))
  expect_true(file.exists("data/medical_research_data.rda"))
  expect_true(file.exists("data/hospital_admission_hourly.rda"))
  
  # Load and test the simple dataset
  load("data/groupsummary_simple.rda")
  
  expect_s3_class(groupsummary_simple, "data.frame")
  expect_gt(nrow(groupsummary_simple), 50)
  expect_gt(ncol(groupsummary_simple), 3)
  
  # Check essential columns for basic grouping
  essential_cols <- c("category", "group", "value1", "value2")
  expect_true(all(essential_cols %in% names(groupsummary_simple)))
  
  # Check data types
  expect_true(is.factor(groupsummary_simple$category))
  expect_true(is.factor(groupsummary_simple$group))
  expect_true(is.numeric(groupsummary_simple$value1))
  expect_true(is.numeric(groupsummary_simple$value2))
})

test_that("groupsummary handles basic categorical grouping", {
  
  # Load package quietly
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load simple test data
    load("data/groupsummary_simple.rda")
    
    # Test basic groupsummary instantiation
    expect_error({
      result <- groupsummary(
        data = groupsummary_simple,
        groupVars = "category",
        sumVars = "value1",
        statistics = c("sum", "mean", "n")
      )
    }, NA)  # Should not error during instantiation
  }
})

test_that("groupsummary with multiple grouping variables", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load sales data
    load("data/groupsummary_sales_data.rda")
    
    # Test multiple grouping variables
    expect_error({
      result <- groupsummary(
        data = groupsummary_sales_data,
        groupVars = c("region", "product_category"),
        sumVars = c("sales_amount", "quantity_sold"),
        statistics = c("sum", "mean", "median"),
        addPercentage = TRUE
      )
    }, NA)
  }
})

test_that("groupsummary with date aggregation", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load sales data with dates
    load("data/groupsummary_sales_data.rda")
    
    # Test date aggregation by month
    expect_error({
      result <- groupsummary(
        data = groupsummary_sales_data,
        groupVars = "sale_date",
        sumVars = "sales_amount",
        statistics = c("sum", "n"),
        dateVar = "sale_date",
        dateFormat = "ymd",
        timeAggregation = "month"
      )
    }, NA)
    
    # Test date aggregation by week
    expect_error({
      result <- groupsummary(
        data = groupsummary_sales_data,
        groupVars = "sale_date",
        sumVars = "quantity_sold",
        statistics = c("sum", "mean"),
        dateVar = "sale_date",
        dateFormat = "ymd", 
        timeAggregation = "week"
      )
    }, NA)
  }
})

test_that("groupsummary with timestamp data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load financial data with timestamps
    load("data/groupsummary_financial_data.rda")
    
    # Test hourly aggregation
    expect_error({
      result <- groupsummary(
        data = groupsummary_financial_data,
        groupVars = "transaction_time",
        sumVars = c("transaction_amount", "volume_traded"),
        statistics = c("sum", "mean", "n"),
        dateVar = "transaction_time",
        dateFormat = "ymd_hms",
        timeAggregation = "hour"
      )
    }, NA)
    
    # Test daily aggregation with grouping
    expect_error({
      result <- groupsummary(
        data = groupsummary_financial_data,
        groupVars = c("transaction_time", "market_sector"),
        sumVars = "transaction_amount",
        statistics = c("sum", "mean"),
        dateVar = "transaction_time",
        dateFormat = "ymd_hms",
        timeAggregation = "day"
      )
    }, NA)
  }
})

test_that("groupsummary statistics options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_simple.rda")
    
    # Test individual statistics
    statistics_options <- list(
      c("sum"),
      c("mean"),
      c("median"),
      c("n"),
      c("sum", "mean"),
      c("mean", "median", "n"),
      c("sum", "mean", "median", "n")
    )
    
    for (stats in statistics_options) {
      expect_error({
        result <- groupsummary(
          data = groupsummary_simple,
          groupVars = "category",
          sumVars = "value1",
          statistics = stats
        )
      }, NA, info = paste("Statistics:", paste(stats, collapse = ", ")))
    }
  }
})

test_that("groupsummary with survey data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load survey data
    load("data/groupsummary_survey_data.rda")
    
    # Test demographic analysis
    expect_error({
      result <- groupsummary(
        data = groupsummary_survey_data,
        groupVars = c("age_group", "gender"),
        sumVars = c("satisfaction_score", "trust_score"),
        statistics = c("mean", "median", "n"),
        sortBy = "first_desc"
      )
    }, NA)
    
    # Test education level analysis
    expect_error({
      result <- groupsummary(
        data = groupsummary_survey_data,
        groupVars = "education",
        sumVars = c("satisfaction_score", "likelihood_recommend"),
        statistics = c("mean", "n"),
        addPercentage = FALSE
      )
    }, NA)
  }
})

test_that("groupsummary with manufacturing data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load manufacturing data
    load("data/groupsummary_manufacturing_data.rda")
    
    # Test shift and quality analysis
    expect_error({
      result <- groupsummary(
        data = groupsummary_manufacturing_data,
        groupVars = c("shift", "quality_grade"),
        sumVars = c("units_produced", "defect_count"),
        statistics = c("sum", "mean", "n")
      )
    }, NA)
    
    # Test monthly production trends
    expect_error({
      result <- groupsummary(
        data = groupsummary_manufacturing_data,
        groupVars = "production_date",
        sumVars = c("units_produced", "efficiency_percent"),
        statistics = c("sum", "mean"),
        dateVar = "production_date",
        dateFormat = "ymd",
        timeAggregation = "month"
      )
    }, NA)
  }
})

test_that("groupsummary with web analytics data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load web analytics data
    load("data/groupsummary_web_analytics.rda")
    
    # Test traffic source analysis
    expect_error({
      result <- groupsummary(
        data = groupsummary_web_analytics,
        groupVars = c("traffic_source", "device_type"),
        sumVars = c("page_views", "session_duration_minutes"),
        statistics = c("sum", "mean", "n"),
        sortBy = "first_desc"
      )
    }, NA)
    
    # Test daily traffic patterns
    expect_error({
      result <- groupsummary(
        data = groupsummary_web_analytics,
        groupVars = "session_time",
        sumVars = c("page_views", "conversion_value"),
        statistics = c("sum", "mean"),
        dateVar = "session_time",
        dateFormat = "ymd_hms",
        timeAggregation = "day"
      )
    }, NA)
  }
})

test_that("groupsummary with medical research data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load medical data
    load("data/medical_research_data.rda")
    
    # Test multi-center treatment analysis
    expect_error({
      result <- groupsummary(
        data = medical_research_data,
        groupVars = c("StudyCenter", "TreatmentGroup"),
        sumVars = c("ClinicalScore", "SystolicBP"),
        statistics = c("mean", "n"),
        showMissing = FALSE
      )
    }, NA)
    
    # Test monthly visit patterns
    expect_error({
      result <- groupsummary(
        data = medical_research_data,
        groupVars = "VisitDate",
        sumVars = "VisitNumber",
        statistics = c("n"),
        dateVar = "VisitDate",
        dateFormat = "ymd",
        timeAggregation = "month"
      )
    }, NA)
  }
})

test_that("groupsummary with hospital hourly data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Load hourly hospital data
    load("data/hospital_admission_hourly.rda")
    
    # Test hourly vital signs monitoring
    expect_error({
      result <- groupsummary(
        data = hospital_admission_hourly,
        groupVars = c("AdmissionTime", "Department"),
        sumVars = c("HeartRate", "SystolicBP", "OxygenSaturation"),
        statistics = c("mean", "n"),
        dateVar = "AdmissionTime",
        dateFormat = "ymd_hms",
        timeAggregation = "hour"
      )
    }, NA)
  }
})

test_that("groupsummary display options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_simple.rda")
    
    # Test different sorting options
    sort_options <- c("groups", "first_desc", "first_asc")
    
    for (sort_option in sort_options) {
      expect_error({
        result <- groupsummary(
          data = groupsummary_simple,
          groupVars = "category",
          sumVars = "value1",
          statistics = c("sum", "mean"),
          sortBy = sort_option
        )
      }, NA, info = paste("Sort option:", sort_option))
    }
    
    # Test missing value handling
    expect_error({
      result <- groupsummary(
        data = groupsummary_simple,
        groupVars = "category",
        sumVars = "value1",
        statistics = c("sum", "n"),
        showMissing = TRUE
      )
    }, NA)
    
    # Test percentage calculation
    expect_error({
      result <- groupsummary(
        data = groupsummary_simple,
        groupVars = "category",
        sumVars = "value1",
        statistics = c("sum"),
        addPercentage = TRUE
      )
    }, NA)
  }
})

test_that("groupsummary with missing data", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Create data with missing values
    load("data/groupsummary_simple.rda")
    test_data <- groupsummary_simple
    
    # Introduce some missing values
    test_data$value1[1:5] <- NA
    test_data$category[6:10] <- NA
    
    expect_error({
      result <- groupsummary(
        data = test_data,
        groupVars = "category",
        sumVars = "value1",
        statistics = c("sum", "mean", "n"),
        showMissing = FALSE
      )
    }, NA)  # Should handle missing data gracefully
    
    # Test with showMissing = TRUE
    expect_error({
      result <- groupsummary(
        data = test_data,
        groupVars = "category",
        sumVars = "value1",
        statistics = c("sum", "mean", "n"),
        showMissing = TRUE
      )
    }, NA)
  }
})

test_that("groupsummary date format options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    # Create test data with different date formats
    test_data <- data.frame(
      date_ymd = as.Date(c("2023-01-15", "2023-02-20", "2023-03-10")),
      date_dmy = c("15/01/2023", "20/02/2023", "10/03/2023"),
      date_mdy = c("01/15/2023", "02/20/2023", "03/10/2023"),
      value = c(100, 200, 150),
      stringsAsFactors = FALSE
    )
    
    # Test ymd format
    expect_error({
      result <- groupsummary(
        data = test_data,
        groupVars = "date_ymd",
        sumVars = "value",
        statistics = c("sum"),
        dateVar = "date_ymd",
        dateFormat = "ymd",
        timeAggregation = "month"
      )
    }, NA)
  }
})

test_that("groupsummary time aggregation options", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_sales_data.rda")
    
    # Test different time aggregations
    time_aggregations <- c("day", "week", "month", "year")
    
    for (aggregation in time_aggregations) {
      expect_error({
        result <- groupsummary(
          data = groupsummary_sales_data,
          groupVars = "sale_date",
          sumVars = "sales_amount",
          statistics = c("sum"),
          dateVar = "sale_date",
          dateFormat = "ymd",
          timeAggregation = aggregation
        )
      }, NA, info = paste("Time aggregation:", aggregation))
    }
  }
})

test_that("groupsummary dependency handling", {
  
  # Test function behavior when required packages are available
  required_packages <- c("dplyr", "ggplot2", "lubridate", "glue")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        suppressMessages(suppressWarnings(devtools::load_all()))
        if (exists("groupsummary")) {
          load("data/groupsummary_simple.rda")
          result <- groupsummary(
            data = groupsummary_simple,
            groupVars = "category",
            sumVars = "value1",
            statistics = c("sum", "mean")
          )
        }
      }, NA, info = paste("Package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("groupsummary required methods exist", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_simple.rda")
    
    result <- groupsummary(
      data = groupsummary_simple,
      groupVars = "category",
      sumVars = "value1",
      statistics = c("sum", "mean")
    )
    
    # Check that required methods exist
    expect_true(exists(".plot", envir = result$.__enclos_env__$private))
    expect_true(exists(".run", envir = result$.__enclos_env__$private))
  }
})

test_that("groupsummary parameter validation", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_simple.rda")
    
    # Test with empty groupVars (should show welcome message)
    expect_error({
      result <- groupsummary(
        data = groupsummary_simple,
        groupVars = character(0),
        sumVars = "value1"
      )
    }, NA)
    
    # Test with empty sumVars (should show welcome message)
    expect_error({
      result <- groupsummary(
        data = groupsummary_simple,
        groupVars = "category",
        sumVars = character(0)
      )
    }, NA)
  }
})

test_that("groupsummary comprehensive parameter combinations", {
  
  suppressMessages(suppressWarnings(devtools::load_all()))
  
  if (exists("groupsummary")) {
    load("data/groupsummary_sales_data.rda")
    
    # Test comprehensive parameter combination
    expect_error({
      result <- groupsummary(
        data = groupsummary_sales_data,
        groupVars = c("region", "product_category"),
        sumVars = c("sales_amount", "quantity_sold"),
        statistics = c("sum", "mean", "median", "n"),
        dateVar = NULL,
        addPercentage = TRUE,
        showMissing = FALSE,
        sortBy = "first_desc"
      )
    }, NA)
  }
})