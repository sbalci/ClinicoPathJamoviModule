test_that("grafify module files exist", {
    
    # Test that required files exist
    expect_true(file.exists("R/grafify.b.R"))
    expect_true(file.exists("jamovi/grafify.a.yaml"))
    expect_true(file.exists("jamovi/grafify.u.yaml"))
    expect_true(file.exists("jamovi/grafify.r.yaml"))
    expect_true(file.exists("R/grafify.h.R"))
})

test_that("grafify class and function availability", {
    
    # Load the package functions quietly
    suppressMessages(suppressWarnings(devtools::load_all()))
    
    # Test that grafify class exists after loading
    expect_true(exists("grafifyClass"))
    expect_true(exists("grafify"))
    
    # Test class inheritance
    if (exists("grafifyClass")) {
        expect_true(inherits(grafifyClass, "R6ClassGenerator"))
    }
})

test_that("grafify backend implementation structure", {
    
    # Read the backend file and check for key elements
    backend_content <- readLines("R/grafify.b.R", warn = FALSE)
    backend_text <- paste(backend_content, collapse = "\n")
    
    # Check for essential methods
    expect_true(grepl(".init\\s*=\\s*function", backend_text))
    expect_true(grepl(".run\\s*=\\s*function", backend_text))
    expect_true(grepl(".plot_main\\s*=\\s*function", backend_text))
    
    # Check for grafify integration
    expect_true(grepl("grafify::", backend_text))
    expect_true(grepl("plot_scatterbar", backend_text))
    expect_true(grepl("plot_scatterbox", backend_text))
    expect_true(grepl("color_palette", backend_text))
    
    # Check for error handling
    expect_true(grepl("tryCatch", backend_text))
    expect_true(grepl("requireNamespace.*grafify", backend_text))
})

test_that("grafify YAML configurations are valid", {
    
    # Test analysis configuration
    if (requireNamespace("yaml", quietly = TRUE)) {
        analysis_config <- yaml::read_yaml("jamovi/grafify.a.yaml")
        
        expect_equal(analysis_config$name, "grafify")
        expect_true("title" %in% names(analysis_config))
        expect_true("options" %in% names(analysis_config))
        
        # Check for key options
        option_names <- sapply(analysis_config$options, function(x) x$name)
        expected_options <- c("plot_type", "color_palette", "x_var", "y_var")
        expect_true(all(expected_options %in% option_names))
        
        # Test results configuration
        results_config <- yaml::read_yaml("jamovi/grafify.r.yaml")
        item_names <- sapply(results_config$items, function(x) x$name)
        expect_true("main_plot" %in% item_names)
        expect_true("summary_stats" %in% item_names)
    }
})

test_that("grafify test datasets exist and are properly structured", {
    
    # Test that datasets were created
    expect_true(file.exists("data/grafify_comprehensive_data.rda"))
    expect_true(file.exists("data/grafify_simple_data.rda"))
    expect_true(file.exists("data/grafify_longitudinal_data.rda"))
    expect_true(file.exists("data/grafify_correlation_data.rda"))
    expect_true(file.exists("data/grafify_dose_response_data.rda"))
    expect_true(file.exists("data/grafify_factorial_data.rda"))
    
    # Load and test the main dataset
    load("data/grafify_comprehensive_data.rda")
    
    expect_s3_class(grafify_comprehensive_data, "data.frame")
    expect_gt(nrow(grafify_comprehensive_data), 100)
    expect_gt(ncol(grafify_comprehensive_data), 10)
    
    # Check essential columns
    essential_cols <- c("treatment", "timepoint", "biomarker_concentration", 
                       "clinical_score", "gender", "subject_id")
    expect_true(all(essential_cols %in% names(grafify_comprehensive_data)))
    
    # Check data types
    expect_true(is.numeric(grafify_comprehensive_data$biomarker_concentration))
    expect_true(is.numeric(grafify_comprehensive_data$clinical_score))
})

test_that("grafify vignette exists and has correct content", {
    
    # Test vignette file exists
    vignette_path <- "vignettes/jjstatsplot-15-grafify-comprehensive.qmd"
    expect_true(file.exists(vignette_path))
    
    # Check vignette content
    vignette_content <- readLines(vignette_path, warn = FALSE)
    content_text <- paste(vignette_content, collapse = "\n")
    
    # Check for key sections
    expect_true(grepl("Introduction to Grafify", content_text))
    expect_true(grepl("Color-blind friendly", content_text))
    expect_true(grepl("Statistical Analysis Integration", content_text))
    expect_true(grepl("Experimental Design", content_text))
    expect_true(grepl("Clinical Research Applications", content_text))
    
    # Check for code examples
    expect_true(grepl("grafify\\(", content_text))
    expect_true(grepl("plot_type.*=", content_text))
    expect_true(grepl("color_palette.*=", content_text))
})

test_that("grafify color palettes are properly configured", {
    
    # Read analysis configuration
    analysis_content <- readLines("jamovi/grafify.a.yaml", warn = FALSE)
    analysis_text <- paste(analysis_content, collapse = "\n")
    
    # Check that color palettes are defined
    expected_palettes <- c("default", "vibrant", "contrast", "bright", "pale", "dark")
    palette_count <- sum(sapply(expected_palettes, function(p) grepl(p, analysis_text)))
    expect_gt(palette_count, 3)  # At least some palettes should be mentioned
    
    # Check backend has palette handling
    backend_content <- readLines("R/grafify.b.R", warn = FALSE)  
    backend_text <- paste(backend_content, collapse = "\n")
    expect_true(grepl("graf_palettes", backend_text))
    expect_true(grepl("reverse_palette", backend_text))
})

test_that("grafify has proper dependency management", {
    
    backend_content <- readLines("R/grafify.b.R", warn = FALSE)
    backend_text <- paste(backend_content, collapse = "\n")
    
    # Check for essential imports
    expect_true(grepl("@importFrom grafify", backend_text))
    expect_true(grepl("@importFrom dplyr", backend_text))
    expect_true(grepl("@importFrom ggplot2", backend_text))
    expect_true(grepl("@import jmvcore", backend_text))
    
    # Check for specific grafify functions
    grafify_functions <- c("plot_scatterbox", "plot_scatterviolin", "plot_density", "plot_histogram")
    for (func in grafify_functions) {
        expect_true(grepl(func, backend_text))
    }
})

test_that("grafify basic instantiation works", {
    
    # Load package quietly
    suppressMessages(suppressWarnings(devtools::load_all()))
    
    if (exists("grafify")) {
        # Create minimal test data
        test_data <- data.frame(
            group = c("A", "B", "A", "B"),
            value = c(1, 2, 3, 4)
        )
        
        # Test that grafify can be instantiated without errors
        expect_error({
            result <- grafify(
                data = test_data,
                x_var = "group",
                y_var = "value",
                plot_type = "scatterbar"
            )
        }, NA)  # Should not error during instantiation
    }
})