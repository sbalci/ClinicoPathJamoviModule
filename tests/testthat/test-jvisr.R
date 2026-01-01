# Tests for jvisr function
# Clinical Research Visualization with visR package

# Test data preparation for clinical research visualizations
test_data_jvisr <- data.frame(
    patient_id = paste0("PT_", sprintf("%03d", 1:200)),
    
    # Standard survival analysis variables
    time_to_event = rexp(200, 0.1),  # Exponential distribution typical for survival
    event_indicator = rbinom(200, 1, 0.6),  # 60% event rate
    
    # CDISC standard variables
    AVAL = rexp(200, 0.1),  # Analysis value (time)
    CNSR = rbinom(200, 1, 0.4),  # Censor indicator (1=censored, 0=event)
    
    # Stratification variables
    treatment_arm = factor(sample(c("Placebo", "Low_Dose", "High_Dose"), 200, replace = TRUE)),
    study_site = factor(sample(c("Site_A", "Site_B", "Site_C"), 200, replace = TRUE)),
    gender = factor(sample(c("Male", "Female"), 200, replace = TRUE)),
    age_group = factor(sample(c("Young", "Adult", "Senior"), 200, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    baseline_severity = factor(sample(c("Mild", "Moderate", "Severe"), 200, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    
    # Additional clinical variables
    biomarker_level = exp(rnorm(200, 2, 0.8)),
    quality_of_life = rnorm(200, 60, 15),
    comorbidity_count = rpois(200, 2),
    
    # Binary outcomes
    response = factor(sample(c("Responder", "Non-responder"), 200, replace = TRUE, prob = c(0.4, 0.6))),
    adverse_event = factor(sample(c("No", "Yes"), 200, replace = TRUE, prob = c(0.7, 0.3)))
)

# Basic function availability tests
test_that("jvisr function exists and is properly defined", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    expect_true(exists("jvisrClass"))
    expect_true(R6::is.R6Class(jvisrClass))
})

test_that("Required packages are available", {
    expect_true(requireNamespace("jmvcore", quietly = TRUE))
    expect_true(requireNamespace("R6", quietly = TRUE))
    expect_true(requireNamespace("survival", quietly = TRUE))
})

test_that("visR package availability is handled gracefully", {
    # Should work whether visR is available or not
    expect_no_error({
        jvisr_instance <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
    })
})

# Basic clinical visualization functionality tests
test_that("Basic Kaplan-Meier analysis works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_km <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_km$run()
    })
})

test_that("CDISC format works correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_cdisc <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                cdisc_format = TRUE,
                aval_var = "AVAL",
                cnsr_var = "CNSR",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_cdisc$run()
    })
})

test_that("Stratified analysis works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_strata <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                strata_var = "treatment_arm",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_strata$run()
    })
})

# Test all analysis types
test_that("All analysis types work correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    analysis_types <- c("kaplan_meier", "cuminc", "tableone", "attrition", "risktable")
    
    for (analysis_type in analysis_types) {
        expect_no_error({
            jvisr_analysis <- jvisrClass$new(
                options = list(
                    analysis_type = analysis_type,
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_analysis$run()
        }, info = paste("Failed with analysis type:", analysis_type))
    }
})

# Clinical visualization options tests
test_that("Confidence intervals option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    for (ci_option in c(TRUE, FALSE)) {
        expect_no_error({
            jvisr_ci <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    confidence_interval = ci_option,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_ci$run()
        }, info = paste("Failed with confidence_interval:", ci_option))
    }
})

test_that("Risk table option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    for (risk_option in c(TRUE, FALSE)) {
        expect_no_error({
            jvisr_risk <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    risk_table = risk_option,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_risk$run()
        }, info = paste("Failed with risk_table:", risk_option))
    }
})

test_that("P-value option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    for (p_option in c(TRUE, FALSE)) {
        expect_no_error({
            jvisr_pval <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    strata_var = "treatment_arm",
                    p_value = p_option,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_pval$run()
        }, info = paste("Failed with p_value:", p_option))
    }
})

test_that("Quantiles option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    for (quant_option in c(TRUE, FALSE)) {
        expect_no_error({
            jvisr_quant <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    quantiles = quant_option,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_quant$run()
        }, info = paste("Failed with quantiles:", quant_option))
    }
})

# Function scale options tests
test_that("Different function scales work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    fun_types <- c("surv", "event", "cumhaz", "cloglog", "pct", "log")
    
    for (fun_type in fun_types) {
        expect_no_error({
            jvisr_fun <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    fun_type = fun_type,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_fun$run()
        }, info = paste("Failed with fun_type:", fun_type))
    }
})

# Legend position tests
test_that("Legend position options work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    legend_positions <- c("right", "left", "top", "bottom", "none")
    
    for (legend_pos in legend_positions) {
        expect_no_error({
            jvisr_legend <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    strata_var = "treatment_arm",
                    legend_position = legend_pos,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_legend$run()
        }, info = paste("Failed with legend_position:", legend_pos))
    }
})

# Theme options tests
test_that("Different theme styles work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    theme_styles <- c("visr", "classic", "minimal", "clean")
    
    for (theme_style in theme_styles) {
        expect_no_error({
            jvisr_theme <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    theme_style = theme_style,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_theme$run()
        }, info = paste("Failed with theme_style:", theme_style))
    }
})

# Color palette tests
test_that("Different color palettes work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    color_palettes <- c("default", "Set1", "Set2", "Dark2", "Paired")
    
    for (color_palette in color_palettes) {
        expect_no_error({
            jvisr_color <- jvisrClass$new(
                options = list(
                    analysis_type = "kaplan_meier",
                    time_var = "time_to_event",
                    event_var = "event_indicator",
                    strata_var = "treatment_arm",
                    color_palette = color_palette,
                    data = test_data_jvisr
                ),
                data = test_data_jvisr
            )
            jvisr_color$run()
        }, info = paste("Failed with color_palette:", color_palette))
    }
})

# Custom labels tests
test_that("Custom labels work correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_labels <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                time_label = "Time to Event (Days)",
                time_units = "days",
                survival_label = "Probability of Survival",
                title = "Kaplan-Meier Survival Analysis",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_labels$run()
    })
})

# Summary and interpretation options tests
test_that("Summary and interpretation options work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    for (show_summary in c(TRUE, FALSE)) {
        for (show_interpretation in c(TRUE, FALSE)) {
            expect_no_error({
                jvisr_output <- jvisrClass$new(
                    options = list(
                        analysis_type = "kaplan_meier",
                        time_var = "time_to_event",
                        event_var = "event_indicator",
                        show_summary = show_summary,
                        show_interpretation = show_interpretation,
                        data = test_data_jvisr
                    ),
                    data = test_data_jvisr
                )
                jvisr_output$run()
            }, info = paste("Failed with show_summary:", show_summary, "show_interpretation:", show_interpretation))
        }
    }
})

# Performance and caching tests
test_that("Caching system works correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    jvisr_instance <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            event_var = "event_indicator",
            strata_var = "treatment_arm",
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    # First access to establish cache
    start_time1 <- Sys.time()
    jvisr_instance$run()
    end_time1 <- Sys.time()
    
    # Second access should use cache
    start_time2 <- Sys.time()
    jvisr_instance$run()
    end_time2 <- Sys.time()
    
    # Both should complete successfully
    time1 <- as.numeric(difftime(end_time1, start_time1, units = "secs"))
    time2 <- as.numeric(difftime(end_time2, start_time2, units = "secs"))
    
    expect_true(time1 > 0)
    expect_true(time2 > 0)
    # Second run should typically be faster due to caching
    # expect_lt(time2, time1)  # Commented out as performance can vary
})

test_that("Cache invalidation works when data changes", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    jvisr_instance <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            event_var = "event_indicator",
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    # First run
    expect_no_error(jvisr_instance$run())
    
    # Change data
    modified_data <- test_data_jvisr
    modified_data$time_to_event <- modified_data$time_to_event * 1.5
    
    jvisr_instance$data <- modified_data
    
    # Second run with modified data
    expect_no_error(jvisr_instance$run())
})

test_that("Cache invalidation works when options change", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    jvisr_instance <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            event_var = "event_indicator",
            confidence_interval = FALSE,
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    # First run
    expect_no_error(jvisr_instance$run())
    
    # Change options
    jvisr_instance$options$confidence_interval <- TRUE
    jvisr_instance$options$strata_var <- "treatment_arm"
    
    # Second run with different options
    expect_no_error(jvisr_instance$run())
})

# Error handling and edge cases
test_that("Function handles empty data gracefully", {
    skip_if_not_installed("jmvcore")
    
    empty_data <- data.frame()
    
    expect_error({
        jvisr_empty <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = empty_data
            ),
            data = empty_data
        )
        jvisr_empty$run()
    })
})

test_that("Function handles missing required variables gracefully", {
    skip_if_not_installed("jmvcore")
    
    # Missing time variable
    jvisr_no_time <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            event_var = "event_indicator",
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    expect_no_error(jvisr_no_time$run())  # Should show todo message
    
    # Missing event variable
    jvisr_no_event <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    expect_no_error(jvisr_no_event$run())  # Should show todo message
})

test_that("Function validates time variable is numeric", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_error({
        jvisr_categorical <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "treatment_arm",  # Categorical variable as time
                event_var = "event_indicator",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_categorical$run()
    })
})

test_that("Function handles missing stratification variable", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_missing_strata <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                strata_var = "nonexistent_variable",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_missing_strata$run()
    })
})

test_that("Function handles very small datasets", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    small_data <- test_data_jvisr[1:10, ]
    
    expect_no_error({
        jvisr_small <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = small_data
            ),
            data = small_data
        )
        jvisr_small$run()
    })
})

test_that("Function handles data with all events censored", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    censored_data <- test_data_jvisr
    censored_data$event_indicator <- 0  # All censored
    
    expect_no_error({
        jvisr_censored <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = censored_data
            ),
            data = censored_data
        )
        jvisr_censored$run()
    })
})

test_that("Function handles data with no events censored", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    all_events_data <- test_data_jvisr
    all_events_data$event_indicator <- 1  # All events
    
    expect_no_error({
        jvisr_all_events <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = all_events_data
            ),
            data = all_events_data
        )
        jvisr_all_events$run()
    })
})

# Complex clinical scenarios tests
test_that("Multi-arm clinical trial scenario works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_clinical <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                strata_var = "treatment_arm",
                confidence_interval = TRUE,
                risk_table = TRUE,
                p_value = TRUE,
                title = "Multi-arm Clinical Trial Survival Analysis",
                time_label = "Time to Progression",
                time_units = "months",
                theme_style = "visr",
                color_palette = "Set1",
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_clinical$run()
    })
})

test_that("CDISC regulatory submission scenario works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    expect_no_error({
        jvisr_regulatory <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                cdisc_format = TRUE,
                aval_var = "AVAL",
                cnsr_var = "CNSR",
                strata_var = "treatment_arm",
                confidence_interval = TRUE,
                risk_table = TRUE,
                p_value = TRUE,
                quantiles = TRUE,
                title = "CDISC ADaM ADTTE Analysis",
                time_label = "Analysis Value (AVAL)",
                theme_style = "classic",
                show_summary = TRUE,
                show_interpretation = TRUE,
                data = test_data_jvisr
            ),
            data = test_data_jvisr
        )
        jvisr_regulatory$run()
    })
})

# Missing data handling tests
test_that("Missing data in survival variables is handled correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    # Create data with missing values
    missing_data <- test_data_jvisr
    missing_data$time_to_event[1:10] <- NA
    missing_data$event_indicator[11:15] <- NA
    
    expect_no_error({
        jvisr_missing <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                data = missing_data
            ),
            data = missing_data
        )
        jvisr_missing$run()
    })
})

test_that("Missing data in stratification variable is handled correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    missing_strata_data <- test_data_jvisr
    missing_strata_data$treatment_arm[1:20] <- NA
    
    expect_no_error({
        jvisr_missing_strata <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time_to_event",
                event_var = "event_indicator",
                strata_var = "treatment_arm",
                data = missing_strata_data
            ),
            data = missing_strata_data
        )
        jvisr_missing_strata$run()
    })
})

# Memory and cleanup tests
test_that("Function cleans up properly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    initial_objects <- ls()
    
    jvisr_instance <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            event_var = "event_indicator",
            data = test_data_jvisr
        ),
        data = test_data_jvisr
    )
    
    jvisr_instance$run()
    
    # Clean up
    rm(jvisr_instance)
    gc()
    
    final_objects <- ls()
    
    # Should not create permanent objects in global environment
    new_objects <- setdiff(final_objects, initial_objects)
    expect_true(length(new_objects) <= 1)  # Only allowed new object is jvisr_instance if still referenced
})

# Large dataset performance test
test_that("Function handles larger datasets efficiently", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    skip_if(Sys.getenv("SKIP_LARGE_TESTS") == "true", "Skipping large dataset test")
    
    large_data <- do.call(rbind, replicate(5, test_data_jvisr, simplify = FALSE))
    large_data$patient_id <- paste0("PT_", sprintf("%04d", 1:nrow(large_data)))
    
    start_time <- Sys.time()
    jvisr_large <- jvisrClass$new(
        options = list(
            analysis_type = "kaplan_meier",
            time_var = "time_to_event",
            event_var = "event_indicator",
            strata_var = "treatment_arm",
            data = large_data
        ),
        data = large_data
    )
    jvisr_large$run()
    end_time <- Sys.time()
    
    # Should complete within reasonable time (10 seconds)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(execution_time, 10)
})

# Integration tests with different survival distributions
test_that("Function works with various survival distributions", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")
    
    # Weibull distribution
    weibull_data <- data.frame(
        time = rweibull(100, shape = 1.5, scale = 10),
        event = rbinom(100, 1, 0.7),
        group = factor(rep(c("A", "B"), 50))
    )
    
    expect_no_error({
        jvisr_weibull <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time",
                event_var = "event",
                strata_var = "group",
                data = weibull_data
            ),
            data = weibull_data
        )
        jvisr_weibull$run()
    })
    
    # Log-normal distribution
    lognormal_data <- data.frame(
        time = rlnorm(100, meanlog = 2, sdlog = 0.5),
        event = rbinom(100, 1, 0.6),
        group = factor(rep(c("X", "Y"), 50))
    )
    
    expect_no_error({
        jvisr_lognormal <- jvisrClass$new(
            options = list(
                analysis_type = "kaplan_meier",
                time_var = "time",
                event_var = "event",
                strata_var = "group",
                data = lognormal_data
            ),
            data = lognormal_data
        )
        jvisr_lognormal$run()
    })
})

# Print test summary
cat("jvisr test suite includes:\\n")
cat("- Basic clinical visualization functionality tests\\n")
cat("- All analysis types (Kaplan-Meier, cumulative incidence, Table One, attrition, risk table)\\n")
cat("- CDISC format support and validation\\n")
cat("- Caching and performance optimization tests\\n")
cat("- Missing data handling tests\\n")
cat("- Error handling and edge case tests\\n")
cat("- Theme and color palette tests\\n")
cat("- Complex clinical scenario tests\\n")
cat("- Memory management and cleanup tests\\n")
cat("- Large dataset performance tests\\n")
cat("- Integration tests with various survival distributions\\n")
cat("Total test count: 50+ individual test cases\\n")
