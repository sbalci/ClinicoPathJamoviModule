#' @title Sequential Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_text geom_line geom_point labs theme_void theme element_blank scale_x_continuous scale_y_continuous annotate
#' @importFrom gridExtra grid.arrange

sequentialtestsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "sequentialtestsClass",
        inherit = sequentialtestsBase,
        private = list(
            NUMERICAL_TOLERANCE = 1e-10,
            POPULATION_SIZE = NULL,
            .init = function() {
                # Add rows to tables during initialization
                individualTable <- self$results$individual_tests_table
                individualTable$addRow(rowKey = "test1",
                                       values = list(test_name = self$options$test1_name))
                individualTable$addRow(rowKey = "test2",
                                       values = list(test_name = self$options$test2_name))
                individualTable$addRow(rowKey = "combined",
                                       values = list(test_name = "Combined Strategy"))

                flowTable <- self$results$population_flow_table
                flowTable$addRow(rowKey = "initial",
                                 values = list(stage = "Initial Population"))
                flowTable$addRow(rowKey = "after_test1",
                                 values = list(stage = "After First Test"))
                flowTable$addRow(rowKey = "after_test2",
                                 values = list(stage = "After Second Test"))
                if (is.null(private$POPULATION_SIZE))
                    private$POPULATION_SIZE <- self$options$population_size
            },

            .run = function() {

                # Ready to run?
                if (is.null(self$options$test1_sens) || is.null(self$options$test1_spec) || 
                    is.null(self$options$test2_sens) || is.null(self$options$test2_spec)) {
                    return()
                }

                # Get parameters from options
                test1_name <- self$options$test1_name
                test1_sens <- self$options$test1_sens
                test1_spec <- self$options$test1_spec

                test2_name <- self$options$test2_name
                test2_sens <- self$options$test2_sens
                test2_spec <- self$options$test2_spec

                strategy <- self$options$strategy
                prevalence <- self$options$prevalence

                format_percent <- function(value) {
                    if (is.na(value))
                        return("not defined")
                    if (is.infinite(value))
                        return("infinite")
                    sprintf("%.1f%%", value * 100)
                }

                # Input validation using jmvcore::Notice system
                notices_error <- list()
                notices_strong <- list()
                notices_warning <- list()

                # Validate probability ranges for all test parameters
                if (is.na(test1_sens) || test1_sens < 0 || test1_sens > 1) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_sens_invalid', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 1 sensitivity must be between 0 and 1 (0% to 100%). Current value is invalid.')
                    notices_error <- c(notices_error, list(n))
                }
                if (is.na(test1_spec) || test1_spec < 0 || test1_spec > 1) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_spec_invalid', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 1 specificity must be between 0 and 1 (0% to 100%). Current value is invalid.')
                    notices_error <- c(notices_error, list(n))
                }
                if (is.na(test2_sens) || test2_sens < 0 || test2_sens > 1) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_sens_invalid', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 2 sensitivity must be between 0 and 1 (0% to 100%). Current value is invalid.')
                    notices_error <- c(notices_error, list(n))
                }
                if (is.na(test2_spec) || test2_spec < 0 || test2_spec > 1) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_spec_invalid', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 2 specificity must be between 0 and 1 (0% to 100%). Current value is invalid.')
                    notices_error <- c(notices_error, list(n))
                }
                if (is.na(prevalence) || prevalence <= 0 || prevalence >= 1) {
                    n <- jmvcore::Notice$new(options=self$options, name='prevalence_invalid', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Prevalence must be greater than 0 and less than 1 (exclusive). Current value is invalid.')
                    notices_error <- c(notices_error, list(n))
                }

                # Validate test names
                if (is.null(test1_name) || nchar(trimws(test1_name)) == 0) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_name_empty', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 1 name cannot be empty. Please provide a descriptive name.')
                    notices_error <- c(notices_error, list(n))
                }
                if (is.null(test2_name) || nchar(trimws(test2_name)) == 0) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_name_empty', type=jmvcore::NoticeType$ERROR)
                    n$setContent('Test 2 name cannot be empty. Please provide a descriptive name.')
                    notices_error <- c(notices_error, list(n))
                }

                # Insert ERROR notices at top and return early
                if (length(notices_error) > 0) {
                    for (i in seq_along(notices_error)) {
                        self$results$insert(i, notices_error[[i]])
                    }
                    return()
                }

                # Clinical plausibility warnings (STRONG_WARNING for serious concerns)
                if (test1_sens < 0.50 || test1_spec < 0.50) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_poor_performance', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent(sprintf('Test 1 performance is unusually low (Sensitivity=%.1f%%, Specificity=%.1f%%). Results may not be clinically useful.', test1_sens*100, test1_spec*100))
                    notices_strong <- c(notices_strong, list(n))
                }
                if (test2_sens < 0.50 || test2_spec < 0.50) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_poor_performance', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent(sprintf('Test 2 performance is unusually low (Sensitivity=%.1f%%, Specificity=%.1f%%). Results may not be clinically useful.', test2_sens*100, test2_spec*100))
                    notices_strong <- c(notices_strong, list(n))
                }

                if (prevalence > 0.90) {
                    n <- jmvcore::Notice$new(options=self$options, name='prevalence_very_high', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent(sprintf('Prevalence is very high (%.1f%%). This is unusual except in confirmatory testing scenarios. Verify this matches your clinical context.', prevalence*100))
                    notices_strong <- c(notices_strong, list(n))
                }
                if (prevalence < 0.001) {
                    n <- jmvcore::Notice$new(options=self$options, name='prevalence_very_low', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent(sprintf('Prevalence is very low (%.3f%%). Even with excellent tests, PPV may be extremely low. Consider pre-test probability carefully.', prevalence*100))
                    notices_strong <- c(notices_strong, list(n))
                }

                # Detect potential test correlation
                test_similarity <- agrepl(test1_name, test2_name, max.distance = 0.3)
                if (test_similarity && test1_name != "Screening Test" && test2_name != "Confirmatory Test") {
                    n <- jmvcore::Notice$new(options=self$options, name='test_correlation_risk', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent(sprintf('Test names are similar ("%s" vs "%s"). If tests measure similar biomarkers or use similar technology, they may be correlated. This violates the independence assumption and combined metrics will be overestimated.', test1_name, test2_name))
                    notices_strong <- c(notices_strong, list(n))
                }

                # Minor plausibility warnings
                if (test1_sens > 0.99) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_sens_high', type=jmvcore::NoticeType$WARNING)
                    n$setContent(sprintf('Test 1 sensitivity >99%% (%.2f%%) is rarely achieved in practice. Verify this value.', test1_sens*100))
                    notices_warning <- c(notices_warning, list(n))
                }
                if (test1_spec > 0.99) {
                    n <- jmvcore::Notice$new(options=self$options, name='test1_spec_high', type=jmvcore::NoticeType$WARNING)
                    n$setContent(sprintf('Test 1 specificity >99%% (%.2f%%) is rarely achieved in practice. Verify this value.', test1_spec*100))
                    notices_warning <- c(notices_warning, list(n))
                }
                if (test2_sens > 0.99) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_sens_high', type=jmvcore::NoticeType$WARNING)
                    n$setContent(sprintf('Test 2 sensitivity >99%% (%.2f%%) is rarely achieved in practice. Verify this value.', test2_sens*100))
                    notices_warning <- c(notices_warning, list(n))
                }
                if (test2_spec > 0.99) {
                    n <- jmvcore::Notice$new(options=self$options, name='test2_spec_high', type=jmvcore::NoticeType$WARNING)
                    n$setContent(sprintf('Test 2 specificity >99%% (%.2f%%) is rarely achieved in practice. Verify this value.', test2_spec*100))
                    notices_warning <- c(notices_warning, list(n))
                }

                # Strategy-specific warnings
                if (strategy == "serial_positive") {
                    if (test1_spec > test2_spec) {
                        n <- jmvcore::Notice$new(options=self$options, name='strategy_serial_pos_suboptimal', type=jmvcore::NoticeType$WARNING)
                        n$setContent(sprintf('Serial positive strategy: Test 2 specificity (%.1f%%) should typically exceed Test 1 specificity (%.1f%%) for confirmation.', test2_spec*100, test1_spec*100))
                        notices_warning <- c(notices_warning, list(n))
                    }
                } else if (strategy == "serial_negative") {
                    if (test1_sens > test2_sens) {
                        n <- jmvcore::Notice$new(options=self$options, name='strategy_serial_neg_suboptimal', type=jmvcore::NoticeType$WARNING)
                        n$setContent(sprintf('Serial negative strategy: Test 2 sensitivity (%.1f%%) should typically exceed Test 1 sensitivity (%.1f%%) for exclusion.', test2_sens*100, test1_sens*100))
                        notices_warning <- c(notices_warning, list(n))
                    }
                } else if (strategy == "parallel") {
                    if (abs(test1_sens - test2_sens) < 0.05 && abs(test1_spec - test2_spec) < 0.05) {
                        n <- jmvcore::Notice$new(options=self$options, name='strategy_parallel_redundant', type=jmvcore::NoticeType$WARNING)
                        n$setContent('Parallel testing: Tests have similar characteristics. Parallel strategy works best with complementary tests that detect different disease manifestations.')
                        notices_warning <- c(notices_warning, list(n))
                    }
                }

                # Insert STRONG_WARNING notices at top
                position <- 1
                for (notice in notices_strong) {
                    self$results$insert(position, notice)
                    position <- position + 1
                }

                # Insert WARNING notices (after strong warnings)
                for (notice in notices_warning) {
                    self$results$insert(position, notice)
                    position <- position + 1
                }

                # Calculate individual test metrics with error handling
                tryCatch({
                    # Calculate PPVs and NPVs with protection against edge cases
                    test1_ppv_denom <- (prevalence * test1_sens) + ((1 - prevalence) * (1 - test1_spec))
                    if (abs(test1_ppv_denom) < private$NUMERICAL_TOLERANCE) {
                        test1_ppv <- NA_real_
                    } else {
                        test1_ppv <- (prevalence * test1_sens) / test1_ppv_denom
                    }

                    test1_npv_denom <- ((1 - prevalence) * test1_spec) + (prevalence * (1 - test1_sens))
                    if (abs(test1_npv_denom) < private$NUMERICAL_TOLERANCE) {
                        test1_npv <- NA_real_
                    } else {
                        test1_npv <- ((1 - prevalence) * test1_spec) / test1_npv_denom
                    }

                    # Calculate likelihood ratios with division by zero protection
                    if (abs(1 - test1_spec) < private$NUMERICAL_TOLERANCE) {
                        test1_plr <- Inf
                    } else {
                        test1_plr <- test1_sens / (1 - test1_spec)
                    }

                    if (abs(test1_spec) < private$NUMERICAL_TOLERANCE) {
                        test1_nlr <- Inf
                    } else {
                        test1_nlr <- (1 - test1_sens) / test1_spec
                    }

                    # Same calculations for test 2
                    test2_ppv_denom <- (prevalence * test2_sens) + ((1 - prevalence) * (1 - test2_spec))
                    if (abs(test2_ppv_denom) < private$NUMERICAL_TOLERANCE) {
                        test2_ppv <- NA_real_
                    } else {
                        test2_ppv <- (prevalence * test2_sens) / test2_ppv_denom
                    }

                    test2_npv_denom <- ((1 - prevalence) * test2_spec) + (prevalence * (1 - test2_sens))
                    if (abs(test2_npv_denom) < private$NUMERICAL_TOLERANCE) {
                        test2_npv <- NA_real_
                    } else {
                        test2_npv <- ((1 - prevalence) * test2_spec) / test2_npv_denom
                    }

                    if (abs(1 - test2_spec) < private$NUMERICAL_TOLERANCE) {
                        test2_plr <- Inf
                    } else {
                        test2_plr <- test2_sens / (1 - test2_spec)
                    }

                    if (abs(test2_spec) < private$NUMERICAL_TOLERANCE) {
                        test2_nlr <- Inf
                    } else {
                        test2_nlr <- (1 - test2_sens) / test2_spec
                    }
                    
                    # Ensure results are in valid probability ranges
                    test1_ppv <- pmax(0, pmin(1, test1_ppv))
                    test1_npv <- pmax(0, pmin(1, test1_npv))
                    test2_ppv <- pmax(0, pmin(1, test2_ppv))
                    test2_npv <- pmax(0, pmin(1, test2_npv))
                    
                }, error = function(e) {
                    # If any calculation fails, show error notice
                    private$.addNotice(
                        type = "ERROR",
                        title = "Calculation Error",
                        content = sprintf('Calculation error with provided values: %s. Please verify all parameters and try again.', e$message)
                    )
                    return()
                })

                # Calculate combined metrics based on strategy
                strategy_note <- "Assumes conditional independence between tests. Correlated tests will overstate combined performance."
                if (strategy == "serial_positive") {
                    # Serial testing of positives (confirmation strategy)
                    combined_sens <- test1_sens * test2_sens
                    combined_spec <- test1_spec + (1 - test1_spec) * test2_spec
                    strategy_name <- "Serial Testing (Test positives)"

                } else if (strategy == "serial_negative") {
                    # Serial testing of negatives (exclusion strategy)
                    combined_sens <- test1_sens + (1 - test1_sens) * test2_sens
                    combined_spec <- test1_spec * test2_spec
                    strategy_name <- "Serial Testing (Test negatives)"

                } else if (strategy == "parallel") {
                    # Parallel testing (both tests for everyone)
                    # Positive if either test is positive
                    combined_sens <- test1_sens + test2_sens - (test1_sens * test2_sens)
                    combined_spec <- test1_spec * test2_spec
                    strategy_name <- "Parallel Testing (Test all)"
                }

                # Calculate PPV and NPV for combined strategy
                combined_ppv_num <- prevalence * combined_sens
                combined_ppv_denom <- combined_ppv_num + (1 - prevalence) * (1 - combined_spec)
                combined_ppv <- private$.safeDivide(combined_ppv_num, combined_ppv_denom)

                if (is.na(combined_ppv)) {
                    n <- jmvcore::Notice$new(options=self$options, name='ppv_undefined', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent('Combined PPV cannot be calculated (denominator is zero). Check for extreme sensitivity/specificity combinations.')
                    self$results$insert(position, n)
                    position <- position + 1
                }

                combined_npv_num <- (1 - prevalence) * combined_spec
                combined_npv_denom <- combined_npv_num + prevalence * (1 - combined_sens)
                combined_npv <- private$.safeDivide(combined_npv_num, combined_npv_denom)

                if (is.na(combined_npv)) {
                    n <- jmvcore::Notice$new(options=self$options, name='npv_undefined', type=jmvcore::NoticeType$STRONG_WARNING)
                    n$setContent('Combined NPV cannot be calculated (denominator is zero). Check for extreme sensitivity/specificity combinations.')
                    self$results$insert(position, n)
                    position <- position + 1
                }

                combined_plr <- private$.safeDivide(combined_sens, 1 - combined_spec, allowInfinite = TRUE)
                if (is.na(combined_plr)) {
                    n <- jmvcore::Notice$new(options=self$options, name='plr_undefined', type=jmvcore::NoticeType$WARNING)
                    n$setContent('Combined positive likelihood ratio is undefined (both numerator and denominator approach zero).')
                    self$results$insert(position, n)
                    position <- position + 1
                } else if (is.infinite(combined_plr)) {
                    private$.addNotice(
                        type = "INFO",
                        title = "Infinite Positive Likelihood Ratio",
                        content = 'Combined positive likelihood ratio is infinite (combined specificity is effectively 100%). This indicates perfect rule-in capability.'
                    )
                }

                combined_nlr <- private$.safeDivide(1 - combined_sens, combined_spec, allowInfinite = TRUE)
                if (is.na(combined_nlr)) {
                    n <- jmvcore::Notice$new(options=self$options, name='nlr_undefined', type=jmvcore::NoticeType$WARNING)
                    n$setContent('Combined negative likelihood ratio is undefined (both numerator and denominator approach zero).')
                    self$results$insert(position, n)
                    position <- position + 1
                } else if (is.infinite(combined_nlr)) {
                    n <- jmvcore::Notice$new(options=self$options, name='nlr_infinite', type=jmvcore::NoticeType$WARNING)
                    n$setContent('Combined negative likelihood ratio is infinite (combined specificity is effectively zero). This indicates poor rule-out capability.')
                    self$results$insert(position, n)
                    position <- position + 1
                }

                # Calculate Number Needed to Screen
                # NNT = 1 / (prevalence × combined_sens)
                # This is the number of people needed to screen to find one true positive
                nnt <- if (prevalence > 0 && combined_sens > 0) {
                    ceiling(1 / (prevalence * combined_sens))
                } else {
                    NA_integer_
                }

                # Update summary table
                summaryTable <- self$results$summary_table
                summaryTable$setRow(
                    rowNo = 1,
                    values = list(
                        strategy_name = strategy_name,
                        prevalence = prevalence,
                        first_test = test1_name,
                        second_test = test2_name,
                        combined_sens = combined_sens,
                        combined_spec = combined_spec,
                        combined_ppv = combined_ppv,
                        combined_npv = combined_npv,
                        nnt = nnt
                    )
                )

                # Generate plain-language summary
                if (self$options$show_explanation) {
                    strategy_desc <- if (strategy == "serial_positive") {
                        "serial positive (confirmation) strategy"
                    } else if (strategy == "serial_negative") {
                        "serial negative (exclusion) strategy"
                    } else {
                        "parallel testing strategy"
                    }

                    clinical_meaning <- if (strategy == "serial_positive") {
                        "This strategy maximizes specificity and is best for ruling in disease when false positives are costly or harmful."
                    } else if (strategy == "serial_negative") {
                        "This strategy maximizes sensitivity and is best for ruling out disease when false negatives are dangerous."
                    } else {
                        "This strategy maximizes sensitivity and is best for rapid diagnosis when both tests can be performed simultaneously."
                    }

                    nnt_text <- if (!is.na(nnt)) {
                        sprintf(" You would need to screen approximately %d people to identify one true positive case.", nnt)
                    } else {
                        ""
                    }

                    summary <- sprintf(
                        "<div style='background:#e8f4f8;padding:15px;border-left:4px solid #0077be;font-size:1.05em;line-height:1.6;'><strong>Clinical Summary:</strong> Using a %s with %s followed by %s, the combined test achieves %.1f%%%% sensitivity (detects %.0f of every 100 diseased individuals) and %.1f%%%% specificity (correctly rules out %.0f of every 100 healthy individuals). At your specified disease prevalence of %.1f%%%%, a positive result indicates a %.1f%%%% chance the person truly has the disease (PPV), while a negative result indicates a %.1f%%%% chance the person is truly disease-free (NPV).%s %s</div>",
                        strategy_desc,
                        test1_name,
                        test2_name,
                        combined_sens*100, combined_sens*100,
                        combined_spec*100, combined_spec*100,
                        prevalence*100,
                        combined_ppv*100,
                        combined_npv*100,
                        nnt_text,
                        clinical_meaning
                    )
                    self$results$plain_summary$setContent(summary)
                }

                # Update individual tests table
                individualTable <- self$results$individual_tests_table

                individualTable$setRow(
                    rowKey = "test1",
                    values = list(
                        test_name = test1_name,
                        sensitivity = test1_sens,
                        specificity = test1_spec,
                        ppv = test1_ppv,
                        npv = test1_npv,
                        plr = test1_plr,
                        nlr = test1_nlr
                    )
                )

                individualTable$setRow(
                    rowKey = "test2",
                    values = list(
                        test_name = test2_name,
                        sensitivity = test2_sens,
                        specificity = test2_spec,
                        ppv = test2_ppv,
                        npv = test2_npv,
                        plr = test2_plr,
                        nlr = test2_nlr
                    )
                )

                individualTable$setRow(
                    rowKey = "combined",
                    values = list(
                        test_name = "Combined Strategy",
                        sensitivity = combined_sens,
                        specificity = combined_spec,
                        ppv = combined_ppv,
                        npv = combined_npv,
                        plr = combined_plr,
                        nlr = combined_nlr
                    )
                )

                # Calculate population flow numbers (assuming default population size)
                pop_size <- private$POPULATION_SIZE
                diseased <- pop_size * prevalence
                healthy <- pop_size - diseased

                # Initial population
                flowTable <- self$results$population_flow_table
                flowTable$setRow(
                    rowKey = "initial",
                    values = list(
                        stage = "Initial Population",
                        total_n = pop_size,
                        disease_pos = diseased,
                        disease_neg = healthy,
                        test_pos = NA,
                        test_neg = NA,
                        true_pos = NA,
                        false_pos = NA,
                        false_neg = NA,
                        true_neg = NA
                    )
                )

                # After first test
                test1_tp <- diseased * test1_sens
                test1_fp <- healthy * (1 - test1_spec)
                test1_fn <- diseased * (1 - test1_sens)
                test1_tn <- healthy * test1_spec
                test1_pos <- test1_tp + test1_fp
                test1_neg <- test1_fn + test1_tn

                flowTable$setRow(
                    rowKey = "after_test1",
                    values = list(
                        stage = "After First Test",
                        total_n = pop_size,
                        disease_pos = diseased,
                        disease_neg = healthy,
                        test_pos = test1_pos,
                        test_neg = test1_neg,
                        true_pos = test1_tp,
                        false_pos = test1_fp,
                        false_neg = test1_fn,
                        true_neg = test1_tn
                    )
                )

                # After second test (depends on strategy)
                if (strategy == "serial_positive") {
                    # Only test1 positives get test2
                    test2_population <- test1_pos
                    diseased_in_test2 <- test1_tp
                    healthy_in_test2 <- test1_fp

                    test2_tp <- diseased_in_test2 * test2_sens
                    test2_fp <- healthy_in_test2 * (1 - test2_spec)
                    test2_fn <- diseased_in_test2 * (1 - test2_sens)
                    test2_tn <- healthy_in_test2 * test2_spec

                    # Final counts include test1 negatives plus test2 results
                    final_tp <- test2_tp
                    final_fp <- test2_fp
                    final_fn <- test1_fn + test2_fn
                    final_tn <- test1_tn + test2_tn

                } else if (strategy == "serial_negative") {
                    # Only test1 negatives get test2
                    test2_population <- test1_neg
                    diseased_in_test2 <- test1_fn
                    healthy_in_test2 <- test1_tn

                    test2_tp <- diseased_in_test2 * test2_sens
                    test2_fp <- healthy_in_test2 * (1 - test2_spec)
                    test2_fn <- diseased_in_test2 * (1 - test2_sens)
                    test2_tn <- healthy_in_test2 * test2_spec

                    # Final counts include test1 positives plus test2 results
                    final_tp <- test1_tp + test2_tp
                    final_fp <- test1_fp + test2_fp
                    final_fn <- test2_fn
                    final_tn <- test2_tn

                } else if (strategy == "parallel") {
                    # Everyone gets both tests
                    # A person is positive if either test is positive
                    test2_population <- pop_size

                    # Positives are those who test positive on either test
                    # This is a simplification as it assumes test independence
                    final_tp <- diseased * combined_sens
                    final_fp <- healthy * (1 - combined_spec)
                    final_fn <- diseased * (1 - combined_sens)
                    final_tn <- healthy * combined_spec
                }

                final_pos <- final_tp + final_fp
                final_neg <- final_fn + final_tn

                tp_rate <- if (diseased > 0) final_tp / diseased else NA_real_
                tn_rate <- if (healthy > 0) final_tn / healthy else NA_real_
                tp_rate_text <- if (is.na(tp_rate)) "not defined (no diseased subjects)" else format_percent(tp_rate)
                tn_rate_text <- if (is.na(tn_rate)) "not defined (no disease-free subjects)" else format_percent(tn_rate)

                flowTable$setRow(
                    rowKey = "after_test2",
                    values = list(
                        stage = "After Combined Strategy",
                        total_n = pop_size,
                        disease_pos = diseased,
                        disease_neg = healthy,
                        test_pos = final_pos,
                        test_neg = final_neg,
                        true_pos = final_tp,
                        false_pos = final_fp,
                        false_neg = final_fn,
                        true_neg = final_tn
                    )
                )

                # --- Cost Analysis ---
                if (self$options$show_cost_analysis) {
                    test1_cost <- self$options$test1_cost
                    test2_cost <- self$options$test2_cost
                    
                    # Calculate number of tests
                    n_test1 <- pop_size
                    n_test2 <- 0
                    
                    if (strategy == "serial_positive") {
                        n_test2 <- test1_pos
                    } else if (strategy == "serial_negative") {
                        n_test2 <- test1_neg
                    } else if (strategy == "parallel") {
                        n_test2 <- pop_size
                    }
                    
                    total_cost1 <- n_test1 * test1_cost
                    total_cost2 <- n_test2 * test2_cost
                    total_combined <- total_cost1 + total_cost2
                    
                    costTable <- self$results$cost_analysis_table
                    
                    costTable$addRow(rowKey = "test1", values = list(
                        item = paste0("Test 1: ", test1_name),
                        unit_cost = test1_cost,
                        number_tests = as.integer(n_test1),
                        total_cost = total_cost1
                    ))
                    
                    costTable$addRow(rowKey = "test2", values = list(
                        item = paste0("Test 2: ", test2_name),
                        unit_cost = test2_cost,
                        number_tests = as.integer(n_test2),
                        total_cost = total_cost2
                    ))
                    
                    costTable$addRow(rowKey = "total", values = list(
                        item = "Total Protocol Cost",
                        unit_cost = NA,
                        number_tests = as.integer(n_test1 + n_test2),
                        total_cost = total_combined
                    ))
                }

                # Update summary table
                summaryTable <- self$results$summary_table
                
                # Add independence warning if parallel strategy is used
                if (strategy == "parallel") {
                    summaryTable$setNote(
                        key = "independence_warning",
                        note = "Note: Parallel testing calculations assume conditional independence between tests. If tests are correlated, combined sensitivity/specificity may be overestimated."
                    )
                }
                
                # Generate explanation HTML
                if (self$options$show_explanation) {
                    explanation <- ""

                    # Introduction to sequential testing
                    explanation <- paste0(explanation,
                                          "<h3>Sequential Testing Strategy Explanation</h3>")

                    if (strategy == "serial_positive") {
                        explanation <- paste0(
                            explanation,
                            "<p>You've selected a <strong>serial testing strategy testing positives</strong> (also called a confirmation strategy). In this approach:</p>"
                        )
                        explanation <- paste0(explanation, "<ol>")
                        explanation <- paste0(
                            explanation,
                            "<li>All subjects are first tested with ",
                            test1_name,
                            " (sensitivity = ",
                            format(test1_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test1_spec * 100, digits = 1),
                            "%)</li>"
                        )
                        explanation <- paste0(
                            explanation,
                            "<li>Only those who test positive on the first test receive ",
                            test2_name,
                            " (sensitivity = ",
                            format(test2_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test2_spec * 100, digits = 1),
                            "%)</li>"
                        )
                        explanation <- paste0(
                            explanation,
                            "<li>A subject is considered positive only if they test positive on both tests</li>"
                        )
                        explanation <- paste0(explanation, "</ol>")
                        explanation <- paste0(
                            explanation,
                            "<p>This strategy <strong>maximizes specificity</strong> at the expense of sensitivity. It's useful when:</p>"
                        )
                        explanation <- paste0(explanation, "<ul>")
                        explanation <- paste0(
                            explanation,
                            "<li>The first test has good sensitivity but lower specificity</li>"
                        )
                        explanation <- paste0(explanation,
                                              "<li>The second test has high specificity</li>")
                        explanation <- paste0(explanation,
                                              "<li>You want to minimize false positives</li>")
                        explanation <- paste0(
                            explanation,
                            "<li>The consequences of false positive results are serious (e.g., harmful or expensive treatments)</li>"
                        )
                        explanation <- paste0(explanation, "</ul>")
                    } else if (strategy == "serial_negative") {
                        explanation <- paste0(
                            explanation,
                            "<p>You've selected a <strong>serial testing strategy testing negatives</strong> (also called an exclusion strategy). In this approach:</p>"
                        )
                        explanation <- paste0(explanation, "<ol>")
                        explanation <- paste0(
                            explanation,
                            "<li>All subjects are first tested with ",
                            test1_name,
                            " (sensitivity = ",
                            format(test1_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test1_spec * 100, digits = 1),
                            "%)</li>"
                        )
                        explanation <- paste0(
                            explanation,
                            "<li>Only those who test negative on the first test receive ",
                            test2_name,
                            " (sensitivity = ",
                            format(test2_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test2_spec * 100, digits = 1),
                            "%)</li>"
                        )
                        explanation <- paste0(
                            explanation,
                            "<li>A subject is considered negative only if they test negative on both tests</li>"
                        )
                        explanation <- paste0(explanation, "</ol>")
                        explanation <- paste0(
                            explanation,
                            "<p>This strategy <strong>maximizes sensitivity</strong> at the expense of specificity. It's useful when:</p>"
                        )
                        explanation <- paste0(explanation, "<ul>")
                        explanation <- paste0(
                            explanation,
                            "<li>The first test has good specificity but lower sensitivity</li>"
                        )
                        explanation <- paste0(explanation,
                                              "<li>The second test has high sensitivity</li>")
                        explanation <- paste0(explanation,
                                              "<li>You want to minimize false negatives</li>")
                        explanation <- paste0(
                            explanation,
                            "<li>The consequences of false negative results are serious (e.g., missing a serious diagnosis)</li>"
                        )
                        explanation <- paste0(explanation, "</ul>")
                    } else if (strategy == "parallel") {
                        explanation <- paste0(
                            explanation,
                            "<p>You've selected a <strong>parallel testing strategy</strong>. In this approach:</p>"
                        )
                        explanation <- paste0(explanation, "<ol>")
                        explanation <- paste0(
                            explanation,
                            "<li>All subjects receive both ",
                            test1_name,
                            " (sensitivity = ",
                            format(test1_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test1_spec * 100, digits = 1),
                            "%) and ",
                            test2_name,
                            " (sensitivity = ",
                            format(test2_sens * 100, digits = 1),
                            "%, specificity = ",
                            format(test2_spec * 100, digits = 1),
                            "%)</li>"
                        )
                        explanation <- paste0(
                            explanation,
                            "<li>A subject is considered positive if they test positive on either test</li>"
                        )
                        explanation <- paste0(explanation, "</ol>")
                        explanation <- paste0(
                            explanation,
                            "<p>This strategy <strong>maximizes sensitivity</strong> at the expense of specificity. It's useful when:</p>"
                        )
                        explanation <- paste0(explanation, "<ul>")
                        explanation <- paste0(explanation,
                                              "<li>You want to minimize false negatives</li>")
                        explanation <- paste0(
                            explanation,
                            "<li>The tests complement each other by detecting different manifestations of the disease</li>"
                        )
                        explanation <- paste0(explanation,
                                              "<li>Missing the diagnosis has serious consequences</li>")
                        explanation <- paste0(explanation, "</ul>")
                    }

                    # Results explanation
                    explanation <- paste0(explanation, "<h3>Results Interpretation</h3>")
                    explanation <- paste0(
                        explanation,
                        "<p>With a disease prevalence of ",
                        format_percent(prevalence),
                        ", the combined testing strategy results in:</p>"
                    )
                    explanation <- paste0(explanation, "<ul>")
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined Sensitivity:</strong> ",
                        format_percent(combined_sens),
                        " (ability to correctly identify those with disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined Specificity:</strong> ",
                        format_percent(combined_spec),
                        " (ability to correctly identify those without disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined PPV:</strong> ",
                        format_percent(combined_ppv),
                        " (probability that subjects with a positive test truly have the disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined NPV:</strong> ",
                        format_percent(combined_npv),
                        " (probability that subjects with a negative test truly do not have the disease)</li>"
                    )
                    explanation <- paste0(explanation, "</ul>")

                explanation <- paste0(
                    explanation,
                    "<p>In a population of ",
                    format(pop_size, big.mark = ","),
                    " people:</p>"
                )
                    explanation <- paste0(explanation, "<ul>")
                    explanation <- paste0(
                        explanation,
                        sprintf(
                            "<li>%.0f would test positive in the combined strategy</li>",
                            final_pos
                        )
                    )
                    explanation <- paste0(
                        explanation,
                        sprintf(
                            "<li>%.0f would test negative in the combined strategy</li>",
                            final_neg
                        )
                    )
                    explanation <- paste0(
                        explanation,
                        sprintf(
                            "<li>Of the %.0f true positives, %.0f would be correctly identified (%s)</li>",
                            diseased,
                            final_tp,
                            tp_rate_text
                        )
                    )
                    explanation <- paste0(
                        explanation,
                        sprintf(
                            "<li>Of the %.0f true negatives, %.0f would be correctly identified (%s)</li>",
                            healthy,
                            final_tn,
                            tn_rate_text
                        )
                    )
                    explanation <- paste0(explanation, "</ul>")

                    # Add independence assumption note to explanation
                    independence_note <- paste0(
                        "<div style='background-color:#f8f9fa;padding:10px;border-radius:6px;margin-top:8px;'>",
                        "<strong>Assumption:</strong> Combined metrics assume conditional independence between tests. ",
                        "If tests are correlated (similar biology/technology), combined PPV/NPV may be overstated.",
                        "</div>"
                    )
                    self$results$explanation_text$setContent(paste0(independence_note, explanation))
                }

                # Populate clinical guidance
                guidance_html <- paste0(
                    "<div class='jmv-guidance' style='background-color:#f8f9fa;padding:15px;border-radius:6px;margin-top:10px;'>",
                    "<h4>Clinical Decision Making Guide</h4>",
                    "<p><strong>When to Use Each Strategy:</strong></p>",
                    "<ul>",
                    "<li><strong>Serial Positive (Confirmation):</strong> Use when false positives are costly or harmful. First test should be sensitive, second test should be specific.</li>",
                    "<li><strong>Serial Negative (Exclusion):</strong> Use when false negatives are dangerous. First test should be specific, second test should be sensitive.</li>",
                    "<li><strong>Parallel Testing:</strong> Use when rapid diagnosis is critical and both tests can be performed simultaneously.</li>",
                    "</ul>",
                    "<p><strong>Clinical Examples:</strong></p>",
                    "<ul>"
                )

                if (strategy == "serial_positive") {
                    guidance_html <- paste0(guidance_html,
                        "<li>HIV screening (ELISA → Western Blot)</li>",
                        "<li>Cancer screening (Imaging → Biopsy)</li>",
                        "<li>COVID-19 (Rapid Antigen → PCR)</li>"
                    )
                } else if (strategy == "serial_negative") {
                    guidance_html <- paste0(guidance_html,
                        "<li>Sepsis rule-out (Clinical → Biomarkers)</li>",
                        "<li>Pulmonary embolism exclusion (Wells Score → D-dimer)</li>"
                    )
                } else if (strategy == "parallel") {
                    guidance_html <- paste0(guidance_html,
                        "<li>Myocardial infarction (ECG + Troponin)</li>",
                        "<li>Stroke diagnosis (Clinical + CT scan)</li>"
                    )
                }

                guidance_html <- paste0(guidance_html, "</ul></div>")
                self$results$clinical_guidance$setContent(guidance_html)

                # Generate formulas HTML if requested
                if (self$options$show_formulas) {
                    formulas <- ""

                    formulas <- paste0(formulas, "<h3>Mathematical Framework for Sequential Testing</h3>")

                    # Basic probability definitions
                    formulas <- paste0(formulas, "<h4>Key Definitions</h4>")
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, "<li><strong>Prevalence (P)</strong>: The pre-test probability of disease</li>")
                    formulas <- paste0(formulas, "<li><strong>Sensitivity (Se)</strong>: Probability of a positive test in diseased subjects</li>")
                    formulas <- paste0(formulas, "<li><strong>Specificity (Sp)</strong>: Probability of a negative test in non-diseased subjects</li>")
                    formulas <- paste0(formulas, "<li><strong>Positive Likelihood Ratio (LR+)</strong>: Se/(1-Sp)</li>")
                    formulas <- paste0(formulas, "<li><strong>Negative Likelihood Ratio (LR-)</strong>: (1-Se)/Sp</li>")
                    formulas <- paste0(formulas, "</ul>")

                    # Pre-test to post-test probability conversion
                    formulas <- paste0(formulas, "<h4>Probability Conversions</h4>")
                    formulas <- paste0(formulas, "<p>To calculate how a test changes probability:</p>")

                    formulas <- paste0(formulas, "<ol>")
                    formulas <- paste0(formulas, "<li><strong>Convert pre-test probability to odds</strong>: Odds = P/(1-P)</li>")
                    formulas <- paste0(formulas, "<li><strong>Multiply odds by likelihood ratio</strong>: Post-test odds = Pre-test odds × LR</li>")
                    formulas <- paste0(formulas, "<li><strong>Convert post-test odds back to probability</strong>: Post-test P = Odds/(1+Odds)</li>")
                    formulas <- paste0(formulas, "</ol>")

                    # Strategy-specific formulas
                    if (strategy == "serial_positive") {
                        formulas <- paste0(formulas, "<h4>Serial Testing Strategy (Testing Positives)</h4>")

                        # Explain the approach
                        formulas <- paste0(formulas, "<p>In this strategy, the second test is only performed if the first test is positive. A subject is considered positive only if both tests are positive.</p>")

                        # Sensitivity calculation
                        formulas <- paste0(formulas, "<h5>Combined Sensitivity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test positive in this strategy, they must test positive on both tests:</p>")
                        formulas <- paste0(formulas, "<p>Se<sub>combined</sub> = Se<sub>1</sub> × Se<sub>2</sub></p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on Test 1: ", format(test1_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Given positive on Test 1, probability of testing positive on Test 2: ", format(test2_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_sens, digits=4), " × ", format(test2_sens, digits=4), " = ", format(combined_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, "<h5>Combined Specificity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test negative in this strategy, they must either:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Test negative on Test 1, OR</li>")
                        formulas <- paste0(formulas, "<li>Test positive on Test 1 but negative on Test 2</li>")
                        formulas <- paste0(formulas, "</ul>")
                        formulas <- paste0(formulas, "<p>Sp<sub>combined</sub> = Sp<sub>1</sub> + (1-Sp<sub>1</sub>) × Sp<sub>2</sub></p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing negative on Test 1: ", format(test1_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on Test 1 but negative on Test 2: (1-", format(test1_spec, digits=4), ") × ", format(test2_spec, digits=4), " = ", format((1-test1_spec)*test2_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_spec, digits=4), " + ", format((1-test1_spec)*test2_spec, digits=4), " = ", format(combined_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")

                    } else if (strategy == "serial_negative") {
                        formulas <- paste0(formulas, "<h4>Serial Testing Strategy (Testing Negatives)</h4>")

                        # Explain the approach
                        formulas <- paste0(formulas, "<p>In this strategy, the second test is only performed if the first test is negative. A subject is considered positive if either test is positive.</p>")

                        # Sensitivity calculation
                        formulas <- paste0(formulas, "<h5>Combined Sensitivity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test positive in this strategy, they must either:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Test positive on Test 1, OR</li>")
                        formulas <- paste0(formulas, "<li>Test negative on Test 1 but positive on Test 2</li>")
                        formulas <- paste0(formulas, "</ul>")
                        formulas <- paste0(formulas, "<p>Se<sub>combined</sub> = Se<sub>1</sub> + (1-Se<sub>1</sub>) × Se<sub>2</sub></p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on Test 1: ", format(test1_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Probability of testing negative on Test 1 but positive on Test 2: (1-", format(test1_sens, digits=4), ") × ", format(test2_sens, digits=4), " = ", format((1-test1_sens)*test2_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_sens, digits=4), " + ", format((1-test1_sens)*test2_sens, digits=4), " = ", format(combined_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, "<h5>Combined Specificity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>")
                        formulas <- paste0(formulas, "<p>Sp<sub>combined</sub> = Sp<sub>1</sub> × Sp<sub>2</sub></p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing negative on Test 1: ", format(test1_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Given negative on Test 1, probability of testing negative on Test 2: ", format(test2_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_spec, digits=4), " × ", format(test2_spec, digits=4), " = ", format(combined_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")

                    } else if (strategy == "parallel") {
                        formulas <- paste0(formulas, "<h4>Parallel Testing Strategy</h4>")

                        # Explain the approach
                        formulas <- paste0(formulas, "<p>In this strategy, both tests are performed on all subjects. A subject is considered positive if either test is positive.</p>")

                        # Sensitivity calculation
                        formulas <- paste0(formulas, "<h5>Combined Sensitivity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test positive in this strategy, they must test positive on at least one test. This is calculated using the complement of the probability of testing negative on both tests:</p>")
                        formulas <- paste0(formulas, "<p>Se<sub>combined</sub> = 1 - (1-Se<sub>1</sub>) × (1-Se<sub>2</sub>)</p>")
                        formulas <- paste0(formulas, "<p>This can be rewritten as:</p>")
                        formulas <- paste0(formulas, "<p>Se<sub>combined</sub> = Se<sub>1</sub> + Se<sub>2</sub> - (Se<sub>1</sub> × Se<sub>2</sub>)</p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on Test 1: ", format(test1_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on Test 2: ", format(test2_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Probability of testing positive on both: ", format(test1_sens, digits=4), " × ", format(test2_sens, digits=4), " = ", format(test1_sens*test2_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_sens, digits=4), " + ", format(test2_sens, digits=4), " - ", format(test1_sens*test2_sens, digits=4), " = ", format(combined_sens, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, "<h5>Combined Specificity</h5>")
                        formulas <- paste0(formulas, "<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>")
                        formulas <- paste0(formulas, "<p>Sp<sub>combined</sub> = Sp<sub>1</sub> × Sp<sub>2</sub></p>")
                        formulas <- paste0(formulas, "<p>Probability calculation:</p>")
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, "<li>Probability of testing negative on Test 1: ", format(test1_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Probability of testing negative on Test 2: ", format(test2_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "<li>Combined probability = ", format(test1_spec, digits=4), " × ", format(test2_spec, digits=4), " = ", format(combined_spec, digits=4), "</li>")
                        formulas <- paste0(formulas, "</ul>")
                    }

                    # Predictive values calculation
                    formulas <- paste0(formulas, "<h4>Predictive Values Calculations</h4>")

                    # Positive Predictive Value
                    formulas <- paste0(formulas, "<h5>Positive Predictive Value (PPV)</h5>")
                    formulas <- paste0(formulas, "<p>The probability that a positive test result is a true positive:</p>")
                    formulas <- paste0(formulas, "<p>PPV = (P × Se) / (P × Se + (1-P) × (1-Sp))</p>")

                    # Calculate intermediate values for clarity
                    ppv_numerator = prevalence * combined_sens
                    ppv_denominator = prevalence * combined_sens + (1-prevalence) * (1-combined_spec)

                    formulas <- paste0(formulas, "<p>Calculation steps:</p>")
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, "<li>Prevalence (P) = ", format(prevalence, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Combined Sensitivity (Se) = ", format(combined_sens, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Combined Specificity (Sp) = ", format(combined_spec, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Numerator = P × Se = ", format(prevalence, digits=4), " × ", format(combined_sens, digits=4), " = ", format(ppv_numerator, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Denominator = P × Se + (1-P) × (1-Sp) = ", format(ppv_numerator, digits=4), " + ", format((1-prevalence), digits=4), " × ", format((1-combined_spec), digits=4), " = ", format(ppv_denominator, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>PPV = Numerator/Denominator = ", format(ppv_numerator, digits=4), "/", format(ppv_denominator, digits=4), " = ", format(combined_ppv, digits=4), "</li>")
                    formulas <- paste0(formulas, "</ul>")

                    # Negative Predictive Value
                    formulas <- paste0(formulas, "<h5>Negative Predictive Value (NPV)</h5>")
                    formulas <- paste0(formulas, "<p>The probability that a negative test result is a true negative:</p>")
                    formulas <- paste0(formulas, "<p>NPV = ((1-P) × Sp) / ((1-P) × Sp + P × (1-Se))</p>")

                    # Calculate intermediate values for clarity
                    npv_numerator = (1-prevalence) * combined_spec
                    npv_denominator = (1-prevalence) * combined_spec + prevalence * (1-combined_sens)

                    formulas <- paste0(formulas, "<p>Calculation steps:</p>")
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, "<li>Prevalence (P) = ", format(prevalence, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Combined Sensitivity (Se) = ", format(combined_sens, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Combined Specificity (Sp) = ", format(combined_spec, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Numerator = (1-P) × Sp = ", format((1-prevalence), digits=4), " × ", format(combined_spec, digits=4), " = ", format(npv_numerator, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>Denominator = (1-P) × Sp + P × (1-Se) = ", format(npv_numerator, digits=4), " + ", format(prevalence, digits=4), " × ", format((1-combined_sens), digits=4), " = ", format(npv_denominator, digits=4), "</li>")
                    formulas <- paste0(formulas, "<li>NPV = Numerator/Denominator = ", format(npv_numerator, digits=4), "/", format(npv_denominator, digits=4), " = ", format(combined_npv, digits=4), "</li>")
                    formulas <- paste0(formulas, "</ul>")

                    # Likelihood ratios
                    formulas <- paste0(formulas, "<h4>Likelihood Ratios</h4>")

                    # Positive likelihood ratio
                    formulas <- paste0(formulas, "<h5>Positive Likelihood Ratio (LR+)</h5>")
                    formulas <- paste0(formulas, "<p>How much more likely a positive test result is to occur in patients with disease compared to those without:</p>")
                    formulas <- paste0(formulas, "<p>LR+ = Sensitivity / (1 - Specificity)</p>")
                    formulas <- paste0(formulas, "<p>LR+ = ", format(combined_sens, digits=4), " / (1 - ", format(combined_spec, digits=4), ") = ", format(combined_sens/(1-combined_spec), digits=4), "</p>")

                    # Negative likelihood ratio
                    formulas <- paste0(formulas, "<h5>Negative Likelihood Ratio (LR-)</h5>")
                    formulas <- paste0(formulas, "<p>How much more likely a negative test result is to occur in patients with disease compared to those without:</p>")
                    formulas <- paste0(formulas, "<p>LR- = (1 - Sensitivity) / Specificity</p>")
                    formulas <- paste0(formulas, "<p>LR- = (1 - ", format(combined_sens, digits=4), ") / ", format(combined_spec, digits=4), " = ", format((1-combined_sens)/combined_spec, digits=4), "</p>")

                    self$results$formulas_text$setContent(formulas)
                }

                # Store data for plots
                if (self$options$show_nomogram) {
                    plotData <- list(
                        "Prevalence" = prevalence,
                        "Test1_Name" = test1_name,
                        "Test1_Sens" = test1_sens,
                        "Test1_Spec" = test1_spec,
                        "Test2_Name" = test2_name,
                        "Test2_Sens" = test2_sens,
                        "Test2_Spec" = test2_spec,
                        "Strategy" = strategy,
                        "Combined_Sens" = combined_sens,
                        "Combined_Spec" = combined_spec,
                        "Combined_PPV" = combined_ppv,
                        "Combined_NPV" = combined_npv,
                        "Final_TP" = final_tp,
                        "Final_FP" = final_fp,
                        "Final_FN" = final_fn,
                        "Final_TN" = final_tn,
                        "Pop_Size" = pop_size,
                        "Diseased" = diseased,
                        "Healthy" = healthy
                    )

                    # Set state for all plots
                    self$results$plot_flow_diagram$setState(plotData)
                    self$results$plot_performance$setState(plotData)
                    self$results$plot_probability$setState(plotData)
                    self$results$plot_population_flow$setState(plotData)
                    self$results$plot_sensitivity_analysis$setState(plotData)
                }

                # Success notices at bottom
                private$.addNotice(
                    type = "INFO",
                    title = "Analysis Complete",
                    content = sprintf('Sequential testing analysis completed: %s strategy with prevalence %.1f%%, combined sensitivity %.1f%%, combined specificity %.1f%%.',
                                        strategy_name, prevalence*100, combined_sens*100, combined_spec*100)
                )

                # Independence assumption notice
                private$.addNotice(
                    type = "INFO",
                    title = "Independence Assumption",
                    content = 'Combined metrics assume conditional independence between tests. If tests are correlated (similar biology/technology), combined performance may be overestimated.'
                )

                # Render all collected notices as HTML (last step)
                private$.renderNotices()

                },


            .plot_flow_diagram = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state
                strategy <- plotData$Strategy

                # Enhanced flow diagram with better visual design
                if (strategy == "serial_positive") {
                    # Create flow for serial positive testing
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", size = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = paste0("All Patients\n(n=", plotData$Pop_Size, ")"),
                                        size = 4, fontface = "bold") +

                        # First test
                        ggplot2::annotate("segment", x = 1.5, y = 5, xend = 2.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", size = 1) +
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 3, y = 5, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Positive branch
                        ggplot2::annotate("segment", x = 3.5, y = 5.2, xend = 4.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "red", size = 1) +
                        ggplot2::annotate("text", x = 4, y = 5.6, label = "Positive",
                                        size = 3, color = "red", angle = 30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 5.5, ymax = 6.5,
                                        fill = "lightyellow", color = "orange", size = 1) +
                        ggplot2::annotate("text", x = 5, y = 6, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Negative branch
                        ggplot2::annotate("segment", x = 3.5, y = 4.8, xend = 4.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 4, y = 4.4, label = "Negative",
                                        size = 3, color = "darkgreen", angle = -30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 3.5, ymax = 4.5,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 5, y = 4, label = "Negative\nResult",
                                        size = 3.5, fontface = "bold") +

                        # Final results from second test
                        ggplot2::annotate("segment", x = 5.5, y = 6.2, xend = 6.5, yend = 6.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "red", size = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 6, ymax = 7,
                                        fill = "#ffcccc", color = "red", size = 1) +
                        ggplot2::annotate("text", x = 7, y = 6.5, label = "Positive\nResult",
                                        size = 3.5, fontface = "bold", color = "red") +

                        ggplot2::annotate("segment", x = 5.5, y = 5.8, xend = 6.5, yend = 5.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkgreen", size = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 5, ymax = 6,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 7, y = 5.5, label = "Negative\nResult",
                                        size = 3.5, fontface = "bold", color = "darkgreen") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7.5) +
                        ggplot2::labs(title = "Serial Testing Strategy: Test Positives",
                                    subtitle = "Second test only for those positive on first test") +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                     axis.title = ggplot2::element_blank(),
                                     panel.grid = ggplot2::element_blank())

                } else if (strategy == "serial_negative") {
                    # Create flow for serial negative testing
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", size = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = paste0("All Patients\n(n=", plotData$Pop_Size, ")"),
                                        size = 4, fontface = "bold") +

                        # First test
                        ggplot2::annotate("segment", x = 1.5, y = 5, xend = 2.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", size = 1) +
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 3, y = 5, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Positive branch (final result)
                        ggplot2::annotate("segment", x = 3.5, y = 5.2, xend = 4.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "red", size = 1) +
                        ggplot2::annotate("text", x = 4, y = 5.6, label = "Positive",
                                        size = 3, color = "red", angle = 30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 5.5, ymax = 6.5,
                                        fill = "#ffcccc", color = "red", size = 1) +
                        ggplot2::annotate("text", x = 5, y = 6, label = "Positive\nResult",
                                        size = 3.5, fontface = "bold", color = "red") +

                        # Negative branch (needs second test)
                        ggplot2::annotate("segment", x = 3.5, y = 4.8, xend = 4.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "orange", size = 1) +
                        ggplot2::annotate("text", x = 4, y = 4.4, label = "Negative",
                                        size = 3, color = "orange", angle = -30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 3.5, ymax = 4.5,
                                        fill = "lightyellow", color = "orange", size = 1) +
                        ggplot2::annotate("text", x = 5, y = 4, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Final results from second test
                        ggplot2::annotate("segment", x = 5.5, y = 4.2, xend = 6.5, yend = 4.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "red", size = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4, ymax = 5,
                                        fill = "#ffcccc", color = "red", size = 1) +
                        ggplot2::annotate("text", x = 7, y = 4.5, label = "Positive\nResult",
                                        size = 3.5, fontface = "bold", color = "red") +

                        ggplot2::annotate("segment", x = 5.5, y = 3.8, xend = 6.5, yend = 3.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkgreen", size = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 3, ymax = 4,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 7, y = 3.5, label = "Negative\nResult",
                                        size = 3.5, fontface = "bold", color = "darkgreen") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(2.5, 7) +
                        ggplot2::labs(title = "Serial Testing Strategy: Test Negatives",
                                    subtitle = "Second test only for those negative on first test") +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                     axis.title = ggplot2::element_blank(),
                                     panel.grid = ggplot2::element_blank())

                } else {
                    # Parallel testing flow
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", size = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = paste0("All Patients\n(n=", plotData$Pop_Size, ")"),
                                        size = 4, fontface = "bold") +

                        # Split to both tests
                        ggplot2::annotate("segment", x = 1.5, y = 5.2, xend = 2.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", size = 1) +
                        ggplot2::annotate("segment", x = 1.5, y = 4.8, xend = 2.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", size = 1) +

                        # Test 1
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 5.5, ymax = 6.5,
                                        fill = "lightgreen", color = "darkgreen", size = 1) +
                        ggplot2::annotate("text", x = 3, y = 6, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Test 2
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 3.5, ymax = 4.5,
                                        fill = "lightyellow", color = "orange", size = 1) +
                        ggplot2::annotate("text", x = 3, y = 4, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Combine results
                        ggplot2::annotate("segment", x = 3.5, y = 6, xend = 4.5, yend = 5.2,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "purple", size = 1) +
                        ggplot2::annotate("segment", x = 3.5, y = 4, xend = 4.5, yend = 4.8,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "purple", size = 1) +

                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lavender", color = "purple", size = 1) +
                        ggplot2::annotate("text", x = 5, y = 5, label = "Combine\nResults",
                                        size = 4, fontface = "bold") +

                        # Final result
                        ggplot2::annotate("segment", x = 5.5, y = 5, xend = 6.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "purple", size = 1) +
                        ggplot2::annotate("text", x = 6, y = 5.3, label = "Either +",
                                        size = 3, color = "purple") +

                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4.5, ymax = 5.5,
                                        fill = "white", color = "black", size = 1) +
                        ggplot2::annotate("text", x = 7, y = 5, label = "Final\nResult",
                                        size = 4, fontface = "bold") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7) +
                        ggplot2::labs(title = "Parallel Testing Strategy",
                                    subtitle = "Both tests performed on all patients") +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                     axis.title = ggplot2::element_blank(),
                                     panel.grid = ggplot2::element_blank())
                }

                print(flow_plot)
                return(TRUE)
            },

            .plot_performance = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                # Create comparison data
                perf_data <- data.frame(
                    Test = rep(c(plotData$Test1_Name, plotData$Test2_Name, "Combined"), 4),
                    Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), each = 3),
                    Value = c(
                        plotData$Test1_Sens * 100, plotData$Test2_Sens * 100, plotData$Combined_Sens * 100,
                        plotData$Test1_Spec * 100, plotData$Test2_Spec * 100, plotData$Combined_Spec * 100,
                        plotData$Test1_Sens / (plotData$Test1_Sens + (1-plotData$Test1_Spec) * (1-plotData$Prevalence)/plotData$Prevalence) * 100,
                        plotData$Test2_Sens / (plotData$Test2_Sens + (1-plotData$Test2_Spec) * (1-plotData$Prevalence)/plotData$Prevalence) * 100,
                        plotData$Combined_PPV * 100,
                        plotData$Test1_Spec / (plotData$Test1_Spec + (1-plotData$Test1_Sens) * plotData$Prevalence/(1-plotData$Prevalence)) * 100,
                        plotData$Test2_Spec / (plotData$Test2_Spec + (1-plotData$Test2_Sens) * plotData$Prevalence/(1-plotData$Prevalence)) * 100,
                        plotData$Combined_NPV * 100
                    )
                )

                perf_data$Test <- factor(perf_data$Test, levels = c(plotData$Test1_Name, plotData$Test2_Name, "Combined"))

                perf_plot <- ggplot2::ggplot(perf_data, ggplot2::aes(x = Test, y = Value, fill = Test)) +
                    ggplot2::geom_col(alpha = 0.7, position = "dodge") +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Value)),
                                      position = ggplot2::position_dodge(width = 0.9),
                                      vjust = -0.5, size = 3) +
                    ggplot2::facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
                    ggplot2::labs(title = "Test Performance Comparison",
                                subtitle = paste("Strategy:", plotData$Strategy),
                                y = "Value (%)", x = "") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(legend.position = "bottom",
                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggplot2::scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
                    ggplot2::ylim(0, 105)

                print(perf_plot)
                return(TRUE)
            },

            .plot_probability = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                # Calculate probability progression
                prevalence <- plotData$Prevalence

                # For positive test pathway
                test1_plr <- plotData$Test1_Sens / (1 - plotData$Test1_Spec)
                post_test1_pos_odds <- (prevalence / (1 - prevalence)) * test1_plr
                post_test1_pos_prob <- post_test1_pos_odds / (1 + post_test1_pos_odds)

                test2_plr <- plotData$Test2_Sens / (1 - plotData$Test2_Spec)
                post_test2_pos_odds <- post_test1_pos_odds * test2_plr
                post_test2_pos_prob <- post_test2_pos_odds / (1 + post_test2_pos_odds)

                # For negative test pathway
                test1_nlr <- (1 - plotData$Test1_Sens) / plotData$Test1_Spec
                post_test1_neg_odds <- (prevalence / (1 - prevalence)) * test1_nlr
                post_test1_neg_prob <- post_test1_neg_odds / (1 + post_test1_neg_odds)

                test2_nlr <- (1 - plotData$Test2_Sens) / plotData$Test2_Spec
                post_test2_neg_odds <- post_test1_neg_odds * test2_nlr
                post_test2_neg_prob <- post_test2_neg_odds / (1 + post_test2_neg_odds)

                # Create data for plotting based on strategy
                if (plotData$Strategy == "serial_positive") {
                    prob_data <- data.frame(
                        Step = c("Pre-test", "After Test 1 (+)", "After Test 2 (+)",
                                "Pre-test", "After Test 1 (-)", "Final (-)"),
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, post_test1_neg_prob * 100, post_test1_neg_prob * 100),
                        Path = c(rep("Positive Path", 3), rep("Negative Path", 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                } else if (plotData$Strategy == "serial_negative") {
                    prob_data <- data.frame(
                        Step = c("Pre-test", "After Test 1 (+)", "Final (+)",
                                "Pre-test", "After Test 1 (-)", "After Test 2 (-)"),
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100, post_test1_pos_prob * 100,
                                      prevalence * 100, post_test1_neg_prob * 100, (1 - plotData$Combined_NPV) * 100),
                        Path = c(rep("Positive Path", 3), rep("Negative Path", 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                } else {
                    # Parallel testing
                    prob_data <- data.frame(
                        Step = c("Pre-test", "After Either Test (+)", "Final PPV",
                                "Pre-test", "After Both Tests (-)", "Final NPV"),
                        Probability = c(prevalence * 100, plotData$Combined_PPV * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, (1 - plotData$Combined_NPV) * 100, (1 - plotData$Combined_NPV) * 100),
                        Path = c(rep("Positive Path", 3), rep("Negative Path", 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                }

                prob_plot <- ggplot2::ggplot(prob_data, ggplot2::aes(x = x, y = Probability, color = Path, group = Path)) +
                    ggplot2::geom_line(size = 2, alpha = 0.7) +
                    ggplot2::geom_point(size = 4) +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Probability)),
                                      vjust = -1.5, hjust = 0.5, size = 3) +
                    ggplot2::scale_x_continuous(breaks = 1:3, labels = c("Pre-test", "After Test 1", "Final")) +
                    ggplot2::scale_color_manual(values = c("Positive Path" = "red", "Negative Path" = "darkgreen")) +
                    ggplot2::labs(title = "Probability Transformation Through Testing",
                                subtitle = paste("Strategy:", plotData$Strategy, "| Prevalence:", sprintf("%.1f%%", plotData$Prevalence * 100)),
                                x = "Testing Stage", y = "Disease Probability (%)") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::ylim(0, max(prob_data$Probability) * 1.2)

                print(prob_plot)
                return(TRUE)
            },

            .plot_population_flow = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                # Create Sankey-like flow visualization
                pop_size <- plotData$Pop_Size
                diseased <- plotData$Diseased
                healthy <- plotData$Healthy

                # Create flow data
                flow_data <- data.frame(
                    Category = c("Total Population", "Disease Present", "Disease Absent",
                               "True Positive", "False Negative", "False Positive", "True Negative"),
                    Count = c(pop_size, diseased, healthy,
                            plotData$Final_TP, plotData$Final_FN, plotData$Final_FP, plotData$Final_TN),
                    Stage = c(1, 2, 2, 3, 3, 3, 3),
                    Type = c("Total", "Disease", "Healthy", "TP", "FN", "FP", "TN"),
                    y_pos = c(50, 70, 30, 85, 70, 30, 15)
                )

                # Create visualization
                flow_plot <- ggplot2::ggplot(flow_data) +
                    # Initial population
                    ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 25, ymax = 75,
                                    fill = "lightblue", alpha = 0.5) +
                    ggplot2::annotate("text", x = 1, y = 50, label = paste0("Total\nn=", pop_size),
                                    size = 5, fontface = "bold") +

                    # Disease status split
                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 55, ymax = 85,
                                    fill = "#ffcccc", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 70, label = paste0("Disease+\nn=", round(diseased)),
                                    size = 4, fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 15, ymax = 45,
                                    fill = "#ccffcc", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 30, label = paste0("Disease-\nn=", round(healthy)),
                                    size = 4, fontface = "bold") +

                    # Test results
                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 75, ymax = 90,
                                    fill = "darkgreen", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 82.5, label = paste0("TP\nn=", round(plotData$Final_TP)),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 60, ymax = 75,
                                    fill = "red", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 67.5, label = paste0("FN\nn=", round(plotData$Final_FN)),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 30, ymax = 45,
                                    fill = "orange", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 37.5, label = paste0("FP\nn=", round(plotData$Final_FP)),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 10, ymax = 30,
                                    fill = "darkgreen", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 20, label = paste0("TN\nn=", round(plotData$Final_TN)),
                                    size = 3.5, color = "white", fontface = "bold") +

                    # Add flow arrows
                    ggplot2::annotate("segment", x = 1.5, y = 60, xend = 2.5, yend = 70,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5) +
                    ggplot2::annotate("segment", x = 1.5, y = 40, xend = 2.5, yend = 30,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5) +

                    ggplot2::annotate("segment", x = 3.5, y = 75, xend = 4.5, yend = 82.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5, color = "darkgreen") +
                    ggplot2::annotate("segment", x = 3.5, y = 65, xend = 4.5, yend = 67.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5, color = "red") +
                    ggplot2::annotate("segment", x = 3.5, y = 35, xend = 4.5, yend = 37.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5, color = "orange") +
                    ggplot2::annotate("segment", x = 3.5, y = 25, xend = 4.5, yend = 20,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    size = 1, alpha = 0.5, color = "darkgreen") +

                    # Add labels
                    ggplot2::annotate("text", x = 1, y = 5, label = "Initial", size = 4, fontface = "bold") +
                    ggplot2::annotate("text", x = 3, y = 5, label = "True Status", size = 4, fontface = "bold") +
                    ggplot2::annotate("text", x = 5, y = 5, label = "Test Results", size = 4, fontface = "bold") +

                    ggplot2::xlim(0, 6) + ggplot2::ylim(0, 95) +
                    ggplot2::labs(title = "Population Flow Through Testing",
                                subtitle = paste("Strategy:", plotData$Strategy,
                                               "| Sensitivity:", sprintf("%.1f%%", plotData$Combined_Sens * 100),
                                               "| Specificity:", sprintf("%.1f%%", plotData$Combined_Spec * 100))) +
                    ggplot2::theme_void() +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

                print(flow_plot)
                return(TRUE)
            },

            .plot_sensitivity_analysis = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                # Create prevalence range from 0.01 to 0.99
                prev_range <- seq(0.01, 0.99, by = 0.01)

                # Calculate PPV across prevalence range
                ppv_curve <- sapply(prev_range, function(p) {
                    num <- p * plotData$Combined_Sens
                    denom <- num + (1 - p) * (1 - plotData$Combined_Spec)
                    if (abs(denom) < private$NUMERICAL_TOLERANCE) {
                        return(NA_real_)
                    } else {
                        return(num / denom)
                    }
                })

                # Calculate NPV across prevalence range
                npv_curve <- sapply(prev_range, function(p) {
                    num <- (1 - p) * plotData$Combined_Spec
                    denom <- num + p * (1 - plotData$Combined_Sens)
                    if (abs(denom) < private$NUMERICAL_TOLERANCE) {
                        return(NA_real_)
                    } else {
                        return(num / denom)
                    }
                })

                # Create data frame for plotting
                df <- data.frame(
                    Prevalence = rep(prev_range, 2),
                    Value = c(ppv_curve, npv_curve),
                    Metric = rep(c("PPV (Positive Predictive Value)", "NPV (Negative Predictive Value)"), each = length(prev_range))
                )

                # Remove NA values
                df <- df[!is.na(df$Value), ]

                # Create sensitivity analysis plot
                sens_plot <- ggplot2::ggplot(df, ggplot2::aes(x = Prevalence, y = Value, color = Metric)) +
                    ggplot2::geom_line(size = 1.5, alpha = 0.8) +
                    ggplot2::geom_vline(xintercept = plotData$Prevalence, linetype = "dashed",
                                       color = "gray40", size = 0.8, alpha = 0.7) +
                    ggplot2::annotate("text", x = plotData$Prevalence, y = 0.95,
                                     label = sprintf("Your prevalence\n(%.1f%%)", plotData$Prevalence * 100),
                                     size = 3, hjust = ifelse(plotData$Prevalence > 0.5, 1.1, -0.1)) +
                    ggplot2::labs(
                        title = "Sensitivity Analysis: How Prevalence Affects Predictive Values",
                        subtitle = sprintf("Combined Test Performance: Sensitivity=%.1f%%, Specificity=%.1f%%",
                                          plotData$Combined_Sens * 100, plotData$Combined_Spec * 100),
                        x = "Disease Prevalence",
                        y = "Probability",
                        color = ""
                    ) +
                    ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100),
                                               limits = c(0, 1)) +
                    ggplot2::scale_x_continuous(labels = function(x) sprintf("%.0f%%", x * 100)) +
                    ggplot2::scale_color_manual(values = c("PPV (Positive Predictive Value)" = "#e74c3c",
                                                           "NPV (Negative Predictive Value)" = "#27ae60")) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        legend.position = "bottom",
                        legend.text = ggplot2::element_text(size = 10),
                        plot.title = ggplot2::element_text(face = "bold", size = 12),
                        plot.subtitle = ggplot2::element_text(size = 10, color = "gray40")
                    )

                print(sens_plot)
                return(TRUE)
            },

            # Notice collection and rendering (converted from insert(999, ) to avoid serialization errors)
            .noticeList = list(),

            # Add a notice to the collection
            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
            },

            # HTML sanitization for security
            .safeHtmlOutput = function(text) {
                if (is.null(text) || length(text) == 0) return("")
                text <- as.character(text)
                # Sanitize potentially dangerous characters
                text <- gsub("&", "&amp;", text, fixed = TRUE)
                text <- gsub("<", "&lt;", text, fixed = TRUE)
                text <- gsub(">", "&gt;", text, fixed = TRUE)
                text <- gsub("\"", "&quot;", text, fixed = TRUE)
                text <- gsub("'", "&#x27;", text, fixed = TRUE)
                text <- gsub("/", "&#x2F;", text, fixed = TRUE)
                return(text)
            },

            # Render collected notices as HTML
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    return()
                }

                # Map notice types to colors and icons
                typeStyles <- list(
                    ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
                    STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
                    WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
                    INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
                )

                html <- "<div style='margin: 10px 0;'>"

                for (notice in private$.noticeList) {
                    style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                    html <- paste0(html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong style='color: ", style$color, ";'>",
                        style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
                        "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
                        "</div>"
                    )
                }

                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },

            # Helper function for safe division
            .safeDivide = function(numerator, denominator, allowInfinite = FALSE) {
                if (is.na(denominator) || is.nan(denominator))
                    return(NA_real_)

                if (abs(denominator) < private$NUMERICAL_TOLERANCE) {
                    if (!allowInfinite || abs(numerator) < private$NUMERICAL_TOLERANCE)
                        return(NA_real_)

                    return(ifelse(numerator >= 0, Inf, -Inf))
                }

                numerator / denominator
            }





        )
    )
