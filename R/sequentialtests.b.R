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
            POPULATION_SIZE = 1000,
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
            },

            .run = function() {
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
                
                # Input validation and error handling
                errors <- c()
                warnings <- c()
                
                # Validate probability ranges for all test parameters
                if (is.na(test1_sens) || test1_sens < 0 || test1_sens > 1) {
                    errors <- c(errors, "Test 1 sensitivity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(test1_spec) || test1_spec < 0 || test1_spec > 1) {
                    errors <- c(errors, "Test 1 specificity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(test2_sens) || test2_sens < 0 || test2_sens > 1) {
                    errors <- c(errors, "Test 2 sensitivity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(test2_spec) || test2_spec < 0 || test2_spec > 1) {
                    errors <- c(errors, "Test 2 specificity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(prevalence) || prevalence <= 0 || prevalence >= 1) {
                    errors <- c(errors, "Prevalence must be greater than 0 and less than 1 (exclusive)")
                }
                
                # Validate test names
                if (is.null(test1_name) || nchar(trimws(test1_name)) == 0) {
                    errors <- c(errors, "Test 1 name cannot be empty")
                }
                if (is.null(test2_name) || nchar(trimws(test2_name)) == 0) {
                    errors <- c(errors, "Test 2 name cannot be empty")
                }
                
                # Clinical plausibility warnings
                if (test1_sens < 0.50) {
                    warnings <- c(warnings, "Test 1 sensitivity <50% is unusual for most clinical tests")
                }
                if (test1_spec < 0.50) {
                    warnings <- c(warnings, "Test 1 specificity <50% is unusual for most clinical tests")
                }
                if (test2_sens < 0.50) {
                    warnings <- c(warnings, "Test 2 sensitivity <50% is unusual for most clinical tests")
                }
                if (test2_spec < 0.50) {
                    warnings <- c(warnings, "Test 2 specificity <50% is unusual for most clinical tests")
                }
                if (test1_sens > 0.99) {
                    warnings <- c(warnings, "Test 1 sensitivity >99% is rarely achieved in practice")
                }
                if (test1_spec > 0.99) {
                    warnings <- c(warnings, "Test 1 specificity >99% is rarely achieved in practice")
                }
                if (test2_sens > 0.99) {
                    warnings <- c(warnings, "Test 2 sensitivity >99% is rarely achieved in practice")
                }
                if (test2_spec > 0.99) {
                    warnings <- c(warnings, "Test 2 specificity >99% is rarely achieved in practice")
                }
                if (prevalence > 0.90) {
                    warnings <- c(warnings, "Prevalence >90% is unusual except for confirmatory testing scenarios")
                }
                if (prevalence < 0.001) {
                    warnings <- c(warnings, "Very low prevalence (<0.1%) may lead to extremely low PPV even with good tests")
                }
                
                # Strategy-specific warnings
                if (strategy == "serial_positive") {
                    if (test1_spec > test2_spec) {
                        warnings <- c(warnings, "For serial positive strategy, Test 2 should typically have higher specificity than Test 1")
                    }
                    if ((test1_sens + test1_spec) < 1.0) {
                        warnings <- c(warnings, "Test 1 performance (sens + spec < 100%) may not be suitable for screening")
                    }
                } else if (strategy == "serial_negative") {
                    if (test1_sens > test2_sens) {
                        warnings <- c(warnings, "For serial negative strategy, Test 2 should typically have higher sensitivity than Test 1")
                    }
                } else if (strategy == "parallel") {
                    if (abs(test1_sens - test2_sens) < 0.05 && abs(test1_spec - test2_spec) < 0.05) {
                        warnings <- c(warnings, "Parallel testing works best when tests are complementary (different strengths)")
                    }
                }
                
                # Test characteristic combinations that may be problematic
                if ((test1_sens + test1_spec) < 1.0 && (test2_sens + test2_spec) < 1.0) {
                    warnings <- c(warnings, "Both tests have poor performance (sens + spec < 100%) - consider test optimization")
                }
                
                # Show errors if any exist
                if (length(errors) > 0) {
                    errorHtml <- paste0(
                        '<div class="alert alert-danger">',
                        '<h4><span class="glyphicon glyphicon-exclamation-sign"></span> Input Errors:</h4>',
                        '<ul>',
                        paste0('<li>', errors, '</li>', collapse = ''),
                        '</ul>',
                        '<p><strong>Please correct these values to proceed with the analysis.</strong></p>',
                        '</div>'
                    )
                    self$results$explanation_text$setContent(errorHtml)
                    return()
                }
                
                # Show warnings if any exist (but continue with calculation)
                warningHtml <- ""
                if (length(warnings) > 0) {
                    warningHtml <- paste0(
                        '<div class="alert alert-warning">',
                        '<h4><span class="glyphicon glyphicon-warning-sign"></span> Clinical Notes:</h4>',
                        '<ul>',
                        paste0('<li>', warnings, '</li>', collapse = ''),
                        '</ul>',
                        '<p>Analysis will proceed, but please verify these parameters are appropriate for your clinical scenario.</p>',
                        '</div>'
                    )
                }

                # Calculate individual test metrics with error handling
                tryCatch({
                    # Calculate PPVs and NPVs with protection against edge cases
                    test1_ppv_denom <- (prevalence * test1_sens) + ((1 - prevalence) * (1 - test1_spec))
                    if (test1_ppv_denom == 0) {
                        test1_ppv <- NaN
                    } else {
                        test1_ppv <- (prevalence * test1_sens) / test1_ppv_denom
                    }
                    
                    test1_npv_denom <- ((1 - prevalence) * test1_spec) + (prevalence * (1 - test1_sens))
                    if (test1_npv_denom == 0) {
                        test1_npv <- NaN
                    } else {
                        test1_npv <- ((1 - prevalence) * test1_spec) / test1_npv_denom
                    }
                    
                    # Calculate likelihood ratios with division by zero protection
                    if (test1_spec == 1) {
                        test1_plr <- Inf
                    } else {
                        test1_plr <- test1_sens / (1 - test1_spec)
                    }
                    
                    if (test1_spec == 0) {
                        test1_nlr <- Inf
                    } else {
                        test1_nlr <- (1 - test1_sens) / test1_spec
                    }
                    
                    # Same calculations for test 2
                    test2_ppv_denom <- (prevalence * test2_sens) + ((1 - prevalence) * (1 - test2_spec))
                    if (test2_ppv_denom == 0) {
                        test2_ppv <- NaN
                    } else {
                        test2_ppv <- (prevalence * test2_sens) / test2_ppv_denom
                    }
                    
                    test2_npv_denom <- ((1 - prevalence) * test2_spec) + (prevalence * (1 - test2_sens))
                    if (test2_npv_denom == 0) {
                        test2_npv <- NaN
                    } else {
                        test2_npv <- ((1 - prevalence) * test2_spec) / test2_npv_denom
                    }
                    
                    if (test2_spec == 1) {
                        test2_plr <- Inf
                    } else {
                        test2_plr <- test2_sens / (1 - test2_spec)
                    }
                    
                    if (test2_spec == 0) {
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
                    # If any calculation fails, set to NA and show error
                    test1_ppv <<- NA
                    test1_npv <<- NA
                    test1_plr <<- NA
                    test1_nlr <<- NA
                    test2_ppv <<- NA
                    test2_npv <<- NA
                    test2_plr <<- NA
                    test2_nlr <<- NA
                    
                    errorMsg <- paste0(
                        '<div class="alert alert-danger">',
                        '<h4><span class="glyphicon glyphicon-exclamation-sign"></span> Calculation Error:</h4>',
                        '<p>Unable to calculate test metrics with the provided values. Error: ', e$message, '</p>',
                        '<p>Please check your input parameters and try again.</p>',
                        '</div>'
                    )
                    self$results$explanation_text$setContent(errorMsg)
                    return()
                })

                # Calculate combined metrics based on strategy
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
                    warnings <- c(warnings, "Combined PPV could not be calculated because the denominator evaluated to zero. Check for extreme sensitivity/specificity combinations.")
                }

                combined_npv_num <- (1 - prevalence) * combined_spec
                combined_npv_denom <- combined_npv_num + prevalence * (1 - combined_sens)
                combined_npv <- private$.safeDivide(combined_npv_num, combined_npv_denom)

                if (is.na(combined_npv)) {
                    warnings <- c(warnings, "Combined NPV could not be calculated because the denominator evaluated to zero. Check for extreme sensitivity/specificity combinations.")
                }

                combined_plr <- private$.safeDivide(combined_sens, 1 - combined_spec, allowInfinite = TRUE)
                if (is.na(combined_plr)) {
                    warnings <- c(warnings, "Combined positive likelihood ratio undefined because both numerator and denominator approach zero.")
                } else if (is.infinite(combined_plr)) {
                    warnings <- c(warnings, "Combined positive likelihood ratio is infinite because combined specificity is effectively 100%.")
                }

                combined_nlr <- private$.safeDivide(1 - combined_sens, combined_spec, allowInfinite = TRUE)
                if (is.na(combined_nlr)) {
                    warnings <- c(warnings, "Combined negative likelihood ratio undefined because both numerator and denominator approach zero.")
                } else if (is.infinite(combined_nlr)) {
                    warnings <- c(warnings, "Combined negative likelihood ratio is infinite because combined specificity is effectively zero.")
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
                        combined_npv = combined_npv
                    )
                )

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

                    # Add warnings to explanation if they exist
                    final_explanation <- explanation
                    if (length(warnings) > 0) {
                        final_explanation <- paste0(warningHtml, explanation)
                    }
                    
                    self$results$explanation_text$setContent(final_explanation)
                }

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
                }


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
                        ggplot2::annotate("text", x = 1, y = 5, label = "All Patients\n(n=1000)",
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
                        ggplot2::annotate("text", x = 1, y = 5, label = "All Patients\n(n=1000)",
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
                        ggplot2::annotate("text", x = 1, y = 5, label = "All Patients\n(n=1000)",
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
