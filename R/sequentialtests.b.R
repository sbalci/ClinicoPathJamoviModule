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
                if (is.na(prevalence) || prevalence < 0 || prevalence > 1) {
                    errors <- c(errors, "Prevalence must be between 0 and 1 (0% to 100%)")
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
                combined_ppv <- (prevalence * combined_sens) / (prevalence * combined_sens + (1 - prevalence) * (1 - combined_spec))
                combined_npv <- ((1 - prevalence) * combined_spec) / ((1 - prevalence) * combined_spec + prevalence * (1 - combined_sens))
                combined_plr <- combined_sens / (1 - combined_spec)
                combined_nlr <- (1 - combined_sens) / combined_spec

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

                # Calculate population flow numbers (assuming population of 1000)
                pop_size <- 1000
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
                        format(prevalence * 100, digits = 1),
                        "%, the combined testing strategy results in:</p>"
                    )
                    explanation <- paste0(explanation, "<ul>")
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined Sensitivity:</strong> ",
                        format(combined_sens * 100, digits = 1),
                        "% (ability to correctly identify those with disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined Specificity:</strong> ",
                        format(combined_spec * 100, digits = 1),
                        "% (ability to correctly identify those without disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined PPV:</strong> ",
                        format(combined_ppv * 100, digits = 1),
                        "% (probability that subjects with a positive test truly have the disease)</li>"
                    )
                    explanation <- paste0(
                        explanation,
                        "<li><strong>Combined NPV:</strong> ",
                        format(combined_npv * 100, digits = 1),
                        "% (probability that subjects with a negative test truly do not have the disease)</li>"
                    )
                    explanation <- paste0(explanation, "</ul>")

                    explanation <- paste0(explanation, "<p>In a population of 1,000 people:</p>")
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
                            "<li>Of the %.0f true positives, %.0f would be correctly identified (%.1f%%)</li>",
                            diseased,
                            final_tp,
                            final_tp / diseased * 100
                        )
                    )
                    explanation <- paste0(
                        explanation,
                        sprintf(
                            "<li>Of the %.0f true negatives, %.0f would be correctly identified (%.1f%%)</li>",
                            healthy,
                            final_tn,
                            final_tn / healthy * 100
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

                # Store data for Fagan nomogram
                # In the .run function, when setting up the nomogram data:
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
                        "Combined_NPV" = combined_npv
                    )

                    image <- self$results$plot_nomogram
                    image$setState(plotData)
                }


                },


            .plot_nomogram = function(image, ggtheme, ...) {
                # Check if required packages are available
                if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("gridExtra", quietly = TRUE)) {
                    # Create simple informative plot when packages unavailable
                    plotData <- image$state
                    
                    info_plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(
                            ggplot2::aes(x = 0.5, y = 0.7), 
                            label = "Sequential Testing Nomogram", 
                            size = 6, 
                            fontface = "bold"
                        ) +
                        ggplot2::geom_text(
                            ggplot2::aes(x = 0.5, y = 0.5), 
                            label = paste0(
                                "Strategy: ", plotData$Strategy, "\n",
                                "Combined Sensitivity: ", round(plotData$Combined_Sens * 100, 1), "%\n",
                                "Combined Specificity: ", round(plotData$Combined_Spec * 100, 1), "%\n",
                                "Combined PPV: ", round(plotData$Combined_PPV * 100, 1), "%"
                            ), 
                            size = 4
                        ) +
                        ggplot2::geom_text(
                            ggplot2::aes(x = 0.5, y = 0.2), 
                            label = "Full nomogram requires ggplot2 and gridExtra packages", 
                            size = 3, 
                            color = "gray50"
                        ) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void() +
                        ggplot2::theme(
                            panel.background = ggplot2::element_rect(fill = "white"),
                            plot.background = ggplot2::element_rect(fill = "white")
                        )
                    
                    print(info_plot)
                    return(TRUE)
                }
                
                plotData <- image$state

                # Extract values
                prevalence <- plotData$Prevalence
                test1_sens <- plotData$Test1_Sens
                test1_spec <- plotData$Test1_Spec
                test2_sens <- plotData$Test2_Sens
                test2_spec <- plotData$Test2_Spec
                strategy <- plotData$Strategy

                # Calculate LRs for both tests
                test1_plr <- test1_sens / (1 - test1_spec)
                test1_nlr <- (1 - test1_sens) / test1_spec
                test2_plr <- test2_sens / (1 - test2_spec)  
                test2_nlr <- (1 - test2_sens) / test2_spec

                # Create modern ggplot2-based nomogram visualization
                tryCatch({
                    # Create strategy flow diagram
                    flow_plot <- private$.createFlowDiagram(plotData, strategy)
                    
                    # Create summary statistics plot
                    stats_plot <- private$.createStatsPlot(plotData)
                    
                    # Create probability transformation plot
                    prob_plot <- private$.createProbabilityPlot(plotData, prevalence, test1_plr, test1_nlr, test2_plr, test2_nlr)
                    
                    # Create formula explanation plot
                    formula_plot <- private$.createFormulaPlot(plotData, strategy)
                    
                    # Combine all plots using gridExtra
                    combined_plot <- gridExtra::grid.arrange(
                        flow_plot, stats_plot,
                        prob_plot, formula_plot,
                        ncol = 2, nrow = 2,
                        top = paste("Sequential Testing Analysis:", plotData$Strategy)
                    )
                    
                    print(combined_plot)
                    return(TRUE)
                    
                }, error = function(e) {
                    # Fallback to simple text-based visualization
                    simple_plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(
                            ggplot2::aes(x = 0.5, y = 0.8), 
                            label = "Sequential Testing Results", 
                            size = 6, 
                            fontface = "bold"
                        ) +
                        ggplot2::geom_text(
                            ggplot2::aes(x = 0.5, y = 0.6), 
                            label = paste0(
                                "Strategy: ", plotData$Strategy, "\n",
                                "Test 1: ", plotData$Test1_Name, " (", round(plotData$Test1_Sens*100,1), "%, ", round(plotData$Test1_Spec*100,1), "%)\n",
                                "Test 2: ", plotData$Test2_Name, " (", round(plotData$Test2_Sens*100,1), "%, ", round(plotData$Test2_Spec*100,1), "%)\n\n",
                                "Combined Performance:\n",
                                "Sensitivity: ", round(plotData$Combined_Sens*100,1), "%\n",
                                "Specificity: ", round(plotData$Combined_Spec*100,1), "%\n",
                                "PPV: ", round(plotData$Combined_PPV*100,1), "%\n",
                                "NPV: ", round(plotData$Combined_NPV*100,1), "%"
                            ), 
                            size = 4,
                            hjust = 0.5
                        ) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    
                    print(simple_plot)
                    return(TRUE)
                })
            },

            # Helper functions for modular plot creation
            .createFlowDiagram = function(plotData, strategy) {
                # Create flow diagram based on strategy
                if (strategy == "serial_positive") {
                    flow_data <- data.frame(
                        x = c(1, 2, 3, 2.5, 2.5),
                        y = c(5, 5, 5, 4, 6), 
                        label = c("Start", plotData$Test1_Name, "Result", "Test2 if +", "Final")
                    )
                } else if (strategy == "serial_negative") {
                    flow_data <- data.frame(
                        x = c(1, 2, 3, 2.5, 2.5),
                        y = c(5, 5, 5, 4, 6),
                        label = c("Start", plotData$Test1_Name, "Result", "Test2 if -", "Final")
                    )
                } else {
                    flow_data <- data.frame(
                        x = c(1, 2, 2, 3),
                        y = c(5, 6, 4, 5),
                        label = c("Start", plotData$Test1_Name, plotData$Test2_Name, "Combined")
                    )
                }
                
                ggplot2::ggplot(flow_data, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(size = 8, color = "steelblue") +
                    ggplot2::geom_text(ggplot2::aes(label = label), size = 3, color = "white", fontface = "bold") +
                    ggplot2::labs(title = paste("Testing Flow:", strategy)) +
                    ggplot2::theme_void() +
                    ggplot2::xlim(0.5, 3.5) +
                    ggplot2::ylim(3, 7)
            },

            .createStatsPlot = function(plotData) {
                # Create summary statistics visualization
                stats_data <- data.frame(
                    Metric = c("Sensitivity", "Specificity", "PPV", "NPV"),
                    Value = c(plotData$Combined_Sens, plotData$Combined_Spec, 
                             plotData$Combined_PPV, plotData$Combined_NPV) * 100,
                    Test = "Combined"
                )
                
                ggplot2::ggplot(stats_data, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
                    ggplot2::geom_col(alpha = 0.7) +
                    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Value, 1), "%")), 
                                      vjust = -0.5, fontface = "bold") +
                    ggplot2::labs(title = "Combined Test Performance", 
                                 y = "Percentage (%)", x = "") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(legend.position = "none") +
                    ggplot2::ylim(0, 100)
            },

            .createProbabilityPlot = function(plotData, prevalence, test1_plr, test1_nlr, test2_plr, test2_nlr) {
                # Create probability transformation visualization
                prob_data <- data.frame(
                    Stage = c("Pre-test", "After Test 1+", "After Test 2+", "Pre-test", "After Test 1-", "After Test 2-"),
                    Probability = c(
                        prevalence * 100,
                        # Positive pathway
                        (prevalence * test1_plr / (1 - prevalence + prevalence * test1_plr)) * 100,
                        plotData$Combined_PPV * 100,
                        # Negative pathway  
                        prevalence * 100,
                        (prevalence * test1_nlr / (1 - prevalence + prevalence * test1_nlr)) * 100,
                        (1 - plotData$Combined_NPV) * 100
                    ),
                    Pathway = rep(c("Positive", "Negative"), each = 3),
                    Step = rep(1:3, 2)
                )
                
                ggplot2::ggplot(prob_data, ggplot2::aes(x = Step, y = Probability, color = Pathway)) +
                    ggplot2::geom_line(size = 2) +
                    ggplot2::geom_point(size = 4) +
                    ggplot2::labs(title = "Probability Transformation", 
                                 x = "Testing Step", y = "Disease Probability (%)") +
                    ggplot2::theme_minimal()
            },

            .createFormulaPlot = function(plotData, strategy) {
                # Create formula explanation
                if (strategy == "serial_positive") {
                    formula_text <- "Serial Positive:\nSens = Se1 × Se2\nSpec = Sp1 + (1-Sp1) × Sp2"
                } else if (strategy == "serial_negative") {
                    formula_text <- "Serial Negative:\nSens = Se1 + (1-Se1) × Se2\nSpec = Sp1 × Sp2"
                } else {
                    formula_text <- "Parallel Testing:\nSens = Se1 + Se2 - Se1×Se2\nSpec = Sp1 × Sp2"
                }
                
                ggplot2::ggplot() +
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = formula_text), 
                                      size = 5, fontface = "bold") +
                    ggplot2::labs(title = "Mathematical Formulas") +
                    ggplot2::theme_void() +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1)
            }





        )
    )
