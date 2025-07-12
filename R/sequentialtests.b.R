#' @title Sequential Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

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
                plotData <- image$state

                # Extract values
                prevalence <- plotData$Prevalence
                test1_sens <- plotData$Test1_Sens
                test1_spec <- plotData$Test1_Spec
                test2_sens <- plotData$Test2_Sens
                test2_spec <- plotData$Test2_Spec
                strategy <- plotData$Strategy

                # Calculate LRs for first test
                test1_plr <- test1_sens / (1 - test1_spec)
                test1_nlr <- (1 - test1_sens) / test1_spec

                # Calculate post-test probability after first test
                test1_pos_odds <- (prevalence / (1 - prevalence)) * test1_plr
                test1_neg_odds <- (prevalence / (1 - prevalence)) * test1_nlr

                test1_pos_post_prob <- test1_pos_odds / (1 + test1_pos_odds)
                test1_neg_post_prob <- test1_neg_odds / (1 + test1_neg_odds)

                # Calculate LRs for second test
                test2_plr <- test2_sens / (1 - test2_spec)
                test2_nlr <- (1 - test2_sens) / test2_spec

                # Set up graphics device
                grDevices::dev.new(width=12, height=8, noRStudioGD = TRUE)

                # Use layout to create a 2x2 grid with appropriate spacing
                layout_matrix <- matrix(c(1, 2, 3, 4), nrow=2, byrow=TRUE)
                layout(layout_matrix, widths=c(1, 1), heights=c(1, 1))

                # Create a custom Fagan nomogram function
                create_nomogram <- function(title, pretest_prob, plr, nlr, ylim_pre=c(0.001, 0.999),
                                            ylim_post=c(0.001, 0.999), highlight_pretest=TRUE, highlight_posttest=TRUE) {
                    # Convert probabilities to percentages for display
                    pretest_prob_pct <- pretest_prob * 100

                    # Calculate post-test probabilities
                    pos_post_odds <- (pretest_prob / (1 - pretest_prob)) * plr
                    neg_post_odds <- (pretest_prob / (1 - pretest_prob)) * nlr

                    pos_post_prob <- pos_post_odds / (1 + pos_post_odds)
                    neg_post_prob <- neg_post_odds / (1 + neg_post_odds)

                    pos_post_prob_pct <- pos_post_prob * 100
                    neg_post_prob_pct <- neg_post_prob * 100

                    # Create the nomogram plot
                    par(mar=c(4, 4, 4, 4))

                    # Set up the plot area
                    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100),
                         xlab="", ylab="", main=title, axes=FALSE)

                    # Draw the three vertical axes
                    axis(2, at=seq(1, 99, by=5), labels=seq(1, 99, by=5), las=1, pos=10)
                    axis(4, at=seq(1, 99, by=5), labels=seq(1, 99, by=5), las=1, pos=90)

                    # Custom labels for LR axis
                    lr_values <- c(1000, 500, 200, 100, 50, 20, 10, 5, 2, 1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001)
                    lr_positions <- rep(NA, length(lr_values))

                    for (i in 1:length(lr_values)) {
                        lr <- lr_values[i]
                        p <- 0.5  # Use a fixed pre-test probability for mapping
                        post_odds <- (p / (1 - p)) * lr
                        post_p <- post_odds / (1 + post_odds)

                        y_pos <- (post_p * 99) + 0.5  # Scale to plot coordinates
                        lr_positions[i] <- y_pos
                    }

                    # Add LR axis
                    axis(3, at=50, labels="Likelihood Ratio", pos=50, tick=FALSE)
                    text(50, lr_positions, labels=lr_values, cex=0.7)

                    # Add axis labels
                    mtext("Pre-test Probability (%)", side=2, line=2.5)
                    mtext("Post-test Probability (%)", side=4, line=2.5)

                    # Draw the nomogram lines
                    pretest_y <- (pretest_prob * 99) + 0.5
                    pos_post_y <- (pos_post_prob * 99) + 0.5
                    neg_post_y <- (neg_post_prob * 99) + 0.5

                    # Draw the positive LR line
                    lines(c(10, 50, 90), c(pretest_y, 50, pos_post_y), col="red", lwd=2)

                    # Draw the negative LR line
                    lines(c(10, 50, 90), c(pretest_y, 50, neg_post_y), col="blue", lwd=2)

                    # Highlight the pre-test probability
                    if (highlight_pretest) {
                        points(10, pretest_y, pch=19, col="black", cex=1.5)
                        text(5, pretest_y, paste0(round(pretest_prob_pct, 1), "%"), cex=0.8)
                    }

                    # Highlight the post-test probabilities
                    if (highlight_posttest) {
                        points(90, pos_post_y, pch=19, col="red", cex=1.5)
                        text(95, pos_post_y, paste0(round(pos_post_prob_pct, 1), "%"), cex=0.8)

                        points(90, neg_post_y, pch=19, col="blue", cex=1.5)
                        text(95, neg_post_y, paste0(round(neg_post_prob_pct, 1), "%"), cex=0.8)
                    }

                    # Return the post-test probabilities
                    return(list(positive=pos_post_prob, negative=neg_post_prob))
                }

                # Now create the sequence of nomograms based on strategy
                if (strategy == "serial_positive") {
                    # For serial testing of positives:
                    # 1. First test with prevalence as pre-test probability
                    test1_results <- create_nomogram(
                        paste0("Step 1: ", plotData$Test1_Name, " (Screening Test)"),
                        prevalence, test1_plr, test1_nlr
                    )

                    # 2. Second test with positive post-test probability from first test as pre-test
                    create_nomogram(
                        paste0("Step 2: ", plotData$Test2_Name, " (For Test 1 Positives Only)"),
                        test1_results$positive, test2_plr, test2_nlr
                    )

                    # 3. Overall result showing combined effect (empty plot for text)
                    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="",
                         main="Serial Testing Strategy (Testing Positives)")
                    text(0.5, 0.8, "Combined Effect:", pos=4)
                    text(0.5, 0.7, paste0("- Combined Sensitivity: ", round(plotData$Combined_Sens*100, 1), "%"), pos=4)
                    text(0.5, 0.6, paste0("- Combined Specificity: ", round(plotData$Combined_Spec*100, 1), "%"), pos=4)
                    text(0.5, 0.5, paste0("- Combined PPV: ", round(plotData$Combined_PPV*100, 1), "%"), pos=4)
                    text(0.5, 0.4, paste0("- Combined NPV: ", round(plotData$Combined_NPV*100, 1), "%"), pos=4)
                    text(0.5, 0.2, "A patient tests positive only if", pos=4)
                    text(0.5, 0.1, "positive on both tests", pos=4)

                    # 4. Sequential flow diagram
                    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), axes=FALSE, xlab="", ylab="",
                         main="Sequential Testing Flow")

                    # Draw the flow diagram
                    arrows(20, 80, 45, 80, lwd=2, length=0.1)
                    arrows(55, 80, 80, 80, lwd=2, length=0.1)
                    arrows(50, 70, 50, 50, lwd=2, length=0.1)

                    # Add labels for the flow
                    text(10, 80, "Start", cex=1.2)
                    text(50, 85, paste0(plotData$Test1_Name), cex=1.2)
                    text(90, 80, "Final Diagnosis", cex=1.2)

                    # Add branches
                    text(60, 70, "Test 1 (+)", cex=1)
                    text(60, 50, paste0(plotData$Test2_Name), cex=1.2)
                    text(40, 70, "Test 1 (-)", cex=1)
                    text(30, 60, "Negative", cex=1, col="blue")

                    # Add final outcomes
                    arrows(50, 40, 40, 30, lwd=2, length=0.1)
                    arrows(50, 40, 60, 30, lwd=2, length=0.1)
                    text(40, 25, "Test 2 (-)\nNegative", cex=1, col="blue")
                    text(60, 25, "Test 2 (+)\nPositive", cex=1, col="red")

                } else if (strategy == "serial_negative") {
                    # For serial testing of negatives:
                    # 1. First test with prevalence as pre-test probability
                    test1_results <- create_nomogram(
                        paste0("Step 1: ", plotData$Test1_Name, " (Initial Test)"),
                        prevalence, test1_plr, test1_nlr
                    )

                    # 2. Second test with negative post-test probability from first test as pre-test
                    create_nomogram(
                        paste0("Step 2: ", plotData$Test2_Name, " (For Test 1 Negatives Only)"),
                        test1_results$negative, test2_plr, test2_nlr
                    )

                    # 3. Overall result showing combined effect (empty plot for text)
                    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="",
                         main="Serial Testing Strategy (Testing Negatives)")
                    text(0.5, 0.8, "Combined Effect:", pos=4)
                    text(0.5, 0.7, paste0("- Combined Sensitivity: ", round(plotData$Combined_Sens*100, 1), "%"), pos=4)
                    text(0.5, 0.6, paste0("- Combined Specificity: ", round(plotData$Combined_Spec*100, 1), "%"), pos=4)
                    text(0.5, 0.5, paste0("- Combined PPV: ", round(plotData$Combined_PPV*100, 1), "%"), pos=4)
                    text(0.5, 0.4, paste0("- Combined NPV: ", round(plotData$Combined_NPV*100, 1), "%"), pos=4)
                    text(0.5, 0.2, "A patient tests positive if", pos=4)
                    text(0.5, 0.1, "positive on either test", pos=4)

                    # 4. Sequential flow diagram
                    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), axes=FALSE, xlab="", ylab="",
                         main="Sequential Testing Flow")

                    # Draw the flow diagram
                    arrows(20, 80, 45, 80, lwd=2, length=0.1)
                    arrows(55, 80, 80, 80, lwd=2, length=0.1)
                    arrows(50, 70, 50, 50, lwd=2, length=0.1)

                    # Add labels for the flow
                    text(10, 80, "Start", cex=1.2)
                    text(50, 85, paste0(plotData$Test1_Name), cex=1.2)
                    text(90, 80, "Final Diagnosis", cex=1.2)

                    # Add branches
                    text(60, 70, "Test 1 (+)", cex=1)
                    text(60, 60, "Positive", cex=1, col="red")
                    text(40, 70, "Test 1 (-)", cex=1)
                    text(50, 50, paste0(plotData$Test2_Name), cex=1.2)

                    # Add final outcomes
                    arrows(50, 40, 40, 30, lwd=2, length=0.1)
                    arrows(50, 40, 60, 30, lwd=2, length=0.1)
                    text(40, 25, "Test 2 (-)\nNegative", cex=1, col="blue")
                    text(60, 25, "Test 2 (+)\nPositive", cex=1, col="red")

                } else if (strategy == "parallel") {
                    # For parallel testing:
                    # 1. First test with prevalence as pre-test probability
                    test1_results <- create_nomogram(
                        paste0("Step 1: ", plotData$Test1_Name),
                        prevalence, test1_plr, test1_nlr
                    )

                    # 2. Second test also with prevalence as pre-test probability
                    test2_results <- create_nomogram(
                        paste0("Step 2: ", plotData$Test2_Name, " (Parallel Test)"),
                        prevalence, test2_plr, test2_nlr
                    )

                    # 3. Overall result showing combined effect (empty plot for text)
                    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="",
                         main="Parallel Testing Strategy")
                    text(0.5, 0.8, "Combined Effect:", pos=4)
                    text(0.5, 0.7, paste0("- Combined Sensitivity: ", round(plotData$Combined_Sens*100, 1), "%"), pos=4)
                    text(0.5, 0.6, paste0("- Combined Specificity: ", round(plotData$Combined_Spec*100, 1), "%"), pos=4)
                    text(0.5, 0.5, paste0("- Combined PPV: ", round(plotData$Combined_PPV*100, 1), "%"), pos=4)
                    text(0.5, 0.4, paste0("- Combined NPV: ", round(plotData$Combined_NPV*100, 1), "%"), pos=4)
                    text(0.5, 0.2, "A patient tests positive if", pos=4)
                    text(0.5, 0.1, "positive on either test", pos=4)

                    # 4. Parallel flow diagram
                    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), axes=FALSE, xlab="", ylab="",
                         main="Parallel Testing Flow")

                    # Draw the flow diagram
                    arrows(10, 80, 30, 80, lwd=2, length=0.1)

                    # Split to parallel tests
                    arrows(30, 80, 50, 90, lwd=2, length=0.1)
                    arrows(30, 80, 50, 70, lwd=2, length=0.1)

                    # From tests to interpretation
                    arrows(50, 90, 70, 80, lwd=2, length=0.1)
                    arrows(50, 70, 70, 80, lwd=2, length=0.1)

                    # To final result
                    arrows(70, 80, 90, 80, lwd=2, length=0.1)

                    # Add labels
                    text(10, 85, "Start", cex=1.2)
                    text(50, 95, paste0(plotData$Test1_Name), cex=1.2)
                    text(50, 65, paste0(plotData$Test2_Name), cex=1.2)
                    text(70, 85, "Interpretation\n(OR logic)", cex=1.2)
                    text(90, 85, "Result", cex=1.2)

                    # Add outcomes
                    text(90, 75, "Positive if either\ntest is positive", cex=1, col="red")
                    text(90, 65, "Negative only if\nboth tests negative", cex=1, col="blue")
                }

                # Capture the plot
                result <- grDevices::recordPlot()

                # Close the device
                grDevices::dev.off()

                # Return the plot
                return(result)
            }





        )
    )
