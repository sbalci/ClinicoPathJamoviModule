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

                # Calculate individual test metrics
                # PPVs and NPVs depend on prevalence
                test1_ppv <- (prevalence * test1_sens) / (prevalence * test1_sens + (1 - prevalence) * (1 - test1_spec))
                test1_npv <- ((1 - prevalence) * test1_spec) / ((1 - prevalence) * test1_spec + prevalence * (1 - test1_sens))
                test1_plr <- test1_sens / (1 - test1_spec)
                test1_nlr <- (1 - test1_sens) / test1_spec

                test2_ppv <- (prevalence * test2_sens) / (prevalence * test2_sens + (1 - prevalence) * (1 - test2_spec))
                test2_npv <- ((1 - prevalence) * test2_spec) / ((1 - prevalence) * test2_spec + prevalence * (1 - test2_sens))
                test2_plr <- test2_sens / (1 - test2_spec)
                test2_nlr <- (1 - test2_sens) / test2_spec

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

                    self$results$explanation_text$setContent(explanation)
                }

                # Generate formulas HTML if requested
                if (self$options$show_formulas) {
                    formulas <- ""

                    formulas <- paste0(formulas, "<h3>Formulas for Sequential Testing</h3>")

                    if (strategy == "serial_positive") {
                        formulas <- paste0(formulas,
                                           "<h4>Serial Testing (Testing Positives)</h4>")
                        formulas <- paste0(
                            formulas,
                            "<p>In this strategy, only those who test positive on the first test receive the second test, and a subject is considered positive only if positive on both tests.</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Sensitivity</strong> = Sensitivity of Test 1 × Sensitivity of Test 2</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_sens, digits = 3),
                            " × ",
                            format(test2_sens, digits = 3),
                            " = ",
                            format(combined_sens, digits = 3),
                            "</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Specificity</strong> = Specificity of Test 1 + (1 - Specificity of Test 1) × Specificity of Test 2</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_spec, digits = 3),
                            " + (1 - ",
                            format(test1_spec, digits = 3),
                            ") × ",
                            format(test2_spec, digits = 3),
                            " = ",
                            format(combined_spec, digits = 3),
                            "</p>"
                        )

                    } else if (strategy == "serial_negative") {
                        formulas <- paste0(formulas,
                                           "<h4>Serial Testing (Testing Negatives)</h4>")
                        formulas <- paste0(
                            formulas,
                            "<p>In this strategy, only those who test negative on the first test receive the second test, and a subject is considered positive if they test positive on either test.</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Sensitivity</strong> = Sensitivity of Test 1 + (1 - Sensitivity of Test 1) × Sensitivity of Test 2</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_sens, digits = 3),
                            " + (1 - ",
                            format(test1_sens, digits = 3),
                            ") × ",
                            format(test2_sens, digits = 3),
                            " = ",
                            format(combined_sens, digits = 3),
                            "</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Specificity</strong> = Specificity of Test 1 × Specificity of Test 2</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_spec, digits = 3),
                            " × ",
                            format(test2_spec, digits = 3),
                            " = ",
                            format(combined_spec, digits = 3),
                            "</p>"
                        )

                    } else if (strategy == "parallel") {
                        formulas <- paste0(formulas, "<h4>Parallel Testing</h4>")
                        formulas <- paste0(
                            formulas,
                            "<p>In this strategy, all subjects receive both tests, and a subject is considered positive if they test positive on either test.</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Sensitivity</strong> = Sensitivity of Test 1 + Sensitivity of Test 2 - (Sensitivity of Test 1 × Sensitivity of Test 2)</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_sens, digits = 3),
                            " + ",
                            format(test2_sens, digits = 3),
                            " - (",
                            format(test1_sens, digits = 3),
                            " × ",
                            format(test2_sens, digits = 3),
                            ") = ",
                            format(combined_sens, digits = 3),
                            "</p>"
                        )

                        formulas <- paste0(
                            formulas,
                            "<p><strong>Combined Specificity</strong> = Specificity of Test 1 × Specificity of Test 2</p>"
                        )
                        formulas <- paste0(
                            formulas,
                            "<p>= ",
                            format(test1_spec, digits = 3),
                            " × ",
                            format(test2_spec, digits = 3),
                            " = ",
                            format(combined_spec, digits = 3),
                            "</p>"
                        )
                    }

                    # Add general formulas for PPV and NPV
                    formulas <- paste0(formulas, "<h4>Predictive Values</h4>")

                    formulas <- paste0(
                        formulas,
                        "<p><strong>Positive Predictive Value (PPV)</strong> = (Prevalence × Sensitivity) / (Prevalence × Sensitivity + (1 - Prevalence) × (1 - Specificity))</p>"
                    )
                    formulas <- paste0(
                        formulas,
                        "<p>= (",
                        format(prevalence, digits = 3),
                        " × ",
                        format(combined_sens, digits = 3),
                        ") / (",
                        format(prevalence, digits = 3),
                        " × ",
                        format(combined_sens, digits = 3),
                        " + (1 - ",
                        format(prevalence, digits = 3),
                        ") × (1 - ",
                        format(combined_spec, digits = 3),
                        "))</p>"
                    )
                    formulas <- paste0(formulas,
                                       "<p>= ",
                                       format(combined_ppv, digits = 3),
                                       "</p>")

                    formulas <- paste0(
                        formulas,
                        "<p><strong>Negative Predictive Value (NPV)</strong> = ((1 - Prevalence) × Specificity) / ((1 - Prevalence) × Specificity + Prevalence × (1 - Sensitivity))</p>"
                    )
                    formulas <- paste0(
                        formulas,
                        "<p>= ((1 - ",
                        format(prevalence, digits = 3),
                        ") × ",
                        format(combined_spec, digits = 3),
                        ") / ((1 - ",
                        format(prevalence, digits = 3),
                        ") × ",
                        format(combined_spec, digits = 3),
                        " + ",
                        format(prevalence, digits = 3),
                        " × (1 - ",
                        format(combined_sens, digits = 3),
                        "))</p>"
                    )
                    formulas <- paste0(formulas,
                                       "<p>= ",
                                       format(combined_npv, digits = 3),
                                       "</p>")

                    self$results$formulas_text$setContent(formulas)
                }

                # Store data for Fagan nomogram
                if (self$options$show_nomogram) {
                    plotData <- list(
                        "Prevalence" = prevalence,
                        "Sens" = combined_sens,
                        "Spec" = combined_spec,
                        "Plr" = combined_plr,
                        "Nlr" = combined_nlr
                    )

                    image <- self$results$plot_nomogram
                    image$setState(plotData)
                }
            },

            .plot_nomogram = function(image, ggtheme, ...) {
                plotData <- image$state

                plot <- nomogrammer(
                    Prevalence = plotData$Prevalence,
                    Sens = plotData$Sens,
                    Spec = plotData$Spec,
                    Plr = plotData$Plr,
                    Nlr = plotData$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                print(plot)
                TRUE
            }
        )
    )
