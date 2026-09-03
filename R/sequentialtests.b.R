#' @title Sequential Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_text geom_line geom_point labs theme_void theme element_blank scale_x_continuous scale_y_continuous annotate
#' @return An \code{R6} class generator object for the \code{sequentialtestsClass} backend; used internally by the jamovi analysis wrapper and not called directly.

sequentialtestsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "sequentialtestsClass",
        inherit = sequentialtestsBase,
        private = list(
            NUMERICAL_TOLERANCE = 1e-10,

            .init = function() {
                # Add rows to tables during initialization
                individualTable <- self$results$individual_tests_table
                individualTable$addRow(rowKey = "test1",
                                       values = list(test_name = self$options$test1_name))
                individualTable$addRow(rowKey = "test2",
                                       values = list(test_name = self$options$test2_name))
                individualTable$addRow(rowKey = "combined",
                                       values = list(test_name = .("Combined Strategy")))

                flowTable <- self$results$population_flow_table
                flowTable$addRow(rowKey = "initial",
                                 values = list(stage = .("Initial Population")))
                flowTable$addRow(rowKey = "after_test1",
                                 values = list(stage = .("After First Test")))
                flowTable$addRow(rowKey = "after_test2",
                                 values = list(stage = .("After Second Test")))

                # The cost table always has exactly these three rows. Creating them here
                # (rather than adding them each run) keeps the structure visible before the
                # analysis runs and removes the need to clear stale rows in .run(). The item
                # labels are placeholders: .run() rewrites them because a teaching example can
                # override the test names.
                costTable <- self$results$cost_analysis_table
                costTable$addRow(rowKey = "test1",
                                 values = list(item = private$.formatTranslated(
                                     .("Test 1: {name}"), list(name = self$options$test1_name))))
                costTable$addRow(rowKey = "test2",
                                 values = list(item = private$.formatTranslated(
                                     .("Test 2: {name}"), list(name = self$options$test2_name))))
                costTable$addRow(rowKey = "total",
                                 values = list(item = .("Total Protocol Cost")))
            },

            .run = function() {

                private$.noticeList <- list()

                # Ready to run?
                if (is.null(self$options$test1_sens) || is.null(self$options$test1_spec) ||
                    is.null(self$options$test2_sens) || is.null(self$options$test2_spec)) {
                    private$.renderNotices()
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

                # Apply the teaching example. In the jamovi GUI the example is applied by
                # jamovi/js/sequentialtests.events.js, which writes the values into the controls;
                # nothing runs that JavaScript when the analysis is called from R, so without
                # this the preset was silently ignored and the defaults were analysed instead.
                # Values must stay in step with SEQUENTIAL_PRESET_CONFIGS in that file --
                # test-sequentialtests-release-review.R compares the two tables.
                preset <- self$options$preset
                if (!is.null(preset) && preset != "custom") {
                    preset_values <- private$.getPresetValues(preset)
                    if (is.null(preset_values)) {
                        private$.addNotice(
                            'WARNING',
                            .('Unknown Teaching Example'),
                            private$.formatTranslated(
                                .('Teaching example "{preset}" is not recognised; the values shown in the panel were used instead.'),
                                list(preset = preset)
                            )
                        )
                    } else {
                        test1_name <- preset_values$test1_name
                        test1_sens <- preset_values$test1_sens
                        test1_spec <- preset_values$test1_spec
                        test2_name <- preset_values$test2_name
                        test2_sens <- preset_values$test2_sens
                        test2_spec <- preset_values$test2_spec
                        prevalence <- preset_values$prevalence
                        strategy   <- preset_values$strategy

                        private$.addNotice(
                            'STRONG_WARNING',
                            .('Teaching Example Values Are Not Clinical Guidance'),
                            private$.formatTranslated(
                                .('The "{preset}" teaching example supplies round, illustrative numbers chosen to demonstrate how sequential testing behaves. They are NOT validated clinical parameters, they are not taken from any specific published study, and the prevalence is not your population\'s. This example is not clinical guidance and must not be used to design a real testing protocol or advise a patient. Replace every value with sensitivity, specificity and prevalence estimates from your own setting or from a source you have checked.'),
                                list(preset = preset)
                            )
                        )
                    }
                }

                format_percent <- function(value) {
                    if (is.na(value))
                        return(.("not defined"))
                    if (is.infinite(value))
                        return(.("infinite"))
                    sprintf("%.1f%%", value * 100)
                }

                # Input validation. Track whether any ERROR-level problem was found so we
                # can early-return after collecting all of them.
                had_error <- FALSE

                # Validate probability ranges for all test parameters
                if (is.na(test1_sens) || test1_sens < 0 || test1_sens > 1) {
                    private$.addNotice('ERROR', .('Test 1 Sensitivity Invalid'), .('Test 1 sensitivity must be between 0 and 1 (0% to 100%). Current value is invalid.'))
                    had_error <- TRUE
                }
                if (is.na(test1_spec) || test1_spec < 0 || test1_spec > 1) {
                    private$.addNotice('ERROR', .('Test 1 Specificity Invalid'), .('Test 1 specificity must be between 0 and 1 (0% to 100%). Current value is invalid.'))
                    had_error <- TRUE
                }
                if (is.na(test2_sens) || test2_sens < 0 || test2_sens > 1) {
                    private$.addNotice('ERROR', .('Test 2 Sensitivity Invalid'), .('Test 2 sensitivity must be between 0 and 1 (0% to 100%). Current value is invalid.'))
                    had_error <- TRUE
                }
                if (is.na(test2_spec) || test2_spec < 0 || test2_spec > 1) {
                    private$.addNotice('ERROR', .('Test 2 Specificity Invalid'), .('Test 2 specificity must be between 0 and 1 (0% to 100%). Current value is invalid.'))
                    had_error <- TRUE
                }
                if (is.na(prevalence) || prevalence <= 0 || prevalence >= 1) {
                    private$.addNotice('ERROR', .('Prevalence Invalid'), .('Prevalence must be greater than 0 and less than 1 (exclusive). Current value is invalid.'))
                    had_error <- TRUE
                }

                # Validate test names
                if (is.null(test1_name) || nchar(trimws(test1_name)) == 0) {
                    private$.addNotice('ERROR', .('Test 1 Name Empty'), .('Test 1 name cannot be empty. Please provide a descriptive name.'))
                    had_error <- TRUE
                }
                if (is.null(test2_name) || nchar(trimws(test2_name)) == 0) {
                    private$.addNotice('ERROR', .('Test 2 Name Empty'), .('Test 2 name cannot be empty. Please provide a descriptive name.'))
                    had_error <- TRUE
                }

                # Stop early if any ERROR notices were posted
                if (had_error) {
                    return()
                }

                # Clinical plausibility warnings (STRONG_WARNING for serious concerns)
                if (test1_sens < 0.50 || test1_spec < 0.50) {
                    private$.addNotice('STRONG_WARNING', .('Test 1 Low Performance'), sprintf(.('Test 1 performance is unusually low (Sensitivity=%.1f%%, Specificity=%.1f%%). Verify whether these assumptions are suitable for the intended illustrative comparison.'), test1_sens*100, test1_spec*100))
                }
                if (test2_sens < 0.50 || test2_spec < 0.50) {
                    private$.addNotice('STRONG_WARNING', .('Test 2 Low Performance'), sprintf(.('Test 2 performance is unusually low (Sensitivity=%.1f%%, Specificity=%.1f%%). Verify whether these assumptions are suitable for the intended illustrative comparison.'), test2_sens*100, test2_spec*100))
                }

                if (prevalence >= 0.90) {
                    private$.addNotice('STRONG_WARNING', .('Prevalence Very High'), sprintf(.('Prevalence is very high (%.1f%%). Verify that this assumption matches the intended population; predictive values are prevalence-dependent.'), prevalence*100))
                }
                if (prevalence <= 0.01) {
                    private$.addNotice('STRONG_WARNING', .('Prevalence Very Low'), sprintf(.('Prevalence is very low (%.3f%%). Even with excellent tests, PPV may be extremely low. Verify the pre-test probability for the intended population.'), prevalence*100))
                }

                # Detect potential test correlation. fixed = TRUE compares the names literally
                # (fuzzy string similarity), so a test name containing regex metacharacters
                # (e.g. "CA-125 [serum]") can't throw a malformed-pattern error that aborts .run().
                test_similarity <- agrepl(test1_name, test2_name, max.distance = 0.3, fixed = TRUE)
                if (test_similarity && test1_name != "Screening Test" && test2_name != "Confirmatory Test") {
                    private$.addNotice('STRONG_WARNING', .('Test Correlation Risk'), sprintf(.('Test names are similar ("%s" vs "%s"). If tests measure similar biomarkers or use similar technology, they may be conditionally dependent. This can bias combined sensitivity and specificity in opposite directions.'), test1_name, test2_name))
                }

                # Minor plausibility warnings
                if (test1_sens >= 0.99) {
                    private$.addNotice('WARNING', .('Test 1 Sensitivity High'), sprintf(.('Test 1 sensitivity is at least 99%% (%.2f%%). Verify that this estimate applies to the intended population and setting.'), test1_sens*100))
                }
                if (test1_spec >= 0.99) {
                    private$.addNotice('WARNING', .('Test 1 Specificity High'), sprintf(.('Test 1 specificity is at least 99%% (%.2f%%). Verify that this estimate applies to the intended population and setting.'), test1_spec*100))
                }
                if (test2_sens >= 0.99) {
                    private$.addNotice('WARNING', .('Test 2 Sensitivity High'), sprintf(.('Test 2 sensitivity is at least 99%% (%.2f%%). Verify that this estimate applies to the intended population and setting.'), test2_sens*100))
                }
                if (test2_spec >= 0.99) {
                    private$.addNotice('WARNING', .('Test 2 Specificity High'), sprintf(.('Test 2 specificity is at least 99%% (%.2f%%). Verify that this estimate applies to the intended population and setting.'), test2_spec*100))
                }

                # Strategy-specific warnings
                if (strategy == "serial_positive") {
                    if (test1_spec > test2_spec) {
                        private$.addNotice('WARNING', .('Serial Positive Assumption Check'), sprintf(.('For this serial-positive example, Test 2 specificity (%.1f%%) is below Test 1 specificity (%.1f%%). This does not invalidate the calculation, but it may not improve specificity as much as expected.'), test2_spec*100, test1_spec*100))
                    }
                } else if (strategy == "serial_negative") {
                    if (test1_sens > test2_sens) {
                        private$.addNotice('WARNING', .('Serial Negative Assumption Check'), sprintf(.('For this serial-negative example, Test 2 sensitivity (%.1f%%) is below Test 1 sensitivity (%.1f%%). This does not invalidate the calculation, but it may not improve sensitivity as much as expected.'), test2_sens*100, test1_sens*100))
                    }
                } else if (strategy == "parallel") {
                    if (abs(test1_sens - test2_sens) < 0.05 && abs(test1_spec - test2_spec) < 0.05) {
                        private$.addNotice('WARNING', .('Parallel Test Assumption Check'), .('The tests have similar marginal sensitivity and specificity. Similar marginal performance does not establish redundancy or independence; assess conditional dependence using external evidence.'))
                    }
                }

                # Calculate individual test metrics with error handling
                calc_failed <- FALSE
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
                    # If any calculation fails, show error notice and flag the failure.
                    # A bare return() here would only exit this handler closure, not .run();
                    # setting a flag lets us early-return from .run() below so downstream
                    # code never runs with unassigned metrics.
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Calculation Error"),
                        content = sprintf(.('Calculation error with provided values: %s. Please verify all parameters and try again.'), e$message)
                    )
                    calc_failed <<- TRUE
                })

                if (calc_failed) {
                    return()
                }

                # Calculate combined metrics based on strategy
                if (strategy == "serial_positive") {
                    # Serial testing of positives (confirmation strategy)
                    combined_sens <- test1_sens * test2_sens
                    combined_spec <- test1_spec + (1 - test1_spec) * test2_spec
                    strategy_name <- .("Serial Testing (Test positives)")

                } else if (strategy == "serial_negative") {
                    # Serial testing of negatives (exclusion strategy)
                    combined_sens <- test1_sens + (1 - test1_sens) * test2_sens
                    combined_spec <- test1_spec * test2_spec
                    strategy_name <- .("Serial Testing (Test negatives)")

                } else if (strategy == "parallel") {
                    # Parallel testing (both tests for everyone)
                    # Positive if either test is positive
                    combined_sens <- test1_sens + test2_sens - (test1_sens * test2_sens)
                    combined_spec <- test1_spec * test2_spec
                    strategy_name <- .("Parallel Testing (Test all)")
                }

                # Calculate PPV and NPV for combined strategy
                combined_ppv_num <- prevalence * combined_sens
                combined_ppv_denom <- combined_ppv_num + (1 - prevalence) * (1 - combined_spec)
                combined_ppv <- private$.safeDivide(combined_ppv_num, combined_ppv_denom)

                if (is.na(combined_ppv)) {
                    private$.addNotice('STRONG_WARNING', .('Combined PPV Undefined'), .('Combined PPV cannot be calculated (denominator is zero). Check for extreme sensitivity/specificity combinations.'))
                }

                combined_npv_num <- (1 - prevalence) * combined_spec
                combined_npv_denom <- combined_npv_num + prevalence * (1 - combined_sens)
                combined_npv <- private$.safeDivide(combined_npv_num, combined_npv_denom)

                if (is.na(combined_npv)) {
                    private$.addNotice('STRONG_WARNING', .('Combined NPV Undefined'), .('Combined NPV cannot be calculated (denominator is zero). Check for extreme sensitivity/specificity combinations.'))
                }

                combined_plr <- private$.safeDivide(combined_sens, 1 - combined_spec, allowInfinite = TRUE)
                if (is.na(combined_plr)) {
                    private$.addNotice('WARNING', .('Positive Likelihood Ratio Undefined'), .('Combined positive likelihood ratio is undefined (both numerator and denominator approach zero).'))
                } else if (is.infinite(combined_plr)) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Infinite Positive Likelihood Ratio"),
                        content = .('Combined positive likelihood ratio is infinite because combined specificity is effectively 100% under the entered assumptions. This does not establish perfect performance in another population.')
                    )
                }

                combined_nlr <- private$.safeDivide(1 - combined_sens, combined_spec, allowInfinite = TRUE)
                if (is.na(combined_nlr)) {
                    private$.addNotice('WARNING', .('Negative Likelihood Ratio Undefined'), .('Combined negative likelihood ratio is undefined (both numerator and denominator approach zero).'))
                } else if (is.infinite(combined_nlr)) {
                    private$.addNotice('WARNING', .('Negative Likelihood Ratio Infinite'), .('Combined negative likelihood ratio is infinite because combined specificity is effectively zero under the entered assumptions.'))
                }

                # Calculate Number Needed to Screen
                # NNT = 1 / (prevalence x combined_sens)
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

                # Under positive conditional dependence the two combined figures move in
                # OPPOSITE directions, so "too optimistic" is wrong for one of them in every
                # run. serial_positive multiplies sensitivities (understates sens) and multiplies
                # false-positive rates (overstates spec); serial_negative and parallel do the
                # reverse. Name the direction that actually applies to this strategy.
                dependence_caveat <- if (strategy == "serial_positive")
                    .("the combined specificity above is too high and the combined sensitivity too low")
                else
                    .("the combined sensitivity above is too high and the combined specificity too low")

                # Generate plain-language summary
                if (self$options$show_explanation) {
                    strategy_desc <- if (strategy == "serial_positive") {
                        .("serial positive (confirmation) strategy")
                    } else if (strategy == "serial_negative") {
                        .("serial negative (exclusion) strategy")
                    } else {
                        .("parallel testing strategy")
                    }

                    clinical_meaning <- if (strategy == "serial_positive") {
                        .("This strategy raises combined specificity above that of either test alone and lowers combined sensitivity: fewer false positives, more missed cases.")
                    } else if (strategy == "serial_negative") {
                        .("This strategy raises combined sensitivity above that of either test alone and lowers combined specificity: fewer missed cases, more false positives.")
                    } else {
                        .("Parallel testing raises combined sensitivity above that of either test alone and lowers combined specificity: fewer missed cases, more false positives.")
                    }

                    nnt_text <- if (!is.na(nnt)) {
                        sprintf(.(" You would need to screen approximately %d people to identify one true positive case."), nnt)
                    } else {
                        ""
                    }

                    summary <- sprintf(
                        .("<div style='background-color: rgba(33, 149, 188, 0.1);padding:15px;border-left:4px solid #0077be;font-size:1.05em;line-height:1.6; color: inherit;'><strong>Illustrative Summary:</strong> Using a %s with %s followed by %s, the combined test achieves %.1f%% sensitivity (detects %.0f of every 100 diseased individuals) and %.1f%% specificity (correctly rules out %.0f of every 100 healthy individuals). At your specified disease prevalence of %.1f%%, a positive result indicates a %s chance the person truly has the disease (PPV), while a negative result indicates a %s chance the person is truly disease-free (NPV).%s %s <em>These combined figures assume the two tests are conditionally independent given disease status, and they treat the sensitivity, specificity and prevalence you entered as exact, so they carry no confidence interval. If the two tests measure related biology, %s.</em></div>"),
                        strategy_desc,
                        private$.safeHtmlOutput(test1_name),
                        private$.safeHtmlOutput(test2_name),
                        combined_sens*100, combined_sens*100,
                        combined_spec*100, combined_spec*100,
                        prevalence*100,
                        format_percent(combined_ppv),
                        format_percent(combined_npv),
                        nnt_text,
                        clinical_meaning,
                        dependence_caveat
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
                        test_name = .("Combined Strategy"),
                        sensitivity = combined_sens,
                        specificity = combined_spec,
                        ppv = combined_ppv,
                        npv = combined_npv,
                        plr = combined_plr,
                        nlr = combined_nlr
                    )
                )

                # Calculate population flow numbers using the user-specified population size
                pop_size <- self$options$population_size
                diseased <- pop_size * prevalence
                healthy <- pop_size - diseased

                # Initial population
                flowTable <- self$results$population_flow_table
                flowTable$setRow(
                    rowKey = "initial",
                    values = list(
                        stage = .("Initial Population"),
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
                        stage = .("After First Test"),
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
                        stage = .("After Combined Strategy"),
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
                    costTable$setTitle(private$.formatTranslated(
                        .('Cost Analysis (Per {n} People)'),
                        list(n = base::format(pop_size, big.mark = ','))
                    ))

                    # Rows are created in .init(); only the computed cells are set here.
                    costTable$setRow(rowKey = "test1", values = list(
                        item = private$.formatTranslated(.("Test 1: {name}"), list(name = test1_name)),
                        unit_cost = test1_cost,
                        number_tests = n_test1,
                        total_cost = total_cost1
                    ))
                    
                    costTable$setRow(rowKey = "test2", values = list(
                        item = private$.formatTranslated(.("Test 2: {name}"), list(name = test2_name)),
                        unit_cost = test2_cost,
                        number_tests = n_test2,
                        total_cost = total_cost2
                    ))
                    
                    costTable$setRow(rowKey = "total", values = list(
                        item = .("Total Protocol Cost"),
                        unit_cost = NA,
                        number_tests = n_test1 + n_test2,
                        total_cost = total_combined
                    ))
                    costTable$setNote(
                        key = "expected_counts",
                        note = .("Numbers of tests and total costs are expected values derived from the entered probabilities. Fractional expected counts are retained so the displayed count and cost agree.")
                    )
                }

                # Update summary table
                summaryTable <- self$results$summary_table
                
                # The independence assumption applies to ALL THREE strategies, not just parallel:
                # every combined figure multiplies the two tests' conditional probabilities. This
                # note used to be set only for `parallel`, and summary_table has no clearWith, so
                # switching to a serial strategy left the parallel wording sitting under a row
                # labelled "Serial Testing". Setting it every run keeps the note and the row in
                # step whichever strategy is chosen.
                # Every input is a point estimate typed in by the user, so nothing here has a
                # confidence interval. That belongs beside the numbers, not only in the guides.
                summaryTable$setNote(
                    key = "fixed_inputs",
                    note = .("Sensitivity, specificity and prevalence are treated as exact. These combined figures therefore carry <i>no</i> confidence interval and do not reflect sampling uncertainty in the values entered \u{2014} published test performance and local prevalence both vary.")
                )

                summaryTable$setNote(
                    key = "independence_warning",
                    note = private$.formatTranslated(
                        .("Combined figures assume the two tests are <i>conditionally independent</i> \u{2014} that, among people with the same disease status, one test's result says nothing about the other's. With positive conditional dependence, {direction}."),
                        list(direction = dependence_caveat)
                    )
                )

                # Serial-negative and parallel testing apply the same rule (positive if either test
                # is positive), so they are algebraically identical in accuracy: sens1 + (1 - sens1)
                # * sens2 is the same number as sens1 + sens2 - sens1 * sens2. They differ only in
                # how many second tests get performed. Without saying so, a user comparing the two
                # sees byte-identical rows and reasonably suspects a bug.
                if (strategy %in% c("serial_negative", "parallel")) {
                    other <- if (strategy == "parallel")
                        .("serial testing of negatives")
                    else
                        .("parallel testing")
                    summaryTable$setNote(
                        key = "equivalence_note",
                        note = private$.formatTranslated(
                            .("This strategy gives exactly the same sensitivity, specificity, PPV and NPV as {other}: both call a subject positive if <i>either</i> test is positive. They differ only in how many second tests are performed, which the cost analysis shows."),
                            list(other = other)
                        )
                    )
                } else {
                    # Not applicable to serial-positive; clear it so it cannot persist from an
                    # earlier run with a different strategy.
                    summaryTable$setNote(key = "equivalence_note", note = NULL)
                }
                
                # Generate explanation HTML
                if (self$options$show_explanation) {
                    strategy_explanation <- if (strategy == "serial_positive") {
                        private$.formatTranslated(
                            .("<p>You selected a <strong>serial-positive teaching strategy</strong>. Everyone first receives {test1} (sensitivity {sens1}%, specificity {spec1}%). Only first-test positives receive {test2} (sensitivity {sens2}%, specificity {spec2}%), and a final result is positive only when both tests are positive.</p><p>The sensitivities and false-positive rates therefore multiply. Combined specificity increases while combined sensitivity decreases. Two positive results are not automatically a confirmed diagnosis; PPV still depends on prevalence, and false negatives are shown in the population-flow table.</p>"),
                            list(
                                test1 = private$.safeHtmlOutput(test1_name),
                                sens1 = sprintf("%.1f", test1_sens * 100),
                                spec1 = sprintf("%.1f", test1_spec * 100),
                                test2 = private$.safeHtmlOutput(test2_name),
                                sens2 = sprintf("%.1f", test2_sens * 100),
                                spec2 = sprintf("%.1f", test2_spec * 100)
                            )
                        )
                    } else if (strategy == "serial_negative") {
                        private$.formatTranslated(
                            .("<p>You selected a <strong>serial-negative teaching strategy</strong>. Everyone first receives {test1} (sensitivity {sens1}%, specificity {spec1}%). Only first-test negatives receive {test2} (sensitivity {sens2}%, specificity {spec2}%), and a final result is negative only when both tests are negative.</p><p>The false-negative rates and specificities therefore multiply. Combined sensitivity increases while combined specificity decreases. Two negative results do not automatically rule disease out; NPV still depends on prevalence, and false positives are shown in the population-flow table.</p>"),
                            list(
                                test1 = private$.safeHtmlOutput(test1_name),
                                sens1 = sprintf("%.1f", test1_sens * 100),
                                spec1 = sprintf("%.1f", test1_spec * 100),
                                test2 = private$.safeHtmlOutput(test2_name),
                                sens2 = sprintf("%.1f", test2_sens * 100),
                                spec2 = sprintf("%.1f", test2_spec * 100)
                            )
                        )
                    } else {
                        private$.formatTranslated(
                            .("<p>You selected a <strong>parallel teaching strategy</strong>. Everyone receives both {test1} (sensitivity {sens1}%, specificity {spec1}%) and {test2} (sensitivity {sens2}%, specificity {spec2}%), and a final result is positive when either test is positive.</p><p>The false-negative rates and specificities therefore multiply. Combined sensitivity increases while combined specificity decreases. These are the same accuracy formulas as serial-negative testing; only second-test utilization differs. Tests that fail on the same cases may add little benefit.</p>"),
                            list(
                                test1 = private$.safeHtmlOutput(test1_name),
                                sens1 = sprintf("%.1f", test1_sens * 100),
                                spec1 = sprintf("%.1f", test1_spec * 100),
                                test2 = private$.safeHtmlOutput(test2_name),
                                sens2 = sprintf("%.1f", test2_sens * 100),
                                spec2 = sprintf("%.1f", test2_spec * 100)
                            )
                        )
                    }

                    results_explanation <- private$.formatTranslated(
                        .("<h3>Sequential Testing Strategy Explanation</h3>{strategy}<h3>Results Interpretation</h3><p>With an entered prevalence of {prevalence}, the combined strategy has:</p><ul><li><strong>Combined sensitivity:</strong> {sensitivity}</li><li><strong>Combined specificity:</strong> {specificity}</li><li><strong>Combined PPV:</strong> {ppv}</li><li><strong>Combined NPV:</strong> {npv}</li></ul><p>For an illustrative population of {population} people:</p><ul><li>{finalPos} have a final positive classification</li><li>{finalNeg} have a final negative classification</li><li>Among {diseased} expected people with disease, {tp} are correctly identified ({tpRate})</li><li>Among {healthy} expected people without disease, {tn} are correctly identified ({tnRate})</li></ul>"),
                        list(
                            strategy = strategy_explanation,
                            prevalence = format_percent(prevalence),
                            sensitivity = format_percent(combined_sens),
                            specificity = format_percent(combined_spec),
                            ppv = format_percent(combined_ppv),
                            npv = format_percent(combined_npv),
                            population = base::format(pop_size, big.mark = ","),
                            finalPos = sprintf("%.0f", final_pos),
                            finalNeg = sprintf("%.0f", final_neg),
                            diseased = sprintf("%.0f", diseased),
                            tp = sprintf("%.0f", final_tp),
                            tpRate = tp_rate_text,
                            healthy = sprintf("%.0f", healthy),
                            tn = sprintf("%.0f", final_tn),
                            tnRate = tn_rate_text
                        )
                    )
                    independence_note <- private$.formatTranslated(
                        .("<div style='background-color: rgba(138, 155, 172, 0.06);padding:10px;border-radius:6px;margin-top:8px; color: inherit;'><strong>Assumption:</strong> Combined metrics assume conditional independence between tests. With positive conditional dependence, {direction}, and PPV and NPV shift accordingly.</div>"),
                        list(direction = dependence_caveat)
                    )
                    self$results$explanation_text$setContent(
                        paste0(independence_note, results_explanation)
                    )
                }

                # Populate strategy notes. Named test pairs are teaching examples only; they are
                # not recommendations, validated pathways, or sources for the illustrative inputs.
                guidance_html <- .("<div class='jmv-guidance' style='background-color: rgba(138, 155, 172, 0.06);padding:15px;border-radius:6px;margin-top:10px; color: inherit;'><h4>Strategy Notes and Teaching Examples</h4><p><strong>Teaching examples only \u{2014} not clinical guidance.</strong> The named pairs illustrate mathematical structure only. They are not recommendations, complete diagnostic algorithms, or validated pathways, and their sensitivity, specificity, prevalence, and cost values are not validated clinical parameters.</p><p><strong>How the strategies combine tests:</strong></p><ul><li><strong>Serial positive:</strong> Test 2 is applied to first-test positives; both tests must be positive. Specificity increases and sensitivity decreases under conditional independence.</li><li><strong>Serial negative:</strong> Test 2 is applied to first-test negatives; either positive makes the final result positive. Sensitivity increases and specificity decreases under conditional independence.</li><li><strong>Parallel:</strong> Both tests are applied to everyone; either positive makes the final result positive. Accuracy is algebraically identical to serial-negative testing, but utilization differs.</li></ul><p><strong>Why conditional dependence matters:</strong> Conditional dependence means that one test result remains informative about the other even after disease status is fixed, for example because the tests share biology, specimen characteristics, or technology. When positive conditional dependence is present in both disease-status groups, independence-based serial-positive calculations can make combined specificity too high and combined sensitivity too low. For serial-negative and parallel interpretation, they can make combined sensitivity too high and combined specificity too low. Gardner et al. (2000), <em>Conditional dependence between tests affects the diagnosis and surveillance of animal diseases</em>, describes these directional effects; see the reference below.</p><p>Test order is an input to explore, not a recommendation. Replace every example value with evidence appropriate to the intended setting.</p><p><strong>Illustrative pair labels:</strong></p><ul>")

                if (strategy == "serial_positive") {
                    guidance_html <- paste0(guidance_html,
                        .("<li>HIV-style teaching pair (Ag/Ab assay \u{2192} differentiation assay); this is not a complete HIV diagnostic algorithm</li><li>Cancer-screening teaching pair (Imaging \u{2192} Tissue sampling)</li><li>Respiratory-infection teaching pair (Rapid antigen \u{2192} Molecular assay)</li>")
                    )
                } else if (strategy == "serial_negative") {
                    guidance_html <- paste0(guidance_html,
                        .("<li>Rule-out teaching pair (Initial assessment \u{2192} Biomarker)</li><li>Exclusion teaching pair (Clinical score \u{2192} Laboratory assay)</li>")
                    )
                } else if (strategy == "parallel") {
                    guidance_html <- paste0(guidance_html,
                        .("<li>Emergency teaching pair (Physiologic test + Biomarker)</li><li>Acute-imaging teaching pair (Clinical assessment + Imaging)</li>")
                    )
                }

                guidance_html <- paste0(guidance_html, "</ul></div>")
                self$results$clinical_guidance$setContent(guidance_html)

                # Generate formulas HTML if requested
                if (self$options$show_formulas) {
                    formulas <- ""

                    formulas <- paste0(formulas, .("<h3>Mathematical Framework for Sequential Testing</h3>"))

                    # Basic probability definitions
                    formulas <- paste0(formulas, .("<h4>Key Definitions</h4>"))
                    formulas <- paste0(formulas, .("<ul>"))
                    formulas <- paste0(formulas, .("<li><strong>Prevalence (P)</strong>: The pre-test probability of disease</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Sensitivity (Se)</strong>: Probability of a positive test in diseased subjects</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Specificity (Sp)</strong>: Probability of a negative test in non-diseased subjects</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Positive Likelihood Ratio (LR+)</strong>: Se/(1-Sp)</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Negative Likelihood Ratio (LR-)</strong>: (1-Se)/Sp</li>"))
                    formulas <- paste0(formulas, .("</ul>"))

                    # Pre-test to post-test probability conversion
                    formulas <- paste0(formulas, .("<h4>Probability Conversions</h4>"))
                    formulas <- paste0(formulas, .("<p>To calculate how a test changes probability:</p>"))

                    formulas <- paste0(formulas, .("<ol>"))
                    formulas <- paste0(formulas, .("<li><strong>Convert pre-test probability to odds</strong>: Odds = P/(1-P)</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Multiply odds by likelihood ratio</strong>: Post-test odds = Pre-test odds \u{00D7} LR</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Convert post-test odds back to probability</strong>: Post-test P = Odds/(1+Odds)</li>"))
                    formulas <- paste0(formulas, .("</ol>"))

                    # Strategy-specific formulas
                    if (strategy == "serial_positive") {
                        formulas <- paste0(formulas, .("<h4>Serial Testing Strategy (Testing Positives)</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, the second test is only performed if the first test is positive. A subject is considered positive only if both tests are positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must test positive on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> \u{00D7} Se<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = base::format(test1_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Given a positive Test 1, probability of testing positive on Test 2: {value}</li>"),
                            list(value = base::format(test2_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {sens1} \u{00D7} {sens2} = {combined}</li>"),
                            list(
                                sens1 = base::format(test1_sens, digits = 4),
                                sens2 = base::format(test2_sens, digits = 4),
                                combined = base::format(combined_sens, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must either:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, .("<li>Test negative on Test 1, OR</li>"))
                        formulas <- paste0(formulas, .("<li>Test positive on Test 1 but negative on Test 2</li>"))
                        formulas <- paste0(formulas, .("</ul>"))
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> + (1-Sp<sub>1</sub>) \u{00D7} Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = base::format(test1_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of positive Test 1 followed by negative Test 2: (1 - {spec1}) \u{00D7} {spec2} = {value}</li>"),
                            list(
                                spec1 = base::format(test1_spec, digits = 4),
                                spec2 = base::format(test2_spec, digits = 4),
                                value = base::format((1 - test1_spec) * test2_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {first} + {second} = {combined}</li>"),
                            list(
                                first = base::format(test1_spec, digits = 4),
                                second = base::format((1 - test1_spec) * test2_spec, digits = 4),
                                combined = base::format(combined_spec, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))

                    } else if (strategy == "serial_negative") {
                        formulas <- paste0(formulas, .("<h4>Serial Testing Strategy (Testing Negatives)</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, the second test is only performed if the first test is negative. A subject is considered positive if either test is positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must either:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, .("<li>Test positive on Test 1, OR</li>"))
                        formulas <- paste0(formulas, .("<li>Test negative on Test 1 but positive on Test 2</li>"))
                        formulas <- paste0(formulas, .("</ul>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> + (1-Se<sub>1</sub>) \u{00D7} Se<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = base::format(test1_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of negative Test 1 followed by positive Test 2: (1 - {sens1}) \u{00D7} {sens2} = {value}</li>"),
                            list(
                                sens1 = base::format(test1_sens, digits = 4),
                                sens2 = base::format(test2_sens, digits = 4),
                                value = base::format((1 - test1_sens) * test2_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {first} + {second} = {combined}</li>"),
                            list(
                                first = base::format(test1_sens, digits = 4),
                                second = base::format((1 - test1_sens) * test2_sens, digits = 4),
                                combined = base::format(combined_sens, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> \u{00D7} Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = base::format(test1_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Given a negative Test 1, probability of testing negative on Test 2: {value}</li>"),
                            list(value = base::format(test2_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {spec1} \u{00D7} {spec2} = {combined}</li>"),
                            list(
                                spec1 = base::format(test1_spec, digits = 4),
                                spec2 = base::format(test2_spec, digits = 4),
                                combined = base::format(combined_spec, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))

                    } else if (strategy == "parallel") {
                        formulas <- paste0(formulas, .("<h4>Parallel Testing Strategy</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, both tests are performed on all subjects. A subject is considered positive if either test is positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must test positive on at least one test. This is calculated using the complement of the probability of testing negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = 1 - (1-Se<sub>1</sub>) \u{00D7} (1-Se<sub>2</sub>)</p>"))
                        formulas <- paste0(formulas, .("<p>This can be rewritten as:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> + Se<sub>2</sub> - (Se<sub>1</sub> \u{00D7} Se<sub>2</sub>)</p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = base::format(test1_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 2: {value}</li>"),
                            list(value = base::format(test2_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on both: {sens1} \u{00D7} {sens2} = {both}</li>"),
                            list(
                                sens1 = base::format(test1_sens, digits = 4),
                                sens2 = base::format(test2_sens, digits = 4),
                                both = base::format(test1_sens * test2_sens, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {sens1} + {sens2} - {both} = {combined}</li>"),
                            list(
                                sens1 = base::format(test1_sens, digits = 4),
                                sens2 = base::format(test2_sens, digits = 4),
                                both = base::format(test1_sens * test2_sens, digits = 4),
                                combined = base::format(combined_sens, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> \u{00D7} Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, .("<ul>"))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = base::format(test1_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 2: {value}</li>"),
                            list(value = base::format(test2_spec, digits = 4))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {spec1} \u{00D7} {spec2} = {combined}</li>"),
                            list(
                                spec1 = base::format(test1_spec, digits = 4),
                                spec2 = base::format(test2_spec, digits = 4),
                                combined = base::format(combined_spec, digits = 4))))
                        formulas <- paste0(formulas, .("</ul>"))
                    }

                    # Predictive values calculation
                    formulas <- paste0(formulas, .("<h4>Predictive Values Calculations</h4>"))

                    # Positive Predictive Value
                    formulas <- paste0(formulas, .("<h5>Positive Predictive Value (PPV)</h5>"))
                    formulas <- paste0(formulas, .("<p>The probability that a positive test result is a true positive:</p>"))
                    formulas <- paste0(formulas, .("<p>PPV = (P \u{00D7} Se) / (P \u{00D7} Se + (1-P) \u{00D7} (1-Sp))</p>"))

                    # Calculate intermediate values for clarity
                    ppv_numerator = prevalence * combined_sens
                    ppv_denominator = prevalence * combined_sens + (1-prevalence) * (1-combined_spec)

                    formulas <- paste0(formulas, .("<p>Calculation steps:</p>"))
                    formulas <- paste0(formulas, .("<ul>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Prevalence (P) = {value}</li>"),
                        list(value = base::format(prevalence, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined sensitivity (Se) = {value}</li>"),
                        list(value = base::format(combined_sens, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined specificity (Sp) = {value}</li>"),
                        list(value = base::format(combined_spec, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Numerator = P \u{00D7} Se = {p} \u{00D7} {se} = {numerator}</li>"),
                        list(
                            p = base::format(prevalence, digits = 4),
                            se = base::format(combined_sens, digits = 4),
                            numerator = base::format(ppv_numerator, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Denominator = P \u{00D7} Se + (1-P) \u{00D7} (1-Sp) = {numerator} + {oneMinusP} \u{00D7} {oneMinusSp} = {denominator}</li>"),
                        list(
                            numerator = base::format(ppv_numerator, digits = 4),
                            oneMinusP = base::format(1 - prevalence, digits = 4),
                            oneMinusSp = base::format(1 - combined_spec, digits = 4),
                            denominator = base::format(ppv_denominator, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>PPV = numerator/denominator = {numerator}/{denominator} = {ppv}</li>"),
                        list(
                            numerator = base::format(ppv_numerator, digits = 4),
                            denominator = base::format(ppv_denominator, digits = 4),
                            ppv = base::format(combined_ppv, digits = 4))))
                    formulas <- paste0(formulas, .("</ul>"))

                    # Negative Predictive Value
                    formulas <- paste0(formulas, .("<h5>Negative Predictive Value (NPV)</h5>"))
                    formulas <- paste0(formulas, .("<p>The probability that a negative test result is a true negative:</p>"))
                    formulas <- paste0(formulas, .("<p>NPV = ((1-P) \u{00D7} Sp) / ((1-P) \u{00D7} Sp + P \u{00D7} (1-Se))</p>"))

                    # Calculate intermediate values for clarity
                    npv_numerator = (1-prevalence) * combined_spec
                    npv_denominator = (1-prevalence) * combined_spec + prevalence * (1-combined_sens)

                    formulas <- paste0(formulas, .("<p>Calculation steps:</p>"))
                    formulas <- paste0(formulas, .("<ul>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Prevalence (P) = {value}</li>"),
                        list(value = base::format(prevalence, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined sensitivity (Se) = {value}</li>"),
                        list(value = base::format(combined_sens, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined specificity (Sp) = {value}</li>"),
                        list(value = base::format(combined_spec, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Numerator = (1-P) \u{00D7} Sp = {oneMinusP} \u{00D7} {sp} = {numerator}</li>"),
                        list(
                            oneMinusP = base::format(1 - prevalence, digits = 4),
                            sp = base::format(combined_spec, digits = 4),
                            numerator = base::format(npv_numerator, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Denominator = (1-P) \u{00D7} Sp + P \u{00D7} (1-Se) = {numerator} + {p} \u{00D7} {oneMinusSe} = {denominator}</li>"),
                        list(
                            numerator = base::format(npv_numerator, digits = 4),
                            p = base::format(prevalence, digits = 4),
                            oneMinusSe = base::format(1 - combined_sens, digits = 4),
                            denominator = base::format(npv_denominator, digits = 4))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>NPV = numerator/denominator = {numerator}/{denominator} = {npv}</li>"),
                        list(
                            numerator = base::format(npv_numerator, digits = 4),
                            denominator = base::format(npv_denominator, digits = 4),
                            npv = base::format(combined_npv, digits = 4))))
                    formulas <- paste0(formulas, .("</ul>"))

                    # Likelihood ratios
                    formulas <- paste0(formulas, .("<h4>Likelihood Ratios</h4>"))

                    # Positive likelihood ratio
                    formulas <- paste0(formulas, .("<h5>Positive Likelihood Ratio (LR+)</h5>"))
                    formulas <- paste0(formulas, .("<p>How much more likely a positive test result is to occur in patients with disease compared to those without:</p>"))
                    formulas <- paste0(formulas, .("<p>LR+ = Sensitivity / (1 - Specificity)</p>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<p>LR+ = {se} / (1 - {sp}) = {lr}</p>"),
                        list(
                            se = base::format(combined_sens, digits = 4),
                            sp = base::format(combined_spec, digits = 4),
                            lr = base::format(combined_sens / (1 - combined_spec), digits = 4))))

                    # Negative likelihood ratio
                    formulas <- paste0(formulas, .("<h5>Negative Likelihood Ratio (LR-)</h5>"))
                    formulas <- paste0(formulas, .("<p>How much more likely a negative test result is to occur in patients with disease compared to those without:</p>"))
                    formulas <- paste0(formulas, .("<p>LR- = (1 - Sensitivity) / Specificity</p>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<p>LR- = (1 - {se}) / {sp} = {lr}</p>"),
                        list(
                            se = base::format(combined_sens, digits = 4),
                            sp = base::format(combined_spec, digits = 4),
                            lr = base::format((1 - combined_sens) / combined_spec, digits = 4))))

                    self$results$formulas_text$setContent(formulas)
                }

                # Store data for plots
                if (self$options$show_plots) {
                    plotData <- list(
                        "Prevalence" = prevalence,
                        "Test1_Name" = test1_name,
                        "Test1_Sens" = test1_sens,
                        "Test1_Spec" = test1_spec,
                        "Test2_Name" = test2_name,
                        "Test2_Sens" = test2_sens,
                        "Test2_Spec" = test2_spec,
                        "Strategy" = strategy,
                        # translated label for plot subtitles; "Strategy" stays the key the renderers branch on
                        "StrategyLabel" = strategy_name,
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
                    title = .("Analysis Complete"),
                    content = sprintf(.('Sequential testing analysis completed: %s strategy with prevalence %.1f%%, combined sensitivity %.1f%%, combined specificity %.1f%%.'),
                                        strategy_name, prevalence*100, combined_sens*100, combined_spec*100)
                )

                # Independence assumption notice
                private$.addNotice(
                    type = "INFO",
                    title = .("Independence Assumption"),
                    content = private$.formatTranslated(
                        .('Combined metrics assume conditional independence between tests. If tests have positive conditional dependence (for example, similar biology or technology), {direction}.'),
                        list(direction = dependence_caveat)
                    )
                )

                # Render all collected notices (plain text, last step)
                private$.renderNotices()

                },


            .plot_flow_diagram = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                if (is.null(plotData))
                    return(FALSE)
                strategy <- plotData$Strategy
                all_people_label <- private$.formatTranslated(
                    .("All People\n(n={n})"), list(n = plotData$Pop_Size))
                positive_label <- .("Positive")
                negative_label <- .("Negative")
                positive_result_label <- .("Positive\nResult")
                negative_result_label <- .("Negative\nResult")

                # Enhanced flow diagram with better visual design
                if (strategy == "serial_positive") {
                    # Create flow for serial positive testing
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = all_people_label,
                                        size = 4, fontface = "bold") +

                        # First test
                        ggplot2::annotate("segment", x = 1.5, y = 5, xend = 2.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 4.5, ymax = 5.5,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 3, y = 5, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Positive branch
                        ggplot2::annotate("segment", x = 3.5, y = 5.2, xend = 4.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 4, y = 5.6, label = positive_label,
                                        size = 3, color = "#D55E00", angle = 30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 5.5, ymax = 6.5,
                                        fill = "#FBE6B5", color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 6, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Negative branch
                        ggplot2::annotate("segment", x = 3.5, y = 4.8, xend = 4.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 4, y = 4.4, label = negative_label,
                                        size = 3, color = "#0072B2", angle = -30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 3.5, ymax = 4.5,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 4, label = negative_result_label,
                                        size = 3.5, fontface = "bold") +

                        # Final results from second test
                        ggplot2::annotate("segment", x = 5.5, y = 6.2, xend = 6.5, yend = 6.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 6, ymax = 7,
                                        fill = "#F4D7CF", color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 6.5, label = positive_result_label,
                                        size = 3.5, fontface = "bold", color = "#D55E00") +

                        ggplot2::annotate("segment", x = 5.5, y = 5.8, xend = 6.5, yend = 5.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 5, ymax = 6,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 5.5, label = negative_result_label,
                                        size = 3.5, fontface = "bold", color = "#0072B2") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7.5) +
                        ggplot2::labs(title = .("Serial Testing Strategy: Test Positives"),
                                    subtitle = .("Second test only for those positive on first test")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                     axis.title = ggplot2::element_blank(),
                                     panel.grid = ggplot2::element_blank())

                } else if (strategy == "serial_negative") {
                    # Create flow for serial negative testing
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = all_people_label,
                                        size = 4, fontface = "bold") +

                        # First test
                        ggplot2::annotate("segment", x = 1.5, y = 5, xend = 2.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 4.5, ymax = 5.5,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 3, y = 5, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Positive branch (final result)
                        ggplot2::annotate("segment", x = 3.5, y = 5.2, xend = 4.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 4, y = 5.6, label = positive_label,
                                        size = 3, color = "#D55E00", angle = 30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 5.5, ymax = 6.5,
                                        fill = "#F4D7CF", color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 6, label = positive_result_label,
                                        size = 3.5, fontface = "bold", color = "#D55E00") +

                        # Negative branch (needs second test)
                        ggplot2::annotate("segment", x = 3.5, y = 4.8, xend = 4.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 4, y = 4.4, label = negative_label,
                                        size = 3, color = "#E69F00", angle = -30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 3.5, ymax = 4.5,
                                        fill = "#FBE6B5", color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 4, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Final results from second test
                        ggplot2::annotate("segment", x = 5.5, y = 4.2, xend = 6.5, yend = 4.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4, ymax = 5,
                                        fill = "#F4D7CF", color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 4.5, label = positive_result_label,
                                        size = 3.5, fontface = "bold", color = "#D55E00") +

                        ggplot2::annotate("segment", x = 5.5, y = 3.8, xend = 6.5, yend = 3.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 3, ymax = 4,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 3.5, label = negative_result_label,
                                        size = 3.5, fontface = "bold", color = "#0072B2") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(2.5, 7) +
                        ggplot2::labs(title = .("Serial Testing Strategy: Test Negatives"),
                                    subtitle = .("Second test only for those negative on first test")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                     axis.title = ggplot2::element_blank(),
                                     panel.grid = ggplot2::element_blank())

                } else {
                    # Parallel testing flow
                    flow_plot <- ggplot2::ggplot() +
                        # Start node
                        ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 4.5, ymax = 5.5,
                                        fill = "lightblue", color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("text", x = 1, y = 5, label = all_people_label,
                                        size = 4, fontface = "bold") +

                        # Split to both tests
                        ggplot2::annotate("segment", x = 1.5, y = 5.2, xend = 2.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", linewidth = 1) +
                        ggplot2::annotate("segment", x = 1.5, y = 4.8, xend = 2.5, yend = 4,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "darkblue", linewidth = 1) +

                        # Test 1
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 5.5, ymax = 6.5,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 3, y = 6, label = plotData$Test1_Name,
                                        size = 4, fontface = "bold") +

                        # Test 2
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 3.5, ymax = 4.5,
                                        fill = "#FBE6B5", color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 3, y = 4, label = plotData$Test2_Name,
                                        size = 4, fontface = "bold") +

                        # Combine results
                        ggplot2::annotate("segment", x = 3.5, y = 6, xend = 4.5, yend = 5.2,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("segment", x = 3.5, y = 4, xend = 4.5, yend = 4.8,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +

                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 4.5, ymax = 5.5,
                                        fill = "#F1DEEB", color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 5, label = .("Combine\nResults"),
                                        size = 4, fontface = "bold") +

                        # Final result
                        ggplot2::annotate("segment", x = 5.5, y = 5, xend = 6.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("text", x = 6, y = 5.3, label = .("Either +"),
                                        size = 3, color = "#CC79A7") +

                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4.5, ymax = 5.5,
                                        fill = "white", color = "black", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 5, label = .("Final\nResult"),
                                        size = 4, fontface = "bold") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7) +
                        ggplot2::labs(title = .("Parallel Testing Strategy"),
                                    subtitle = .("Both tests performed on everyone")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
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

                if (is.null(plotData))
                    return(FALSE)

                # Create comparison data
                metric_labels <- c(
                    .("Sensitivity"), .("Specificity"), .("PPV"), .("NPV")
                )
                perf_data <- data.frame(
                    TestKey = rep(c("test1", "test2", "combined"), 4),
                    Metric = rep(metric_labels, each = 3),
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

                perf_data$TestKey <- factor(
                    perf_data$TestKey,
                    levels = c("test1", "test2", "combined")
                )
                test_labels <- c(
                    test1 = plotData$Test1_Name,
                    test2 = plotData$Test2_Name,
                    combined = .("Combined")
                )

                perf_plot <- ggplot2::ggplot(perf_data, ggplot2::aes(x = TestKey, y = Value, fill = TestKey)) +
                    ggplot2::geom_col(alpha = 0.7, position = "dodge") +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Value)),
                                      position = ggplot2::position_dodge(width = 0.9),
                                      vjust = -0.5, size = 3) +
                    ggplot2::facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
                    ggplot2::labs(
                        title = .("Test Performance Comparison"),
                        subtitle = private$.formatTranslated(
                            .("Strategy: {strategy}"), list(strategy = if (is.null(plotData$StrategyLabel)) plotData$Strategy else plotData$StrategyLabel)),
                        y = .("Value (%)"), x = "", fill = ""
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom",
                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggplot2::scale_x_discrete(labels = test_labels) +
                    ggplot2::scale_fill_manual(
                        values = c("#66c2a5", "#fc8d62", "#8da0cb"),
                        labels = test_labels
                    ) +
                    ggplot2::ylim(0, 105)

                print(perf_plot)
                return(TRUE)
            },

            .plot_probability = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                if (is.null(plotData))
                    return(FALSE)

                # Calculate probability progression
                prevalence <- plotData$Prevalence

                # For positive test pathway
                test1_plr <- plotData$Test1_Sens / (1 - plotData$Test1_Spec)
                post_test1_pos_odds <- (prevalence / (1 - prevalence)) * test1_plr
                post_test1_pos_prob <- post_test1_pos_odds / (1 + post_test1_pos_odds)

                # For negative test pathway
                test1_nlr <- (1 - plotData$Test1_Sens) / plotData$Test1_Spec
                post_test1_neg_odds <- (prevalence / (1 - prevalence)) * test1_nlr
                post_test1_neg_prob <- post_test1_neg_odds / (1 + post_test1_neg_odds)

                test2_nlr <- (1 - plotData$Test2_Sens) / plotData$Test2_Spec
                post_test2_neg_odds <- post_test1_neg_odds * test2_nlr
                post_test2_neg_prob <- post_test2_neg_odds / (1 + post_test2_neg_odds)
                positive_path <- .("Positive Path")
                negative_path <- .("Negative Path")

                # Create data for plotting based on strategy
                if (plotData$Strategy == "serial_positive") {
                    prob_data <- data.frame(
                        Step = c(.("Pre-test"), .("After Test 1 (+)"), .("After Test 2 (+)"),
                                .("Pre-test"), .("After Test 1 (-)"), .("Final (-)")),
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, post_test1_neg_prob * 100,
                                      (1 - plotData$Combined_NPV) * 100),
                        Path = c(rep(positive_path, 3), rep(negative_path, 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                } else if (plotData$Strategy == "serial_negative") {
                    prob_data <- data.frame(
                        Step = c(.("Pre-test"), .("After Test 1 (+)"), .("Final (+)"),
                                .("Pre-test"), .("After Test 1 (-)"), .("After Test 2 (-)")),
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, post_test1_neg_prob * 100, post_test2_neg_prob * 100),
                        Path = c(rep(positive_path, 3), rep(negative_path, 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                } else {
                    # Parallel testing
                    prob_data <- data.frame(
                        Step = c(.("Pre-test"), .("After Either Test (+)"), .("Final PPV"),
                                .("Pre-test"), .("After Both Tests (-)"), .("Final NPV")),
                        Probability = c(prevalence * 100, plotData$Combined_PPV * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, (1 - plotData$Combined_NPV) * 100, (1 - plotData$Combined_NPV) * 100),
                        Path = c(rep(positive_path, 3), rep(negative_path, 3)),
                        x = c(1, 2, 3, 1, 2, 3)
                    )
                }

                prob_plot <- ggplot2::ggplot(
                    prob_data,
                    ggplot2::aes(
                        x = x, y = Probability, color = Path,
                        linetype = Path, group = Path
                    )
                ) +
                    ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
                    ggplot2::geom_point(size = 4) +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Probability)),
                                      vjust = -1.5, hjust = 0.5, size = 3) +
                    ggplot2::scale_x_continuous(
                        breaks = 1:3,
                        labels = c(.("Pre-test"), .("After Test 1"), .("Final"))
                    ) +
                    ggplot2::scale_color_manual(
                        values = stats::setNames(
                            c("#D55E00", "#0072B2"), c(positive_path, negative_path))) +
                    ggplot2::scale_linetype_manual(
                        values = stats::setNames(
                            c("solid", "dashed"), c(positive_path, negative_path))) +
                    ggplot2::labs(
                        title = .("Probability Transformation Through Testing"),
                        subtitle = private$.formatTranslated(
                            .("Strategy: {strategy} | Prevalence: {prevalence}"),
                            list(
                                strategy = if (is.null(plotData$StrategyLabel)) plotData$Strategy else plotData$StrategyLabel,
                                prevalence = sprintf("%.1f%%", plotData$Prevalence * 100))),
                        x = .("Testing Stage"), y = .("Disease Probability (%)"),
                        color = "", linetype = ""
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::ylim(0, max(prob_data$Probability) * 1.2)

                print(prob_plot)
                return(TRUE)
            },

            .plot_population_flow = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                if (is.null(plotData))
                    return(FALSE)

                # Create Sankey-like flow visualization
                pop_size <- plotData$Pop_Size
                diseased <- plotData$Diseased
                healthy <- plotData$Healthy

                # Create flow data
                flow_data <- data.frame(
                    Category = c(.("Total Population"), .("Disease Present"), .("Disease Absent"),
                               .("True Positive"), .("False Negative"), .("False Positive"), .("True Negative")),
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
                    ggplot2::annotate("text", x = 1, y = 50, label = private$.formatTranslated(
                                        .("Total\nn={n}"), list(n = pop_size)),
                                    size = 5, fontface = "bold") +

                    # Disease status split
                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 55, ymax = 85,
                                    fill = "#F4D7CF", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 70, label = private$.formatTranslated(
                                        .("Disease+\nn={n}"), list(n = round(diseased))),
                                    size = 4, fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 15, ymax = 45,
                                    fill = "#DCEAF7", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 30, label = private$.formatTranslated(
                                        .("Disease-\nn={n}"), list(n = round(healthy))),
                                    size = 4, fontface = "bold") +

                    # Test results
                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 75, ymax = 90,
                                    fill = "#0072B2", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 82.5, label = private$.formatTranslated(
                                        .("TP\nn={n}"), list(n = round(plotData$Final_TP))),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 60, ymax = 75,
                                    fill = "#D55E00", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 67.5, label = private$.formatTranslated(
                                        .("FN\nn={n}"), list(n = round(plotData$Final_FN))),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 30, ymax = 45,
                                    fill = "#E69F00", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 37.5, label = private$.formatTranslated(
                                        .("FP\nn={n}"), list(n = round(plotData$Final_FP))),
                                    size = 3.5, color = "white", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 10, ymax = 30,
                                    fill = "#0072B2", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 20, label = private$.formatTranslated(
                                        .("TN\nn={n}"), list(n = round(plotData$Final_TN))),
                                    size = 3.5, color = "white", fontface = "bold") +

                    # Add flow arrows
                    ggplot2::annotate("segment", x = 1.5, y = 60, xend = 2.5, yend = 70,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5) +
                    ggplot2::annotate("segment", x = 1.5, y = 40, xend = 2.5, yend = 30,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5) +

                    ggplot2::annotate("segment", x = 3.5, y = 75, xend = 4.5, yend = 82.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5, color = "#0072B2") +
                    ggplot2::annotate("segment", x = 3.5, y = 65, xend = 4.5, yend = 67.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5, color = "#D55E00") +
                    ggplot2::annotate("segment", x = 3.5, y = 35, xend = 4.5, yend = 37.5,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5, color = "#E69F00") +
                    ggplot2::annotate("segment", x = 3.5, y = 25, xend = 4.5, yend = 20,
                                    arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                    linewidth = 1, alpha = 0.5, color = "#0072B2") +

                    # Add labels
                    ggplot2::annotate("text", x = 1, y = 5, label = .("Initial"), size = 4, fontface = "bold") +
                    ggplot2::annotate("text", x = 3, y = 5, label = .("True Status"), size = 4, fontface = "bold") +
                    ggplot2::annotate("text", x = 5, y = 5, label = .("Test Results"), size = 4, fontface = "bold") +

                    ggplot2::xlim(0, 6) + ggplot2::ylim(0, 95) +
                    ggplot2::labs(
                        title = .("Population Flow Through Testing"),
                        subtitle = private$.formatTranslated(
                            .("Strategy: {strategy} | Sensitivity: {sensitivity} | Specificity: {specificity}"),
                            list(
                                strategy = if (is.null(plotData$StrategyLabel)) plotData$Strategy else plotData$StrategyLabel,
                                sensitivity = sprintf("%.1f%%", plotData$Combined_Sens * 100),
                                specificity = sprintf("%.1f%%", plotData$Combined_Spec * 100)))
                    ) +
                    ggplot2::theme_void() +
                    ggtheme +
                    ggplot2::theme(
                        axis.text = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5)
                    )

                print(flow_plot)
                return(TRUE)
            },

            .plot_sensitivity_analysis = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                if (is.null(plotData))
                    return(FALSE)

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
                ppv_label <- .("PPV (Positive Predictive Value)")
                npv_label <- .("NPV (Negative Predictive Value)")
                df <- data.frame(
                    Prevalence = rep(prev_range, 2),
                    Value = c(ppv_curve, npv_curve),
                    Metric = rep(c(ppv_label, npv_label), each = length(prev_range))
                )

                # Remove NA values
                df <- df[!is.na(df$Value), ]

                # Create sensitivity analysis plot
                sens_plot <- ggplot2::ggplot(
                    df,
                    ggplot2::aes(
                        x = Prevalence, y = Value, color = Metric, linetype = Metric)
                ) +
                    ggplot2::geom_line(linewidth = 1.5, alpha = 0.8) +
                    ggplot2::geom_vline(xintercept = plotData$Prevalence, linetype = "dashed",
                                       color = "gray40", linewidth = 0.8, alpha = 0.7) +
                    ggplot2::annotate("text", x = plotData$Prevalence, y = 0.95,
                                     label = sprintf(.("Your prevalence\n(%.1f%%)"), plotData$Prevalence * 100),
                                     size = 3, hjust = ifelse(plotData$Prevalence > 0.5, 1.1, -0.1)) +
                    ggplot2::labs(
                        title = .("Sensitivity Analysis: How Prevalence Affects Predictive Values"),
                        subtitle = sprintf(.("Combined Test Performance: Sensitivity=%.1f%%, Specificity=%.1f%%"),
                                          plotData$Combined_Sens * 100, plotData$Combined_Spec * 100),
                        x = .("Disease Prevalence"),
                        y = .("Probability"),
                        color = "",
                        linetype = ""
                    ) +
                    ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100),
                                               limits = c(0, 1)) +
                    ggplot2::scale_x_continuous(labels = function(x) sprintf("%.0f%%", x * 100)) +
                    ggplot2::scale_color_manual(values = stats::setNames(
                        c("#D55E00", "#0072B2"), c(ppv_label, npv_label))) +
                    ggplot2::scale_linetype_manual(values = stats::setNames(
                        c("solid", "dashed"), c(ppv_label, npv_label))) +
                    ggplot2::theme_minimal() +
                    ggtheme +
                    ggplot2::theme(
                        legend.position = "bottom",
                        legend.text = ggplot2::element_text(size = 10),
                        plot.title = ggplot2::element_text(face = "bold", size = 12),
                        plot.subtitle = ggplot2::element_text(size = 10, color = "gray40")
                    )

                print(sens_plot)
                return(TRUE)
            },

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            # Teaching examples. This table MUST match SEQUENTIAL_PRESET_CONFIGS in
            # jamovi/js/sequentialtests.events.js -- the JavaScript applies examples in the GUI,
            # this applies them for callers from R, and a regression test compares the two.
            .getPresetValues = function(preset) {
                presets <- list(
                    covid_screening_confirmation = list(
                        test1_name = .('Rapid Antigen Test'), test1_sens = 0.75, test1_spec = 0.95,
                        test2_name = .('RT-PCR'),             test2_sens = 0.95, test2_spec = 0.99,
                        prevalence = 0.08, strategy = 'serial_positive'),
                    breast_cancer_screening = list(
                        test1_name = .('Mammography'),        test1_sens = 0.85, test1_spec = 0.90,
                        test2_name = .('Tissue Biopsy'),      test2_sens = 0.98, test2_spec = 0.99,
                        prevalence = 0.06, strategy = 'serial_positive'),
                    mi_emergency_parallel = list(
                        test1_name = .('Troponin'),           test1_sens = 0.90, test1_spec = 0.95,
                        test2_name = .('ECG'),                test2_sens = 0.70, test2_spec = 0.90,
                        prevalence = 0.20, strategy = 'parallel'),
                    tb_screening_confirmation = list(
                        test1_name = .('Chest X-ray'),        test1_sens = 0.75, test1_spec = 0.80,
                        test2_name = .('Sputum Culture'),     test2_sens = 0.85, test2_spec = 0.98,
                        prevalence = 0.12, strategy = 'serial_positive'),
                    prostate_screening_exclusion = list(
                        test1_name = .('PSA Test'),           test1_sens = 0.80, test1_spec = 0.70,
                        test2_name = .('MRI'),                test2_sens = 0.90, test2_spec = 0.85,
                        prevalence = 0.15, strategy = 'serial_negative'),
                    hiv_screening_confirmation = list(
                        test1_name = .('HIV Ag/Ab Assay'),    test1_sens = 0.98, test1_spec = 0.95,
                        test2_name = .('Differentiation Assay'), test2_sens = 0.99, test2_spec = 0.99,
                        prevalence = 0.02, strategy = 'serial_positive'),
                    stroke_emergency_parallel = list(
                        test1_name = .('Clinical Assessment'), test1_sens = 0.85, test1_spec = 0.75,
                        test2_name = .('CT Scan'),             test2_sens = 0.95, test2_spec = 0.98,
                        prevalence = 0.25, strategy = 'parallel')
                )
                if (!preset %in% names(presets))
                    return(NULL)
                presets[[preset]]
            },

            .noticeList = list(),

            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },

            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = .("ERROR: "),
                        STRONG_WARNING = .("STRONG WARNING: "),
                        WARNING        = .("WARNING: "),
                        "")
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },

            # .fmt() takes named replacements through `...`; passing a single list
            # produces an ellipsis character. Keep replacement values together at call sites,
            # then expand them here so translated templates retain reorderable {name} tokens.
            .formatTranslated = function(template, values) {
                # .fmt, not jmvcore::format: the substituter re-scans after each
                # replacement, so a value containing its own placeholder loops forever.
                do.call(.fmt, c(list(template), values))
            },

            # HTML sanitization for security (used by Html output items: plain_summary,
            # explanation_text, clinical_guidance - NOT the plain-text notices sink)
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
