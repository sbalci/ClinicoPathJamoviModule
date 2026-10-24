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
            # jmvcore 2.7.38 writes an OptionString into the syntax pane as paste0('"', value, '"'),
            # unescaped: a name containing " broke the snippet, and a crafted one parsed as live R.
            .sourcifyOption = function(option) {
                value <- option$value
                if (option$name %in% c("test1_name", "test2_name") && is.character(value) &&
                    length(value) == 1L && !identical(value, option$default))
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                super$.sourcifyOption(option)
            },

            .init = function() {
                # Add rows to tables during initialization. Test names are user text and jamovi
                # parses table cells as HTML, so "<LOD assay" rendered blank: every cell sink
                # goes through .diagCellText() (R/utils-diagnostic-comparator.R).
                individualTable <- self$results$individual_tests_table
                individualTable$addRow(rowKey = "test1",
                                       values = list(test_name = .diagCellText(self$options$test1_name)))
                individualTable$addRow(rowKey = "test2",
                                       values = list(test_name = .diagCellText(self$options$test2_name)))
                individualTable$addRow(rowKey = "combined",
                                       values = list(test_name = .("Combined Strategy")))

                flowTable <- self$results$population_flow_table
                flowTable$addRow(rowKey = "initial",
                                 values = list(stage = .("Initial Population")))
                flowTable$addRow(rowKey = "after_test1",
                                 values = list(stage = .("After First Test")))
                flowTable$addRow(rowKey = "after_test2",
                                 values = list(stage = .("After Combined Strategy")))

                # The cost table always has exactly these three rows. Creating them here
                # (rather than adding them each run) keeps the structure visible before the
                # analysis runs and removes the need to clear stale rows in .run(). The item
                # labels are placeholders: .run() rewrites them because a teaching example can
                # override the test names.
                costTable <- self$results$cost_analysis_table
                costTable$addRow(rowKey = "test1",
                                 values = list(item = private$.formatTranslated(
                                     .("Test 1: {name}"), list(name = .diagCellText(self$options$test1_name)))))
                costTable$addRow(rowKey = "test2",
                                 values = list(item = private$.formatTranslated(
                                     .("Test 2: {name}"), list(name = .diagCellText(self$options$test2_name)))))
                costTable$addRow(rowKey = "total",
                                 values = list(item = .("Total Protocol Cost")))
            },

            .run = function() {

                private$.noticeList <- list()

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
                                list(preset = preset_values$title)
                            )
                        )

                        # In the GUI these controls are disabled while an example is selected and
                        # the JavaScript writes the example's values into them, so they agree. An R
                        # call can pass its own values next to a preset; name the ones replaced.
                        # Names are display labels (translated here, English in the JS): not compared.
                        override_labels <- c(
                            test1_sens = .("Test 1 sensitivity"), test1_spec = .("Test 1 specificity"),
                            test2_sens = .("Test 2 sensitivity"), test2_spec = .("Test 2 specificity"),
                            prevalence = .("Prevalence"), strategy = .("Testing strategy"))
                        overridden <- character(0)
                        for (key in names(override_labels)) {
                            entered <- self$options$get(key)
                            used <- preset_values[[key]]
                            if (!isTRUE(all.equal(entered, used)))
                                overridden <- c(overridden, sprintf("%s: %s -> %s",
                                    override_labels[[key]], format(entered), format(used)))
                        }
                        if (length(overridden) > 0) {
                            private$.addNotice(
                                'WARNING',
                                .('Teaching Example Replaced Entered Values'),
                                private$.formatTranslated(
                                    .('The teaching example replaced these entered values (entered -> used): {values}. Set the teaching example to "Custom Values" to analyse your own values.'),
                                    list(values = paste(overridden, collapse = "; "))
                                )
                            )
                        }
                    }
                }

                format_percent <- private$.formatPercent

                # Sensitivity, specificity and prevalence need no range check here: the .a.yaml
                # bounds (0.01-0.99 and 0.001-0.999) are enforced when the options are built, so
                # no value outside them reaches .run(). A name is only a display label, so an
                # empty one falls back to a generic label instead of blocking the calculation.
                both_named <- !is.null(test1_name) && nzchar(trimws(test1_name)) &&
                    !is.null(test2_name) && nzchar(trimws(test2_name))
                if (is.null(test1_name) || !nzchar(trimws(test1_name)))
                    test1_name <- .("Test 1")
                if (is.null(test2_name) || !nzchar(trimws(test2_name)))
                    test2_name <- .("Test 2")

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
                # The module's clinical threshold for an unstable PPV is 5%, not 1%: the whole
                # 1%-5% band (where the shipped HIV teaching example sits, at 2%) used to pass
                # with no notice at all. Two tiers, so the very-low wording is kept.
                if (prevalence <= 0.01) {
                    private$.addNotice('STRONG_WARNING', .('Prevalence Very Low'), sprintf(.('Prevalence is very low (%.3f%%). Even with excellent tests, PPV may be extremely low. Verify the pre-test probability for the intended population.'), prevalence*100))
                } else if (prevalence < 0.05) {
                    # "more than 15 percentage points", not "tens": with specificity capped at 0.99 a one-point change at
                    # most halves the false-positive rate, and PPV = a / (a + FPR) then moves by at most
                    # (sqrt(2) - 1)^2 = 17.2 points. The 5% edge is this module's convention, not a property:
                    # the same 15+ point move occurs at 10% prevalence with sensitivity 0.20, so the text
                    # says "this analysis flags" rather than "below 5%, PPV is dominated by specificity".
                    private$.addNotice('STRONG_WARNING', .('Prevalence Low'), sprintf(.('Prevalence is low (%.2f%%). This analysis flags pre-test probabilities below 5%%, where PPV can be highly sensitive to specificity: in this range a one-percentage-point change in specificity can move PPV by more than 15 percentage points. Verify the specificity assumptions and the intended population.'), prevalence*100))
                }

                # Detect potential test correlation. fixed = TRUE compares the names literally
                # (fuzzy string similarity), so a test name containing regex metacharacters
                # (e.g. "CA-125 [serum]") can't throw a malformed-pattern error that aborts .run().
                # The edit budget is floor(30% of the name), passed as an absolute count: agrep turns
                # a FRACTION into ceiling(0.3 * nchar) edits, one edit for a one-letter name, so "A"
                # and "B" were reported as similar. 0 edits is an exact substring match. Checked both
                # ways round, since test order says nothing about similarity ("RT-PCR" then "PCR").
                similar_name <- function(a, b)
                    agrepl(a, b, max.distance = floor(0.3 * nchar(a)), fixed = TRUE)
                test_similarity <- similar_name(test1_name, test2_name) ||
                    similar_name(test2_name, test1_name)
                # The exemption is for the UNTOUCHED default pair only. With `&&` on each name
                # separately the notice was suppressed whenever EITHER box still held its
                # default, so naming both tests "Confirmatory Test" - the strongest possible
                # conditional-dependence signal - posted nothing.
                is_default_pair <- identical(test1_name, "Screening Test") &&
                    identical(test2_name, "Confirmatory Test")
                # WARNING, not STRONG_WARNING: similar names are a prompt to check, not evidence of
                # dependence ("CT Scan"/"MR Scan" match; "Rapid Antigen Test"/"RT-PCR" do not).
                # Fallback labels ("Test 1"/"Test 2") are one edit apart; they say nothing.
                if (both_named && test_similarity && !is_default_pair) {
                    private$.addNotice('WARNING', .('Test Correlation Risk'), sprintf(.('Test names are similar ("%s" vs "%s"). If tests measure similar biomarkers or use similar technology, they may be conditionally dependent. This can bias combined sensitivity and specificity in opposite directions.'), test1_name, test2_name))
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

                # Test order. Both serial rules are symmetric in the two tests (both-positive and
                # either-positive), so swapping them changes no combined Se, Sp, PPV or NPV. The
                # WARNING that used to sit here ("Test 2 specificity is below Test 1 ... may not
                # improve specificity as much as expected") implied the opposite, and fired on the
                # order that needs FEWER second tests (98 vs 365 per 1000 on the swapped defaults).
                # What order does change is how many second tests are performed, so say that, and
                # only when the swap would perform fewer.
                if (strategy %in% c("serial_positive", "serial_negative")) {
                    p_pos <- function(se, sp) prevalence * se + (1 - prevalence) * (1 - sp)
                    retest_share <- if (strategy == "serial_positive")
                        c(now = p_pos(test1_sens, test1_spec), swapped = p_pos(test2_sens, test2_spec))
                    else
                        c(now = 1 - p_pos(test1_sens, test1_spec), swapped = 1 - p_pos(test2_sens, test2_spec))
                    order_n <- as.integer(self$options$population_size)
                    retests <- round(order_n * retest_share)
                    if (retests[["swapped"]] < retests[["now"]]) {
                        private$.addNotice('INFO', .('Test Order'), private$.formatTranslated(
                            .('Test order does not change the combined sensitivity, specificity, PPV or NPV of this strategy. It changes how many second tests are performed: about {now} per {n} people in this order, about {swapped} if the two tests were swapped. Which order costs less also depends on which test is then given to everyone; the Cost Analysis shows the current order.'),
                            lapply(c(as.list(retests), n = order_n), base::format, big.mark = ",")))
                    }
                } else if (strategy == "parallel") {
                    if (abs(test1_sens - test2_sens) < 0.05 && abs(test1_spec - test2_spec) < 0.05) {
                        private$.addNotice('WARNING', .('Parallel Test Assumption Check'), .('The tests have similar marginal sensitivity and specificity. Similar marginal performance does not establish redundancy or independence; assess conditional dependence using external evidence.'))
                    }
                }

                # Predictive values and likelihood ratios. Every denominator is positive: the .a.yaml
                # bounds keep sensitivity, specificity and prevalence strictly inside (0, 1), and the
                # combined values (products and complements of products) stay inside it, within 1e-4.
                ppv_of <- function(se, sp) private$.ppvOf(prevalence, se, sp)
                npv_of <- function(se, sp) private$.npvOf(prevalence, se, sp)

                test1_ppv <- ppv_of(test1_sens, test1_spec)
                test1_npv <- npv_of(test1_sens, test1_spec)
                test1_plr <- test1_sens / (1 - test1_spec)
                test1_nlr <- (1 - test1_sens) / test1_spec
                test2_ppv <- ppv_of(test2_sens, test2_spec)
                test2_npv <- npv_of(test2_sens, test2_spec)
                test2_plr <- test2_sens / (1 - test2_spec)
                test2_nlr <- (1 - test2_sens) / test2_spec

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
                    # Same expression as serial-negative (algebraically equal to s1 + s2 - s1 s2), so
                    # the two strategies the output calls "exactly the same" agree to the last bit.
                    combined_sens <- test1_sens + (1 - test1_sens) * test2_sens
                    combined_spec <- test1_spec * test2_spec
                    strategy_name <- .("Parallel Testing (Test all)")
                }

                combined_ppv <- ppv_of(combined_sens, combined_spec)
                combined_npv <- npv_of(combined_sens, combined_spec)
                combined_plr <- combined_sens / (1 - combined_spec)
                combined_nlr <- (1 - combined_sens) / combined_spec

                # Calculate Number Needed to Screen
                # NNT = 1 / (prevalence x combined_sens)
                # This is the number of people needed to screen to find one true positive
                # ceiling() of a floating reciprocal: parallel's .2 + .75 - .2 * .75 is
                # 0.7999999999999999, so 1 / (.625 x .8) came out as 2.0000000000000004 and was
                # shown as 3, while serial-negative (same .8, computed another way) showed 2.
                # A relative 1e-14 (about 45 ulps) absorbs that error (1-2 ulps observed). A genuine
                # value within 1e-14 x NNS above an integer needs inputs carrying more than 14
                # decimal digits between them, so typed values are never rounded down.
                nnt <- ceiling((1 / (prevalence * combined_sens)) * (1 - 1e-14))

                # Update summary table
                summaryTable <- self$results$summary_table
                summaryTable$setRow(
                    rowNo = 1,
                    values = list(
                        strategy_name = strategy_name,
                        prevalence = prevalence,
                        first_test = .diagCellText(test1_name),
                        second_test = .diagCellText(test2_name),
                        combined_sens = combined_sens,
                        combined_spec = combined_spec,
                        combined_ppv = combined_ppv,
                        combined_npv = combined_npv,
                        nnt = nnt
                    )
                )
                private$.footnoteRoundedPercents(
                    summaryTable, c("combined_sens", "combined_spec", "combined_ppv", "combined_npv"))

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

                    nnt_text <- sprintf(.("You would need to screen approximately %d people to identify one true positive case."), nnt)

                    # Parallel testing applies both tests at once; "X followed by Y" described a
                    # sequence that parallel testing does not have.
                    tests_phrase <- private$.formatTranslated(
                        if (strategy == "parallel") .("{first} and {second} applied together")
                        else .("{first} followed by {second}"),
                        list(first = private$.safeHtmlOutput(test1_name),
                             second = private$.safeHtmlOutput(test2_name)))

                    summary <- sprintf(
                        .("<div style='background-color: rgba(33, 149, 188, 0.1);padding:15px;border-left:4px solid #0077be;font-size:1.05em;line-height:1.6; color: inherit;'><strong>Illustrative Summary:</strong> Using a %1$s with %2$s, the combined test achieves %3$s sensitivity (detects %4$s of every 100 diseased individuals) and %5$s specificity (correctly rules out %6$s of every 100 healthy individuals). At your specified disease prevalence of %7$.1f%%, a positive result indicates a %8$s chance the person truly has the disease (PPV), while a negative result indicates a %9$s chance the person is truly disease-free (NPV). %10$s %11$s <em>These combined figures assume the two tests are conditionally independent given disease status, and they treat the sensitivity, specificity and prevalence you entered as exact, so they carry no confidence interval. If the two tests are positively dependent in both disease-status groups (for example, because they measure related biology), %12$s.</em></div>"),
                        strategy_desc,
                        tests_phrase,
                        format_percent(combined_sens), private$.formatPer100(combined_sens),
                        format_percent(combined_spec), private$.formatPer100(combined_spec),
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
                        test_name = .diagCellText(test1_name),
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
                        test_name = .diagCellText(test2_name),
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
                private$.footnoteRoundedPercents(
                    individualTable, c("sensitivity", "specificity", "ppv", "npv"))
                # Without this, "Confirmatory Test PPV 81.6%" (defaults) read as the confirmatory
                # step's value; within the serial-positive pathway it is P(D | T1+, T2+) = 93.4%.
                individualTable$setNote(
                    key = "standalone",
                    note = .("PPV and NPV in each test's row are for that test used on its own at the entered prevalence. In a serial strategy the second test is given only to first-test positives or negatives, whose disease probability can differ from the entered prevalence; the Combined Strategy row is the result of the whole strategy.")
                )

                # Calculate population flow numbers using the user-specified population size
                # An R caller passes a double, and format(100000, big.mark = ",") prints "1e+05" in
                # the cost title, the explanation and the flow diagram. The option is an Integer.
                pop_size <- as.integer(self$options$population_size)
                diseased <- pop_size * prevalence
                healthy <- pop_size - diseased

                # population_size has min 100 and prevalence min 0.001, so the flow table can
                # legitimately print "Disease Positive 0.100". Nothing told the user that the
                # illustrative population is simply too small to illustrate anything.
                if (diseased < 10) {
                    private$.addNotice(
                        'WARNING', .('Illustrative Population Too Small'),
                        sprintf(.('An illustrative population of %1$.0f at %2$.2f%% prevalence contains only %3$.2f expected people with disease, so the population-flow counts are fractional. Increase the population size (for example to 10000) for readable whole-person counts.'),
                                pop_size, prevalence * 100, diseased))
                }

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
                # These are spliced into the translated Explanation HTML, so they must be
                # translatable like the format_percent() fallbacks beside them.
                tp_rate_text <- if (is.na(tp_rate)) .("not defined (no diseased subjects)") else format_percent(tp_rate)
                tn_rate_text <- if (is.na(tn_rate)) .("not defined (no disease-free subjects)") else format_percent(tn_rate)

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
                flowTable$setNote(
                    key = "expected_counts",
                    note = .("Counts are expected values for the illustrative population, derived from the entered probabilities, so they can be fractional.")
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
                        item = private$.formatTranslated(.("Test 1: {name}"), list(name = .diagCellText(test1_name))),
                        unit_cost = test1_cost,
                        number_tests = n_test1,
                        total_cost = total_cost1
                    ))
                    
                    costTable$setRow(rowKey = "test2", values = list(
                        item = private$.formatTranslated(.("Test 2: {name}"), list(name = .diagCellText(test2_name))),
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
                    # Both unit costs default to 0, so ticking the box alone showed a table of
                    # zero costs with nothing saying why.
                    costTable$setNote(
                        key = "zero_costs",
                        note = if (test1_cost == 0 && test2_cost == 0)
                            .("Both unit costs are 0, so every cost is 0. Enter the unit costs under Cost Analysis; the expected numbers of tests do not depend on them.")
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
                    note = .("Sensitivity, specificity and prevalence are treated as exact. These combined figures therefore carry <i>no</i> confidence interval and do not reflect sampling uncertainty in the values entered \u2014 published test performance and local prevalence both vary.")
                )

                # "Number Needed to Screen" is 1 / (prevalence x combined sensitivity): it counts
                # entries into the pathway. A serial strategy also consumes second tests, which
                # this single figure does not convey. The name's usual meaning (Rembold 1998) is
                # people screened per death or adverse event PREVENTED, which is NNT times larger,
                # so the note has to say which one this is.
                summaryTable$setNote(
                    key = "nns_scope",
                    note = .("<i>Number Needed to Screen</i> here is the number of people entering the pathway per true-positive case detected, 1 / (prevalence \u00D7 combined sensitivity), rounded up. It is not the screening-benefit statistic of the same name, people screened per death or adverse event prevented. It counts first tests only: a serial strategy also performs a second test on part of that group, and the Cost Analysis table shows the expected number of each test.")
                )

                summaryTable$setNote(
                    key = "independence_warning",
                    note = private$.formatTranslated(
                        .("Combined figures assume the two tests are <i>conditionally independent</i> \u2014 that, among people with the same disease status, one test's result says nothing about the other's. With positive conditional dependence in both disease-status groups, {direction}. Dependence among people with the disease alone biases the combined sensitivity but not the specificity, and dependence among people without it the reverse; PPV and NPV move with either."),
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
                            finalPos = private$.formatCount(final_pos),
                            finalNeg = private$.formatCount(final_neg),
                            diseased = private$.formatCount(diseased),
                            tp = private$.formatCount(final_tp),
                            tpRate = tp_rate_text,
                            healthy = private$.formatCount(healthy),
                            tn = private$.formatCount(final_tn),
                            tnRate = tn_rate_text
                        )
                    )
                    independence_note <- private$.formatTranslated(
                        .("<div style='background-color: rgba(138, 155, 172, 0.06);padding:10px;border-radius:6px;margin-top:8px; color: inherit;'><strong>Assumption:</strong> Combined metrics assume conditional independence between tests. With positive conditional dependence in both disease-status groups, {direction}, and PPV and NPV shift accordingly.</div>"),
                        list(direction = dependence_caveat)
                    )
                    self$results$explanation_text$setContent(
                        paste0(independence_note, results_explanation)
                    )
                }

                # Built only when shown: clinical_guidance is visible: (show_explanation), and
                # filling it on every run populated an item the user had hidden.
                if (self$options$show_explanation) {
                    # Populate strategy notes. Named test pairs are teaching examples only; they are
                    # not recommendations, validated pathways, or sources for the illustrative inputs.
                    guidance_html <- .("<div class='jmv-guidance' style='background-color: rgba(138, 155, 172, 0.06);padding:15px;border-radius:6px;margin-top:10px; color: inherit;'><h4>Strategy Notes and Teaching Examples</h4><p><strong>Teaching examples only \u2014 not clinical guidance.</strong> The named pairs illustrate mathematical structure only. They are not recommendations, complete diagnostic algorithms, or validated pathways, and their sensitivity, specificity, prevalence, and cost values are not validated clinical parameters.</p><p><strong>How the strategies combine tests:</strong></p><ul><li><strong>Serial positive:</strong> Test 2 is applied to first-test positives; both tests must be positive. Specificity increases and sensitivity decreases under conditional independence.</li><li><strong>Serial negative:</strong> Test 2 is applied to first-test negatives; either positive makes the final result positive. Sensitivity increases and specificity decreases under conditional independence.</li><li><strong>Parallel:</strong> Both tests are applied to everyone; either positive makes the final result positive. Accuracy is algebraically identical to serial-negative testing, but utilization differs.</li></ul><p><strong>Why conditional dependence matters:</strong> Conditional dependence means that one test result remains informative about the other even after disease status is fixed, for example because the tests share biology, specimen characteristics, or technology. When positive conditional dependence is present in both disease-status groups, independence-based serial-positive calculations can make combined specificity too high and combined sensitivity too low. For serial-negative and parallel interpretation, they can make combined sensitivity too high and combined specificity too low. Gardner et al. (2000), <em>Conditional dependence between tests affects the diagnosis and surveillance of animal diseases</em>, describes these directional effects; see the reference below.</p><p>Test order is an input to explore, not a recommendation. Replace every example value with evidence appropriate to the intended setting.</p><p><strong>Illustrative pair labels:</strong></p><ul>")

                    if (strategy == "serial_positive") {
                        guidance_html <- paste0(guidance_html,
                            .("<li>HIV-style teaching pair (Ag/Ab assay \u2192 differentiation assay); this is not a complete HIV diagnostic algorithm</li><li>Cancer-screening teaching pair (Imaging \u2192 Tissue sampling)</li><li>Respiratory-infection teaching pair (Rapid antigen \u2192 Molecular assay)</li>")
                        )
                    } else if (strategy == "serial_negative") {
                        guidance_html <- paste0(guidance_html,
                            .("<li>Rule-out teaching pair (Initial assessment \u2192 Biomarker)</li><li>Exclusion teaching pair (Clinical score \u2192 Laboratory assay)</li>")
                        )
                    } else if (strategy == "parallel") {
                        guidance_html <- paste0(guidance_html,
                            .("<li>Emergency teaching pair (Physiologic test + Biomarker)</li><li>Acute-imaging teaching pair (Clinical assessment + Imaging)</li>")
                        )
                    }

                    guidance_html <- paste0(guidance_html, "</ul></div>")
                    self$results$clinical_guidance$setContent(guidance_html)
                }

                # Generate formulas HTML if requested
                if (self$options$show_formulas) {
                    formulas <- ""

                    formulas <- paste0(formulas, .("<h3>Mathematical Framework for Sequential Testing</h3>"))

                    # Basic probability definitions
                    formulas <- paste0(formulas, .("<h4>Key Definitions</h4>"))
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, .("<li><strong>Prevalence (P)</strong>: The pre-test probability of disease</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Sensitivity (Se)</strong>: Probability of a positive test in diseased subjects</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Specificity (Sp)</strong>: Probability of a negative test in non-diseased subjects</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Positive Likelihood Ratio (LR+)</strong>: Se/(1-Sp)</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Negative Likelihood Ratio (LR-)</strong>: (1-Se)/Sp</li>"))
                    formulas <- paste0(formulas, "</ul>")

                    # Pre-test to post-test probability conversion
                    formulas <- paste0(formulas, .("<h4>Probability Conversions</h4>"))
                    formulas <- paste0(formulas, .("<p>To calculate how a test changes probability:</p>"))

                    formulas <- paste0(formulas, "<ol>")
                    formulas <- paste0(formulas, .("<li><strong>Convert pre-test probability to odds</strong>: Odds = P/(1-P)</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Multiply odds by likelihood ratio</strong>: Post-test odds = Pre-test odds \u00D7 LR</li>"))
                    formulas <- paste0(formulas, .("<li><strong>Convert post-test odds back to probability</strong>: Post-test P = Odds/(1+Odds)</li>"))
                    formulas <- paste0(formulas, "</ol>")

                    # Strategy-specific formulas. Every product below is a probability within one
                    # disease-status group and holds only under conditional independence; say so
                    # once here, and again on each line that conditions on the other test.
                    formulas <- paste0(formulas, .("<p><strong>Assumption:</strong> combined sensitivity is computed among people with the disease and combined specificity among people without it. All products below assume the two tests are conditionally independent given disease status. If the tests are correlated within a disease-status group (for example, the same specimen or related biology), the true combined values differ.</p>"))
                    if (strategy == "serial_positive") {
                        formulas <- paste0(formulas, .("<h4>Serial Testing Strategy (Testing Positives)</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, the second test is only performed if the first test is positive. A subject is considered positive only if both tests are positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must test positive on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> \u00D7 Se<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Among people with the disease, given a positive Test 1, the probability that Test 2 is positive is assumed equal to Se<sub>2</sub> (conditional independence): {value}</li>"),
                            list(value = private$.formatProb(test2_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {sens1} \u00D7 {sens2} = {combined}</li>"),
                            list(
                                sens1 = private$.formatProb(test1_sens),
                                sens2 = private$.formatProb(test2_sens),
                                combined = private$.formatProb(combined_sens))))
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must either:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, .("<li>Test negative on Test 1, OR</li>"))
                        formulas <- paste0(formulas, .("<li>Test positive on Test 1 but negative on Test 2</li>"))
                        formulas <- paste0(formulas, "</ul>")
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> + (1-Sp<sub>1</sub>) \u00D7 Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of positive Test 1 followed by negative Test 2: (1 - {spec1}) \u00D7 {spec2} = {value}</li>"),
                            list(
                                spec1 = private$.formatProb(test1_spec),
                                spec2 = private$.formatProb(test2_spec),
                                value = private$.formatProb((1 - test1_spec) * test2_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {first} + {second} = {combined}</li>"),
                            list(
                                first = private$.formatProb(test1_spec),
                                second = private$.formatProb((1 - test1_spec) * test2_spec),
                                combined = private$.formatProb(combined_spec))))
                        formulas <- paste0(formulas, "</ul>")

                    } else if (strategy == "serial_negative") {
                        formulas <- paste0(formulas, .("<h4>Serial Testing Strategy (Testing Negatives)</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, the second test is only performed if the first test is negative. A subject is considered positive if either test is positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must either:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, .("<li>Test positive on Test 1, OR</li>"))
                        formulas <- paste0(formulas, .("<li>Test negative on Test 1 but positive on Test 2</li>"))
                        formulas <- paste0(formulas, "</ul>")
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> + (1-Se<sub>1</sub>) \u00D7 Se<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of negative Test 1 followed by positive Test 2: (1 - {sens1}) \u00D7 {sens2} = {value}</li>"),
                            list(
                                sens1 = private$.formatProb(test1_sens),
                                sens2 = private$.formatProb(test2_sens),
                                value = private$.formatProb((1 - test1_sens) * test2_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {first} + {second} = {combined}</li>"),
                            list(
                                first = private$.formatProb(test1_sens),
                                second = private$.formatProb((1 - test1_sens) * test2_sens),
                                combined = private$.formatProb(combined_sens))))
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> \u00D7 Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Among people without the disease, given a negative Test 1, the probability that Test 2 is negative is assumed equal to Sp<sub>2</sub> (conditional independence): {value}</li>"),
                            list(value = private$.formatProb(test2_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {spec1} \u00D7 {spec2} = {combined}</li>"),
                            list(
                                spec1 = private$.formatProb(test1_spec),
                                spec2 = private$.formatProb(test2_spec),
                                combined = private$.formatProb(combined_spec))))
                        formulas <- paste0(formulas, "</ul>")

                    } else if (strategy == "parallel") {
                        formulas <- paste0(formulas, .("<h4>Parallel Testing Strategy</h4>"))

                        # Explain the approach
                        formulas <- paste0(formulas, .("<p>In this strategy, both tests are performed on all subjects. A subject is considered positive if either test is positive.</p>"))

                        # Sensitivity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Sensitivity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test positive in this strategy, they must test positive on at least one test. This is calculated using the complement of the probability of testing negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = 1 - (1-Se<sub>1</sub>) \u00D7 (1-Se<sub>2</sub>)</p>"))
                        formulas <- paste0(formulas, .("<p>This can be rewritten as:</p>"))
                        formulas <- paste0(formulas, .("<p>Se<sub>combined</sub> = Se<sub>1</sub> + Se<sub>2</sub> - (Se<sub>1</sub> \u00D7 Se<sub>2</sub>)</p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing positive on Test 2: {value}</li>"),
                            list(value = private$.formatProb(test2_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Among people with the disease, probability of testing positive on both, assuming conditional independence: {sens1} \u00D7 {sens2} = {both}</li>"),
                            list(
                                sens1 = private$.formatProb(test1_sens),
                                sens2 = private$.formatProb(test2_sens),
                                both = private$.formatProb(test1_sens * test2_sens))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {sens1} + {sens2} - {both} = {combined}</li>"),
                            list(
                                sens1 = private$.formatProb(test1_sens),
                                sens2 = private$.formatProb(test2_sens),
                                both = private$.formatProb(test1_sens * test2_sens),
                                combined = private$.formatProb(combined_sens))))
                        formulas <- paste0(formulas, "</ul>")

                        # Specificity calculation
                        formulas <- paste0(formulas, .("<h5>Combined Specificity</h5>"))
                        formulas <- paste0(formulas, .("<p>For a subject to test negative in this strategy, they must test negative on both tests:</p>"))
                        formulas <- paste0(formulas, .("<p>Sp<sub>combined</sub> = Sp<sub>1</sub> \u00D7 Sp<sub>2</sub></p>"))
                        formulas <- paste0(formulas, .("<p>Probability calculation:</p>"))
                        formulas <- paste0(formulas, "<ul>")
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 1: {value}</li>"),
                            list(value = private$.formatProb(test1_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Probability of testing negative on Test 2: {value}</li>"),
                            list(value = private$.formatProb(test2_spec))))
                        formulas <- paste0(formulas, private$.formatTranslated(
                            .("<li>Combined probability = {spec1} \u00D7 {spec2} = {combined}</li>"),
                            list(
                                spec1 = private$.formatProb(test1_spec),
                                spec2 = private$.formatProb(test2_spec),
                                combined = private$.formatProb(combined_spec))))
                        formulas <- paste0(formulas, "</ul>")
                    }

                    # Predictive values calculation
                    formulas <- paste0(formulas, .("<h4>Predictive Values Calculations</h4>"))

                    # Positive Predictive Value
                    formulas <- paste0(formulas, .("<h5>Positive Predictive Value (PPV)</h5>"))
                    formulas <- paste0(formulas, .("<p>The probability that a positive test result is a true positive:</p>"))
                    formulas <- paste0(formulas, .("<p>PPV = (P \u00D7 Se) / (P \u00D7 Se + (1-P) \u00D7 (1-Sp))</p>"))

                    # Calculate intermediate values for clarity
                    ppv_numerator <- prevalence * combined_sens
                    ppv_denominator <- prevalence * combined_sens + (1-prevalence) * (1-combined_spec)

                    formulas <- paste0(formulas, .("<p>Calculation steps:</p>"))
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Prevalence (P) = {value}</li>"),
                        list(value = private$.formatProb(prevalence))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined sensitivity (Se) = {value}</li>"),
                        list(value = private$.formatProb(combined_sens))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined specificity (Sp) = {value}</li>"),
                        list(value = private$.formatProb(combined_spec))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Numerator = P \u00D7 Se = {p} \u00D7 {se} = {numerator}</li>"),
                        list(
                            p = private$.formatProb(prevalence),
                            se = private$.formatProb(combined_sens),
                            numerator = private$.formatProb(ppv_numerator))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Denominator = P \u00D7 Se + (1-P) \u00D7 (1-Sp) = {numerator} + {oneMinusP} \u00D7 {oneMinusSp} = {denominator}</li>"),
                        list(
                            numerator = private$.formatProb(ppv_numerator),
                            oneMinusP = private$.formatProb(1 - prevalence),
                            oneMinusSp = private$.formatProb(1 - combined_spec),
                            denominator = private$.formatProb(ppv_denominator))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>PPV = numerator/denominator = {numerator}/{denominator} = {ppv}</li>"),
                        list(
                            numerator = private$.formatProb(ppv_numerator),
                            denominator = private$.formatProb(ppv_denominator),
                            ppv = private$.formatProb(combined_ppv))))
                    formulas <- paste0(formulas, "</ul>")

                    # Negative Predictive Value
                    formulas <- paste0(formulas, .("<h5>Negative Predictive Value (NPV)</h5>"))
                    formulas <- paste0(formulas, .("<p>The probability that a negative test result is a true negative:</p>"))
                    formulas <- paste0(formulas, .("<p>NPV = ((1-P) \u00D7 Sp) / ((1-P) \u00D7 Sp + P \u00D7 (1-Se))</p>"))

                    # Calculate intermediate values for clarity
                    npv_numerator <- (1-prevalence) * combined_spec
                    npv_denominator <- (1-prevalence) * combined_spec + prevalence * (1-combined_sens)

                    formulas <- paste0(formulas, .("<p>Calculation steps:</p>"))
                    formulas <- paste0(formulas, "<ul>")
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Prevalence (P) = {value}</li>"),
                        list(value = private$.formatProb(prevalence))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined sensitivity (Se) = {value}</li>"),
                        list(value = private$.formatProb(combined_sens))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Combined specificity (Sp) = {value}</li>"),
                        list(value = private$.formatProb(combined_spec))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Numerator = (1-P) \u00D7 Sp = {oneMinusP} \u00D7 {sp} = {numerator}</li>"),
                        list(
                            oneMinusP = private$.formatProb(1 - prevalence),
                            sp = private$.formatProb(combined_spec),
                            numerator = private$.formatProb(npv_numerator))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>Denominator = (1-P) \u00D7 Sp + P \u00D7 (1-Se) = {numerator} + {p} \u00D7 {oneMinusSe} = {denominator}</li>"),
                        list(
                            numerator = private$.formatProb(npv_numerator),
                            p = private$.formatProb(prevalence),
                            oneMinusSe = private$.formatProb(1 - combined_sens),
                            denominator = private$.formatProb(npv_denominator))))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<li>NPV = numerator/denominator = {numerator}/{denominator} = {npv}</li>"),
                        list(
                            numerator = private$.formatProb(npv_numerator),
                            denominator = private$.formatProb(npv_denominator),
                            npv = private$.formatProb(combined_npv))))
                    formulas <- paste0(formulas, "</ul>")

                    # Likelihood ratios
                    formulas <- paste0(formulas, .("<h4>Likelihood Ratios</h4>"))

                    # Positive likelihood ratio
                    formulas <- paste0(formulas, .("<h5>Positive Likelihood Ratio (LR+)</h5>"))
                    formulas <- paste0(formulas, .("<p>How much more likely a positive test result is to occur in patients with disease compared to those without:</p>"))
                    formulas <- paste0(formulas, .("<p>LR+ = Sensitivity / (1 - Specificity)</p>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<p>LR+ = {se} / (1 - {sp}) = {lr}</p>"),
                        list(
                            se = private$.formatProb(combined_sens),
                            sp = private$.formatProb(combined_spec),
                            lr = private$.formatProb(combined_sens / (1 - combined_spec)))))

                    # Negative likelihood ratio
                    formulas <- paste0(formulas, .("<h5>Negative Likelihood Ratio (LR-)</h5>"))
                    formulas <- paste0(formulas, .("<p>How much more likely a negative test result is to occur in patients with disease compared to those without:</p>"))
                    formulas <- paste0(formulas, .("<p>LR- = (1 - Sensitivity) / Specificity</p>"))
                    formulas <- paste0(formulas, private$.formatTranslated(
                        .("<p>LR- = (1 - {se}) / {sp} = {lr}</p>"),
                        list(
                            se = private$.formatProb(combined_sens),
                            sp = private$.formatProb(combined_spec),
                            lr = private$.formatProb((1 - combined_sens) / combined_spec))))

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
                    content = private$.formatTranslated(
                        .('Sequential testing analysis completed: {strategy} strategy with prevalence {prevalence}, combined sensitivity {sensitivity}, combined specificity {specificity}.'),
                        list(strategy = strategy_name, prevalence = format_percent(prevalence),
                             sensitivity = format_percent(combined_sens),
                             specificity = format_percent(combined_spec)))
                )

                # Independence assumption notice
                private$.addNotice(
                    type = "INFO",
                    title = .("Independence Assumption"),
                    content = private$.formatTranslated(
                        .('Combined metrics assume conditional independence between tests. If tests have positive conditional dependence in both disease-status groups (for example, similar biology or technology), {direction}. Dependence among people with the disease alone biases the combined sensitivity but not the specificity, and dependence among people without it the reverse; PPV and NPV move with either.'),
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
                all_people_label <- private$.wrapLabel(private$.formatTranslated(
                    .("All People (n={n})"), list(n = plotData$Pop_Size)))
                positive_label <- .("Positive")
                negative_label <- .("Negative")
                positive_result_label <- private$.wrapLabel(.("Positive Result"))
                negative_result_label <- private$.wrapLabel(.("Negative Result"))
                # Boxes are one x-unit wide, so unwrapped names ("Differentiation Assay") ran
                # out of them and across the arrows.
                # A single word wider than the box ("Immunohistochemistry") still ran across the
                # arrows, so such a label is also drawn smaller (.labelSize).
                # ponytail: a name of 6+ lines still overflows vertically; size boxes to text if that shows up.
                test1_label <- private$.wrapLabel(plotData$Test1_Name)
                test2_label <- private$.wrapLabel(plotData$Test2_Name)
                test1_size <- private$.labelSize(test1_label)
                test2_size <- private$.labelSize(test2_label)
                # jamovi's themes set child elements explicitly (axis.text.x/.y and axis.line in
                # "default", panel.grid.major/minor in "minimal"), so blanking only the parents
                # left coordinate axes and grid lines on this schematic.
                schematic_theme <- ggplot2::theme(
                    axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                    axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank())

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
                        ggplot2::annotate("text", x = 3, y = 5, label = test1_label,
                                        size = test1_size, fontface = "bold") +

                        # Positive branch
                        ggplot2::annotate("segment", x = 3.5, y = 5.2, xend = 4.5, yend = 6,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 4, y = 5.6, label = positive_label,
                                        size = 3, color = "#D55E00", angle = 30) +
                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 5.5, ymax = 6.5,
                                        fill = "#FBE6B5", color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 6, label = test2_label,
                                        size = test2_size, fontface = "bold") +

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
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 6.05, ymax = 7,
                                        fill = "#F4D7CF", color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 6.5, label = positive_result_label,
                                        size = 3.5, fontface = "bold", color = "#D55E00") +

                        ggplot2::annotate("segment", x = 5.5, y = 5.8, xend = 6.5, yend = 5.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 5, ymax = 5.95,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 5.5, label = negative_result_label,
                                        size = 3.5, fontface = "bold", color = "#0072B2") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7.5) +
                        ggplot2::labs(title = .("Serial Testing Strategy: Test Positives"),
                                    subtitle = .("Second test only for those positive on first test")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
                        schematic_theme

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
                        ggplot2::annotate("text", x = 3, y = 5, label = test1_label,
                                        size = test1_size, fontface = "bold") +

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
                        ggplot2::annotate("text", x = 5, y = 4, label = test2_label,
                                        size = test2_size, fontface = "bold") +

                        # Final results from second test
                        ggplot2::annotate("segment", x = 5.5, y = 4.2, xend = 6.5, yend = 4.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4.05, ymax = 5,
                                        fill = "#F4D7CF", color = "#D55E00", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 4.5, label = positive_result_label,
                                        size = 3.5, fontface = "bold", color = "#D55E00") +

                        ggplot2::annotate("segment", x = 5.5, y = 3.8, xend = 6.5, yend = 3.5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 3, ymax = 3.95,
                                        fill = "#DCEAF7", color = "#0072B2", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 3.5, label = negative_result_label,
                                        size = 3.5, fontface = "bold", color = "#0072B2") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(2.5, 7) +
                        ggplot2::labs(title = .("Serial Testing Strategy: Test Negatives"),
                                    subtitle = .("Second test only for those negative on first test")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
                        schematic_theme

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
                        ggplot2::annotate("text", x = 3, y = 6, label = test1_label,
                                        size = test1_size, fontface = "bold") +

                        # Test 2
                        ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 3.5, ymax = 4.5,
                                        fill = "#FBE6B5", color = "#E69F00", linewidth = 1) +
                        ggplot2::annotate("text", x = 3, y = 4, label = test2_label,
                                        size = test2_size, fontface = "bold") +

                        # Combine results
                        ggplot2::annotate("segment", x = 3.5, y = 6, xend = 4.5, yend = 5.2,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("segment", x = 3.5, y = 4, xend = 4.5, yend = 4.8,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +

                        ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 4.5, ymax = 5.5,
                                        fill = "#F1DEEB", color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("text", x = 5, y = 5, label = private$.wrapLabel(.("Combine Results")),
                                        size = 4, fontface = "bold") +

                        # Final result
                        ggplot2::annotate("segment", x = 5.5, y = 5, xend = 6.5, yend = 5,
                                        arrow = grid::arrow(length = grid::unit(0.3, "cm")),
                                        color = "#CC79A7", linewidth = 1) +
                        ggplot2::annotate("text", x = 6, y = 5.3, label = .("Either +"),
                                        size = 3, color = "#CC79A7") +

                        ggplot2::annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 4.5, ymax = 5.5,
                                        fill = "white", color = "black", linewidth = 1) +
                        ggplot2::annotate("text", x = 7, y = 5, label = private$.wrapLabel(.("Final Result")),
                                        size = 4, fontface = "bold") +

                        ggplot2::xlim(0, 8) + ggplot2::ylim(3, 7) +
                        ggplot2::labs(title = .("Parallel Testing Strategy"),
                                    subtitle = .("Both tests performed on everyone")) +
                        ggplot2::theme_minimal() +
                        ggtheme +
                        schematic_theme
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
                        private$.ppvOf(plotData$Prevalence, plotData$Test1_Sens, plotData$Test1_Spec) * 100,
                        private$.ppvOf(plotData$Prevalence, plotData$Test2_Sens, plotData$Test2_Spec) * 100,
                        plotData$Combined_PPV * 100,
                        private$.npvOf(plotData$Prevalence, plotData$Test1_Sens, plotData$Test1_Spec) * 100,
                        private$.npvOf(plotData$Prevalence, plotData$Test2_Sens, plotData$Test2_Spec) * 100,
                        plotData$Combined_NPV * 100
                    )
                )

                # Sensitivity, Specificity, PPV, NPV - not alphabetical facets.
                perf_data$Metric <- factor(perf_data$Metric, levels = metric_labels)
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
                    ggplot2::geom_text(ggplot2::aes(label = vapply(Value / 100, private$.formatPercent, "")),
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
                    # vjust = 1 is required as well as hjust: jamovi's ggtheme drops vjust,
                    # so rotated (and often long) test names ran into the axis title.
                    # No legend: the x axis already names each bar, and the legend squeezed the
                    # panels until the value labels ran into the facet strips.
                    ggplot2::theme(legend.position = "none",
                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)) +
                    ggplot2::scale_x_discrete(labels = test_labels) +
                    ggplot2::scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
                    # A positional ylim() applies ONE limit to every panel, which cancelled the
                    # facet's scales = "free_y" and squashed the PPV panel at low prevalence.
                    # Headroom for the value labels comes from the upper expansion instead.
                    ggplot2::scale_y_continuous(
                        expand = ggplot2::expansion(mult = c(0, 0.15)))

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
                test2_plr <- plotData$Test2_Sens / (1 - plotData$Test2_Spec)
                post_test2_neg_odds <- post_test1_neg_odds * test2_nlr
                post_test2_neg_prob <- post_test2_neg_odds / (1 + post_test2_neg_odds)
                # The subgroup that is retested and then crosses over: T1+/T2- under
                # serial_positive, T1-/T2+ under serial_negative. It shares the FINAL class with
                # the branch that stopped at test 1, and it is the higher-risk half of it.
                cross_pos_neg_odds <- post_test1_pos_odds * test2_nlr
                cross_pos_neg_prob <- cross_pos_neg_odds / (1 + cross_pos_neg_odds)
                cross_neg_pos_odds <- post_test1_neg_odds * test2_plr
                cross_neg_pos_prob <- cross_neg_pos_odds / (1 + cross_neg_pos_odds)
                positive_path <- .("Positive Path")
                negative_path <- .("Negative Path")

                # Create data for plotting based on strategy.
                # A branch's line may only span the steps its own subjects go through. The
                # combined PPV/NPV is a mixture over BOTH branches, so using it as the endpoint
                # of the branch that LEAVES the pathway after test 1 drew a probability change
                # for a test those subjects never received: on the defaults the negative path
                # was drawn 10.0% -> 0.79% -> 2.61%, and on the serial-negative teaching example
                # the positive path 15.0% -> 32.0% -> 29.9%. Each branch now stops where its
                # subjects stop. `step_labels` replaces the hardcoded three-label x axis, which
                # was per-plot wrong (it read "After Test 1" over the combined parallel PPV) and
                # made the dropped `Step` column dead data.
                #
                # Stopping each branch where its subjects stop leaves a SECOND discrepancy that
                # the caption below has to own: the truncated branch is only PART of its final
                # class, so its endpoint is not the pooled figure the tables report. On the
                # teaching example (prev 20%, both tests 60/70) serial_positive's final-negative
                # class is {T1-} at P(D) = 12.50% with weight 0.7477 plus {T1+,T2-} at 22.22%
                # with weight 0.2523, which mix to the tabled 1 - NPV = 14.95%; the retested
                # crossover subgroup is 21.6% of the cohort and is drawn nowhere. serial_negative
                # is the mirror image: {T1+} at 33.33% (weight 0.625) plus {T1-,T2+} at 22.22%
                # (weight 0.375) mix to the tabled PPV = 29.17%. Shipping the plotted number and
                # the tabled number in the same pane without saying they answer different
                # questions is what made this a defect; the plotted values are correct for the
                # subgroup they describe, so the caption names the subgroup rather than the plot
                # being bent to match the table.
                if (plotData$Strategy == "serial_positive") {
                    # Only test-1 positives receive test 2; test-1 negatives stop at test 1.
                    prob_data <- data.frame(
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, post_test1_neg_prob * 100),
                        Path = c(rep(positive_path, 3), rep(negative_path, 2)),
                        x = c(1, 2, 3, 1, 2)
                    )
                    step_labels <- c(.("Pre-test"), .("After Test 1"), .("After Test 2 (+)"))
                    truncation_note <- sprintf(
                        .("%1$s ends at the first-test-negative subgroup: P(disease | Test 1 negative) = %2$s. That is not the pooled final-negative figure in the tables, 1 - combined NPV = %3$s, which also averages in the Test 1 positive / Test 2 negative subgroup (P(disease) = %4$s). Those subjects are classified negative but are not drawn here."),
                        negative_path, private$.formatPercent(post_test1_neg_prob),
                        private$.formatPercent(1 - plotData$Combined_NPV),
                        private$.formatPercent(cross_pos_neg_prob))
                } else if (plotData$Strategy == "serial_negative") {
                    # Only test-1 negatives receive test 2; test-1 positives stop at test 1.
                    prob_data <- data.frame(
                        Probability = c(prevalence * 100, post_test1_pos_prob * 100,
                                      prevalence * 100, post_test1_neg_prob * 100, post_test2_neg_prob * 100),
                        Path = c(rep(positive_path, 2), rep(negative_path, 3)),
                        x = c(1, 2, 1, 2, 3)
                    )
                    step_labels <- c(.("Pre-test"), .("After Test 1"), .("After Test 2 (-)"))
                    truncation_note <- sprintf(
                        .("%1$s ends at the first-test-positive subgroup: P(disease | Test 1 positive) = %2$s. That is not the pooled final-positive figure in the tables, combined PPV = %3$s, which also averages in the Test 1 negative / Test 2 positive subgroup (P(disease) = %4$s). Those subjects are classified positive but are not drawn here."),
                        positive_path, private$.formatPercent(post_test1_pos_prob),
                        private$.formatPercent(plotData$Combined_PPV),
                        private$.formatPercent(cross_neg_pos_prob))
                } else {
                    # Parallel testing: both tests are applied at once, so there is no
                    # intermediate step. The third point merely repeated the second.
                    prob_data <- data.frame(
                        Probability = c(prevalence * 100, plotData$Combined_PPV * 100,
                                      prevalence * 100, (1 - plotData$Combined_NPV) * 100),
                        Path = c(rep(positive_path, 2), rep(negative_path, 2)),
                        x = c(1, 2, 1, 2)
                    )
                    step_labels <- c(.("Pre-test"), .("After Both Tests"))
                    # Nothing is truncated: both endpoints ARE the pooled tabled values. (The
                    # positive one is still a mixture of {T1+,T2+}, {T1+,T2-} and {T1-,T2+}, but
                    # the plot shows the mixture, not one of its parts, so nothing contradicts.)
                    truncation_note <- NULL
                }
                if (!is.null(truncation_note))
                    truncation_note <- paste(strwrap(truncation_note, width = 95), collapse = "\n")
                # ylim(0, 1.2 * max) ran the HIV example's axis to 117% under the title "Disease
                # Probability (%)". Cap it at 100 and put a near-top label under its point instead.
                y_top <- min(100, max(prob_data$Probability) * 1.2)

                prob_plot <- ggplot2::ggplot(
                    prob_data,
                    ggplot2::aes(
                        x = x, y = Probability, color = Path,
                        linetype = Path, group = Path
                    )
                ) +
                    ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
                    ggplot2::geom_point(size = 4) +
                    # show.legend = FALSE: otherwise an "a" glyph is drawn under each legend key.
                    ggplot2::geom_text(ggplot2::aes(label = vapply(Probability / 100, private$.formatPercent, ""),
                                                    vjust = ifelse(Probability > 0.85 * y_top, 2, -1.5)),
                                      hjust = 0.5, size = 3, show.legend = FALSE) +
                    ggplot2::labs(
                        title = .("Probability Transformation Through Testing"),
                        subtitle = private$.formatTranslated(
                            .("Strategy: {strategy} | Prevalence: {prevalence}"),
                            list(
                                strategy = if (is.null(plotData$StrategyLabel)) plotData$Strategy else plotData$StrategyLabel,
                                prevalence = sprintf("%.1f%%", plotData$Prevalence * 100))),
                        x = .("Testing Stage"), y = .("Disease Probability (%)"),
                        color = "", linetype = "",
                        caption = truncation_note
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme +
                    # Scales go AFTER ggtheme: jamovi's global theme carries its own discrete
                    # colour scale and silently replaced the colourblind-safe pair below when
                    # they were added first.
                    # The default 5% expansion clipped the last tick label ("After Test 2 (+").
                    ggplot2::scale_x_continuous(
                        breaks = seq_along(step_labels),
                        labels = step_labels,
                        expand = ggplot2::expansion(add = 0.4)
                    ) +
                    ggplot2::scale_color_manual(
                        values = stats::setNames(
                            c("#D55E00", "#0072B2"), c(positive_path, negative_path))) +
                    ggplot2::scale_linetype_manual(
                        values = stats::setNames(
                            c("solid", "dashed"), c(positive_path, negative_path))) +
                    # Left-aligned and smaller: the caption is a sentence, not a credit line,
                    # and ggplot's default right-aligns it at the body font size.
                    ggplot2::theme(legend.position = "bottom",
                                   plot.caption = ggplot2::element_text(hjust = 0, size = 8)) +
                    ggplot2::scale_y_continuous(limits = c(0, y_top), expand = ggplot2::expansion(0)) +
                    ggplot2::coord_cartesian(clip = "off")

                print(prob_plot)
                return(TRUE)
            },

            .plot_population_flow = function(image, ggtheme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(TRUE)

                plotData <- image$state

                if (is.null(plotData))
                    return(FALSE)

                # Schematic flow: box sizes are fixed, not proportional to the counts.
                pop_size <- plotData$Pop_Size
                diseased <- plotData$Diseased
                healthy <- plotData$Healthy

                flow_plot <- ggplot2::ggplot() +
                    # Initial population
                    ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 25, ymax = 75,
                                    fill = "lightblue", alpha = 0.5) +
                    ggplot2::annotate("text", x = 1, y = 50, label = private$.wrapLabel(private$.formatTranslated(
                                        .("Total (n={n})"), list(n = pop_size)), 8),
                                    size = 5, fontface = "bold") +

                    # Disease status split
                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 55, ymax = 85,
                                    fill = "#F4D7CF", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 70, label = private$.wrapLabel(private$.formatTranslated(
                                        .("Disease+ (n={n})"), list(n = private$.formatCount(diseased))), 8),
                                    size = 4, fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 15, ymax = 45,
                                    fill = "#DCEAF7", alpha = 0.5) +
                    ggplot2::annotate("text", x = 3, y = 30, label = private$.wrapLabel(private$.formatTranslated(
                                        .("Disease- (n={n})"), list(n = private$.formatCount(healthy))), 8),
                                    size = 4, fontface = "bold") +

                    # Test results
                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 75, ymax = 90,
                                    fill = "#0072B2", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 82.5, label = private$.wrapLabel(private$.formatTranslated(
                                        .("TP (n={n})"), list(n = private$.formatCount(plotData$Final_TP))), 8),
                                    size = 3.5, color = "black", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 60, ymax = 75,
                                    fill = "#D55E00", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 67.5, label = private$.wrapLabel(private$.formatTranslated(
                                        .("FN (n={n})"), list(n = private$.formatCount(plotData$Final_FN))), 8),
                                    size = 3.5, color = "black", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 30, ymax = 45,
                                    fill = "#E69F00", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 37.5, label = private$.wrapLabel(private$.formatTranslated(
                                        .("FP (n={n})"), list(n = private$.formatCount(plotData$Final_FP))), 8),
                                    size = 3.5, color = "black", fontface = "bold") +

                    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 10, ymax = 30,
                                    fill = "#0072B2", alpha = 0.7) +
                    ggplot2::annotate("text", x = 5, y = 20, label = private$.wrapLabel(private$.formatTranslated(
                                        .("TN (n={n})"), list(n = private$.formatCount(plotData$Final_TN))), 8),
                                    size = 3.5, color = "black", fontface = "bold") +

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
                                sensitivity = private$.formatPercent(plotData$Combined_Sens),
                                specificity = private$.formatPercent(plotData$Combined_Spec)))
                    ) +
                    ggplot2::theme_void() +
                    ggtheme +
                    # Child elements too: jamovi's themes set them explicitly (see the flow diagram).
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.line = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
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

                # PPV and NPV across the prevalence range. No denominator can be zero: prevalence
                # stays inside (0, 1) and combined sensitivity/specificity within 1e-4 of the ends.
                se <- plotData$Combined_Sens
                sp <- plotData$Combined_Spec
                ppv_curve <- private$.ppvOf(prev_range, se, sp)
                npv_curve <- private$.npvOf(prev_range, se, sp)

                # Create data frame for plotting
                ppv_label <- .("PPV (Positive Predictive Value)")
                npv_label <- .("NPV (Negative Predictive Value)")
                df <- data.frame(
                    Prevalence = rep(prev_range, 2),
                    Value = c(ppv_curve, npv_curve),
                    Metric = rep(c(ppv_label, npv_label), each = length(prev_range))
                )

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
                                     label = private$.wrapLabel(private$.formatTranslated(
                                         .("Your prevalence ({value})"),
                                         list(value = sprintf("%.1f%%", plotData$Prevalence * 100))), 16),
                                     size = 3, hjust = ifelse(plotData$Prevalence > 0.5, 1.1, -0.1)) +
                    ggplot2::labs(
                        title = .("Sensitivity Analysis: How Prevalence Affects Predictive Values"),
                        subtitle = private$.formatTranslated(
                            .("Combined Test Performance: Sensitivity={sensitivity}, Specificity={specificity}"),
                            list(sensitivity = private$.formatPercent(plotData$Combined_Sens),
                                 specificity = private$.formatPercent(plotData$Combined_Spec))),
                        x = .("Disease Prevalence"),
                        y = .("Probability"),
                        color = "",
                        linetype = ""
                    ) +
                    ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100),
                                               limits = c(0, 1)) +
                    ggplot2::scale_x_continuous(labels = function(x) sprintf("%.0f%%", x * 100)) +
                    ggplot2::theme_minimal() +
                    ggtheme +
                    # Scales AFTER ggtheme (see .plot_probability): the global theme's own
                    # discrete colour scale otherwise replaces these.
                    ggplot2::scale_color_manual(values = stats::setNames(
                        c("#D55E00", "#0072B2"), c(ppv_label, npv_label))) +
                    ggplot2::scale_linetype_manual(values = stats::setNames(
                        c("solid", "dashed"), c(ppv_label, npv_label))) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        legend.text = ggplot2::element_text(size = 10),
                        plot.title = ggplot2::element_text(face = "bold", size = 12),
                        plot.subtitle = ggplot2::element_text(size = 10, color = "gray40")
                    )

                print(sens_plot)
                return(TRUE)
            },

            # Teaching examples. This table MUST match SEQUENTIAL_PRESET_CONFIGS in
            # jamovi/js/sequentialtests.events.js -- the JavaScript applies examples in the GUI,
            # this applies them for callers from R, and a regression test compares the two.
            .getPresetValues = function(preset) {
                presets <- list(
                    covid_screening_confirmation = list(
                        title = .('Respiratory Testing (Illustrative)'),
                        test1_name = .('Rapid Antigen Test'), test1_sens = 0.75, test1_spec = 0.95,
                        test2_name = .('RT-PCR'),             test2_sens = 0.95, test2_spec = 0.99,
                        prevalence = 0.08, strategy = 'serial_positive'),
                    breast_cancer_screening = list(
                        title = .('Breast Imaging + Tissue Sampling (Illustrative)'),
                        test1_name = .('Mammography'),        test1_sens = 0.85, test1_spec = 0.90,
                        test2_name = .('Tissue Biopsy'),      test2_sens = 0.98, test2_spec = 0.99,
                        prevalence = 0.06, strategy = 'serial_positive'),
                    mi_emergency_parallel = list(
                        title = .('Biomarker + ECG (Illustrative)'),
                        test1_name = .('Troponin'),           test1_sens = 0.90, test1_spec = 0.95,
                        test2_name = .('ECG'),                test2_sens = 0.70, test2_spec = 0.90,
                        prevalence = 0.20, strategy = 'parallel'),
                    tb_screening_confirmation = list(
                        title = .('Chest Imaging + Culture (Illustrative)'),
                        test1_name = .('Chest X-ray'),        test1_sens = 0.75, test1_spec = 0.80,
                        test2_name = .('Sputum Culture'),     test2_sens = 0.85, test2_spec = 0.98,
                        prevalence = 0.12, strategy = 'serial_positive'),
                    prostate_screening_exclusion = list(
                        title = .('PSA + MRI (Illustrative)'),
                        test1_name = .('PSA Test'),           test1_sens = 0.80, test1_spec = 0.70,
                        test2_name = .('MRI'),                test2_sens = 0.90, test2_spec = 0.85,
                        prevalence = 0.15, strategy = 'serial_negative'),
                    hiv_screening_confirmation = list(
                        title = .('HIV Ag/Ab + Differentiation Assay (Illustrative)'),
                        test1_name = .('HIV Ag/Ab Assay'),    test1_sens = 0.98, test1_spec = 0.95,
                        test2_name = .('Differentiation Assay'), test2_sens = 0.99, test2_spec = 0.99,
                        prevalence = 0.02, strategy = 'serial_positive'),
                    stroke_emergency_parallel = list(
                        title = .('Clinical Assessment + CT (Illustrative)'),
                        test1_name = .('Clinical Assessment'), test1_sens = 0.85, test1_spec = 0.75,
                        test2_name = .('CT Scan'),             test2_sens = 0.95, test2_spec = 0.98,
                        prevalence = 0.25, strategy = 'parallel')
                )
                if (!preset %in% names(presets))
                    return(NULL)
                presets[[preset]]
            },

            # Notice collection helpers. A single Preformatted (plain-text) output item, so the
            # whole set renders with no HTML (project convention: notice content must be plain
            # text). NOTE (corrected 2026-09-23): jmvcore::Notice itself serializes fine. The
            # "attempt to apply non-function" crash once blamed on it came from
            # self$results$insert(999, ...) -- Group$insert() has no bounds check, so it pads the
            # results tree with NULLs and the next traversal calls NULL$asProtoBuf(). An Html
            # element at the same index fails identically. Use $add(), never insert(999, ...).
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
                    # The ": " separator lives here, not inside the msgid: a translator
                    # cannot see trailing whitespace, and a catalog that drops it silently
                    # runs the label into the title.
                    label <- switch(notice$type,
                        ERROR          = .("Error"),
                        STRONG_WARNING = .("Strong warning"),
                        WARNING        = .("Warning"),
                        "")
                    prefix <- if (nzchar(label)) paste0(label, ": ") else ""
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

            # One decimal, except where one decimal would print an imperfect value as 0.0% or
            # 100.0%: serial-positive specificity reaches 0.9999, and PPV/NPV come arbitrarily
            # close to 1 at prevalence 0.999. Then two decimals, and an inequality if even those
            # round to an endpoint. Decided on the printed string, not a threshold, so float noise
            # at the boundary (0.95 + 0.05 * 0.99) cannot slip through.
            .formatPercent = function(value) {
                if (is.na(value))
                    return(.("not defined"))
                out <- sprintf("%.1f%%", value * 100)
                if (private$.roundsToBound(value)) {
                    out <- sprintf("%.2f%%", value * 100)
                    if (out == "100.00%") out <- ">99.99%"
                    if (out == "0.00%") out <- "<0.01%"
                }
                out
            },

            # An interior proportion that one decimal place prints as 0.0% or 100.0%.
            .roundsToBound = function(value)
                isTRUE(is.numeric(value) && value > 0 && value < 1 &&
                       sprintf("%.1f%%", value * 100) %in% c("0.0%", "100.0%")),

            # The jamovi client prints a `pc` column as (100 * x).toFixed(1) at its default number
            # format, and no column option changes that, so the COVID and HIV examples' combined
            # specificity 0.9995 read "100.0 %" beside sentences that said 99.95%. Call after
            # setRow(): setRow() clears a cell's footnotes, so none survives into a later run.
            # ponytail: assumes the default number format; at 2 significant figures jamovi shows
            # whole percentages and a value from 99.5% up already reads "100 %".
            .footnoteRoundedPercents = function(table, cols) {
                for (row in seq_len(table$rowCount)) for (col in cols) {
                    value <- table$getCell(rowNo = row, col = col)$value
                    if (private$.roundsToBound(value))
                        table$addFootnote(rowNo = row, col = col, note = private$.formatTranslated(
                            .("Rounded for display: the value is {value}, not {bound}."),
                            list(value = private$.formatPercent(value),
                                 bound = if (value > 0.5) "100%" else "0%")))
                }
            },

            # "N of every 100" for a proportion. %.0f printed the HIV example's specificity
            # .9995 as "rules out 100 of every 100"; an interior value never reaches 0 or 100.
            .formatPer100 = function(value) {
                n <- round(value * 100)
                if (value < 1 && n >= 100) return(.("more than 99"))
                if (value > 0 && n <= 0) return(.("fewer than 1"))
                sprintf("%.0f", n)
            },

            # Four significant digits for the formulas panel, where format() printed a PPV of
            # 0.9999999 (prevalence 0.999) as "= 1": then as many digits as it takes to differ
            # from 1. scientific = FALSE: this is a teaching panel, and "0.01 x 0.01 = 1e-04" is
            # not how a pathologist reads 0.0001.
            .formatProb = function(value) {
                out <- base::format(value, digits = 4, scientific = FALSE)
                if (value < 1 && out == "1")
                    out <- base::format(value, digits = min(15, ceiling(-log10(1 - value)) + 1),
                                        scientific = FALSE)
                out
            },

            # Expected counts in the explanation. "%.0f" printed 0.1 expected diseased people (N 100,
            # prevalence 0.001) as "Among 0 expected people with disease, 0 are correctly identified".
            .formatCount = function(value) {
                if (value >= 10) sprintf("%.0f", value)
                else trimws(formatC(value, digits = 2, format = "fg"))  # 9.96 comes back " 10"
            },

            # The one Bayes formula: .run() and both plots that show predictive values use it.
            .ppvOf = function(prev, se, sp) prev * se / (prev * se + (1 - prev) * (1 - sp)),
            .npvOf = function(prev, se, sp) (1 - prev) * sp / ((1 - prev) * sp + prev * (1 - se)),

            # Plot labels are single-line msgids (a "\n" inside .() never reaches the translation
            # catalog); the line breaks are added here, after translation.
            .wrapLabel = function(x, width = 12) paste(strwrap(x, width), collapse = "\n"),

            # About 15 characters of the size-3.2 bold label fill a one-unit flow-diagram box at the
            # default 600 px, edge to edge. A line longer than .wrapLabel()'s 12 (one word it could
            # not break) is drawn proportionally smaller, so it keeps a margin inside the box.
            .labelSize = function(label, size = 3.2)
                size * min(1, 12 / max(nchar(strsplit(label, "\n", fixed = TRUE)[[1]]))),

            # HTML sanitization for security (used by Html output items: plain_summary,
            # explanation_text, clinical_guidance - NOT the plain-text notices sink)
            .safeHtmlOutput = function(text) {
                if (is.null(text) || length(text) == 0) return("")
                text <- as.character(text)
                # Names land in text nodes, never in attributes, so & < > " are all that need
                # escaping; the slash in "HIV Ag/Ab" used to reach the page as a numeric entity.
                text <- gsub("&", "&amp;", text, fixed = TRUE)
                text <- gsub("<", "&lt;", text, fixed = TRUE)
                text <- gsub(">", "&gt;", text, fixed = TRUE)
                gsub("\"", "&quot;", text, fixed = TRUE)
            }





        )
    )
