#' @title Co-Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'
#' @return An \code{R6} class generator object for the \code{cotestClass} backend; used internally by the jamovi analysis wrapper and not called directly.

cotestClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "cotestClass",
        inherit = cotestBase,
        private = list(
            # Constants for better maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,
            NUMERICAL_TOLERANCE = 1e-10,

            # Cache for expensive nomogram calculations
            .nomogramCache = NULL,
            .lastNomogramParams = NULL,
            .notices = NULL,
            # Translation coverage of the long educational HTML remains incomplete; keep
            # statistical formulas and notices covered by test-cotest-release-review.R while
            # migrating prose as complete messages rather than sentence fragments.
            .init = function() {
                # Add welcome instructions
                instructions <- '
<div style="max-width: 900px; font-family: sans-serif;">
<h3>Welcome to Co-Testing Analysis</h3>
<p><strong>Purpose:</strong> This analysis evaluates the combined diagnostic performance of two tests applied <strong>in parallel</strong>, accounting for possible dependence between them.</p>

<p><strong>What "in parallel" means here:</strong> both tests are performed on the same subject at the same time, before either result is known, and the two results are then combined into a single conclusion. Neither test gates the other: Test 2 is run whether or not Test 1 came back positive. That is what separates parallel co-testing from <em>sequential</em> (serial) testing, where a second test is ordered only after a particular first result, and where the second test\'s operating characteristics apply to the selected subgroup rather than to everyone. If your tests are run one after the other with the second conditional on the first, this is the wrong analysis \u{2014} use <em>Sequential Test Analysis</em> instead.</p>

<p><strong>Choosing between this and Sequential Test Analysis for parallel testing.</strong> Sequential Test Analysis also has a parallel strategy, and for <em>conditionally independent</em> tests the two agree. It computes its combined figures under that independence assumption and warns you in prose when dependence would matter. This analysis is the one that actually models the dependence: if the two tests share a specimen, a modality, an observer or a biological pathway, use this one and enter a dependence parameter.</p>

<p>Because both tests are always performed, each subject falls into exactly one of four result combinations \u{2014} both positive, Test 1 only, Test 2 only, both negative \u{2014} and the table below gives the post-test probability of disease for each, plus the combined <em>either positive</em> arm used by the parallel decision rule.</p>

<h4>Quick Start Guide</h4>
<ol>
<li><strong>Choose a worked example</strong> (optional): HPV+Pap, PSA+DRE, Troponin+ECG and the rest fill in every parameter for you so you can see the calculation end to end. <strong>They are illustrations, not clinical inputs</strong> \u{2014} see the warning below.</li>
<li><strong>Enter your own test parameters</strong>: with <em>Custom values</em> selected, input sensitivity and specificity for each test. While a worked example is selected these boxes are locked, because the example supplies them.</li>
<li><strong>Set disease prevalence</strong>: the pre-test probability of disease in the population you are actually testing \u{2014} not the figure quoted in the paper the test was validated in.</li>
<li><strong>Configure test independence</strong>: state whether the two tests are conditionally independent or dependent (see "Understanding Test Dependence" below). This choice moves the answer more than any other.</li>
<li><strong>Review Results</strong>: The analysis provides post-test probabilities for all test combination outcomes, including the critical <em>Either Test Positive (Parallel Rule)</em> used in clinical co-testing algorithms.</li>
</ol>

<h4>Key Clinical Scenarios</h4>
<ul>
<li><strong>Either Test Positive (Parallel Rule)</strong>: At least one test is positive. This combination rule raises sensitivity relative to either test used alone, at the cost of specificity, so the complementary arm carries the rule-out information. The <em>Either Test Positive</em> row is the probability of disease given <em>at least one</em> positive result, so it pools single-positive and double-positive cases; when only one test is positive read the single-positive row for that test instead (labelled <em>Test 1 Positive Only</em> / <em>Test 2 Positive Only</em>, or by the names you give the two tests). How far a result moves the probability is set by its likelihood ratio: the positive and negative likelihood ratios of each test on its own are listed in the <em>Test Parameters</em> table, while the combined effect of a scenario is reported as its <em>Post-test Probability</em> and <em>Post-test Odds</em> in the <em>Co-Testing Results</em> table rather than as a separate combined likelihood ratio.</li>
<li><strong>Both Tests Positive</strong>: Both tests are positive. The resulting post-test probability depends on the pre-test prevalence, on the positive likelihood ratios of both tests (listed in the <em>Test Parameters</em> table) and on any conditional dependence you specify. Read the resulting figure in the <em>Post-test Probability</em> column of the <em>Both Tests Positive</em> row.</li>
<li><strong>Both Tests Negative</strong>: Neither test is positive. This is the arm the parallel rule is designed to act on. The <em>Both Tests Negative</em> row gives the resulting <em>Post-test Probability</em>; its <em>Relative to Prevalence</em> entry is that probability divided by the prevalence you entered, so a value of 0.20 means two negatives cut the probability of disease to one fifth of where it started. The combined negative likelihood ratio behind that shift is not printed as its own number.</li>
<li><strong>Single Positive</strong>: Only one of the two tests is positive. This result is driven by the positive likelihood ratio of one test together with the negative likelihood ratio of the other; both are listed in the <em>Test Parameters</em> table. The resulting probability is in the corresponding single-positive row of the <em>Co-Testing Results</em> table, and it can sit well below the <em>Both Tests Positive</em> row because the negative test pulls the probability back down.</li>
</ul>

<h4>Worked examples included</h4>
<ul>
<li><strong>HPV + Pap cytology</strong>: sensitive test plus specific test, same specimen (dependent)</li>
<li><strong>PSA + digital rectal examination</strong>: the one independent example (biochemical plus physical)</li>
<li><strong>Troponin + ECG</strong>: both driven by extent of injury (dependent)</li>
<li><strong>Mammography + ultrasound</strong>: two views of the same tissue, strongest dependence (dependent)</li>
<li><strong>Rapid antigen + PCR</strong>: two assays, one swab, both track viral load (dependent)</li>
<li><strong>Chest radiograph + sputum microscopy</strong>: imaging plus microbiology (dependent)</li>
</ul>

<div style="background-color: rgba(220, 53, 69, 0.12); border-left: 3px solid rgba(220, 53, 69, 0.85); border-radius: 3px; padding: 10px 14px; margin: 12px 0; color: inherit;">
<p style="margin: 0;"><strong>The worked examples are for demonstration only.</strong> Every sensitivity, specificity, prevalence and dependence value in them is a round illustrative number picked to show how the calculation behaves \u{2014} how a sensitive test pairs with a specific one, how dependence pulls the answer back. They are <strong>not</strong> pooled estimates from the literature, they are <strong>not</strong> drawn from any guideline, and they have <strong>not</strong> been checked for clinical accuracy. Do not quote them, and do not make any clinical decision from a result computed with them. For real work select <em>Custom values</em> and enter sensitivity and specificity from a validation study in a population like yours, together with your own local prevalence.</p>
</div>

<p><strong>No uncertainty is propagated:</strong> sensitivity, specificity and prevalence are treated as exact. Every probability reported here is conditional on those numbers being right, and carries no confidence interval \u{2014} published test performance and local prevalence both vary considerably.</p>

<p><strong>Tip:</strong> Enable "Detailed footnotes" for explanations of each metric. Enable "Fagan nomogram" for a visual of the probability update \u{2014} note that the nomogram plots the <em>parallel rule</em> (positive if either test is positive, negative only if both are negative), so its positive arm corresponds to the "Either Test Positive" row of the results table, not to "Both Tests Positive".</p>
</div>'

                self$results$instructions$setContent(instructions)

                # Initialize tables with row headers
                testParamsTable <- self$results$testParamsTable
                testParamsTable$addRow(rowKey = "test1", values = list(test = "Test 1"))
                testParamsTable$addRow(rowKey = "test2", values = list(test = "Test 2"))

                cotestResultsTable <- self$results$cotestResultsTable
                cotestResultsTable$addRow(rowKey = "either_pos",
                                          values = list(scenario = "Either Test Positive (Parallel Rule)"))
                cotestResultsTable$addRow(rowKey = "test1_pos",
                                          values = list(scenario = "Test 1 Positive Only"))
                cotestResultsTable$addRow(rowKey = "test2_pos",
                                          values = list(scenario = "Test 2 Positive Only"))
                cotestResultsTable$addRow(rowKey = "both_pos",
                                          values = list(scenario = "Both Tests Positive"))
                cotestResultsTable$addRow(rowKey = "both_neg",
                                          values = list(scenario = "Both Tests Negative"))
            },

            .run = function() {
                # Initialize notices collection for user-visible validation feedback
                private$.notices <- list()

                # Names for the two tests. Every panel, both tables and the pasteable summary
                # used to say only "Test 1" / "Test 2", so a manuscript sentence never said what
                # was tested. Defaults are exactly the old strings, so nothing moves unless the
                # user names them.
                t1_name <- private$.testLabel(self$options$test1_name, "Test 1")
                t2_name <- private$.testLabel(self$options$test2_name, "Test 2")
                # A worked example names its own tests, unless the user typed something.
                # (events.js writes the same names into the boxes, so the GUI agrees.)
                if (self$options$preset != "custom") {
                    pv <- private$.getPresetValues(self$options$preset)
                    if (!is.null(pv)) {
                        if (!nzchar(trimws(self$options$test1_name)) && !is.null(pv$test1_name))
                            t1_name <- private$.escapeHtml(pv$test1_name)
                        if (!nzchar(trimws(self$options$test2_name)) && !is.null(pv$test2_name))
                            t2_name <- private$.escapeHtml(pv$test2_name)
                    }
                }

                # Get parameters from user inputs
                test1_sens <- self$options$test1_sens
                test1_spec <- self$options$test1_spec
                test2_sens <- self$options$test2_sens
                test2_spec <- self$options$test2_spec
                indep <- self$options$indep
                cond_dep_pos <- self$options$cond_dep_pos
                cond_dep_neg <- self$options$cond_dep_neg
                prevalence <- self$options$prevalence

                # Apply the selected worked example. These values also reach the input boxes via
                # jamovi/js/cotest.events.js, so what is displayed and what is computed agree.
                preset <- self$options$preset
                preset_label <- NULL
                if (preset != "custom") {
                    preset_values <- private$.getPresetValues(preset)
                    preset_label <- preset_values$label
                    if (!is.null(preset_values)) {
                        test1_sens <- preset_values$test1_sens
                        test1_spec <- preset_values$test1_spec
                        test2_sens <- preset_values$test2_sens
                        test2_spec <- preset_values$test2_spec
                        prevalence <- preset_values$prevalence
                        indep <- preset_values$indep
                        if (!is.null(preset_values$cond_dep_pos)) cond_dep_pos <- preset_values$cond_dep_pos
                        if (!is.null(preset_values$cond_dep_neg)) cond_dep_neg <- preset_values$cond_dep_neg

                        # A worked example silently replacing whatever is in the boxes is exactly
                        # how a demonstration figure ends up quoted as if it were evidence. Say so
                        # every single time one is in use.
                        private$.addNotice(sprintf(
                            paste0("Worked example in use: %s. %s These are round illustrative numbers ",
                                   "chosen to demonstrate the calculation, not pooled estimates from the ",
                                   "literature and not values to use for patient care \u{2014} select ",
                                   "Custom values and enter sensitivity, specificity and prevalence ",
                                   "from your own population before acting on any result below."),
                            preset_values$label, preset_values$note), "info")

                        # The .u.yaml `enable:` locks are a GUI affordance only; a script calling
                        # cotest(preset = "hpv_pap", prevalence = 0.60) gets its prevalence
                        # silently discarded and no box to notice it by. Name what was overridden,
                        # and name it loudly if the caller actually passed something different.
                        # Compare against the SCHEMA DEFAULT, not merely against the preset:
                        # cotest(preset = "hpv_pap") leaves every option at its default, and those
                        # defaults differ from the preset's values, so a naive comparison warned on
                        # every plain preset call. A value that differs from BOTH its default and
                        # the preset is one the caller deliberately set. In the GUI events.js has
                        # already written the preset into the controls, so nothing differs and the
                        # warning stays silent there.
                        # Reads jmvcore's private per-option object. If a jmvcore release renames
                        # it this must break a test rather than quietly disable the warning, so the
                        # miss is recorded as a notice instead of being swallowed.
                        opt_default <- function(nm) {
                            o <- self$options$.__enclos_env__$private[[paste0("..", nm)]]
                            if (is.null(o)) {
                                private$.addNotice(sprintf(
                                    "Internal: could not read the default for '%s', so the analysis cannot report whether a worked example replaced a value you supplied. Please report this.",
                                    nm), "warning")
                                return(NULL)
                            }
                            o$default
                        }
                        differing <- character(0)
                        for (nm in c("test1_sens", "test1_spec", "test2_sens", "test2_spec",
                                     "prevalence", "cond_dep_pos", "cond_dep_neg")) {
                            used  <- get(nm)
                            given <- self$options[[nm]]
                            dflt  <- opt_default(nm)
                            if (length(given) != 1L || length(used) != 1L || length(dflt) != 1L)
                                next
                            if (isTRUE(all.equal(given, dflt)))   # untouched: not the caller's doing
                                next
                            if (isTRUE(all.equal(given, used)))   # already agrees with the example
                                next
                            differing <- c(differing, sprintf("%s (%s given, %s used)",
                                                              nm, format(given), format(used)))
                        }

                        # `indep` is handled WITHOUT the schema-default test. It is a Bool
                        # defaulting to FALSE, so half of all deliberate user choices are
                        # indistinguishable from "untouched" -- and it is the one option that
                        # selects which of the two models runs. Before this,
                        # cotest(preset = "psa_dre", indep = FALSE, cond_dep_pos = 0.30) ran the
                        # INDEPENDENT model and said nothing at all.
                        if (!identical(isTRUE(self$options$indep), isTRUE(indep)))
                            differing <- c(differing,
                                           sprintf("indep (%s given, %s used \u{2014} this selects which model is fitted)",
                                                   isTRUE(self$options$indep), isTRUE(indep)))
                        if (length(differing) > 0) {
                            private$.addNotice(sprintf(
                                paste0("The worked example replaced values you supplied: %s. A worked ",
                                       "example sets every parameter; to use your own numbers set the ",
                                       "worked example to Custom values."),
                                paste(differing, collapse = "; ")), "warning")
                        }
                    }
                }

                # Enhanced input validation with specific guidance
                private$.validateInputParameters(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg)

                # Calculate pretest odds early (needed for "Either Test Positive" calculation)
                pretest_odds <- prevalence / (1 - prevalence)

                # Calculate likelihood ratios with numerical stability
                test1_plr <- private$.calculateLikelihoodRatio(test1_sens, (1 - test1_spec), "Test 1 Positive LR")
                test1_nlr <- private$.calculateLikelihoodRatio((1 - test1_sens), test1_spec, "Test 1 Negative LR")
                test2_plr <- private$.calculateLikelihoodRatio(test2_sens, (1 - test2_spec), "Test 2 Positive LR")
                test2_nlr <- private$.calculateLikelihoodRatio((1 - test2_sens), test2_spec, "Test 2 Negative LR")

                # Update test parameters table using helper method
                private$.updateTestParametersTable(test1_sens, test1_spec, test1_plr, test1_nlr,
                                                   test2_sens, test2_spec, test2_plr, test2_nlr,
                                                   t1_name, t2_name)

                # Calculate post-test probabilities for different scenarios
                results <- private$.calculatePostTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec, 
                                                                   prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                                   test1_plr, test1_nlr, test2_plr, test2_nlr)
                
                phi_d_used <- results$phi_d
                phi_n_used <- results$phi_n

                # Extract results for easier access
                postest_prob_t1 <- results$postest_prob_t1
                postest_prob_t2 <- results$postest_prob_t2
                postest_prob_both <- results$postest_prob_both
                postest_prob_both_neg <- results$postest_prob_both_neg
                postest_odds_t1 <- results$postest_odds_t1
                postest_odds_t2 <- results$postest_odds_t2
                postest_odds_both <- results$postest_odds_both
                postest_odds_both_neg <- results$postest_odds_both_neg
                dependence_info <- results$dependence_info

                # Calculate "Either Test Positive" (clinical parallel rule)
                # P(Disease | Either+) from P(Either+ | Disease) and P(Either+ | no Disease)
                if (indep) {
                    # P(Either+ | Disease+) = 1 - P(Both- | Disease+)
                    p_either_pos_D <- 1 - ((1 - test1_sens) * (1 - test2_sens))
                    p_either_pos_nD <- 1 - (test1_spec * test2_spec)
                } else {
                    # For dependent tests, use clamped joint probabilities to maintain coherence
                    p_either_pos_D <- 1 - results$p_both_neg_D
                    p_either_pos_nD <- 1 - results$p_both_neg_nD
                }

                lr_either_pos <- private$.calculateLikelihoodRatio(p_either_pos_D, p_either_pos_nD, "Either Positive LR")
                postest_odds_either <- pretest_odds * lr_either_pos
                postest_prob_either <- private$.oddsToProbability(postest_odds_either)
                rel_prob_either <- postest_prob_either / prevalence

                # Calculate relative probabilities compared to prevalence
                rel_prob_t1 <- postest_prob_t1 / prevalence
                rel_prob_t2 <- postest_prob_t2 / prevalence
                rel_prob_both <- postest_prob_both / prevalence
                rel_prob_both_neg <- postest_prob_both_neg / prevalence

                # Update co-test results table using helper method
                private$.updateCotestResultsTable(postest_prob_either, postest_prob_t1, postest_prob_t2, postest_prob_both, postest_prob_both_neg,
                                                  rel_prob_either, rel_prob_t1, rel_prob_t2, rel_prob_both, rel_prob_both_neg,
                                                  postest_odds_either, postest_odds_t1, postest_odds_t2, postest_odds_both, postest_odds_both_neg,
                                                  t1_name, t2_name, preset, indep)

                # Add footnotes if requested
                if (self$options$fnote) {
                    private$.addFootnotes(t1_name, t2_name)
                }

                # Always populated, for both models. This panel used to be written only in the
                # dependent branch while its .r.yaml `visible:` expression tested the *option*
                # `indep` -- which a worked example can override -- so it could show an empty box
                # for an independent fit, or hide the dependence disclosure for a dependent one.
                # Writing it unconditionally removes the mismatch instead of restating it.
                self$results$dependenceInfo$setContent(dependence_info)

                # Create enhanced explanation with clinical interpretation
                plr_interpretation <- private$.interpretPLR(if(indep) test1_plr * test2_plr else results$lr_both_pos)
                explanation <- sprintf(
                    "<p><strong>Clinical Interpretation:</strong></p>
                <p>Disease prevalence (pre-test probability): <strong>%s</strong></p>
                <p><strong>Both tests positive:</strong> %s probability (%s) - %s</p>
                <p><strong>Both tests negative:</strong> %s probability (%s) %s</p>
                <p><strong>Single positive test:</strong></p>
                <ul>
                <li>%s positive only: <strong>%s</strong> %s</li>
                <li>%s positive only: <strong>%s</strong> %s</li>
                </ul>
                <div style='background-color: rgba(33, 152, 255, 0.07); padding: 10px; border-radius: 5px; margin-top: 15px; color: inherit;'>
                <p><strong>Copy-ready summary:</strong></p>
                <p style='font-family: monospace; font-size: 12px;'>%s</p>
                </div>",
                    private$.fmtPct(prevalence),
                    private$.fmtPct(postest_prob_both), private$.fmtChange(rel_prob_both), plr_interpretation,
                    private$.fmtPct(postest_prob_both_neg), private$.fmtChange(rel_prob_both_neg),
                    private$.getClinicalSignificance(postest_prob_both_neg, prevalence),
                    t1_name, private$.fmtPct(postest_prob_t1), private$.getClinicalSignificance(postest_prob_t1, prevalence),
                    t2_name, private$.fmtPct(postest_prob_t2), private$.getClinicalSignificance(postest_prob_t2, prevalence),
                    private$.generateReportSentence(test1_sens, test1_spec, test2_sens, test2_spec,
                                                   prevalence, postest_prob_both, rel_prob_both,
                                                   postest_prob_both_neg, rel_prob_both_neg,
                                                   preset, indep, phi_d_used, phi_n_used,
                                                   cond_dep_pos, cond_dep_neg, t1_name, t2_name)
                )

                self$results$explanation$setContent(explanation)

                # Create dependency explanation
                self$results$dependenceExplanation$setContent(private$.buildDependenceExplanation())

                # Store data for Fagan nomogram if requested. This runs BEFORE .displayNotices()
                # because it can decide the nomogram is not drawable and add a notice saying so.
                if (self$options$fagan) {
                    private$.prepareFaganPlotData(prevalence, test1_sens, test1_spec, test2_sens, test2_spec,
                                                 indep, lr_either_pos, if (indep) test1_nlr * test2_nlr else results$lr_both_neg,
                                                 preset, preset_label)
                }

                # Display validation notices if any were collected
                private$.displayNotices()
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                # .prepareFaganPlotData() already explained the reason in a notice.
                if (is.null(plotData) || !isTRUE(plotData$drawable))
                    return(FALSE)

                # Check cache to avoid expensive recalculations
                params_key <- paste(plotData$Prevalence, plotData$Plr_PositiveRule, plotData$Nlr_NegativeRule, sep="_")

                if (is.null(private$.lastNomogramParams) || private$.lastNomogramParams != params_key) {
                    private$.nomogramCache <- nomogrammer(
                        Prevalence = plotData$Prevalence,
                        Plr = plotData$Plr_PositiveRule,
                        Nlr = plotData$Nlr_NegativeRule,
                        Detail = TRUE,
                        NullLine = TRUE,
                        LabelSize = private$NOMOGRAM_LABEL_SIZE,
                        # Verbose printed "Sensitivity = 94% / Specificity = 86%" to the console.
                        # Those are the operating characteristics nomogrammer back-derives for the
                        # COMBINED parallel rule; they belong to neither test, and in jamovi they
                        # surfaced in Analysis Notes unlabelled.
                        Verbose = FALSE
                    )
                    private$.lastNomogramParams <- params_key
                }

                p <- private$.nomogramCache
                if (!is.null(plotData$Caption) && nzchar(plotData$Caption))
                    p <- p + ggplot2::labs(caption = plotData$Caption) +
                             ggplot2::theme(plot.caption = ggplot2::element_text(
                                 hjust = 0, size = 8, colour = "grey30"))
                print(p)
                TRUE
            },

            # Enhanced input validation with specific guidance
            .validateInputParameters = function(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg) {
                if (test1_sens <= 0 || test1_sens >= 1) {
                    jmvcore::reject("Test 1 sensitivity must be between 0 and 1. Typical clinical values range from 0.60-0.95 for most diagnostic tests.")
                }
                if (test1_spec <= 0 || test1_spec >= 1) {
                    jmvcore::reject("Test 1 specificity must be between 0 and 1. Typical clinical values range from 0.70-0.98 for most diagnostic tests.")
                }
                if (test2_sens <= 0 || test2_sens >= 1) {
                    jmvcore::reject("Test 2 sensitivity must be between 0 and 1. Typical clinical values range from 0.60-0.95 for most diagnostic tests.")
                }
                if (test2_spec <= 0 || test2_spec >= 1) {
                    jmvcore::reject("Test 2 specificity must be between 0 and 1. Typical clinical values range from 0.70-0.98 for most diagnostic tests.")
                }
                if (prevalence <= 0 || prevalence >= 1) {
                    jmvcore::reject("Disease prevalence must be between 0 and 1. Consider realistic clinical prevalences: rare diseases (0.001-0.01), common conditions (0.05-0.20).")
                }
                if (!indep && (cond_dep_pos < -1 || cond_dep_pos > 1)) {
                    jmvcore::reject("Conditional dependence for subjects with disease must be between -1 and 1. Typical positive values: 0.05 (weak), 0.15 (moderate), 0.30 (strong). Negative values describe tests that compensate for each other's errors.")
                }
                if (!indep && (cond_dep_neg < -1 || cond_dep_neg > 1)) {
                    jmvcore::reject("Conditional dependence for subjects without disease must be between -1 and 1. Typical positive values: 0.05 (weak), 0.15 (moderate), 0.30 (strong). Negative values describe tests that compensate for each other's errors.")
                }
                
                # Additional clinical validity checks
                if (test1_sens + test1_spec < 1.1) {
                    private$.addNotice("Test 1 has low discriminatory power (sensitivity plus specificity below 1.1, i.e. a Youden index below 0.1). Check its likelihood ratios in the results table: an operating point close to the chance diagonal can still give an informative LR+ or LR- when sensitivity and specificity are very unequal.", "warning")
                }
                if (test2_sens + test2_spec < 1.1) {
                    private$.addNotice("Test 2 has low discriminatory power (sensitivity plus specificity below 1.1, i.e. a Youden index below 0.1). Check its likelihood ratios in the results table: an operating point close to the chance diagonal can still give an informative LR+ or LR- when sensitivity and specificity are very unequal.", "warning")
                }

                # Check for extreme prevalence that might cause numerical issues
                if (prevalence < 0.001) {
                    private$.addNotice("Very low prevalence (below 0.1%) may lead to unstable results; the computed post-test probabilities are highly sensitive to small changes in the entered prevalence.", "warning")
                }
                if (prevalence > 0.5) {
                    private$.addNotice("High prevalence (above 50%) detected. Ensure this reflects your actual clinical population.", "info")
                }
            },

            # Calculate likelihood ratios with numerical stability checks
            .calculateLikelihoodRatio = function(numerator, denominator, scenario_name) {
                tol <- private$NUMERICAL_TOLERANCE
                num_zero <- abs(numerator) < tol
                den_zero <- abs(denominator) < tol

                # A zero cell here is not a rounding accident. The dependence parameters can push
                # a joint probability onto its Frechet bound, which forces one of the four test
                # combinations to have probability exactly zero in one group. The likelihood ratio
                # is then degenerate, and printing it as an ordinary number hides the fact that the
                # value is a structural consequence of the assumed model rather than an estimate.
                if (num_zero && den_zero) {
                    private$.addNotice(sprintf(
                        "%s is undefined: with the current parameters this combination of test results has probability zero in both the diseased and the non-diseased group, so it cannot occur at all and no post-test probability exists for it. That row is left blank.",
                        scenario_name), "warning")
                    return(NA_real_)
                }

                if (den_zero) {
                    private$.addNotice(sprintf(
                        "%s is infinite: with the current parameters this combination of test results cannot occur in a subject without disease, so its post-test probability is 1 by construction rather than estimated. Lower the conditional dependence if that is not intended.",
                        scenario_name), "warning")
                    return(Inf)
                }

                if (num_zero) {
                    private$.addNotice(sprintf(
                        "%s is zero: with the current parameters this combination of test results cannot occur in a subject with disease, so its post-test probability is 0 by construction rather than estimated. Lower the conditional dependence if that is not intended.",
                        scenario_name), "warning")
                    return(0)
                }

                result <- numerator / denominator

                # Check for extreme values that might indicate issues
                if (result > 1000) {
                    private$.addNotice(paste("Very high", scenario_name, "ratio (", round(result, 1), ") - indicates highly informative test."), "info")
                }

                return(result)
            },

            # Convert post-test odds to a probability. Odds of Inf are a certainty, not NaN, and an
            # undefined likelihood ratio must stay undefined rather than collapsing to 0.
            .oddsToProbability = function(odds) {
                if (length(odds) != 1L || !is.numeric(odds) || is.na(odds))
                    return(NA_real_)
                if (is.infinite(odds))
                    return(if (odds > 0) 1 else NA_real_)
                odds / (1 + odds)
            },

            # Clamp probabilities to valid ranges while providing informative notices
            .clampProbability = function(value, lower, upper, context) {
                if (is.nan(value) || is.infinite(value)) {
                    jmvcore::reject("{ctx} resulted in a non-finite probability.",
                                    code = NULL, ctx = context)
                }

                adjusted <- value

                # No notice is raised here. The CALLER already reports every clamp that matters,
                # in clinical terms and with both values (see .calculateDependentTestProbabilities).
                # Emitting a second, info-severity message naming an internal quantity
                # ("P(Test1+, Test2+ | Disease+) adjusted from 0.755885 to 0.750000") meant a
                # doubly-infeasible rho produced five notices for one problem, burying the
                # degenerate-likelihood-ratio warning that is the one the reader needs.
                if (adjusted < lower - private$NUMERICAL_TOLERANCE)
                    adjusted <- lower

                if (adjusted > upper + private$NUMERICAL_TOLERANCE)
                    adjusted <- upper

                adjusted <- min(max(adjusted, lower), upper)
                return(adjusted)
            },

            # Confirm the four joint probability cells really are a distribution, and that they
            # reproduce the marginals they were built from.
            #
            # Checking only that the cells sum to 1 is vacuous: the fourth cell is DEFINED as
            # 1 minus the other three, so that sum is 1 by construction and the test can never
            # fail. The invariants worth checking are that no cell has left [0, 1] and that
            # P(both) + P(first only) still equals the sensitivity (or false-positive rate) the
            # cells were derived from -- clamping one cell without adjusting the rest would
            # silently change the test parameters the user entered.
            .validateJointDistribution = function(p_both, p_first_only, p_second_only, p_neither,
                                                  marginal1, marginal2, label) {
                cells <- c(p_both, p_first_only, p_second_only, p_neither)

                if (any(!is.finite(cells)) || any(cells < -1e-9) || any(cells > 1 + 1e-9)) {
                    private$.addNotice(sprintf(
                        "The joint probabilities for %s are not all valid probabilities (%s). Review the dependence parameters.",
                        label, paste(sprintf("%.4f", cells), collapse = ", ")), "warning")
                    return(invisible(FALSE))
                }

                # Kept for completeness even though the caller derives the fourth cell by
                # subtraction, which makes this true by construction today.
                if (abs(sum(cells) - 1) > 1e-6) {
                    private$.addNotice(sprintf(
                        "The joint probabilities for %s sum to %.6f rather than 1. Review the dependence parameters.",
                        label, sum(cells)), "warning")
                    return(invisible(FALSE))
                }

                implied1 <- p_both + p_first_only
                implied2 <- p_both + p_second_only
                if (abs(implied1 - marginal1) > 1e-6 || abs(implied2 - marginal2) > 1e-6) {
                    private$.addNotice(sprintf(
                        "The joint probabilities for %s do not add back up to the test parameters entered (they imply %.4f and %.4f, against %.4f and %.4f). Review the dependence parameters.",
                        label, implied1, implied2, marginal1, marginal2), "warning")
                    return(invisible(FALSE))
                }

                invisible(TRUE)
            },

            # Update test parameters table
            .updateTestParametersTable = function(test1_sens, test1_spec, test1_plr, test1_nlr,
                                                  test2_sens, test2_spec, test2_plr, test2_nlr,
                                                  t1_name = "Test 1", t2_name = "Test 2") {
                testParamsTable <- self$results$testParamsTable
                testParamsTable$setRow(
                    rowKey = "test1",
                    values = list(
                        test = t1_name,
                        sens = test1_sens,
                        spec = test1_spec,
                        plr = test1_plr,
                        nlr = test1_nlr
                    )
                )

                # LR+ and LR- are the two headings most often misread, and their definitions
                # used to sit behind the off-by-default "Detailed footnotes" checkbox, so the
                # default run showed seven column headings with no definition of any of them.
                testParamsTable$setNote("lr_defs", paste0(
                    "LR+ is how many times more likely a positive result is in someone with the ",
                    "disease than in someone without it; LR- is the same ratio for a negative ",
                    "result. Above 10 (or below 0.1) a single result shifts the probability of ",
                    "disease substantially; between 0.5 and 2 it barely moves it."))

                testParamsTable$setRow(
                    rowKey = "test2",
                    values = list(
                        test = t2_name,
                        sens = test2_sens,
                        spec = test2_spec,
                        plr = test2_plr,
                        nlr = test2_nlr
                    )
                )
            },

            # Calculate post-test probabilities for all scenarios
            .calculatePostTestProbabilities = function(test1_sens, test1_spec, test2_sens, test2_spec, 
                                                       prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                       test1_plr, test1_nlr, test2_plr, test2_nlr) {
                # Convert prevalence to odds
                pretest_odds <- prevalence / (1 - prevalence)
                
                if (indep) {
                    # Independent tests: combine appropriate likelihood ratios for each outcome
                    lr_t1_only <- test1_plr * test2_nlr
                    lr_t2_only <- test1_nlr * test2_plr
                    lr_both_pos <- test1_plr * test2_plr
                    lr_both_neg <- test1_nlr * test2_nlr

                    postest_odds_t1 <- pretest_odds * lr_t1_only
                    postest_prob_t1 <- private$.oddsToProbability(postest_odds_t1)

                    postest_odds_t2 <- pretest_odds * lr_t2_only
                    postest_prob_t2 <- private$.oddsToProbability(postest_odds_t2)

                    postest_odds_both <- pretest_odds * lr_both_pos
                    postest_prob_both <- private$.oddsToProbability(postest_odds_both)

                    postest_odds_both_neg <- pretest_odds * lr_both_neg
                    postest_prob_both_neg <- private$.oddsToProbability(postest_odds_both_neg)

                    dependence_info <- paste0(
                        "<p>The two tests are run in parallel on the same subject and are assumed to be ",
                        "<strong>conditionally independent</strong>: within the diseased group, and again ",
                        "within the non-diseased group, the result of one test carries no information ",
                        "about the other. The joint probabilities below are therefore plain products of ",
                        "the entered sensitivities and specificities.</p>",
                        "<p>Independence is the optimistic assumption. If the two tests in fact tend to ",
                        "err together \u{2014} same specimen, same operator, same biology \u{2014} it ",
                        "overstates how much the pair adds over either test alone. Clear ",
                        "<em>Assume the two tests are conditionally independent</em> and enter a ",
                        "dependence parameter to see how far the conclusion moves.</p>")
                } else {
                    # Dependent tests scenario with numerical stability
                    dep_results <- private$.calculateDependentTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec,
                                                                                cond_dep_pos, cond_dep_neg, pretest_odds)
                    
                    postest_prob_t1 <- dep_results$postest_prob_t1
                    postest_prob_t2 <- dep_results$postest_prob_t2
                    postest_prob_both <- dep_results$postest_prob_both
                    postest_prob_both_neg <- dep_results$postest_prob_both_neg
                    postest_odds_t1 <- dep_results$postest_odds_t1
                    postest_odds_t2 <- dep_results$postest_odds_t2
                    postest_odds_both <- dep_results$postest_odds_both
                    postest_odds_both_neg <- dep_results$postest_odds_both_neg
                    dependence_info <- dep_results$dependence_info
                    p_both_neg_D <- dep_results$p_both_neg_D
                    p_both_neg_nD <- dep_results$p_both_neg_nD
                    phi_d <- dep_results$phi_d
                    phi_n <- dep_results$phi_n
                }
                
                # Store LRs for plot data (set to NA for independent case)
                if (indep) {
                    lr_both_pos <- test1_plr * test2_plr
                    lr_both_neg <- test1_nlr * test2_nlr
                    p_both_neg_D <- (1 - test1_sens) * (1 - test2_sens)
                    p_both_neg_nD <- test1_spec * test2_spec
                    phi_d <- 0; phi_n <- 0   # independence IS zero correlation
                } else {
                    lr_both_pos <- dep_results$lr_both_pos
                    lr_both_neg <- dep_results$lr_both_neg
                }
                
                return(list(
                    postest_prob_t1 = postest_prob_t1,
                    postest_prob_t2 = postest_prob_t2,
                    postest_prob_both = postest_prob_both,
                    postest_prob_both_neg = postest_prob_both_neg,
                    postest_odds_t1 = postest_odds_t1,
                    postest_odds_t2 = postest_odds_t2,
                    postest_odds_both = postest_odds_both,
                    postest_odds_both_neg = postest_odds_both_neg,
                    dependence_info = dependence_info,
                    p_both_neg_D = p_both_neg_D,
                    p_both_neg_nD = p_both_neg_nD,
                    lr_both_pos = lr_both_pos,
                    lr_both_neg = lr_both_neg,
                    phi_d = phi_d,
                    phi_n = phi_n
                ))
            },

            # Calculate probabilities for dependent tests
            .calculateDependentTestProbabilities = function(test1_sens, test1_spec, test2_sens, test2_spec,
                                                            cond_dep_pos, cond_dep_neg, pretest_odds) {
                # Helper values for marginal probabilities
                fp_test1 <- 1 - test1_spec
                fp_test2 <- 1 - test2_spec

                # Probability of both tests positive given disease present
                p_both_pos_D_raw <- (test1_sens * test2_sens) + (cond_dep_pos * sqrt(
                    test1_sens * (1 - test1_sens) * test2_sens * (1 - test2_sens)
                ))
                lower_pos_D <- max(0, test1_sens + test2_sens - 1)
                upper_pos_D <- min(test1_sens, test2_sens)
                p_both_pos_D <- private$.clampProbability(p_both_pos_D_raw, lower_pos_D, upper_pos_D,
                                                          "P(Test1+, Test2+ | Disease+)")
                if (abs(p_both_pos_D_raw - p_both_pos_D) > private$NUMERICAL_TOLERANCE) {
                    # Worded from the REALIZED value rather than asserting a direction. The old
                    # text always said "a more strongly dependent model" and "lower the value",
                    # which is backwards for a negative parameter: truncating rho = -0.20 yields a
                    # realized correlation of smaller magnitude, and lowering it further makes the
                    # truncation worse. The realized phi is reported in the Test Dependence panel.
                    private$.addNotice(sprintf(
                        "The dependence parameter for the diseased group (%.2f) is not attainable with these sensitivities, so the joint positive probability was truncated from %.4f to its bound %.4f. The model actually fitted is therefore not the one you specified, and can force some test combinations to be impossible. Move the value toward 0 until this note disappears; the realized correlation is reported in the Test Dependence panel.",
                        cond_dep_pos, p_both_pos_D_raw, p_both_pos_D), "warning")
                }

                p_t1_only_D <- private$.clampProbability(test1_sens - p_both_pos_D, 0, test1_sens,
                                                         "P(Test1+, Test2- | Disease+)")
                p_t2_only_D <- private$.clampProbability(test2_sens - p_both_pos_D, 0, test2_sens,
                                                         "P(Test1-, Test2+ | Disease+)")
                p_both_neg_D <- 1 - (p_both_pos_D + p_t1_only_D + p_t2_only_D)

                # Probability of both tests positive given disease absent (false positives)
                p_both_pos_nD_raw <- (fp_test1 * fp_test2) + (cond_dep_neg * sqrt(
                    fp_test1 * (1 - fp_test1) * fp_test2 * (1 - fp_test2)
                ))
                lower_pos_nD <- max(0, fp_test1 + fp_test2 - 1)
                upper_pos_nD <- min(fp_test1, fp_test2)
                p_both_pos_nD <- private$.clampProbability(p_both_pos_nD_raw, lower_pos_nD, upper_pos_nD,
                                                          "P(Test1+, Test2+ | Disease-)")
                if (abs(p_both_pos_nD_raw - p_both_pos_nD) > private$NUMERICAL_TOLERANCE) {
                    private$.addNotice(sprintf(
                        "The dependence parameter for the non-diseased group (%.2f) is not attainable with these specificities, so the joint false-positive probability was truncated from %.4f to its bound %.4f. The model actually fitted is therefore not the one you specified, and can force some test combinations to be impossible. Move the value toward 0 until this note disappears; the realized correlation is reported in the Test Dependence panel.",
                        cond_dep_neg, p_both_pos_nD_raw, p_both_pos_nD), "warning")
                }

                p_t1_only_nD <- private$.clampProbability(fp_test1 - p_both_pos_nD, 0, fp_test1,
                                                          "P(Test1+, Test2- | Disease-)")
                p_t2_only_nD <- private$.clampProbability(fp_test2 - p_both_pos_nD, 0, fp_test2,
                                                          "P(Test1-, Test2+ | Disease-)")
                p_both_neg_nD <- 1 - (p_both_pos_nD + p_t1_only_nD + p_t2_only_nD)

                # Ensure each set is a valid distribution that still reproduces its marginals
                private$.validateJointDistribution(
                    p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D,
                    test1_sens, test2_sens, "subjects with disease"
                )
                private$.validateJointDistribution(
                    p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD,
                    fp_test1, fp_test2, "subjects without disease"
                )

                # Calculate likelihood ratios with stability checks
                lr_t1_only <- private$.calculateLikelihoodRatio(p_t1_only_D, p_t1_only_nD, "Test 1 Only LR")
                lr_t2_only <- private$.calculateLikelihoodRatio(p_t2_only_D, p_t2_only_nD, "Test 2 Only LR")
                lr_both_pos <- private$.calculateLikelihoodRatio(p_both_pos_D, p_both_pos_nD, "Both Positive LR")
                lr_both_neg <- private$.calculateLikelihoodRatio(p_both_neg_D, p_both_neg_nD, "Both Negative LR")

                # Calculate post-test odds and probabilities
                postest_odds_t1 <- pretest_odds * lr_t1_only
                postest_prob_t1 <- private$.oddsToProbability(postest_odds_t1)

                postest_odds_t2 <- pretest_odds * lr_t2_only
                postest_prob_t2 <- private$.oddsToProbability(postest_odds_t2)

                postest_odds_both <- pretest_odds * lr_both_pos
                postest_prob_both <- private$.oddsToProbability(postest_odds_both)

                postest_odds_both_neg <- pretest_odds * lr_both_neg
                postest_prob_both_neg <- private$.oddsToProbability(postest_odds_both_neg)

                # Compute realized phi coefficients
                phi_calc <- function(p11, p10, p01, p00) {
                    denom <- sqrt((p11 + p10) * (p01 + p00) * (p11 + p01) * (p10 + p00))
                    if (denom <= private$NUMERICAL_TOLERANCE) return(NA_real_)
                    (p11 * p00 - p10 * p01) / denom
                }
                phi_d <- phi_calc(p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D)
                phi_n <- phi_calc(p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD)

                dependence_info <- sprintf(
                    "<p>Tests are modeled with conditional dependence:<br>
                Dependence for subjects with disease: %.2f<br>
                Dependence for subjects without disease: %.2f<br>
                Realized phi (disease): %s<br>
                Realized phi (no disease): %s<br>
                <span style='font-size: 90%%;'>(Phi is the correlation between the two test results
                actually achieved by the fitted model, on a \u{2212}1 to +1 scale, where 0 is
                conditional independence. It differs from the value you entered when that value
                was not attainable given the sensitivities and specificities supplied.)</span></p>
                <p>Joint probabilities after accounting for dependence:<br>
                P(Test1+,Test2+ | Disease+): %.4f<br>
                P(Test1+,Test2- | Disease+): %.4f<br>
                P(Test1-,Test2+ | Disease+): %.4f<br>
                P(Test1-,Test2- | Disease+): %.4f<br>
                P(Test1+,Test2+ | Disease-): %.4f<br>
                P(Test1+,Test2- | Disease-): %.4f<br>
                P(Test1-,Test2+ | Disease-): %.4f<br>
                P(Test1-,Test2- | Disease-): %.4f</p>",
                    cond_dep_pos, cond_dep_neg,
                    ifelse(is.na(phi_d), "NA", sprintf("%.2f", phi_d)),
                    ifelse(is.na(phi_n), "NA", sprintf("%.2f", phi_n)),
                    p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D,
                    p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD
                )
                
                return(list(
                    postest_prob_t1 = postest_prob_t1,
                    postest_prob_t2 = postest_prob_t2,
                    postest_prob_both = postest_prob_both,
                    postest_prob_both_neg = postest_prob_both_neg,
                    postest_odds_t1 = postest_odds_t1,
                    postest_odds_t2 = postest_odds_t2,
                    postest_odds_both = postest_odds_both,
                    postest_odds_both_neg = postest_odds_both_neg,
                    dependence_info = dependence_info,
                    lr_both_pos = lr_both_pos,
                    lr_both_neg = lr_both_neg,
                    p_both_neg_D = p_both_neg_D,
                    p_both_neg_nD = p_both_neg_nD,
                    # the correlation the fitted model actually achieved, which differs from the
                    # requested one whenever a joint cell was clamped to its Frechet bound
                    phi_d = phi_d,
                    phi_n = phi_n
                ))
            },

            # Update co-test results table with reduced duplication
            .updateCotestResultsTable = function(postest_prob_either, postest_prob_t1, postest_prob_t2, postest_prob_both, postest_prob_both_neg,
                                                 rel_prob_either, rel_prob_t1, rel_prob_t2, rel_prob_both, rel_prob_both_neg,
                                                 postest_odds_either, postest_odds_t1, postest_odds_t2, postest_odds_both, postest_odds_both_neg,
                                                 t1_name = "Test 1", t2_name = "Test 2",
                                                 preset = "custom", indep = TRUE) {
                cotestResultsTable <- self$results$cotestResultsTable

                # Define scenarios with their data (including clinical parallel rule)
                scenarios <- list(
                    list(key = "either_pos", scenario = "Either Test Positive (Parallel Rule)", postProb = postest_prob_either,
                         relativeProbability = rel_prob_either, orValue = postest_odds_either),
                    list(key = "test1_pos", scenario = paste(t1_name, "Positive Only"), postProb = postest_prob_t1,
                         relativeProbability = rel_prob_t1, orValue = postest_odds_t1),
                    list(key = "test2_pos", scenario = paste(t2_name, "Positive Only"), postProb = postest_prob_t2,
                         relativeProbability = rel_prob_t2, orValue = postest_odds_t2),
                    list(key = "both_pos", scenario = "Both Tests Positive", postProb = postest_prob_both,
                         relativeProbability = rel_prob_both, orValue = postest_odds_both),
                    list(key = "both_neg", scenario = "Both Tests Negative", postProb = postest_prob_both_neg,
                         relativeProbability = rel_prob_both_neg, orValue = postest_odds_both_neg)
                )

                # Every input is a point estimate the user typed in, so every number in this table
                # is conditional on those values being exact. That limitation belongs next to the
                # numbers, not only in the welcome panel, which a user can collapse.
                cotestResultsTable$setNote("fixed_inputs", paste0(
                    "Sensitivity, specificity and prevalence are treated as exact. These post-test ",
                    "probabilities therefore carry <i>no</i> confidence interval and do not reflect ",
                    "sampling uncertainty in the values entered \u{2014} published test performance ",
                    "estimates and local prevalence both vary."))

                # A worked example's numbers must not leave this table looking like estimates.
                # Without this the table is byte-identical between preset = "custom" and a
                # demonstration run, so an exported table carries no marking at all.
                if (!identical(preset, "custom")) {
                    cotestResultsTable$setNote("demo", paste0(
                        "<b>Demonstration only.</b> These figures come from a built-in worked ",
                        "example: round illustrative sensitivity, specificity, prevalence and ",
                        "dependence values chosen to show how the calculation behaves. They are ",
                        "not pooled literature estimates, are not taken from any guideline, and ",
                        "must not be used for patient care."))
                    self$results$testParamsTable$setNote("demo", paste0(
                        "<b>Demonstration only.</b> Illustrative test performance from a built-in ",
                        "worked example, not values for clinical use."))
                }

                # Under a dependent model the combined likelihood ratio is NOT the product of the
                # two marginal LRs printed in the Test Parameters table above. A reader who
                # multiplies them to transport the result to another prevalence overstates the
                # evidence roughly two-fold at the shipped hpv_pap example.
                if (!isTRUE(indep)) {
                    self$results$testParamsTable$setNote("marginal_only", paste0(
                        "These likelihood ratios describe each test <i>on its own</i>. Because the ",
                        "two tests are modelled as conditionally dependent, the combined likelihood ",
                        "ratio is <b>not</b> their product \u{2014} use the Post-test Odds column of ",
                        "the Co-Testing Results table instead."))
                }

                # Update all rows using loop to reduce duplication
                for (scenario in scenarios) {
                    cotestResultsTable$setRow(
                        rowKey = scenario$key,
                        values = list(
                            scenario = scenario$scenario,
                            postProb = scenario$postProb,
                            relativeProbability = scenario$relativeProbability,
                            orValue = scenario$orValue
                        )
                    )

                    # A post-test probability of exactly 1 or 0 here is a STRUCTURAL consequence of
                    # a dependence parameter that was clamped to its Frechet bound, not an
                    # estimate: the cell was driven to probability zero in one disease group so the
                    # likelihood ratio is Inf or 0. Printing a bare "100.00%" with "Inf" odds and
                    # nothing on the row asserts diagnostic certainty the model never claimed.
                    # Reachable from ordinary inputs (spec .99 / .90 with rho- = 0.40).
                    odds <- scenario$orValue
                    if (length(odds) == 1L && !is.na(odds) && (is.infinite(odds) || odds == 0)) {
                        cotestResultsTable$addFootnote(
                            rowKey = scenario$key, col = "postProb",
                            paste0("Not an estimate: with these parameters this result combination ",
                                   "cannot occur in one of the two disease groups, so the probability ",
                                   "is ", if (is.infinite(odds)) "1" else "0", " by construction. ",
                                   "This follows from the conditional dependence being truncated to ",
                                   "the largest value the entered sensitivities and specificities ",
                                   "allow \u{2014} move it toward 0 to leave this regime."))
                    }
                }
            },

            # Add footnotes to tables
            .addFootnotes = function(t1_name = "Test 1", t2_name = "Test 2") {
                testParamsTable <- self$results$testParamsTable
                cotestResultsTable <- self$results$cotestResultsTable
                
                # Test parameters footnotes
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "sens",
                    sprintf("Proportion of diseased patients correctly identified by %s", t1_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "spec",
                    sprintf("Proportion of non-diseased patients correctly identified by %s", t1_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "plr",
                    "Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "nlr",
                    "Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "sens",
                    sprintf("Proportion of diseased patients correctly identified by %s", t2_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "spec",
                    sprintf("Proportion of non-diseased patients correctly identified by %s", t2_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "plr",
                    "Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "nlr",
                    "Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients"
                )

                # Results footnotes
                # "either_pos" included: it is the headline row for parallel co-testing and was
                # the only one of the five left without footnotes.
                for (row_key in c("either_pos", "test1_pos", "test2_pos", "both_pos", "both_neg")) {
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "postProb",
                        "Probability of disease after obtaining this test result combination"
                    )
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "relativeProbability",
                        "How many times more (or less) likely disease is after testing compared to before testing"
                    )
                    # orValue had no explanation in ANY option state.
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "orValue",
                        "Post-test odds: probability of disease divided by probability of no disease. Odds of 1 mean a 50% probability; odds of 9 mean 90%."
                    )
                }
            },

            # Prepare Fagan nomogram plot data
            .prepareFaganPlotData = function(prevalence, test1_sens, test1_spec, test2_sens, test2_spec,
                                            indep, lr_positive_rule, lr_negative_rule,
                                            preset = "custom", preset_label = NULL) {
                # Checkpoint before potentially expensive nomogram calculation
                private$.checkpoint()
                
                # nomogrammer() refuses a positive likelihood ratio below 1, and cannot place a
                # non-finite ratio on the axis at all. Decide here whether the nomogram can be
                # drawn, so the user gets an explanation instead of a raw R error in the results.
                finite1 <- function(x) length(x) == 1L && is.numeric(x) && is.finite(x)
                reason <- NULL
                if (!finite1(lr_positive_rule) || !finite1(lr_negative_rule)) {
                    reason <- "one of the combined likelihood ratios is not a finite number with these parameters"
                } else if (lr_positive_rule < 1) {
                    reason <- sprintf(paste0(
                        "the positive-rule likelihood ratio is %.2f, which is below 1. A Fagan nomogram ",
                        "assumes a positive result raises the probability of disease; here it lowers it, ",
                        "which means the tests as specified perform worse than chance"), lr_positive_rule)
                } else if (lr_negative_rule <= 0) {
                    reason <- "the negative-rule likelihood ratio is zero, which cannot be placed on the nomogram's logarithmic axis"
                } else if (abs(lr_positive_rule - lr_negative_rule) < .Machine$double.eps ||
                           isTRUE(all.equal(lr_positive_rule, lr_negative_rule))) {
                    # nomogrammer() stop()s on equal ratios ("PLR and NLR cannot be equal"). This
                    # happens whenever BOTH tests sit exactly on the chance diagonal
                    # (sensitivity = 1 - specificity for each): the parallel rule then gives
                    # LR+ = LR- = 1. Without this branch drawable stayed TRUE, no notice was
                    # written, and the renderer surfaced nomogrammer's raw internal error.
                    # all.equal() also catches the near-equal case (|LR+ - LR-| ~ 4e-16), which
                    # squeaked past the eps test and drew a nomogram for two worthless tests.
                    reason <- sprintf(paste0(
                        "the two tests together carry no information: the positive-rule and ",
                        "negative-rule likelihood ratios are both %.2f, so neither a positive nor ",
                        "a negative combined result changes the probability of disease. Each test ",
                        "is sitting on the chance diagonal (sensitivity = 1 - specificity)"),
                        lr_positive_rule)
                }

                if (!is.null(reason)) {
                    private$.addNotice(sprintf(
                        "The Fagan nomogram was not drawn because %s. Check the sensitivity and specificity values you entered.",
                        reason), "warning")
                }

                plotData <- list(
                    "Prevalence" = prevalence,
                    "Test1Sens" = test1_sens,
                    "Test1Spec" = test1_spec,
                    "Test2Sens" = test2_sens,
                    "Test2Spec" = test2_spec,
                    "Plr_PositiveRule" = lr_positive_rule,
                    "Nlr_NegativeRule" = lr_negative_rule,
                    "drawable" = is.null(reason),
                    # The rendered image is the one export route that carried no demonstration
                    # marker and never said which decision rule it plots. A PNG dropped into a
                    # slide deck travels without any of the surrounding text.
                    "Caption" = paste0(
                        "Parallel rule: positive if either test is positive, negative only if both are negative.",
                        if (!identical(preset, "custom") && !is.null(preset_label))
                            paste0("  |  DEMONSTRATION ONLY \u{2014} ", preset_label,
                                   ", illustrative values, not for clinical use")
                        else "")
                )

                image1 <- self$results$plot1
                image1$setState(plotData)
            },

            # Clinical interpretation helpers
            # sprintf("%.1f%%", NA) renders the literal "NA%". A post-test probability is NA
            # whenever a result combination is impossible in BOTH disease groups -- correct, and
            # explained in a notice -- so the prose has to say that rather than print "NA%".
            # A user-typed name reaches HTML panels and table cells, so escape it, and fall
            # back to the positional label when it is blank.
            .testLabel = function(value, fallback) {
                if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) return(fallback)
                private$.escapeHtml(trimws(value))
            },

            .fmtPct = function(p) if (length(p) != 1L || is.na(p)) "not estimable" else sprintf("%.1f%%", p * 100),
            .fmtRatio = function(r) if (length(r) != 1L || is.na(r)) "not estimable" else sprintf("%.2fx", r),

            # Direction of effect, chosen from the ratio instead of asserted. The templates used
            # to hard-code "increase" and "reduced to"; neither ratio is constrained, so for any
            # test pair with sensitivity + specificity < 1 (an inverted operating point, or
            # sensitivity and specificity entered the wrong way round) both clauses stated the
            # opposite of the numbers printed beside them.
            .fmtChange = function(r) {
                if (length(r) != 1L || is.na(r)) return("not estimable")
                if (r >= 1) sprintf("%.1fx increase", r) else sprintf("%.2fx of prevalence, a decrease", r)
            },

            .interpretPLR = function(plr) {
                if (length(plr) != 1L || is.na(plr)) return("not estimable")
                if (plr > 10) return("strong evidence for disease")
                if (plr > 5) return("moderate evidence for disease")
                if (plr > 2) return("weak evidence for disease")
                if (plr > 1) return("minimal evidence for disease")
                return("no diagnostic value")
            },

            .getClinicalSignificance = function(post_prob, prevalence) {
                # post_prob is NA when the dependence parameters make a test combination impossible
                # in both groups, so there is no change from prevalence to describe.
                if (length(post_prob) != 1L || is.na(post_prob) ||
                    length(prevalence) != 1L || is.na(prevalence) || prevalence <= 0)
                    return("(not estimable)")
                change_factor <- post_prob / prevalence
                if (change_factor > 3) return("(major increase)")
                if (change_factor > 1.5) return("(moderate increase)")
                if (change_factor > 1.1) return("(slight increase)")
                if (change_factor < 0.5) return("(major decrease)")
                if (change_factor < 0.8) return("(moderate decrease)")
                return("(minimal change)")
            },

            # The disclosure has to travel WITH the sentence. This block is monospaced and
            # labelled "copy-ready" precisely to invite lifting it into a manuscript, and the
            # demonstration-only warning lives in a different result item that does not get
            # copied along with it. The model actually fitted is named for the same reason: a
            # bare post-test probability is meaningless without knowing whether the two tests
            # were assumed independent.
            .generateReportSentence = function(test1_sens, test1_spec, test2_sens, test2_spec,
                                               prevalence, postest_prob_both, rel_prob_both,
                                               postest_prob_both_neg, rel_prob_both_neg,
                                               preset = "custom", indep = FALSE,
                                               phi_d = NA, phi_n = NA,
                                               cond_dep_pos = NA, cond_dep_neg = NA,
                                               t1_name = "Test 1", t2_name = "Test 2") {
                marker <- if (!identical(preset, "custom"))
                    "[DEMONSTRATION ONLY \u{2014} illustrative values, not for clinical use] " else ""

                # REALIZED correlation, not the requested one. When the requested rho is outside
                # the Frechet-feasible range the joint cell is clamped and the model actually
                # fitted uses a different correlation -- which the Test Dependence panel already
                # reports. Quoting the requested value here put a number in a pasteable methods
                # sentence that described a model nobody ran (0.50 quoted against 0.35 fitted).
                fmt2 <- function(x) if (length(x) != 1L || is.na(x)) "unavailable" else sprintf("%.2f", x)
                model <- if (isTRUE(indep)) {
                    " Both tests were applied in parallel and assumed conditionally independent."
                } else {
                    trunc_note <- ""
                    if (!is.na(phi_d) && !is.na(cond_dep_pos) && abs(phi_d - cond_dep_pos) > 5e-3)
                        trunc_note <- sprintf(" (requested %.2f among diseased, truncated to the largest value the entered sensitivities allow)",
                                              cond_dep_pos)
                    else if (!is.na(phi_n) && !is.na(cond_dep_neg) && abs(phi_n - cond_dep_neg) > 5e-3)
                        trunc_note <- sprintf(" (requested %.2f among non-diseased, truncated to the largest value the entered specificities allow)",
                                              cond_dep_neg)
                    sprintf(paste0(" Both tests were applied in parallel, allowing conditional ",
                                   "dependence (correlation %s among diseased and %s among ",
                                   "non-diseased subjects)%s."),
                            fmt2(phi_d), fmt2(phi_n), trunc_note)
                }

                # Direction chosen from the ratio. Hard-coding "increase" and "reduced to" made
                # this sentence assert the opposite of its own numbers for any test pair with
                # sensitivity + specificity < 1.
                dir_pos <- if (is.na(rel_prob_both)) "not estimable"
                           else if (rel_prob_both >= 1) sprintf("%.1fx increase", rel_prob_both)
                           else sprintf("%.2fx of prevalence, a decrease", rel_prob_both)
                dir_neg <- if (is.na(rel_prob_both_neg)) "not estimable"
                           else if (rel_prob_both_neg <= 1) sprintf("reduced to %.2fx of prevalence", rel_prob_both_neg)
                           else sprintf("raised to %.2fx of prevalence", rel_prob_both_neg)

                paste0(marker, sprintf(
                    "Co-testing with %s (sensitivity %.0f%%, specificity %.0f%%) and %s (sensitivity %.0f%%, specificity %.0f%%) in a population with %.1f%% disease prevalence showed: when both tests are positive, disease probability is %s (%s); when both are negative, disease probability is %s (%s).",
                    t1_name, test1_sens * 100, test1_spec * 100,
                    t2_name, test2_sens * 100, test2_spec * 100,
                    prevalence * 100,
                    private$.fmtPct(postest_prob_both), dir_pos,
                    private$.fmtPct(postest_prob_both_neg), dir_neg
                ), model,
                " Sensitivity, specificity and prevalence were treated as exact, so these",
                " probabilities carry no confidence interval.")
            },

            # THE canonical preset table. jamovi/js/cotest.events.js writes the same numbers
            # into the input boxes so the user can see them; tests/testthat/test-cotest.R asserts
            # the two copies agree field-for-field. They used to disagree in 25 of 48 fields --
            # three presets even disagreed about whether the tests were independent -- so the
            # boxes on screen described one model while the table below reported another.
            #
            # Every number here is a ROUND ILLUSTRATIVE FIGURE chosen to demonstrate the
            # calculation, not a pooled estimate from a meta-analysis and not a value anyone
            # should carry into patient care. `note` says what each example is meant to show.
            .getPresetValues = function(preset) {
                presets <- list(
                    hpv_pap = list(
                        test1_sens = 0.95, test1_spec = 0.85,
                        test2_sens = 0.70, test2_spec = 0.95,
                        prevalence = 0.05, indep = FALSE,
                        cond_dep_pos = 0.15, cond_dep_neg = 0.10,
                        test1_name = "HPV", test2_name = "Pap cytology",
                        label = "HPV (Test 1) + Pap cytology (Test 2)",
                        note = paste0("A sensitive test paired with a specific one, both read from the same ",
                                      "cervical sample, so they are modelled as conditionally dependent.")
                    ),
                    psa_dre = list(
                        test1_sens = 0.80, test1_spec = 0.70,
                        test2_sens = 0.50, test2_spec = 0.85,
                        prevalence = 0.15, indep = TRUE,
                        cond_dep_pos = NULL, cond_dep_neg = NULL,
                        test1_name = "PSA", test2_name = "Rectal examination",
                        label = "PSA (Test 1) + digital rectal examination (Test 2)",
                        note = paste0("The one worked example that assumes conditional independence: a ",
                                      "biochemical measurement and a physical examination.")
                    ),
                    troponin_ecg = list(
                        test1_sens = 0.90, test1_spec = 0.95,
                        test2_sens = 0.70, test2_spec = 0.90,
                        prevalence = 0.20, indep = FALSE,
                        cond_dep_pos = 0.20, cond_dep_neg = 0.05,
                        test1_name = "Troponin", test2_name = "ECG",
                        label = "Troponin (Test 1) + ECG (Test 2)",
                        note = paste0("Both tests are driven by the extent of myocardial injury, so they are ",
                                      "modelled as dependent among diseased subjects in particular.")
                    ),
                    mammogram_ultrasound = list(
                        test1_sens = 0.85, test1_spec = 0.90,
                        test2_sens = 0.80, test2_spec = 0.85,
                        prevalence = 0.08, indep = FALSE,
                        cond_dep_pos = 0.25, cond_dep_neg = 0.15,
                        test1_name = "Mammography", test2_name = "Ultrasound",
                        label = "Mammography (Test 1) + ultrasound (Test 2)",
                        note = paste0("Two imaging modalities of the same tissue: the strongest dependence ",
                                      "among the worked examples, which is what dense tissue would produce.")
                    ),
                    covid_antigen_pcr = list(
                        test1_sens = 0.70, test1_spec = 0.95,
                        test2_sens = 0.95, test2_spec = 0.99,
                        prevalence = 0.10, indep = FALSE,
                        cond_dep_pos = 0.30, cond_dep_neg = 0.10,
                        test1_name = "Rapid antigen", test2_name = "PCR",
                        label = "Rapid antigen (Test 1) + PCR (Test 2)",
                        note = paste0("Two assays for the same organism on the same swab; both track viral ",
                                      "load, so dependence among diseased subjects is set high.")
                    ),
                    tb_xray_sputum = list(
                        test1_sens = 0.75, test1_spec = 0.80,
                        test2_sens = 0.85, test2_spec = 0.98,
                        prevalence = 0.12, indep = FALSE,
                        cond_dep_pos = 0.20, cond_dep_neg = 0.08,
                        test1_name = "Chest radiograph", test2_name = "Sputum microscopy",
                        label = "Chest radiograph (Test 1) + sputum microscopy (Test 2)",
                        note = paste0("A sensitive imaging test paired with a highly specific microbiological ",
                                      "one; advanced disease makes both more likely to be positive.")
                    )
                )

                if (!(preset %in% names(presets))) {
                    return(NULL)  # Fall back to custom values
                }

                return(presets[[preset]])
            },

            # Notice text is concatenated into the notices markup, and some messages interpolate
            # user-entered numbers and context labels containing "<" / ">" (e.g. "P(Test1+, Test2- |
            # Disease-)"). Escape the five structural characters so a message can never open a tag.
            .escapeHtml = function(x) {
                x <- gsub("&", "&amp;", x, fixed = TRUE)
                x <- gsub("<", "&lt;", x, fixed = TRUE)
                x <- gsub(">", "&gt;", x, fixed = TRUE)
                x <- gsub('"', "&quot;", x, fixed = TRUE)
                gsub("'", "&apos;", x, fixed = TRUE)
            },

            # Add a notice to the collection (stores level so it can be rendered distinctly)
            .addNotice = function(message, level = "warning") {
                if (!level %in% c("error", "warning", "info"))
                    level <- "warning"
                private$.notices <- c(private$.notices,
                                      list(list(level = level, message = message)))
            },

            # Display collected notices to the user, styled per severity level
            .displayNotices = function() {
                if (length(private$.notices) == 0) {
                    # Empty content rather than setVisible(FALSE): jamovi greys a programmatically
                    # hidden pane, and a hide/restore pair drifts out of step with the declarative
                    # `visible:` in the .r.yaml. Nothing to say renders as nothing.
                    self$results$notices$setContent("")
                    return()
                }

                # Per-level presentation. The severity is carried by a translucent rgba tint plus
                # a left rule, NOT by a hard-coded dark foreground: #842029 / #664d03 / #055160 on
                # text alone measured 1.50 / 1.77 / 1.59 : 1 against jamovi's dark theme, so the
                # error and warning text was effectively invisible exactly where it mattered most.
                # rgba tints composite over either theme and `color: inherit` follows the pane.
                # (tools/theme_safe_html.py only scans background declarations, so it read clean.)
                styleFor <- function(level) {
                    switch(level,
                        "error"   = list(label = "Error",
                                         tint = "rgba(220, 53, 69, 0.14)",  rule = "rgba(220, 53, 69, 0.85)"),
                        "warning" = list(label = "Warning",
                                         tint = "rgba(255, 193, 7, 0.16)",  rule = "rgba(217, 164, 6, 0.90)"),
                        "info"    = list(label = "Note",
                                         tint = "rgba(13, 202, 240, 0.12)", rule = "rgba(13, 140, 180, 0.85)"),
                        list(label = "Note",
                             tint = "rgba(255, 193, 7, 0.16)", rule = "rgba(217, 164, 6, 0.90)")
                    )
                }

                items_html <- ""
                for (notice in private$.notices) {
                    sty <- styleFor(notice$level)
                    items_html <- paste0(
                        items_html,
                        '<li style="margin: 6px 0; padding: 6px 10px; list-style: none; ',
                        'background-color: ', sty$tint, '; border-left: 3px solid ', sty$rule, '; ',
                        'border-radius: 3px; color: inherit;"><strong>',
                        sty$label, ':</strong> ', private$.escapeHtml(notice$message), '</li>'
                    )
                }

                notices_html <- paste0(
                    '<div style="background-color: rgba(138, 155, 172, 0.06); border: 1px solid rgba(138, 155, 172, 0.35); border-radius: 5px; padding: 15px; margin: 10px 0; color: inherit;">',
                    '<ul style="margin-bottom: 0; padding-left: 0; list-style: none;">',
                    items_html,
                    '</ul></div>'
                )

                self$results$notices$setContent(notices_html)
            },

            # Helper method to build dependence explanation content
            .buildDependenceExplanation = function() {
                explanation <- '
<div style="max-width: 800px;">
<h3>Understanding Test Dependence in Diagnostic Testing</h3>

<p>This applies to two tests read <strong>in parallel</strong>: both are performed on the same subject at the same time, before either result is known, and the two results are then combined. For tests performed one after another, where the second is ordered only after a particular first result, use a sequential testing analysis instead \u{2014} the second test\'s sensitivity and specificity there apply to the selected subgroup, not to everyone.</p>

<h4>What is conditional independence vs. dependence?</h4>
<p>Two diagnostic tests are <strong>conditionally independent</strong> if the result of one test does not influence the result of the other test, <em>given the disease status</em>. In other words, within the diseased population, the probability of Test 1 being positive is not affected by knowing the result of Test 2, and vice versa. The same applies within the non-diseased population.</p>

<p>Tests are <strong>conditionally dependent</strong> when the result of one test affects the probability of the other test result, even when we know the patient\'s true disease status.</p>

<h4>Mathematical Formulation</h4>

<p><strong>Independent Tests:</strong> When tests are independent, joint probabilities are simply the product of individual probabilities:</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = P(Test1+ | Disease+) \u{00D7} P(Test2+ | Disease+) = Sens\u{2081} \u{00D7} Sens\u{2082}</li>
  <li>P(Test1+ and Test2+ | Disease\u{2212}) = P(Test1+ | Disease\u{2212}) \u{00D7} P(Test2+ | Disease\u{2212}) = (1\u{2212}Spec\u{2081}) \u{00D7} (1\u{2212}Spec\u{2082})</li>
  <li>P(Test1\u{2212} and Test2\u{2212} | Disease+) = P(Test1\u{2212} | Disease+) \u{00D7} P(Test2\u{2212} | Disease+) = (1\u{2212}Sens\u{2081}) \u{00D7} (1\u{2212}Sens\u{2082})</li>
  <li>P(Test1\u{2212} and Test2\u{2212} | Disease\u{2212}) = P(Test1\u{2212} | Disease\u{2212}) \u{00D7} P(Test2\u{2212} | Disease\u{2212}) = Spec\u{2081} \u{00D7} Spec\u{2082}</li>
</ul>

<p><strong>Dependent Tests:</strong> When tests are dependent, we adjust these probabilities using a correlation parameter (denoted as \u{03C1} or \u{03C8}) that runs from \u{2212}1 through 0 (independence) to +1. Positive values describe tests that tend to err together; negative values describe tests that compensate for each other\'s errors, so that one tends to be positive where the other is negative. Both are permitted here, though the attainable range is narrower than [\u{2212}1, +1] and depends on the sensitivities and specificities entered \u{2014} values outside it are truncated, and the analysis says so:</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = (Sens\u{2081} \u{00D7} Sens\u{2082}) + \u{03C1}\u{1D68}\u{2092}\u{209B} \u{00D7} \u{221A}(Sens\u{2081} \u{00D7} (1\u{2212}Sens\u{2081}) \u{00D7} Sens\u{2082} \u{00D7} (1\u{2212}Sens\u{2082}))</li>
  <li>P(Test1+ and Test2+ | Disease\u{2212}) = ((1\u{2212}Spec\u{2081}) \u{00D7} (1\u{2212}Spec\u{2082})) + \u{03C1}\u{2099}\u{2091}\U{0001D454} \u{00D7} \u{221A}((1\u{2212}Spec\u{2081}) \u{00D7} Spec\u{2081} \u{00D7} (1\u{2212}Spec\u{2082}) \u{00D7} Spec\u{2082})</li>
</ul>

<p>Extreme values are automatically truncated to stay within feasible joint bounds; the realized correlation after truncation is reported.</p>

<p>Note: Similar adjustments are made for the other joint probabilities.</p>

<h4>When to Use Dependent vs. Independent Models</h4>

<p><strong>Independence is the assumption that has to be justified, not the fallback.</strong> It is the optimistic choice: it credits the pair with more combined information than dependent tests actually provide. Use it only when you can point to a positive reason, such as:</p>
<ul>
  <li>The tests measure genuinely different biological phenomena</li>
  <li>They use different specimens, and different mechanisms of measurement</li>
  <li>Paired data from your own or a published series shows no residual correlation within disease status</li>
</ul>

<p><strong>If you simply do not know how the two tests interact, independence is the wrong default.</strong> Tests that share a specimen, a modality, an observer or a biological pathway are dependent unless shown otherwise. Enter a small positive dependence (0.05 to 0.15) and see how far the conclusion moves; if it moves a lot, the honest report gives the range rather than the independent figure. This is why the independence checkbox is off by default.</p>

<p><strong>Use the dependence model when:</strong></p>
<ul>
  <li>Tests measure the same or similar biological phenomena</li>
  <li>Tests are based on the same biological specimen or mechanism</li>
  <li>Previous studies indicate correlation between test results</li>
  <li>Both tests are affected by the same confounding factors</li>
  <li>You have observed that knowing one test result predicts the other</li>
</ul>

<h4>Real-World Examples of Dependent Tests</h4>
<ul>
  <li>Two imaging tests (e.g., MRI and CT) looking at the same anatomical structure</li>
  <li>Two serological tests that detect different antibodies but against the same pathogen</li>
  <li>Tests that may both be affected by the same confounding factor (e.g., inflammation)</li>
  <li>Multiple readings of the same test by different observers</li>
  <li>Two different molecular tests detecting different genes of the same pathogen</li>
</ul>

<h4>Estimating Dependency Parameters</h4>
<p>The conditional dependence parameters (\u{03C1}\u{1D68}\u{2092}\u{209B} for diseased subjects and \u{03C1}\u{2099}\u{2091}\U{0001D454} for non-diseased subjects) ideally should be estimated from paired testing data with known disease status. Positive values between 0 and 0.5 are the common case in practice, with larger values indicating stronger dependence; negative values are unusual but legitimate where the two tests genuinely compensate for one another. When no data is available, sensitivity analyses using a range of plausible values (e.g., 0.05, 0.1, 0.2) can reveal how much dependence affects results.</p>

<h4>Impact of Ignoring Dependence</h4>
<p>Ignoring conditional dependence when it exists overstates how much the second test adds. The <em>direction</em> of the error differs by row, so it is worth being precise rather than saying post-test probabilities are simply "exaggerated":</p>
<ul>
  <li><strong>Both tests positive</strong> \u{2014} assuming independence makes the probability of disease <strong>too high</strong>. Two correlated positives are partly the same evidence counted twice. (Simulation across 400 ordinary clinical parameter sets: this held in 100% of them.)</li>
  <li><strong>Both tests negative</strong> \u{2014} assuming independence makes the probability of disease <strong>too low</strong>, i.e. it overstates how safely the pair rules disease out. This is the error that matters most in screening. (100% of the same sets.)</li>
  <li><strong>Either test positive (the parallel rule)</strong> \u{2014} here independence makes the probability of disease <strong>too low</strong> in the large majority of cases (85%), not too high, because dependence concentrates false positives into the double-positive cell. Do not assume the independent figure is the conservative one for this row.</li>
  <li><strong>One test positive only</strong> \u{2014} the direction is not determined by dependence alone; across the same sets it went each way about half the time, depending on the two operating points.</li>
</ul>
<p>The common thread is an <strong>overly optimistic assessment of what the two tests achieve together</strong>, not a uniform inflation of every number. Note also that this analysis reports no confidence intervals at all: every figure is conditional on the sensitivity, specificity and prevalence you entered being exact.</p>
</div>'
                
                return(explanation)
            }
        )
    )
