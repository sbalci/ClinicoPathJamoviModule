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

            # No nomogram memo. nomogrammer() only assembles a ggplot object, which is
            # microseconds, and the cache was keyed on the three numbers alone -- so a change of
            # jamovi theme (or of the caption) re-used a plot built under the previous theme.
            .notices = NULL,

            # jmvcore pastes a String option between double quotes without escaping, so a test
            # named `A "q"` or `C:\x` exported unparseable syntax (`test1_name = "A "q""`).
            # Same override as decisioncompare's free-text options.
            .sourcifyOption = function(option) {
                value <- option$value
                if (option$name %in% c("test1_name", "test2_name") && is.character(value) &&
                    length(value) == 1L && !identical(value, option$default))
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                super$.sourcifyOption(option)
            },

            .init = function() {
                # Add welcome instructions
                # The leading newline is layout, not message: keeping it out of the msgid
                # stops the catalog entry starting with whitespace.
                instructions <- paste0("\n", .('<div style="max-width: 900px; font-family: sans-serif;">
<h3>Welcome to Co-Testing Analysis</h3>
<p><strong>Purpose:</strong> This analysis evaluates the combined diagnostic performance of two tests applied <strong>in parallel</strong>, accounting for possible dependence between them.</p>

<p><strong>What "in parallel" means here:</strong> both tests are performed on the same subject at the same time, before either result is known, and the two results are then combined into a single conclusion. Neither test gates the other: Test 2 is run whether or not Test 1 came back positive. That is what separates parallel co-testing from <em>sequential</em> (serial) testing, where a second test is ordered only after a particular first result, and where the second test\'s operating characteristics apply to the selected subgroup rather than to everyone. If your tests are run one after the other with the second conditional on the first, this is the wrong analysis \u2014 use <em>Sequential Test Analysis</em> instead.</p>

<p><strong>Choosing between this and Sequential Test Analysis for parallel testing.</strong> Sequential Test Analysis also has a parallel strategy, and for <em>conditionally independent</em> tests the two agree. It computes its combined figures under that independence assumption and warns you in prose when dependence would matter. This analysis is the one that actually models the dependence: if the two tests share a specimen, a modality, an observer or a biological pathway, use this one and enter a dependence parameter.</p>

<p>Because both tests are always performed, each subject falls into exactly one of four result combinations \u2014 both positive, Test 1 only, Test 2 only, both negative \u2014 and the table below gives the post-test probability of disease for each, plus the combined <em>either positive</em> arm used by the parallel decision rule.</p>

<h4>Quick Start Guide</h4>
<ol>
<li><strong>Choose a worked example</strong> (optional): HPV+Pap, PSA+DRE, Troponin+ECG and the rest fill in every parameter for you so you can see the calculation end to end. <strong>They are illustrations, not clinical inputs</strong> \u2014 see the warning below.</li>
<li><strong>Enter your own test parameters</strong>: with <em>Custom values</em> selected, input sensitivity and specificity for each test. While a worked example is selected these boxes are locked, because the example supplies them.</li>
<li><strong>Set disease prevalence</strong>: the pre-test probability of disease in the population you are actually testing \u2014 not the figure quoted in the paper the test was validated in.</li>
<li><strong>Configure test independence</strong>: state whether the two tests are conditionally independent or dependent (see "Understanding Test Dependence" below). This choice can move the answer substantially, so compare both settings.</li>
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
<li><strong>Mammography + ultrasound</strong>: two views of the same tissue, dependence set relatively high in both groups (dependent)</li>
<li><strong>Rapid antigen + PCR</strong>: two assays, one swab, both track viral load (dependent)</li>
<li><strong>Chest radiograph + sputum microscopy</strong>: imaging plus microbiology (dependent)</li>
</ul>

<div style="background-color: rgba(220, 53, 69, 0.12); border-left: 3px solid rgba(220, 53, 69, 0.85); border-radius: 3px; padding: 10px 14px; margin: 12px 0; color: inherit;">
<p style="margin: 0;"><strong>The worked examples are for demonstration only.</strong> Every sensitivity, specificity, prevalence and dependence value in them is a round illustrative number picked to show how the calculation behaves \u2014 how a sensitive test pairs with a specific one, how dependence pulls the answer back. They are <strong>not</strong> pooled estimates from the literature, they are <strong>not</strong> drawn from any guideline, and they have <strong>not</strong> been checked for clinical accuracy. Do not quote them, and do not make any clinical decision from a result computed with them. For real work select <em>Custom values</em> and enter sensitivity and specificity from a validation study in a population like yours, together with your own local prevalence.</p>
</div>

<p><strong>No uncertainty is propagated:</strong> sensitivity, specificity and prevalence are treated as exact. Every probability reported here is conditional on those numbers being right, and carries no confidence interval \u2014 published test performance and local prevalence both vary considerably.</p>

<p><strong>Where the inputs come from matters:</strong> sensitivity and specificity measured in a case-control design (clear-cut cases against healthy controls), in a study that checked only some results against the reference standard, or in a population with a different spectrum of disease are often optimistic (spectrum and verification bias). Take both tests\' sensitivity and specificity, and any dependence value, from the same population and the same reference standard where you can: figures from two separate studies do not describe the pair tested together. Indeterminate or invalid results (an unsatisfactory smear, an invalid PCR) are outside this calculation.</p>

<p><strong>Tip:</strong> Enable "Detailed footnotes" for explanations of each metric. Enable "Fagan nomogram" for a visual of the probability update \u2014 note that the nomogram plots the <em>parallel rule</em> (positive if either test is positive, negative only if both are negative), so its positive arm corresponds to the "Either Test Positive" row of the results table, not to "Both Tests Positive".</p>
</div>'))

                self$results$instructions$setContent(instructions)

                # Initialize tables with row headers
                testParamsTable <- self$results$testParamsTable
                testParamsTable$addRow(rowKey = "test1", values = list(test = .("Test 1")))
                testParamsTable$addRow(rowKey = "test2", values = list(test = .("Test 2")))

                cotestResultsTable <- self$results$cotestResultsTable
                cotestResultsTable$addRow(rowKey = "either_pos",
                                          values = list(scenario = .("Either Test Positive (Parallel Rule)")))
                # Same msgid as .updateCotestResultsTable() uses, so the scaffolded label and the
                # finished one cannot be translated differently and visibly change wording.
                cotestResultsTable$addRow(rowKey = "test1_pos",
                                          values = list(scenario = sprintf(.("%s Positive Only"), .("Test 1"))))
                cotestResultsTable$addRow(rowKey = "test2_pos",
                                          values = list(scenario = sprintf(.("%s Positive Only"), .("Test 2"))))
                cotestResultsTable$addRow(rowKey = "both_pos",
                                          values = list(scenario = .("Both Tests Positive")))
                cotestResultsTable$addRow(rowKey = "both_neg",
                                          values = list(scenario = .("Both Tests Negative")))
            },

            .run = function() {
                # Initialize notices collection for user-visible validation feedback
                private$.notices <- list()

                # Names for the two tests. Every panel, both tables and the pasteable summary
                # used to say only "Test 1" / "Test 2", so a manuscript sentence never said what
                # was tested. Defaults are exactly the old strings, so nothing moves unless the
                # user names them.
                t1_name <- private$.testLabel(self$options$test1_name, .("Test 1"))
                t2_name <- private$.testLabel(self$options$test2_name, .("Test 2"))
                # A worked example names its own tests, unless the user typed something.
                # (events.js writes the same names into the boxes, so the GUI agrees.)
                if (self$options$preset != "custom") {
                    pv <- private$.getPresetValues(self$options$preset)
                    if (!is.null(pv)) {
                        if (!nzchar(trimws(self$options$test1_name)) && !is.null(pv$test1_name))
                            t1_name <- pv$test1_name
                        if (!nzchar(trimws(self$options$test2_name)) && !is.null(pv$test2_name))
                            t2_name <- pv$test2_name
                    }
                }

                # Table cells and footnotes must not carry an entity: "Ki-67 <10%" escaped read
                # "Ki-67 &lt;10%" in both tables and in any table exported from them. But the
                # client passes string cells through its HTML sanitiser (measured 2026-09-27 against
                # the 28.3 client), so a "<" followed by a letter opens a tag and "HPV <LOD" renders
                # as "HPV ". .cellText() breaks only that form; "Ki-67 <10%" is untouched. Escape
                # fully only where this file builds HTML itself (independent review 2026-10-05, IR-09).
                t1_cell <- private$.cellText(t1_name)
                t2_cell <- private$.cellText(t2_name)
                t1_html <- private$.escapeHtml(t1_name)
                t2_html <- private$.escapeHtml(t2_name)

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
                        # A msgid must be ONE string literal, so the paste0() fragments are
                        # merged into a single .() literal. Rendered text is unchanged.
                        private$.addNotice(sprintf(
                            .("Worked example in use: %1$s. %2$s These are round illustrative numbers chosen to demonstrate the calculation, not pooled estimates from the literature and not values to use for patient care \u2014 select Custom values and enter sensitivity, specificity and prevalence from your own population before acting on any result below."),
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
                                    .("Internal: could not read the default for '%s', so the analysis cannot report whether a worked example replaced a value you supplied. Please report this."),
                                    nm), "warning")
                                return(NULL)
                            }
                            o$default
                        }
                        # The user never sees "test1_sens"; they see the control's label. Use the
                        # .a.yaml titles so the warning names something they can act on.
                        opt_titles <- c(
                            test1_sens   = .("Test 1 sensitivity"),
                            test1_spec   = .("Test 1 specificity"),
                            test2_sens   = .("Test 2 sensitivity"),
                            test2_spec   = .("Test 2 specificity"),
                            prevalence   = .("Disease prevalence"),
                            cond_dep_pos = .("Correlation (phi) among subjects with disease"),
                            cond_dep_neg = .("Correlation (phi) among subjects without disease"))
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
                            differing <- c(differing, sprintf(.("%1$s (%2$s given, %3$s used)"),
                                                              opt_titles[[nm]], format(given), format(used)))
                        }

                        # `indep` is handled WITHOUT the schema-default test. It is a Bool
                        # defaulting to FALSE, so half of all deliberate user choices are
                        # indistinguishable from "untouched" -- and it is the one option that
                        # selects which of the two models runs. Before this,
                        # cotest(preset = "psa_dre", indep = FALSE, cond_dep_pos = 0.30) ran the
                        # INDEPENDENT model and said nothing at all.
                        # Only a value that differs from the default is known to be "supplied": a
                        # plain cotest(preset = "psa_dre") leaves indep at its default, and the
                        # notice used to tell that caller it had replaced "values you supplied".
                        indep_given <- isTRUE(self$options$indep)
                        if (!identical(indep_given, isTRUE(indep))) {
                            if (identical(indep_given, isTRUE(opt_default("indep")))) {
                                private$.addNotice(sprintf(
                                    .("%1$s was %2$s (its default, or as you set it) and the worked example changed it to %3$s, which selects which model is fitted. To fit the other model, set the worked example to Custom values."),
                                    .("Assume conditional independence"), indep_given, isTRUE(indep)), "warning")
                            } else {
                                differing <- c(differing,
                                               sprintf(.("%1$s (%2$s given, %3$s used \u2014 this selects which model is fitted)"),
                                                       .("Assume conditional independence"),
                                                       indep_given, isTRUE(indep)))
                            }
                        }
                        if (length(differing) > 0) {
                            private$.addNotice(sprintf(
                                .("The worked example replaced values you supplied: %s. A worked example sets every parameter; to use your own numbers set the worked example to Custom values."),
                                paste(differing, collapse = "; ")), "warning")
                        }
                        # A typed name wins over the example's own (so the GUI boxes agree), which
                        # labels the example's numbers with the user's test (IR-16). Say so.
                        # events.js writes the example's OWN names into the boxes, in English, so
                        # a name equal to that slot's example name (or, in a translated session, to
                        # its English source) is not the user's: without this, every worked example
                        # in the GUI warned that the numbers "do not describe those tests".
                        typed <- trimws(c(self$options$test1_name, self$options$test2_name))
                        ex_names <- tolower(c(preset_values$test1_name, preset_values$test2_name))
                        is_example <- tolower(typed) == ex_names |
                            tolower(vapply(typed, self$options$translate, "")) == ex_names
                        typed <- typed[nzchar(typed) & !is_example]
                        if (length(typed) > 0)
                            private$.addNotice(sprintf(
                                .("You named the tests %1$s, but every value below comes from the worked example (%2$s). The labels follow your names; the numbers do not describe those tests."),
                                paste(typed, collapse = " / "), preset_values$label), "warning")
                    }
                }

                # Enhanced input validation with specific guidance
                private$.validateInputParameters(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                 t1_name, t2_name)

                # Calculate pretest odds early (needed for "Either Test Positive" calculation)
                pretest_odds <- prevalence / (1 - prevalence)

                # Calculate likelihood ratios with numerical stability
                test1_plr <- private$.calculateLikelihoodRatio(test1_sens, (1 - test1_spec), sprintf(.("%s Positive LR"), t1_name))
                test1_nlr <- private$.calculateLikelihoodRatio((1 - test1_sens), test1_spec, sprintf(.("%s Negative LR"), t1_name))
                test2_plr <- private$.calculateLikelihoodRatio(test2_sens, (1 - test2_spec), sprintf(.("%s Positive LR"), t2_name))
                test2_nlr <- private$.calculateLikelihoodRatio((1 - test2_sens), test2_spec, sprintf(.("%s Negative LR"), t2_name))

                # Update test parameters table using helper method
                private$.updateTestParametersTable(test1_sens, test1_spec, test1_plr, test1_nlr,
                                                   test2_sens, test2_spec, test2_plr, test2_nlr,
                                                   t1_cell, t2_cell)

                # Calculate post-test probabilities for different scenarios
                results <- private$.calculatePostTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec, 
                                                                   prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                                   test1_plr, test1_nlr, test2_plr, test2_nlr,
                                                                   t1_name, t2_name, t1_html, t2_html)
                
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

                lr_either_pos <- private$.calculateLikelihoodRatio(p_either_pos_D, p_either_pos_nD, .("Either Positive LR"))
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
                                                  t1_cell, t2_cell, preset, indep)

                # Add footnotes if requested
                if (self$options$fnote) {
                    private$.addFootnotes(t1_cell, t2_cell)
                }

                # Always populated, for both models. This panel used to be written only in the
                # dependent branch while its .r.yaml `visible:` expression tested the *option*
                # `indep` -- which a worked example can override -- so it could show an empty box
                # for an independent fit, or hide the dependence disclosure for a dependent one.
                # Writing it unconditionally removes the mismatch instead of restating it.
                self$results$dependenceInfo$setContent(dependence_info)

                # Every row is described from its COMBINED likelihood ratio, on one prevalence-free
                # scale, and worded as a reading aid rather than a verdict. The probability-ratio
                # bands used before cannot exceed 1/prevalence, so at a prevalence of 92% a rise to
                # 99.9% (LR 69) read "minimal change", while the Both+ row alone used LR bands
                # (validation 2026-10-05, VAL-cotest-07 / -12). The parallel rule's own positive
                # arm was missing from both the panel and the copy-ready sentence (VAL-cotest-11).
                lr_of <- function(odds) odds / pretest_odds
                explanation <- sprintf(
                    .("<p><strong>Reading the results (a rough guide):</strong></p>
                <p>Disease prevalence (pre-test probability): <strong>%1$s</strong></p>
                <p><strong>Either test positive (parallel rule):</strong> %2$s probability (%3$s) - %4$s</p>
                <p><strong>Both tests positive:</strong> %5$s probability (%6$s) - %7$s</p>
                <p><strong>Both tests negative:</strong> %8$s probability (%9$s) - %10$s</p>
                <p><strong>Single positive test:</strong></p>
                <ul>
                <li>%11$s positive only: <strong>%12$s</strong> - %13$s</li>
                <li>%14$s positive only: <strong>%15$s</strong> - %16$s</li>
                </ul>
                <p style='font-size: 90%%;'>These descriptions apply a conventional rule of thumb for likelihood ratios (Jaeschke et al. 1994) to help read the table. They are a guide, not a clinical judgement; the numbers are in the table above.</p>
                <div style='background-color: rgba(33, 152, 255, 0.07); padding: 10px; border-radius: 5px; margin-top: 15px; color: inherit;'>
                <p><strong>Copy-ready summary:</strong></p>
                <p style='font-family: monospace; font-size: 12px;'>%17$s</p>
                </div>"),
                    private$.fmtPct(prevalence),
                    private$.fmtPct(postest_prob_either), private$.fmtChange(rel_prob_either),
                    private$.describeShift(lr_of(postest_odds_either)),
                    private$.fmtPct(postest_prob_both), private$.fmtChange(rel_prob_both),
                    private$.describeShift(lr_of(postest_odds_both)),
                    private$.fmtPct(postest_prob_both_neg), private$.fmtChange(rel_prob_both_neg),
                    private$.describeShift(lr_of(postest_odds_both_neg)),
                    t1_html, private$.fmtPct(postest_prob_t1), private$.describeShift(lr_of(postest_odds_t1)),
                    t2_html, private$.fmtPct(postest_prob_t2), private$.describeShift(lr_of(postest_odds_t2)),
                    private$.generateReportSentence(test1_sens, test1_spec, test2_sens, test2_spec,
                                                   prevalence, postest_prob_both, rel_prob_both,
                                                   postest_prob_both_neg, rel_prob_both_neg,
                                                   preset, indep, phi_d_used, phi_n_used,
                                                   cond_dep_pos, cond_dep_neg, t1_html, t2_html,
                                                   postest_prob_either, rel_prob_either,
                                                   dep_default = !indep && identical(preset, "custom") &&
                                                       private$.isDefault("cond_dep_pos") && private$.isDefault("cond_dep_neg"))
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

            # `ggtheme = NULL`, not a bare parameter: a guard on a missing promise forces it and
            # errors when the renderer is called outside jamovi's engine.
            .plot1 = function(image1, ggtheme = NULL, ...) {
                plotData <- image1$state

                # .prepareFaganPlotData() already explained the reason in a notice.
                if (is.null(plotData) || !isTRUE(plotData$drawable))
                    return(FALSE)

                p <- nomogrammer(
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
                    Verbose = FALSE,
                    # jamovi's global theme, so the nomogram stops being a white rectangle in a
                    # dark results pane. nomogrammer() layers its structural theme() (hidden x
                    # axis, no grid, no legend) ON TOP of it, so the layout survives.
                    Theme = ggtheme,
                    # nomogrammer() is a file-level function: `.()` inside it throws "object
                    # 'self' not found". The figure's own text is therefore translated HERE and
                    # passed in, otherwise it is the one result item that bypasses translation
                    # entirely -- and it travels into slide decks as a PNG.
                    Title = .("Fagan Nomogram"),
                    PriorLabel = .("Prior\nProb.\n(%)"),
                    PosteriorLabel = .("Posterior\nProb.\n(%)"),
                    DetailLabels = c(prevalence = .("Prevalence"), plr = .("PLR"), nlr = .("NLR"),
                                     post_pos = .("Post(+)"), post_neg = .("Post(-)"))
                )

                # After the theme, so it is not replaced by it. The colour is left to the theme:
                # a pinned "grey30" was the lowest-contrast element on the figure at export
                # resolution and unreadable on a dark background.
                if (!is.null(plotData$Caption) && nzchar(plotData$Caption))
                    p <- p + ggplot2::labs(caption = plotData$Caption) +
                             ggplot2::theme(plot.caption = ggplot2::element_text(
                                 hjust = 0, size = 8))
                print(p)
                TRUE
            },

            # Enhanced input validation with specific guidance
            .validateInputParameters = function(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                t1_name = "Test 1", t2_name = "Test 2") {
                # No range guards here. Every one of these options is bounded in cotest.a.yaml
                # (sens/spec 0.01-0.99, prevalence 0.001-0.999, dependence -1 to 1) and
                # OptionNumber$check() REJECTS an out-of-range value -- it does not clamp -- before
                # .run() is reached, so the six jmvcore::reject() calls that used to stand here
                # could never fire and only put six dead msgids in the catalog. The calibration
                # guidance they carried is now a note on the Test Parameters table, where it is
                # actually read (see .updateTestParametersTable).

                # The narrower, marginal-dependent Frechet range is deliberately NOT enforced
                # either: a value inside -1 to 1 but unattainable with the entered sensitivities
                # is truncated to its bound and reported (.calculateDependentTestProbabilities),
                # so e.g. rho = -0.5 with Sens 0.80 / 0.70 (attainable range -0.33 to +0.76) is a
                # legitimate request, not an error.

                # Clinical validity checks. A test BELOW the chance diagonal (Se + Sp < 1) is not a
                # weak test: a positive result makes disease less likely, which is almost always a
                # reversed entry. It used to be called "close to the chance diagonal" (IR-06).
                # Named as in the tables: the notice used to say "Test 1" beside a row labelled "HPV".
                test_names <- c(t1_name, t2_name)
                sum_ss <- c(test1_sens + test1_spec, test2_sens + test2_spec)
                for (k in 1:2) {
                    if (sum_ss[k] < 1) {
                        private$.addNotice(sprintf(
                            .("%s is below the chance diagonal (sensitivity plus specificity below 1): a positive result makes disease less likely, not more. Check whether its sensitivity and specificity, or its positive and negative categories, have been entered the wrong way round."),
                            test_names[k]), "warning")
                    } else if (sum_ss[k] < 1.1) {
                        private$.addNotice(sprintf(
                            .("%s has low discriminatory power (sensitivity plus specificity below 1.1, i.e. a Youden index below 0.1). Check its likelihood ratios in the Test Parameters table: an operating point close to the chance diagonal can still give an informative LR+ or LR- when sensitivity and specificity are very unequal."),
                            test_names[k]), "warning")
                    }
                }

                # Extreme prevalence. The old guard tested `< 0.001`, which is the option's own
                # minimum, so it could never be TRUE and the most unstable reachable point (0.001)
                # warned about nothing. Both tails now warn inside the reachable range.
                if (prevalence < 0.05) {
                    # Worded as a check, not as a claim that prevalence "drives" the result: post-test
                    # odds are prior odds times the LR, and a strong pair can overwhelm a 4% prior
                    # (99.7% after two positives) or a 96% one (0.3% after two negatives) (IR-03).
                    private$.addNotice(.("Low prevalence (below 5%): check that this is the pre-test probability in the population you are actually testing. At a prevalence this low a small absolute change (for example from 1% to 2%) about doubles the pre-test odds and moves every post-test probability with it, so re-run with a plausible range before acting on the numbers."), "warning")
                } else if (prevalence > 0.95) {
                    private$.addNotice(.("Very high prevalence (above 95%): check that this is the pre-test probability in the population you are actually testing. At a prevalence this high a small absolute change (for example from 98% to 99%) about doubles the pre-test odds and moves every post-test probability with it, so re-run with a plausible range before acting on the numbers."), "warning")
                } else if (prevalence > 0.5) {
                    private$.addNotice(.("High prevalence (above 50%) detected. Ensure this reflects your actual clinical population."), "info")
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
                        .("%s is undefined: with the current parameters this combination of test results has probability zero in both the diseased and the non-diseased group, so it cannot occur at all and no post-test probability exists for it. That row is left blank."),
                        scenario_name), "warning")
                    return(NA_real_)
                }

                if (den_zero) {
                    private$.addNotice(sprintf(
                        .("%s is infinite: with the current parameters this combination of test results cannot occur in a subject without disease, so its post-test probability is 1 by construction rather than estimated. Move the conditional dependence toward 0 if that is not intended."),
                        scenario_name), "warning")
                    return(Inf)
                }

                if (num_zero) {
                    private$.addNotice(sprintf(
                        .("%s is zero: with the current parameters this combination of test results cannot occur in a subject with disease, so its post-test probability is 0 by construction rather than estimated. Move the conditional dependence toward 0 if that is not intended."),
                        scenario_name), "warning")
                    return(0)
                }

                result <- numerator / denominator

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
                    jmvcore::reject(.("{ctx} resulted in a non-finite probability."),
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
                        .("The joint probabilities for %1$s are not all valid probabilities (%2$s). Review the dependence parameters."),
                        label, paste(sprintf("%.4f", cells), collapse = ", ")), "warning")
                    return(invisible(FALSE))
                }

                # Kept for completeness even though the caller derives the fourth cell by
                # subtraction, which makes this true by construction today.
                if (abs(sum(cells) - 1) > 1e-6) {
                    private$.addNotice(sprintf(
                        .("The joint probabilities for %1$s sum to %2$.6f rather than 1. Review the dependence parameters."),
                        label, sum(cells)), "warning")
                    return(invisible(FALSE))
                }

                implied1 <- p_both + p_first_only
                implied2 <- p_both + p_second_only
                if (abs(implied1 - marginal1) > 1e-6 || abs(implied2 - marginal2) > 1e-6) {
                    private$.addNotice(sprintf(
                        .("The joint probabilities for %1$s do not add back up to the test parameters entered (they imply %2$.4f and %3$.4f, against %4$.4f and %5$.4f). Review the dependence parameters."),
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

                # The orientation that used to sit in unreachable reject() messages. Worded as a
                # check to make, not as a fact about diagnostic tests: the old "most tests fall
                # between 0.60 and 0.95" bands had no source (validation 2026-10-05, VAL-cotest-12).
                testParamsTable$setNote("typical_ranges", .("Worth checking: if a test's sensitivity plus specificity is below 1, a positive result makes disease less likely, which usually means its sensitivity and specificity (or its positive and negative categories) have been entered the wrong way round. Prevalence should be the pre-test probability in the population you are testing, not the figure from the study that validated the test."))

                # LR+ and LR- are the two headings most often misread, and their definitions
                # used to sit behind the off-by-default "Detailed footnotes" checkbox, so the
                # default run showed seven column headings with no definition of any of them.
                # The bands are a convention (Jaeschke et al. 1994; Deeks and Altman 2004 give only
                # the outer 10 / 0.1 band), so they are named as one rather than stated as fact.
                testParamsTable$setNote("lr_defs", .("LR+ is how many times more likely a positive result is in someone with the disease than in someone without it; LR- is the same ratio for a negative result. A common rule of thumb (Jaeschke et al. 1994) reads values above 10 or below 0.1 as large shifts in the probability of disease, 5 to 10 or 0.1 to 0.2 as moderate, 2 to 5 or 0.2 to 0.5 as small, and 0.5 to 2 as little change; it is a convention to help reading, not a threshold."))

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
                                                       test1_plr, test1_nlr, test2_plr, test2_nlr,
                                                       t1_name = "Test 1", t2_name = "Test 2",
                                                       t1_html = t1_name, t2_html = t2_name) {
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

                    # One .() literal per message: a msgid cannot be assembled by paste0().
                    dependence_info <- .("<p>The two tests are run in parallel on the same subject and are assumed to be <strong>conditionally independent</strong>: within the diseased group, and again within the non-diseased group, the result of one test carries no information about the other, so each joint probability is the plain product of the two tests' sensitivities (or false-positive rates).</p><p>If the two tests in fact tend to err together \u2014 same specimen, same operator, same biology \u2014 independence usually overstates how much the pair adds, and which rows move up or down depends on where the dependence lies (see <em>Understanding Test Dependence</em>). Clear <em>Assume the two tests are conditionally independent</em> and enter a dependence parameter to see how far the conclusion moves.</p>")
                } else {
                    # Dependent tests scenario with numerical stability
                    dep_results <- private$.calculateDependentTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec,
                                                                                cond_dep_pos, cond_dep_neg, pretest_odds,
                                                                                t1_name, t2_name, t1_html, t2_html)
                    
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
                                                            cond_dep_pos, cond_dep_neg, pretest_odds,
                                                            t1_name = "Test 1", t2_name = "Test 2",
                                                            t1_html = t1_name, t2_html = t2_name) {
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
                        .("The dependence parameter for the diseased group (%1$.2f) is not attainable with these sensitivities, so the joint positive probability was truncated from %2$.4f to its bound %3$.4f. The model actually fitted is therefore not the one you specified, and at least one combination of results becomes impossible in that group. Move the value toward 0 until this note disappears; the realized correlation is reported in the Test Dependence panel."),
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
                        .("The dependence parameter for the non-diseased group (%1$.2f) is not attainable with these specificities, so the joint false-positive probability was truncated from %2$.4f to its bound %3$.4f. The model actually fitted is therefore not the one you specified, and at least one combination of results becomes impossible in that group. Move the value toward 0 until this note disappears; the realized correlation is reported in the Test Dependence panel."),
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
                    test1_sens, test2_sens, .("subjects with disease")
                )
                private$.validateJointDistribution(
                    p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD,
                    fp_test1, fp_test2, .("subjects without disease")
                )

                # Calculate likelihood ratios with stability checks
                # Named as the results table names its rows, so a notice points at a row the user
                # can find (raw names: .displayNotices() escapes every message).
                lr_t1_only <- private$.calculateLikelihoodRatio(p_t1_only_D, p_t1_only_nD, sprintf(.("%s Positive Only LR"), t1_name))
                lr_t2_only <- private$.calculateLikelihoodRatio(p_t2_only_D, p_t2_only_nD, sprintf(.("%s Positive Only LR"), t2_name))
                lr_both_pos <- private$.calculateLikelihoodRatio(p_both_pos_D, p_both_pos_nD, .("Both Positive LR"))
                lr_both_neg <- private$.calculateLikelihoodRatio(p_both_neg_D, p_both_neg_nD, .("Both Negative LR"))

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
                    .("<p>Tests are modeled with conditional dependence:<br>
                Dependence for subjects with disease: %1$.2f<br>
                Dependence for subjects without disease: %2$.2f<br>
                Realized phi (disease): %3$s<br>
                Realized phi (no disease): %4$s<br>
                <span style='font-size: 90%%;'>(Phi is the correlation between the two test results
                actually achieved by the fitted model, on a \u22121 to +1 scale, where 0 is
                conditional independence. It differs from the value you entered when that value
                was not attainable given the sensitivities and specificities supplied.)</span></p>
                <p>Joint probabilities after accounting for dependence:<br>
                P(%13$s+, %14$s+ | Disease+): %5$.4f<br>
                P(%13$s+, %14$s- | Disease+): %6$.4f<br>
                P(%13$s-, %14$s+ | Disease+): %7$.4f<br>
                P(%13$s-, %14$s- | Disease+): %8$.4f<br>
                P(%13$s+, %14$s+ | Disease-): %9$.4f<br>
                P(%13$s+, %14$s- | Disease-): %10$.4f<br>
                P(%13$s-, %14$s+ | Disease-): %11$.4f<br>
                P(%13$s-, %14$s- | Disease-): %12$.4f</p>"),
                    cond_dep_pos, cond_dep_neg,
                    ifelse(is.na(phi_d), "NA", sprintf("%.2f", phi_d)),
                    ifelse(is.na(phi_n), "NA", sprintf("%.2f", phi_n)),
                    p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D,
                    p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD,
                    t1_html, t2_html
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
                    list(key = "either_pos", scenario = .("Either Test Positive (Parallel Rule)"), postProb = postest_prob_either,
                         relativeProbability = rel_prob_either, orValue = postest_odds_either),
                    list(key = "test1_pos", scenario = sprintf(.("%s Positive Only"), t1_name), postProb = postest_prob_t1,
                         relativeProbability = rel_prob_t1, orValue = postest_odds_t1),
                    list(key = "test2_pos", scenario = sprintf(.("%s Positive Only"), t2_name), postProb = postest_prob_t2,
                         relativeProbability = rel_prob_t2, orValue = postest_odds_t2),
                    list(key = "both_pos", scenario = .("Both Tests Positive"), postProb = postest_prob_both,
                         relativeProbability = rel_prob_both, orValue = postest_odds_both),
                    list(key = "both_neg", scenario = .("Both Tests Negative"), postProb = postest_prob_both_neg,
                         relativeProbability = rel_prob_both_neg, orValue = postest_odds_both_neg)
                )

                # Every input is a point estimate the user typed in, so every number in this table
                # is conditional on those values being exact. That limitation belongs next to the
                # numbers, not only in the welcome panel, which a user can collapse.
                cotestResultsTable$setNote("fixed_inputs", .("Sensitivity, specificity and prevalence are treated as exact. These post-test probabilities therefore carry <i>no</i> confidence interval and do not reflect sampling uncertainty in the values entered \u2014 published test performance estimates and local prevalence both vary."))

                # A worked example's numbers must not leave this table looking like estimates.
                # Without this the table is byte-identical between preset = "custom" and a
                # demonstration run, so an exported table carries no marking at all.
                if (!identical(preset, "custom")) {
                    cotestResultsTable$setNote("demo", .("<b>Demonstration only.</b> These figures come from a built-in worked example: round illustrative sensitivity, specificity, prevalence and dependence values chosen to show how the calculation behaves. They are not pooled literature estimates, are not taken from any guideline, and must not be used for patient care."))
                    self$results$testParamsTable$setNote("demo", .("<b>Demonstration only.</b> Illustrative test performance from a built-in worked example, not values for clinical use."))
                }

                # Under a dependent model the combined likelihood ratio is NOT the product of the
                # two marginal LRs printed in the Test Parameters table above. A reader who
                # multiplies them to transport the result to another prevalence overstates the
                # evidence roughly two-fold at the shipped hpv_pap example.
                if (!isTRUE(indep)) {
                    self$results$testParamsTable$setNote("marginal_only", .("These likelihood ratios describe each test <i>on its own</i>. Because the two tests are modelled as conditionally dependent, the combined likelihood ratio is <b>not</b> their product \u2014 use the Post-test Odds column of the Co-Testing Results table instead."))
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
                            sprintf(.("Not an estimate: with these parameters this result combination cannot occur in one of the two disease groups, so the probability is %s by construction. This follows from the conditional dependence being at, or truncated to, the limit the entered sensitivities and specificities allow \u2014 move it toward 0 to leave this regime."),
                                    if (is.infinite(odds)) "1" else "0"))
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
                    sprintf(.("Proportion of diseased patients correctly identified by %s"), t1_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "spec",
                    sprintf(.("Proportion of non-diseased patients correctly identified by %s"), t1_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "plr",
                    .("Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients")
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "nlr",
                    .("Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients")
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "sens",
                    sprintf(.("Proportion of diseased patients correctly identified by %s"), t2_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "spec",
                    sprintf(.("Proportion of non-diseased patients correctly identified by %s"), t2_name)
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "plr",
                    .("Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients")
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "nlr",
                    .("Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients")
                )

                # Results footnotes
                # "either_pos" included: it is the headline row for parallel co-testing and was
                # the only one of the five left without footnotes.
                for (row_key in c("either_pos", "test1_pos", "test2_pos", "both_pos", "both_neg")) {
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "postProb",
                        .("Probability of disease after obtaining this test result combination")
                    )
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "relativeProbability",
                        .("How many times more (or less) likely disease is after testing compared to before testing")
                    )
                    # orValue had no explanation in ANY option state.
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "orValue",
                        .("Post-test odds: probability of disease divided by probability of no disease. Odds of 1 mean a 50% probability; odds of 9 mean 90%.")
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
                    reason <- .("The Fagan nomogram was not drawn because one of the combined likelihood ratios is not a finite number with these parameters: a combination of results is impossible in one disease group because the dependence is at, or was truncated to, the limit the entered values allow (see the notice above). Move the dependence toward 0.")
                } else if (lr_positive_rule < 1) {
                    reason <- sprintf(.("The Fagan nomogram was not drawn because the positive-rule likelihood ratio is %.2f, which is below 1. A Fagan nomogram assumes a positive result raises the probability of disease; here it lowers it, which means the tests as specified perform worse than chance. Check the sensitivity and specificity values you entered."), lr_positive_rule)
                } else if (lr_negative_rule <= 0) {
                    reason <- .("The Fagan nomogram was not drawn because the negative-rule likelihood ratio is zero, which cannot be placed on the nomogram's logarithmic axis: two negative results are impossible in subjects with the disease because the dependence is at, or was truncated to, the limit the entered values allow (see the notice above). Move the dependence toward 0.")
                } else if (abs(lr_positive_rule - lr_negative_rule) < .Machine$double.eps ||
                           isTRUE(all.equal(lr_positive_rule, lr_negative_rule))) {
                    # nomogrammer() stop()s on equal ratios ("PLR and NLR cannot be equal"). This
                    # happens whenever BOTH tests sit exactly on the chance diagonal
                    # (sensitivity = 1 - specificity for each): the parallel rule then gives
                    # LR+ = LR- = 1. Without this branch drawable stayed TRUE, no notice was
                    # written, and the renderer surfaced nomogrammer's raw internal error.
                    # all.equal() also catches the near-equal case (|LR+ - LR-| ~ 4e-16), which
                    # squeaked past the eps test and drew a nomogram for two worthless tests.
                    reason <- sprintf(.("The Fagan nomogram was not drawn because the two tests together carry no information: the positive-rule and negative-rule likelihood ratios are both %.2f, so neither a positive nor a negative combined result changes the probability of disease. Each test is sitting on the chance diagonal (sensitivity = 1 - specificity). Check the sensitivity and specificity values you entered."),
                        lr_positive_rule)
                }

                if (!is.null(reason)) {
                    private$.addNotice(reason, "warning")
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
                        .("Parallel rule: positive if either test is positive, negative only if both are negative."),
                        if (!identical(preset, "custom") && !is.null(preset_label))
                            paste0("  |  ", sprintf(.("DEMONSTRATION ONLY \u2014 %s, illustrative values, not for clinical use"),
                                                    preset_label))
                        else "")
                )

                image1 <- self$results$plot1
                image1$setState(plotData)
            },

            # Clinical interpretation helpers
            # sprintf("%.1f%%", NA) renders the literal "NA%". A post-test probability is NA
            # whenever a result combination is impossible in BOTH disease groups -- correct, and
            # explained in a notice -- so the prose has to say that rather than print "NA%".
            # Returns the name AS TYPED (trimmed), or the positional label when blank. It is
            # deliberately NOT escaped here: the same string feeds plain-text table cells and
            # HTML panels, and escaping at the source corrupted the cells. The caller escapes.
            .testLabel = function(value, fallback) {
                if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) return(fallback)
                trimws(value)
            },

            # One decimal place printed P = 5.9e-5 as "0.0%" and 0.9999999 as "100.0%", so a
            # copy-ready rule-out sentence asserted zero residual risk (VAL-cotest-09). Between 1%
            # and 99% the output is unchanged; nearer 0 or 1, two significant digits of p (or of
            # 1 - p) are kept, as nomogrammer()'s p2percent already does for the plot. An exact
            # 0 or 1 is structural (a truncated dependence) and the row's footnote says so.
            .fmtPct = function(p) {
                if (length(p) != 1L || is.na(p)) return(.("not estimable"))
                if (p <= 0) return("0%")
                if (p >= 1) return("100%")
                if (p < 1e-5) return("<0.001%")
                if (p > 1 - 1e-5) return(">99.999%")
                tail <- min(p, 1 - p)
                dp <- if (tail >= 0.01) 1L else as.integer(ceiling(-log10(100 * tail))) + 1L
                sprintf("%.*f%%", dp, 100 * p)
            },
            .fmtRatio = function(r) if (length(r) != 1L || is.na(r)) .("not estimable") else sprintf("%.2fx", r),
            # An entered value as typed: 0.925 -> "92.5%", not "%.0f"'s half-to-even "92%" (IR-05).
            .fmtInput = function(x) paste0(trimws(formatC(100 * x, digits = 4, format = "fg")), "%"),
            # TRUE when the option still holds its schema default (read from jmvcore's option
            # object, as the worked-example override check does); FALSE if that cannot be read.
            .isDefault = function(nm) {
                o <- self$options$.__enclos_env__$private[[paste0("..", nm)]]
                !is.null(o) && isTRUE(all.equal(self$options[[nm]], o$default))
            },
            # Three significant digits: 9.3, 0.0585, 120. Never "0.06x" for 0.0585 or "1.0x" for 1.03.
            # trimws(): format = "fg" left-pads to `digits` characters (" 9.3").
            .fmtNum = function(x) trimws(formatC(x, digits = 3, format = "fg")),

            # Direction of effect, chosen from the ratio instead of asserted. The templates used
            # to hard-code "increase" and "reduced to"; neither ratio is constrained, so for any
            # test pair with sensitivity + specificity < 1 (an inverted operating point, or
            # sensitivity and specificity entered the wrong way round) both clauses stated the
            # opposite of the numbers printed beside them. A ratio that displays as 1 is no
            # change at all: ">= 1" printed "1.0x increase" for two tests on the chance diagonal
            # (VAL-cotest-10).
            .fmtChange = function(r) {
                if (length(r) != 1L || is.na(r)) return(.("not estimable"))
                if (private$.fmtNum(r) == "1") return(.("about the same as the prevalence"))
                if (r > 1) sprintf(.("%sx increase"), private$.fmtNum(r)) else sprintf(.("%sx of prevalence, a decrease"), private$.fmtNum(r))
            },

            # A reading aid for one row, from its COMBINED likelihood ratio. The bands are the
            # conventional ones (Jaeschke et al. 1994: 10 / 5 / 2 and their reciprocals; Deeks and
            # Altman 2004 give the outer 10 / 0.1 band), mirrored so a ratio below 1 is read the
            # same way as one above it, and phrased as a rule of thumb because that is all they
            # are. They replace two interpreters that disagreed: LR bands for the Both+ row only,
            # and bands on post-test/prevalence for the rest, which cannot exceed 1/prevalence
            # and so read a rise from 92% to 99.9% as "minimal change" (VAL-cotest-07, -12).
            .describeShift = function(lr) {
                if (length(lr) != 1L || is.na(lr)) return(.("not estimable"))
                if (is.infinite(lr))
                    return(.("certain by construction under the dependence entered, not an estimate (see the table footnote)"))
                if (lr == 0)
                    return(.("excluded by construction under the dependence entered, not an estimate (see the table footnote)"))
                shift <- if (lr > 10) .("by the usual rule of thumb, a large shift towards disease")
                    else if (lr > 5) .("by the usual rule of thumb, a moderate shift towards disease")
                    else if (lr > 2) .("by the usual rule of thumb, a small shift towards disease")
                    else if (lr >= 0.5) .("by the usual rule of thumb, little shift either way")
                    else if (lr >= 0.2) .("by the usual rule of thumb, a small shift away from disease")
                    else if (lr >= 0.1) .("by the usual rule of thumb, a moderate shift away from disease")
                    else .("by the usual rule of thumb, a large shift away from disease")
                sprintf(.("combined likelihood ratio %1$s; %2$s"), private$.fmtNum(lr), shift)
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
                                               t1_name = "Test 1", t2_name = "Test 2",
                                               postest_prob_either = NA, rel_prob_either = NA,
                                               dep_default = FALSE) {
                marker <- if (!identical(preset, "custom"))
                    paste0("[", .("DEMONSTRATION ONLY \u2014 illustrative values, not for clinical use"), "] ") else ""

                # REALIZED correlation, not the requested one. When the requested rho is outside
                # the Frechet-feasible range the joint cell is clamped and the model actually
                # fitted uses a different correlation -- which the Test Dependence panel already
                # reports. Quoting the requested value here put a number in a pasteable methods
                # sentence that described a model nobody ran (0.50 quoted against 0.35 fitted).
                fmt2 <- function(x) if (length(x) != 1L || is.na(x)) .("unavailable") else sprintf("%.2f", x)
                model <- if (isTRUE(indep)) {
                    paste0(" ", .("The calculation assumes both tests are performed in parallel and are conditionally independent."))
                } else {
                    # Collected, not if/else-if. When BOTH parameters were truncated the old form
                    # named only the diseased one, so a manuscript sentence implied the
                    # non-diseased correlation had been fitted as entered.
                    trunc <- character(0)
                    if (!is.na(phi_d) && !is.na(cond_dep_pos) && abs(phi_d - cond_dep_pos) > 5e-3)
                        trunc <- c(trunc, sprintf(.("(requested %.2f among diseased, truncated to the nearest value the entered sensitivities allow)"),
                                                  cond_dep_pos))
                    if (!is.na(phi_n) && !is.na(cond_dep_neg) && abs(phi_n - cond_dep_neg) > 5e-3)
                        trunc <- c(trunc, sprintf(.("(requested %.2f among non-diseased, truncated to the nearest value the entered specificities allow)"),
                                                  cond_dep_neg))
                    trunc_note <- if (length(trunc) > 0) paste0(" ", paste(trunc, collapse = " ")) else ""
                    # A calculation states its assumptions; "were applied" described a study that
                    # never happened (IR-05). An untouched default is not a chosen value (IR-08).
                    paste0(" ", sprintf(.("The calculation assumes both tests are performed in parallel and allows conditional dependence (correlation %1$s among diseased and %2$s among non-diseased subjects)%3$s."),
                                        fmt2(phi_d), fmt2(phi_n), trunc_note),
                           if (isTRUE(dep_default)) paste0(" ", .("These dependence values are the analysis defaults, not estimates from data.")) else "")
                }

                # Direction chosen from the ratio. Hard-coding "increase" and "reduced to" made
                # this sentence assert the opposite of its own numbers for any test pair with
                # sensitivity + specificity < 1. A ratio that displays as 1 is "about the same".
                dir_neg <- if (is.na(rel_prob_both_neg)) .("not estimable")
                           else if (private$.fmtNum(rel_prob_both_neg) == "1") .("about the same as the prevalence")
                           else if (rel_prob_both_neg < 1) sprintf(.("reduced to %sx of prevalence"), private$.fmtNum(rel_prob_both_neg))
                           else sprintf(.("raised to %sx of prevalence"), private$.fmtNum(rel_prob_both_neg))

                # "calculated", not "showed": these are Bayes'-theorem consequences of the values
                # typed in, not observations. The parallel rule's own positive arm (either test
                # positive) leads; it used to be absent, inviting the Both+ figure to be quoted as
                # the rule's result -- 93.0% against 42.1% at the defaults (VAL-cotest-11).
                paste0(marker, sprintf(
                    .("Co-testing with %1$s (sensitivity %2$s, specificity %3$s) and %4$s (sensitivity %5$s, specificity %6$s) at a pre-test probability of %7$s gave calculated post-test probabilities of disease of %8$s after a positive result on either test (the parallel rule; %9$s), %10$s when both tests were positive (%11$s) and %12$s when both were negative (%13$s)."),
                    t1_name, private$.fmtInput(test1_sens), private$.fmtInput(test1_spec),
                    t2_name, private$.fmtInput(test2_sens), private$.fmtInput(test2_spec),
                    private$.fmtPct(prevalence),
                    private$.fmtPct(postest_prob_either), private$.fmtChange(rel_prob_either),
                    private$.fmtPct(postest_prob_both), private$.fmtChange(rel_prob_both),
                    private$.fmtPct(postest_prob_both_neg), dir_neg
                ), model,
                # An exact 0% or 100% can only come from a cell forced to zero by a truncated
                # dependence; the table footnotes it, and the pasteable sentence must too.
                if (any(c(postest_prob_either, postest_prob_both, postest_prob_both_neg) %in% c(0, 1)))
                    paste0(" ", .("A probability of exactly 0% or 100% here is set by the dependence being at the limit the entered values allow; it is not an estimate.")) else "",
                " ", .("Sensitivity, specificity and prevalence were treated as exact, so these probabilities carry no confidence interval."))
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
                # TODO (i18n): events.js writes these names into the boxes in English and a typed
                # name wins, so in the GUI the translated test names below are never displayed
                # (only from R syntax with blank names). Decide which one the GUI should show.
                presets <- list(
                    hpv_pap = list(
                        test1_sens = 0.95, test1_spec = 0.85,
                        test2_sens = 0.70, test2_spec = 0.95,
                        prevalence = 0.05, indep = FALSE,
                        cond_dep_pos = 0.15, cond_dep_neg = 0.10,
                        test1_name = .("HPV"), test2_name = .("Pap cytology"),
                        label = .("HPV (Test 1) + Pap cytology (Test 2)"),
                        note = .("A sensitive test paired with a specific one, both read from the same cervical sample, so they are modelled as conditionally dependent.")
                    ),
                    psa_dre = list(
                        test1_sens = 0.80, test1_spec = 0.70,
                        test2_sens = 0.50, test2_spec = 0.85,
                        prevalence = 0.15, indep = TRUE,
                        cond_dep_pos = NULL, cond_dep_neg = NULL,
                        test1_name = .("PSA"), test2_name = .("Rectal examination"),
                        label = .("PSA (Test 1) + digital rectal examination (Test 2)"),
                        note = .("The one worked example that assumes conditional independence: a biochemical measurement and a physical examination.")
                    ),
                    troponin_ecg = list(
                        test1_sens = 0.90, test1_spec = 0.95,
                        test2_sens = 0.70, test2_spec = 0.90,
                        prevalence = 0.20, indep = FALSE,
                        cond_dep_pos = 0.20, cond_dep_neg = 0.05,
                        test1_name = .("Troponin"), test2_name = .("ECG"),
                        label = .("Troponin (Test 1) + ECG (Test 2)"),
                        note = .("Both tests are driven by the extent of myocardial injury, so they are modelled as dependent among diseased subjects in particular.")
                    ),
                    mammogram_ultrasound = list(
                        test1_sens = 0.85, test1_spec = 0.90,
                        test2_sens = 0.80, test2_spec = 0.85,
                        prevalence = 0.08, indep = FALSE,
                        cond_dep_pos = 0.25, cond_dep_neg = 0.15,
                        test1_name = .("Mammography"), test2_name = .("Ultrasound"),
                        label = .("Mammography (Test 1) + ultrasound (Test 2)"),
                        # Not "the strongest": covid_antigen_pcr carries the larger cond_dep_pos
                        # (0.30 vs 0.25). This is the example dependent in BOTH groups.
                        note = .("Two imaging modalities of the same tissue, so dependence is set relatively high in both disease groups, as dense tissue might produce.")
                    ),
                    covid_antigen_pcr = list(
                        test1_sens = 0.70, test1_spec = 0.95,
                        test2_sens = 0.95, test2_spec = 0.99,
                        prevalence = 0.10, indep = FALSE,
                        cond_dep_pos = 0.30, cond_dep_neg = 0.10,
                        test1_name = .("Rapid antigen"), test2_name = .("PCR"),
                        label = .("Rapid antigen (Test 1) + PCR (Test 2)"),
                        note = .("Two assays for the same organism on the same swab; both track viral load, so dependence among diseased subjects is set high.")
                    ),
                    tb_xray_sputum = list(
                        test1_sens = 0.75, test1_spec = 0.80,
                        test2_sens = 0.85, test2_spec = 0.98,
                        prevalence = 0.12, indep = FALSE,
                        cond_dep_pos = 0.20, cond_dep_neg = 0.08,
                        test1_name = .("Chest radiograph"), test2_name = .("Sputum microscopy"),
                        label = .("Chest radiograph (Test 1) + sputum microscopy (Test 2)"),
                        note = .("A sensitive imaging test paired with a highly specific microbiological one; advanced disease makes both more likely to be positive.")
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
            # For table cells and footnotes: break only a "<" that would open a tag in the
            # client's sanitiser ("HPV <LOD" -> "HPV < LOD"); "Ki-67 <10%" is left alone.
            .cellText = function(x) gsub("<(?=[A-Za-z/!?])", "< ", x, perl = TRUE),

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
                        "error"   = list(label = .("Error"),
                                         tint = "rgba(220, 53, 69, 0.14)",  rule = "rgba(220, 53, 69, 0.85)"),
                        "warning" = list(label = .("Warning"),
                                         tint = "rgba(255, 193, 7, 0.16)",  rule = "rgba(217, 164, 6, 0.90)"),
                        "info"    = list(label = .("Note"),
                                         tint = "rgba(13, 202, 240, 0.12)", rule = "rgba(13, 140, 180, 0.85)"),
                        list(label = .("Note"),
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
                # One complete .() literal, exactly as the welcome panel above: a msgid must be a
                # string literal, and a translator needs the whole passage rather than fragments.
                explanation <- paste0("\n", .('<div style="max-width: 800px;">
<h3>Understanding Test Dependence in Diagnostic Testing</h3>

<p>This applies to two tests read <strong>in parallel</strong>: both are performed on the same subject at the same time, before either result is known, and the two results are then combined. For tests performed one after another, where the second is ordered only after a particular first result, use a sequential testing analysis instead \u2014 the second test\'s sensitivity and specificity there apply to the selected subgroup, not to everyone.</p>

<h4>What is conditional independence vs. dependence?</h4>
<p>Two diagnostic tests are <strong>conditionally independent</strong> if the result of one test does not influence the result of the other test, <em>given the disease status</em>. In other words, within the diseased population, the probability of Test 1 being positive is not affected by knowing the result of Test 2, and vice versa. The same applies within the non-diseased population.</p>

<p>Tests are <strong>conditionally dependent</strong> when the result of one test affects the probability of the other test result, even when we know the patient\'s true disease status.</p>

<h4>Mathematical Formulation</h4>

<p><strong>Independent Tests:</strong> When tests are independent, joint probabilities are simply the product of individual probabilities:</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = P(Test1+ | Disease+) \u00D7 P(Test2+ | Disease+) = Sens\u2081 \u00D7 Sens\u2082</li>
  <li>P(Test1+ and Test2+ | Disease\u2212) = P(Test1+ | Disease\u2212) \u00D7 P(Test2+ | Disease\u2212) = (1\u2212Spec\u2081) \u00D7 (1\u2212Spec\u2082)</li>
  <li>P(Test1\u2212 and Test2\u2212 | Disease+) = P(Test1\u2212 | Disease+) \u00D7 P(Test2\u2212 | Disease+) = (1\u2212Sens\u2081) \u00D7 (1\u2212Sens\u2082)</li>
  <li>P(Test1\u2212 and Test2\u2212 | Disease\u2212) = P(Test1\u2212 | Disease\u2212) \u00D7 P(Test2\u2212 | Disease\u2212) = Spec\u2081 \u00D7 Spec\u2082</li>
</ul>

<p><strong>Dependent Tests:</strong> When tests are dependent, we adjust these probabilities using a correlation parameter (denoted as \u03C1 or \u03C8) that runs from \u22121 through 0 (independence) to +1. Positive values describe tests that tend to err together; negative values describe tests that compensate for each other\'s errors, so that one tends to be positive where the other is negative. Both are permitted here, though the attainable range is narrower than \u22121 to +1 and depends on the sensitivities and specificities entered \u2014 values outside it are truncated, and the analysis says so:</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = (Sens\u2081 \u00D7 Sens\u2082) + \u03C1<sub>pos</sub> \u00D7 \u221A(Sens\u2081 \u00D7 (1\u2212Sens\u2081) \u00D7 Sens\u2082 \u00D7 (1\u2212Sens\u2082))</li>
  <li>P(Test1+ and Test2+ | Disease\u2212) = ((1\u2212Spec\u2081) \u00D7 (1\u2212Spec\u2082)) + \u03C1<sub>neg</sub> \u00D7 \u221A((1\u2212Spec\u2081) \u00D7 Spec\u2081 \u00D7 (1\u2212Spec\u2082) \u00D7 Spec\u2082)</li>
</ul>

<p>Extreme values are automatically truncated to stay within feasible joint bounds; the realized correlation after truncation is reported.</p>

<p>Note: Similar adjustments are made for the other joint probabilities.</p>

<h4>When to Use Dependent vs. Independent Models</h4>

<p><strong>Independence is the assumption that has to be justified, not the fallback.</strong> It is usually the optimistic choice: it credits the pair with more combined information than tests that tend to err together actually provide. Use it only when you can point to a positive reason, such as:</p>
<ul>
  <li>The tests measure genuinely different biological phenomena</li>
  <li>They use different specimens, and different mechanisms of measurement</li>
  <li>Paired data from your own or a published series shows no residual correlation within disease status</li>
</ul>

<p><strong>If you simply do not know how the two tests interact, independence is the wrong default.</strong> Tests that share a specimen, a modality, an observer or a biological pathway are dependent unless shown otherwise. Try a few small positive values (for example 0.05, 0.10 and 0.20) and see how far the conclusion moves; if it moves a lot, the honest report gives the range rather than the independent figure. This is why the independence checkbox is off by default.</p>

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
<p>The conditional dependence parameters (\u03C1<sub>pos</sub> for diseased subjects and \u03C1<sub>neg</sub> for non-diseased subjects) ideally should be estimated from paired testing data with known disease status. Positive values are the usual case, with larger values indicating stronger dependence; negative values are unusual but legitimate where the two tests genuinely compensate for one another. When no data is available, sensitivity analyses using a range of plausible values (e.g., 0.05, 0.1, 0.2) can reveal how much dependence affects results.</p>

<p><strong>The value entered is a correlation (phi), not a covariance or a kappa.</strong> Several sources, Gardner et al. (2000) among them, report the conditional <em>covariance</em> instead. Convert it before entering it: among subjects with disease, phi = covariance / \u221A(Sens\u2081 \u00D7 (1\u2212Sens\u2081) \u00D7 Sens\u2082 \u00D7 (1\u2212Sens\u2082)); among subjects without disease, the same with (1\u2212Spec) in place of Sens. A kappa measures agreement, not this correlation, and is not interchangeable with it.</p>

<h4>Impact of Ignoring Dependence</h4>
<p>When two tests tend to err together, assuming independence usually overstates how much the second test adds; for the parallel rule, dependence among subjects with the disease lowers the combined sensitivity (Gardner et al. 2000). <em>Which way</em> each post-test probability moves, however, depends on where the dependence lies \u2014 among subjects with the disease, among those without it, or both \u2014 and on the sensitivities and specificities, so no single direction holds for every row. For tests that do better than chance (sensitivity plus specificity above 1):</p>
<ul>
  <li><strong>Both tests positive</strong> \u2014 when the tests share false positives (dependence mainly among subjects <em>without</em> the disease), independence makes this probability too high: two correlated positives are partly the same evidence counted twice. When the dependence lies mainly among subjects <em>with</em> the disease, it can go the other way.</li>
  <li><strong>Both tests negative</strong> \u2014 when the tests miss the disease together (dependence mainly among subjects <em>with</em> the disease), independence makes this probability too low, overstating how safely the pair rules disease out. Dependence mainly among subjects without the disease can reverse this.</li>
  <li><strong>Either test positive (the parallel rule)</strong> and <strong>one test positive only</strong> \u2014 the direction can go either way.</li>
</ul>
<p>The dependable way to see the effect for your own tests is to compare: run the analysis once assuming independence and once with your dependence values, and read the change row by row. The points above are a guide to reading that comparison, not rules. Note also that this analysis reports no confidence intervals at all: every figure is conditional on the sensitivity, specificity and prevalence you entered being exact.</p>
</div>'))

                return(explanation)
            }
        )
    )
