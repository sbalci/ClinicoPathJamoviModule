#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent_format
#' @importFrom rlang .data
#' @return An \code{R6} class generator object for the \code{decisioncombineClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncombineClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .noticeList = list(),
            # Rows where a ratio needed the 0.5 continuity correction (an empty denominator
            # cell; .diagRatioRules()), accumulated across
            # .analyzeSinglePattern() calls and reported as ONE notice. Emitting per
            # pattern produced up to eleven near-identical banners in a 3-test analysis.
            .continuityPatterns = character(),
            # Patterns whose LR+/LR- point estimate is determinate but whose delta-method
            # SE collapses to exactly 0 (a structurally empty test margin), so the printed
            # interval would be the point estimate repeated. Collected here and disclosed
            # as ONE table note, for the same reason .continuityPatterns is.
            .zeroSeIntervalPatterns = character(),
            # Rows where LR+, LR- or the diagnostic odds ratio is exactly 0 (an empty
            # numerator cell, no correction): their interval is [0, U] (.diagZeroRatioUpper).
            # Disclosed as ONE table note, like the two above.
            .zeroRatioPatterns = character(),
            # Every row of the combination table as a rule, keyed by its label:
            # list(flag = patient-level positive call, type = row type, components = the
            # tests it uses). Filled by .analyzeSinglePattern(); read by the paired
            # comparisons and the optimism bootstrap, which need the rules themselves,
            # not the table's summary counts.
            .rules = list(),
            # Findings of .screenSelectiveMissingness(), worded by .emitMissingnessNotices() once
            # it is known whether the combination table will be built.
            .missingness = NULL,
            # Tests (1-3) named by the "Has Only/No Positive Results" notice of .prepareData(), so
            # .assessTopRuleDiscrimination() can leave that diagnosis to it.
            .constantTests = integer(),

            # jmvcore's OptionVariable$valueAsSource returns the column name verbatim, so the
            # exported syntax read `gold = Golden Standart` -- not parseable R for any name
            # with a space or symbol, including this analysis's own documented example.
            # Emit character literals instead (the wrapper's resolveQuo() resolves a string
            # to the same column), escaped by encodeString(); same for the level labels, which
            # jmvcore quotes but does not escape. Pattern from R/tableone.b.R.
            .sourcifyOption = function(option) {
                quoted <- c("gold", "test1", "test2", "test3", "goldPositive",
                            "test1Positive", "test2Positive", "test3Positive")
                value <- option$value
                if (option$name %in% quoted && is.character(value) &&
                    length(value) == 1L && nzchar(value)) {
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                }
                super$.sourcifyOption(option)
            },

            # `refs`: 00refs keys that only this notice's text relies on. .renderNotices() lists
            # the sources of the notices actually shown, as R/decision.b.R does.
            .addNotice = function(type, title, content, refs = character(0)) {
                notice <- list(
                    type = type,
                    title = title,
                    content = content,
                    refs = refs
                )
                private$.noticeList[[length(private$.noticeList) + 1]] <- notice
            },
            # Theme-safe panel styling: translucent rgba tint + an explicit
            # "color: inherit" body, matching R/waterfall.b.R. An opaque hex fill reads
            # correctly only against jamovi's light theme.
            .panelHtml = function(title, body) {
                paste0(
                    "<div style='background-color: rgba(37, 99, 235, 0.06); ",
                    "border-left: 4px solid #93c5fd; padding: 14px; margin: 10px 0; ",
                    "border-radius: 4px; color: inherit;'>",
                    "<h3 style='margin-top: 0; color: inherit;'>",
                    jmvcore::htmlEscape(title), "</h3>", body, "</div>"
                )
            },

            .renderAboutPanels = function() {
                if (!isTRUE(self$options$showAbout)) {
                    return()
                }

                li <- function(...) paste0("<li style='margin-bottom: 6px;'>", ..., "</li>")
                esc <- function(x) jmvcore::htmlEscape(x)

                about <- paste0(
                    "<p>", esc(.("This analysis scores two or three diagnostic tests against the reference standard on the same patients: every exact result pattern, each test alone, and the usual ways of combining them -- Parallel and Serial across all the tests and, with three tests, across every pair, plus Majority. Mixed rules such as \"Test 1 positive and either Test 2 or Test 3 positive\" are not listed; the ranking says when some other way of calling the result patterns positive would score higher. With a single test selected it reports that one test alone (a single-test row, described below); the pattern and strategy rows do not arise.")), "</p>",
                    "<h4>", esc(.("The kinds of row")), "</h4><ul>",
                    li(esc(.("A PATTERN row (\"+/-\") is a mutually exclusive group of patients: those whose results were exactly that. It can be read as a rule -- \"call positive only when the results are exactly this\" -- and the ranking treats it that way, but then every column refers to that exact pattern. Sensitivity is the share of diseased patients showing it; Specificity the share of non-diseased patients showing anything else; LR- the likelihood ratio for \"any other pattern\" rather than for a negative test."))),
                    li(esc(.("Accuracy and NPV on a pattern row are dominated by the patients who did NOT show that pattern, so a rare pattern reports both close to 1 minus the prevalence however uninformative it is. A pattern seen in 5 of 50 diseased and 10 of 150 non-diseased patients reports Accuracy 0.73 and NPV 0.76 against a prevalence of 0.25, while its Youden's J is 0.03. Read Youden's J or Balanced Accuracy on these rows, not Accuracy. PPV and LR+ are the two columns that read cleanly: PPV is the probability of disease given exactly that result combination, and LR+ is the likelihood ratio for that combination."))),
                    li(esc(.("A SINGLE-TEST row (\"Test 1 alone\") applies one test by itself to the same patients, so every combination can be compared with the tests it is built from. A combination that is no better than one of its own tests on sensitivity and no better on specificity adds cost without adding accuracy. One with a lower Youden's J than a single test can still suit one clinical role, because J weighs a missed case and a false positive equally: Parallel raises sensitivity (to rule out disease), Serial raises specificity (to rule it in)."))),
                    li(esc(.("A STRATEGY row is a rule you would normally apply to a patient. Parallel (>=1 pos) calls positive if any test is positive, which raises sensitivity and lowers specificity. Serial (all pos) requires every test to be positive, which does the reverse. With three tests the same two rules are also listed for each pair of tests (\"Parallel 1+2 (>=1 pos)\", \"Serial 1+2 (both pos)\"), and Majority (>=2/3 pos) needs two of three."))),
                    "</ul><h4>", esc(.("Reading the columns")), "</h4><ul>",
                    li(esc(.("Prevalence is identical in every row: it is the disease rate in the whole sample, not the rate within that pattern. Every row is scored on the same complete cases; the individual-test tables use each test's own pairwise-complete cases, so with missing data they can differ from the \"alone\" rows."))),
                    li(esc(.("PPV and NPV depend on prevalence, so they transfer to another population only if its disease rate is similar. Sensitivity, specificity and the likelihood ratios are less prevalence-dependent, though not independent of it. LR+ and LR- are the more useful numbers to carry elsewhere not because they are more stable -- being functions of sensitivity and specificity, they inherit exactly the same dependence -- but because they convert a pre-test probability into a post-test probability at whatever prevalence the new setting has."))),
                    li(esc(.("Balanced Accuracy and Youden's J are the same statistic on two scales: Balanced Accuracy = (J + 1) / 2, so ranking by one is identical to ranking by the other."))),
                    li(esc(.("LR+, LR- and the diagnostic odds ratio add 0.5 to all four cells only when a cell in the ratio's denominator is zero and its numerator is not (FP for LR+, TN for LR-, FP or FN for the odds ratio), so the ratio stays finite (for the odds ratio this is the Haldane-Anscombe correction; the likelihood ratios use the same adjustment). When the numerator cell is zero instead (TP for LR+, FN for LR-, TP or TN for the odds ratio), the ratio is exactly 0 and is not corrected: adding 0.5 there would turn a test that never detected disease into one that seems to point towards it. Its confidence interval runs from 0 to the upper limit of the same log-scale interval computed with 0.5 added to every cell. A corrected ratio stands in for an infinite one, so when the correction would bring it to 1 or below (an empty cell in a much smaller reference group) it is left blank as not estimable. The proportions on the same row use the observed counts, so the two need not agree exactly at a zero cell. The correction repairs a sampling zero, not a structural one. On a row with no test-positive patients (a pattern nobody shows, or a rule that is never positive), LR+ and the odds ratio are 0/0 and are left BLANK rather than corrected, while LR- is exactly 1: any other result leaves the odds unchanged. On a row with no test-negative patients, LR- and the odds ratio are blank and LR+ is exactly 1. In a sample with only one reference outcome all three are blank in every row, as are PPV and NPV, which the sampling alone fixes, and Accuracy, which then only repeats the sensitivity or specificity."))),
                    "</ul><h4>", esc(.("What this analysis does NOT assume")), "</h4><p>",
                    esc(.("Every row is estimated directly from the observed joint results, not derived from the individual tests' sensitivity and specificity. These figures therefore do NOT require the tests to be conditionally independent given disease status, and they stay valid when the tests share biology, specimen or technology. That is the main difference from a sequential-testing calculator, which must assume independence and warns about it.")),
                    "</p><p>",
                    esc(.("Two things that assumption-free estimation does not buy. Precision: the sample is split across four or eight pattern cells instead of being pooled into two marginal proportions, so the intervals are wider -- that width is what the assumption was paying for. Transport: these numbers carry to another population only if the whole joint distribution of results given disease status carries over, which is a stronger requirement than each test's sensitivity and specificity carrying over. Parallel and Serial figures here will also generally NOT match the textbook independence formulas computed by hand from the individual tests. With positively correlated tests each strategy gains less than those formulas predict on the measure it is chosen for -- Parallel raises sensitivity less, Serial raises specificity less -- while the other measure loses less than predicted, so Youden's J can be the same as the formulas give.")),
                    "</p><h4>", esc(.("The ranking")), "</h4><p>",
                    esc(.("The candidate-rule ranking is a descriptive argmax over observed Youden's J, taken over every row of the table -- result patterns, single tests and strategies -- with no significance test and no multiplicity correction. On a tie the rule using fewer tests is shown first. On data with no real signal it will still name a winner. It is a hypothesis to confirm in new data, not a validated recommendation. Tick \"Youden's J corrected for rule selection\" for a bootstrap estimate of how much of the highest J is the luck of being picked from many rules.")),
                    "</p><p>", esc(.("The pattern-type and statistic filters affect the plots only. The performance tables always show every row.")), "</p>"
                )

                assumptions <- paste0(
                    "<p>", esc(.("Diagnostic accuracy estimates are shaped by how the study was designed, not only by how the tests perform. The three biases below dominate real pathology accuracy studies. Spectrum and incorporation bias cannot be detected from the data in front of this analysis; partial verification can leave a trace, reference results missing according to test results, which this analysis checks for and reports.")), "</p><ul>",
                    li("<strong>", esc(.("Verification (work-up) bias.")), "</strong> ",
                       esc(.("If the reference standard was obtained mainly on patients whose test was positive, then test-negative patients of BOTH disease states are under-represented: the missing false negatives inflate sensitivity, and the missing true negatives deflate specificity. This is the usual situation when the reference standard is a biopsy or resection that is only performed after a positive test."))),
                    li("<strong>", esc(.("Spectrum bias.")), "</strong> ",
                       esc(.("Accuracy measured on clearly diseased versus clearly healthy patients is higher than accuracy in the population the test is actually used on, which is full of early, partial and equivocal presentations. A case-control (two-gate) design inflates sensitivity and specificity and makes the Prevalence column an artefact of the sampling ratio rather than of any clinical population, so PPV is inflated while NPV is DEFLATED relative to a low-prevalence setting. At sensitivity and specificity of 0.90, a 50% sampled prevalence gives PPV 0.90 and NPV 0.90, while a true 5% prevalence gives PPV 0.32 and NPV 0.99."))),
                    li("<strong>", esc(.("Incorporation bias.")), "</strong> ",
                       esc(.("If one of the tests being evaluated also contributed to establishing the reference standard, that test is being compared against itself and its apparent accuracy is inflated. This is easy to do accidentally when the reference is a final diagnosis assembled from all available information."))),
                    "</ul><h4>", esc(.("Other requirements")), "</h4><ul>",
                    li("<strong>", esc(.("One row per patient.")), "</strong> ",
                       esc(.("Every row must be a different patient. Several specimens, cores, blocks, lesions or lymph nodes from the same patient are correlated, and every confidence interval here treats the rows as independent, so with such data the intervals are too narrow. Use one result per patient, or a method that accounts for clustering."))),
                    li(esc(.("Complete-case analysis: a patient missing the reference standard or any selected test is excluded from the combination table. When any are excluded, the number is given in the warning \"Removed N case(s) with missing values\" in the notices at the top of the results, and a separate warning reports results that are missing according to another result (reflex or referee testing, partial verification)."))),
                    li(esc(.("Each test is reduced to two levels. Any level that is not the one chosen as positive counts as negative, so equivocal and indeterminate results are folded into the negative arm unless you recode them as missing first."))),
                    li(esc(.("The rows are not independent: every rule is scored on the same patients, so the differences between them are correlated. A formal comparison needs a paired procedure -- McNemar's test applied SEPARATELY within the diseased patients to compare sensitivity and within the non-diseased patients to compare specificity, because a single McNemar over the whole sample compares positivity rates rather than accuracy. Paired PPV and NPV comparisons need a generalised score test instead. The optional table \"Combination Rules vs Each Test Alone\" (option \"Combinations vs each test alone\") makes this comparison within each reference group for every combination strategy (Parallel, Serial and Majority) against each test it combines: a Parallel or Serial strategy can move only one way, so the table gives its change with an interval, and McNemar's test is used for Majority. Paired PPV and NPV comparisons are not performed."))),
                    li(esc(.("Sample size and cell counts are reported as notices when they fall below this analysis's thresholds (20, 50 and 100 cases in all; 10 per reference group; 5 per cell). These are rules of thumb, not requirements: the number of cases a study needs depends on the expected sensitivity and specificity, the precision wanted and the prevalence (Buderer 1996). Read the confidence intervals rather than the point estimates whenever a notice appears."))),
                    "</ul>"
                )

                self$results$about$setContent(
                    private$.panelHtml(.("What this analysis does"), about))
                self$results$assumptions$setContent(
                    private$.panelHtml(.("Assumptions, biases and requirements"), assumptions))
            },

            .renderNotices = function() {
                # The .r.yaml refs of the notices item would otherwise be listed on every run,
                # beside a notice that is not shown. setRefs() is sent with each run's results
                # and never restored from a saved one, so this is what jamovi lists.
                self$results$notices$setRefs(
                    unique(unlist(lapply(private$.noticeList, `[[`, "refs"))))
                if (length(private$.noticeList) == 0) {
                    # Not "": the jamovi client skips an empty Html update, so the previous
                    # run's warnings would stay on screen.
                    self$results$notices$setContent("<div></div>")
                    return()
                }

                # Notices were rendered in the order they happened to be emitted, which put
                # a STRONG_WARNING below routine INFO banners -- the reader had to scroll past
                # the reassuring notes to reach the reason to distrust the numbers. Sort by
                # severity, stably, so equally severe notices keep the order in which the
                # analysis produced them. (Where the panel sits among the results is set by
                # its position in the .r.yaml: second, under the Getting Started panel.)
                severity_rank <- c(ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L)
                types <- vapply(private$.noticeList, function(n) n$type, character(1))
                ranks <- severity_rank[types]
                ranks[is.na(ranks)] <- length(severity_rank) + 1L
                ordered_notices <- private$.noticeList[order(ranks, method = "radix")]

                # STRONG_WARNING previously fell through to the INFO branch and was
                # rendered as a blue informational note -- so "Gold Standard Has Only
                # One Outcome" looked like a tip rather than a reason to distrust the
                # numbers. ERROR also used the warning triangle; give it a stop sign so
                # the three severities are visually distinct.
                #
                # Backgrounds are translucent rgba tints with an explicit "color: inherit"
                # body, matching the house theme-safe pattern in R/waterfall.b.R: an opaque
                # fill reads correctly only against jamovi's light theme. Icons are \u{}
                # escapes rather than HTML entities -- only the five structural entities
                # survive Word/PDF export.
                #
                # Removing the per-severity title colour (the right theme-safety fix) left
                # WARNING and STRONG_WARNING sharing an icon, a text colour and a near
                # identical tint, with nothing naming the severity in words -- so "Extreme
                # Disease Prevalence" was indistinguishable from a routine complete-case
                # note. Prefix the title with a translated severity word: it survives every
                # theme, Word/PDF export and greyscale printing, which no colour does.
                type_styles <- list(
                    ERROR = list(
                        bgcolor = "rgba(220, 38, 38, 0.10)",
                        border = "#fca5a5", icon = "\u{26D4}",       # no-entry sign
                        label = .("Error")),
                    STRONG_WARNING = list(
                        bgcolor = "rgba(234, 88, 12, 0.10)",
                        border = "#fdba74", icon = "\u{26A0}",       # warning sign
                        label = .("Serious warning")),
                    WARNING = list(
                        bgcolor = "rgba(202, 138, 4, 0.12)",
                        border = "#fde047", icon = "\u{26A0}",
                        label = .("Warning")),
                    INFO = list(
                        bgcolor = "rgba(37, 99, 235, 0.08)",
                        border = "#93c5fd", icon = "\u{2139}",       # info sign
                        label = .("Note"))
                )

                html <- '<div style="margin: 10px 0;">'
                for (notice in ordered_notices) {
                    style <- type_styles[[notice$type]]
                    if (is.null(style)) {
                        style <- type_styles$INFO
                    }

                    html <- paste0(
                        html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong>",
                        style$icon, " ", jmvcore::htmlEscape(style$label), ": ",
                        jmvcore::htmlEscape(notice$title),
                        "</strong><br>",
                        "<span style='color: inherit;'>",
                        jmvcore::htmlEscape(notice$content),
                        "</span>",
                        "</div>"
                    )
                }
                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },
            # A cell that is undefined by design (0/0 ratio, PPV in a one-class sample, the p
            # of a nested paired rule, a pending row) must be BLANK, as the notes promise.
            # jmvcore sends a numeric NA as a double, which jamovi prints as "NaN", and a
            # logical NA as missing; "" is the only empty cell, and $asDF reads it back as NA,
            # so the ranking and plot states still see NA. Values that exist keep their type.
            .blankNA = function(values) {
                lapply(values, function(v) {
                    if (length(v) == 1L && !is.character(v) && is.na(v)) "" else v
                })
            },
            .safeProp = function(num, den) {
                # Zero-safe proportion: the definitional form of sensitivity,
                # specificity, PPV and NPV, returning NA rather than NaN on an empty margin.
                if (length(den) != 1 || is.na(den) || den == 0) {
                    return(NA_real_)
                }
                num / den
            },

            .patternFilterLabel = function(value) {
                switch(value,
                    all = .("All Patterns"),
                    allPositive = .("All Tests Positive"),
                    allNegative = .("All Tests Negative"),
                    mixed = .("Mixed/Discordant"),
                    value
                )
            },

            .metricLabel = function(metric) {
                switch(metric,
                    prevalence = .("Prevalence"),
                    sens = .("Sensitivity"),
                    spec = .("Specificity"),
                    ppv = .("PPV"),
                    npv = .("NPV"),
                    acc = .("Accuracy"),
                    balancedAccuracy = .("Balanced Accuracy"),
                    youden = .("Youden's J"),
                    lrPos = .("LR+"),
                    lrNeg = .("LR-"),
                    dor = .("DOR"),
                    metric
                )
            },

            .normalizeMissing = function(df) {
                # addNA() / factor(exclude = NULL) turns NA into a real LEVEL. Such values
                # are NOT is.na(), so they survive both stats::complete.cases() and
                # jmvcore::naOmit() -- but as.character() maps them straight back to NA.
                # Two things then went wrong downstream, silently:
                #   * .analyzeIndividualTest's ifelse() recode produced an all-NA test
                #     column, so its 2x2 came back all zeros;
                #   * .prepareData's case_when() leads with is.na(), which does not match
                #     an explicit-NA level, so the row fell through to TRUE ~ "Negative"
                #     and a genuinely missing observation was COUNTED AS NEGATIVE -- for
                #     the reference standard as well as for each test.
                # Restore real NA at ingress so every missing-data path below (exclusion
                # counts, pairwise denominators, the dropped-cases notice) sees the truth.
                for (nm in names(df)) {
                    col <- df[[nm]]
                    if (is.factor(col) && anyNA(levels(col))) {
                        df[[nm]] <- factor(col, levels = levels(col)[!is.na(levels(col))])
                    }
                }
                df
            },

            # Also the unset test for a Level option: allowNone gives NULL; "" and NA count too.
            .optionSelected = function(value) {
                !is.null(value) && length(value) == 1L && !is.na(value) && nzchar(value)
            },

            # "level (n = k)" for each non-missing level present in a column, in level order.
            # Unused factor levels, an explicit NA level and "" are left out.
            .observedLevels = function(var) {
                counts <- table(factor(self$data[[var]], exclude = c(NA, "")))
                if (length(counts) == 0) return(.("none"))
                shown <- sprintf("%s (n = %d)", names(counts), as.integer(counts))
                if (length(shown) > 10) shown <- c(shown[1:10], "...")
                paste(shown, collapse = ", ")
            },

            # Forest plot height from the options alone, so .init() can set it. The rows are
            # fixed by the number of tests k: 2^k result patterns, each test alone, Parallel
            # and Serial, plus (k = 3) the six pairwise rules and Majority; one row for a
            # single test. About 22 px per row per facet keeps the rule labels from
            # overprinting.
            .selectedTestCount = function() {
                sum(vapply(c("test1", "test2", "test3"), function(n)
                    private$.optionSelected(self$options[[n]]), logical(1)))
            },
            .forestPlotHeight = function() {
                k <- private$.selectedTestCount()
                n_rules <- if (k < 2L) {
                    1L
                } else {
                    switch(self$options$filterPattern,
                        allPositive = 1L,
                        allNegative = 1L,
                        mixed = 2L^k - 2L,
                        2L^k + k + 2L + if (k == 3L) 7L else 0L)
                }
                n_stats <- if (identical(self$options$filterStatistic, "all")) 8L else 1L
                max(400, 170 + n_stats * (45 + 22 * n_rules))
            },

            # NOTE: the declarative `visible: (showIndividual)` on these three Group items
            # in the .r.yaml is INERT and this method is what actually hides them. jmvcore's
            # Group overrides the `visible` active binding to return TRUE if ANY child item
            # is visible, ignoring the group's own value, and ResultsElement$asProtoBuf
            # honours .visibleExpr directly only when it is the literal "TRUE"/"FALSE" that
            # setVisible() writes -- an expression like "(showIndividual)" falls through to
            # that child-scanning binding. Deleting this method would silently show all
            # three groups, always. It also adds the per-test refinement the declarative
            # form cannot express: hide Test 2's group when no test 2 is selected.
            .updateIndividualVisibility = function() {
                show <- isTRUE(self$options$showIndividual)
                self$results$individualTest1$setVisible(
                    show && private$.optionSelected(self$options$test1))
                self$results$individualTest2$setVisible(
                    show && private$.optionSelected(self$options$test2))
                self$results$individualTest3$setVisible(
                    show && private$.optionSelected(self$options$test3))
            },

            .clearDynamicResults = function() {
                for (name in c("combinationTable", "combinationTableCI",
                               "combinationTableCIRatios", "goldFreqTable",
                               "crossTabTable")) {
                    item <- self$results[[name]]
                    if (item$rowCount > 0) {
                        .deleteTableRows(item)
                    }
                }

                for (i in seq_len(3L)) {
                    group <- self$results[[paste0("individualTest", i)]]
                    cont <- group[[paste0("test", i, "Contingency")]]
                    stats <- group[[paste0("test", i, "Stats")]]
                    cont$setRow(rowKey = "Positive", values = list(
                        goldPos = NA_integer_, goldNeg = NA_integer_, total = NA_integer_))
                    cont$setRow(rowKey = "Negative", values = list(
                        goldPos = NA_integer_, goldNeg = NA_integer_, total = NA_integer_))
                    cont$setRow(rowKey = "Total", values = list(
                        goldPos = NA_integer_, goldNeg = NA_integer_, total = NA_integer_))
                    for (key in c("sens", "spec", "ppv", "npv")) {
                        stats$setRow(rowKey = key, values = list(estimate = ""))
                    }
                }

                # recommendationTable has a fixed one-row schema. Deleting that row makes
                # the later setRow(rowNo = 1) fail, so clear its cells while preserving the
                # result structure established by the schema.
                # Hidden until something is actually ranked. The schema fixes this table at
                # rows: 1, so the seeded all-NA row survives every early return -- a user who
                # ticks the ranking box and then hits a validation error was shown a blank row
                # under the header "Highest-Ranked Rule". Same defect as crossTabTable; the
                # populate method turns it back on, and both paths run on every .run().
                # crossTabTable has exactly the same defect, and the comment above already
                # named it: its setVisible() is reached only from .populateFrequencyTables(),
                # which .run() calls only when showFrequency is TRUE. An imperative
                # setVisible() permanently replaces the declarative visible: (showFrequency)
                # binding, so the literal TRUE written on a previous run was never undone --
                # unticking the box left an empty "Test Results Cross-Tabulation" header on
                # screen, in the export and in the saved .omv. Reset here; the populate path
                # turns it back on, and this method runs on every .run().
                self$results$crossTabTable$setVisible(FALSE)
                # A conditional note must not outlive the condition: without this, a run
                # whose intervals are all estimable would still show the previous run's
                # "interval left blank" footnote.
                self$results$combinationTableCIRatios$setNote("nonestimable_ci", NULL)
                self$results$combinationTableCIRatios$setNote("zero_ratio_ci", NULL)

                self$results$recommendationTable$setVisible(FALSE)
                # Set only beside the "advantage is not established" sentence it explains.
                self$results$recommendationTable$setNote("bound", NULL)
                # The optimism notes, seed and reference exist only when the bootstrap ran.
                self$results$recommendationTable$setNote("optimism", NULL)
                self$results$recommendationTable$setNote("seed", NULL)
                self$results$recommendationTable$setRefs(character(0))
                self$results$recommendationTable$setRow(rowNo = 1, values = list(
                    pattern = NA_character_,
                    method = NA_character_,
                    youden = NA_real_,
                    youdenCorrected = NA_real_,
                    optimism = NA_real_,
                    sens = NA_real_,
                    spec = NA_real_,
                    acc = NA_real_,
                    rationale = NA_character_
                ))

                # Rows from .init(), so reset rather than deleted: jamovi restores an
                # existing row's cells from the previous run. Hidden until filled, like the
                # ranking above: after a validation error it showed labelled rows of blanks.
                paired <- self$results$pairedComparisonTable
                paired$setVisible(FALSE)
                paired$setRefs(character(0))
                blank <- list(sensGained = NA_integer_, sensLost = NA_integer_,
                              sensChange = NA_real_, sensLower = NA_real_,
                              sensUpper = NA_real_, sensP = NA_real_,
                              specGained = NA_integer_, specLost = NA_integer_,
                              specChange = NA_real_, specLower = NA_real_,
                              specUpper = NA_real_, specP = NA_real_)
                for (key in paired$rowKeys) {
                    paired$setRow(rowKey = key, values = private$.blankNA(blank))
                }

                for (name in c("barPlot", "heatmapPlot", "forestPlot",
                               "decisionTreePlot")) {
                    self$results[[name]]$setState(list(valid = FALSE))
                }
            },

            .init = function() {
                private$.noticeList <- list()
                private$.continuityPatterns <- character()
                private$.zeroSeIntervalPatterns <- character()
                private$.zeroRatioPatterns <- character()
                private$.rules <- list()
                private$.missingness <- NULL
                private$.constantTests <- integer()

                # Initialize fixed-structure tables for Test 1, 2, 3
                for (i in seq_len(3L)) {
                    group <- self$results[[paste0("individualTest", i)]]
                    contTable <- group[[paste0("test", i, "Contingency")]]
                    statsTable <- group[[paste0("test", i, "Stats")]]
                    
                    contTable$addRow(rowKey = "Positive", values = list(testResult = .("Test Positive")))
                    contTable$addRow(rowKey = "Negative", values = list(testResult = .("Test Negative")))
                    contTable$addRow(rowKey = "Total", values = list(testResult = .("Total")))
                    
                    statsTable$addRow(rowKey = "sens", values = list(statistic = .("Sensitivity")))
                    statsTable$addRow(rowKey = "spec", values = list(statistic = .("Specificity")))
                    statsTable$addRow(rowKey = "ppv", values = list(statistic = .("PPV")))
                    statsTable$addRow(rowKey = "npv", values = list(statistic = .("NPV")))
                }

                # The paired-comparison rows depend on the options alone (4 with two tests,
                # 21 with three), so they are built here and filled with setRow() in .run().
                if (isTRUE(self$options$showPairedComparison)) {
                    k <- private$.selectedTestCount()
                    if (k >= 2L) {
                        paired <- self$results$pairedComparisonTable
                        for (def in private$.strategyDefinitions(k)) {
                            for (i in def$components) {
                                paired$addRow(rowKey = paste0(def$key, "_", i), values = list(
                                    rule = def$label, comparator = private$.singleTestLabel(i)))
                            }
                        }
                    }
                }

                # Getting Started panel: shown by the declarative visible: in the .r.yaml
                # until the gold standard and Test 1 are chosen. The content is static, so it
                # is set here. Every string doubles as an option title, a control label in the
                # .u.yaml or a line of R/decision.b.R's welcome, so each is translated once.
                step <- function(label, text) {
                    paste0("<li><strong>", jmvcore::htmlEscape(label), ":</strong> ",
                           jmvcore::htmlEscape(text), "</li>")
                }
                self$results$welcome$setContent(private$.panelHtml(
                    .("Combine Medical Decision Tests"),
                    paste0(
                        "<h4 style='color: inherit;'>", jmvcore::htmlEscape(.("Quick Start")),
                        "</h4><ol>",
                        step(.("Gold Standard (Reference Test)"),
                             .("Choose the reference-standard variable that defines disease status in this analysis (e.g., biopsy result, final diagnosis)")),
                        step(.("Disease present level"),
                             .("Choose which level indicates disease is present")),
                        step(.("Test 1 (Required)"),
                             .("Choose the diagnostic test you want to evaluate")),
                        step(.("Positive level"),
                             .("Choose which level represents a positive test result")),
                        step(.("Test 2 (Required for Combinations)"),
                             .("Second diagnostic test for combination analysis. Leave empty for single test only.")),
                        step(.("Test 3 (Optional)"),
                             .("Optional third test for 3-way combination analysis (8 patterns).")),
                        "</ol>"
                    )
                ))

                # The declarative visible: (showIndividual) on these Groups is inert (see
                # .updateIndividualVisibility above), so without this call the three groups
                # are visible between .init() and the first .run(): three empty "Test N
                # Performance" panels of all-NA tables flash on screen, and are what the
                # user looks at for the whole duration of a slow first run.
                private$.updateIndividualVisibility()

                # Sized here, not in .run(): jamovi's export/copy and every redraw after a
                # resize, theme change or .omv reopen build a fresh object and run init() and
                # .load() but never .run(), and Image$fromProtoBuf() recomputes the size from
                # what setSize() set. Sized only in .run(), an exported forest plot fell back
                # to the yaml 800 x 600 and its labels overprinted again (VAL-04).
                self$results$forestPlot$setSize(800, private$.forestPlotHeight())
            },
            .run = function() {
                private$.noticeList <- list()
                private$.continuityPatterns <- character()
                private$.zeroSeIntervalPatterns <- character()
                private$.zeroRatioPatterns <- character()
                private$.rules <- list()
                private$.missingness <- NULL
                private$.constantTests <- integer()
                private$.clearDynamicResults()
                private$.updateIndividualVisibility()

                # Static educational content: render BEFORE the validation early-returns, so
                # a user who ticks "About this analysis" while still choosing variables gets
                # the explanation rather than an empty pane. It depends on no data.
                private$.renderAboutPanels()

                # Render notices on failed validation, failed preparation and pending
                # selections too. Rendering only at the bottom meant every
                # notice explaining WHY the analysis stopped -- "Missing Level",
                # "No Complete Cases", every validation error -- was collected and then
                # discarded, leaving the user with a blank analysis and no message. on.exit
                # covers every exit path, including ones added later.
                on.exit(private$.renderNotices(), add = TRUE)

                # A .hasRequiredVars() pre-gate used to sit here, returning FALSE silently
                # on exactly the first five conditions .validateInputs() re-tests with an
                # ERROR notice each. The early return fired first, so those five notices
                # could never be shown: a user who picked a gold standard but no positive
                # level got a blank analysis and no message, while the sentence telling
                # them which control to fill in shipped to translators unreachable. Same
                # defect the duplicate block in .prepareData() had -- a defensive guard is
                # only defensive if it can fire. Validation now owns the decision.
                #
                # Step 1: Validate inputs (will stop on errors)
                validation_result <- private$.validateInputs()
                # The missingness notices say what the combination table contains, so they are
                # worded only now that it is known whether the table will be built: after a
                # failed validation they described a table, and a ranking, that never existed.
                private$.emitMissingnessNotices(isTRUE(validation_result))
                if (!validation_result) {
                    .diagRejectInputErrors(private$.noticeList)
                    return() # Halt execution if validation failed
                }

                # Each individual test uses its own gold/test complete cases. An optional
                # co-test must never alter another test's diagnostic estimates.
                if (self$options$showIndividual) {
                    for (test_num in seq_len(3L)) {
                        test_option <- self$options[[paste0("test", test_num)]]
                        if (private$.optionSelected(test_option)) {
                            private$.analyzeIndividualTest(test_num)
                        }
                    }
                }

                # Combination rules require joint complete cases across every selected test.
                data_prep <- private$.prepareData()
                if (is.null(data_prep)) {
                    .diagRejectInputErrors(private$.noticeList)
                    return()
                }

                # Step 4: Combination analysis
                private$.analyzeCombinations(data_prep)
                private$.populatePairedComparison(data_prep)

                # Step 5: Populate frequency tables (if requested)
                if (self$options$showFrequency) {
                    private$.populateFrequencyTables(data_prep)
                }

                # Step 6: Rank the candidate rules. Runs on EVERY run, not only when
                # showRecommendation is ticked, because two of its guards are clinical
                # rather than cosmetic: "Positive Levels May Be Inverted" and "No Rule
                # Performs Better Than Chance" are the only detectors of a swapped
                # positive level, which silently inverts every number this analysis
                # prints. With the ranking box off by default, nothing detected it in the
                # configuration almost everyone uses. The same goes for the check on how well
                # the top-ranked rule discriminates. The table itself stays gated --
                # .clearDynamicResults() hides it and .populateRecommendation() re-shows it
                # only when the option is on -- and the method is called exactly once per
                # .run(), so neither notice can be emitted twice.
                private$.populateRecommendation(data_prep)

                # Step 7: Add pattern to data (if requested)
                # House idiom for a pure Output item (R/ctdnadynamics.b.R:139): gate on
                # isNotFilled() alone. A `type: Output` option is NOT an argument of the
                # generated wrapper, so gating on its value would make this column
                # unreachable from the R API; and in the GUI jamovi materialises the column
                # only when the Output control is enabled, so writing unconditionally here
                # is both harmless and simpler.
                if (self$results$addedPattern$isNotFilled()) {
                    private$.addPatternColumn()
                }

                private$.setPlotStates()

                # Notices are rendered by the on.exit handler registered above.
            },
            .validateInputs = function() {
                # Strict validation with clear error messages using HTML notices
                # Returns TRUE if validation passes, FALSE otherwise

                # Variable selection is checked BEFORE the empty-data check. self$data
                # contains only the columns named by the options, so with nothing selected
                # it is a 0-column frame whose nrow() is 0 -- the "No Data" branch would
                # then greet every freshly opened analysis with "Load data before running
                # the analysis" while the data were loaded and only the controls empty.
                # Nothing below up to the "No Data" check needs rows (an empty level list in a
                # pending notice reads "none").
                #
                # Empty state: nothing chosen at all. A freshly opened analysis must not
                # greet the user with a red ERROR before they have configured anything, so
                # return quietly here and let the Getting Started panel (welcome) speak. The checks
                # below then only fire once the user has STARTED choosing -- a partial
                # selection (a gold standard with no positive level, say) is the case that
                # genuinely needs telling, because nothing on screen says which control is
                # still empty. A Test 2 or Test 3 chosen first is a started selection too: it
                # returned here silently while Test 1 alone drew "No Gold Standard".
                if (!any(vapply(c("gold", "test1", "test2", "test3"), function(name)
                    private$.optionSelected(self$options[[name]]), logical(1)))) {
                    return(FALSE)
                }

                # A selected variable whose positive level is unset leaves the analysis pending:
                # an INFO, not an ERROR. allowNone stops jamovi pre-selecting each variable's
                # FIRST level, usually the negative one ("No", "0", "Benign"), so this is the
                # state every new analysis starts in. Test 2 and Test 3 are checked only when
                # selected, and a selected one is never silently dropped. The structural errors
                # below are still reported beside it; nothing is computed until all are chosen.
                pending <- FALSE
                # The three tests share one title and one sentence, with {test} filled in.
                for (slot in list(
                    list(var = "gold", level = "goldPositive", test = "",
                         title = .("Choose the Disease Present Level"),
                         ask = .('Select the level of {var} that means disease is present, under "Disease present level".')),
                    list(var = "test1", level = "test1Positive", test = .("Test 1")),
                    list(var = "test2", level = "test2Positive", test = .("Test 2")),
                    list(var = "test3", level = "test3Positive", test = .("Test 3")))) {
                    variable <- self$options[[slot$var]]
                    if (!private$.optionSelected(variable) ||
                        private$.optionSelected(self$options[[slot$level]])) next
                    pending <- TRUE
                    private$.addNotice("INFO",
                        slot$title %||% .fmt(.("Choose the Positive Level for {test}"), test = slot$test),
                        paste(
                            .fmt(slot$ask %||% .('Select the level of {var} that is a positive result for {test}, under "Positive level".'),
                                 var = variable, test = slot$test),
                            .fmt(.("Levels found: {levels}."), levels = private$.observedLevels(variable)),
                            .("Nothing is computed until it is chosen.")))
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    private$.addNotice(
                        "INFO",
                        .("No Gold Standard"),
                        .("A gold standard variable is required. Select a reference test.")
                    )
                    return(FALSE)
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    private$.addNotice(
                        "INFO",
                        .("No Test 1"),
                        .("Test 1 is required. Select at least one test variable.")
                    )
                    return(FALSE)
                }

                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        "ERROR",
                        .("No Data"),
                        .("No data are available. Load data before running the analysis.")
                    )
                    return(FALSE)
                }

                # Check if we have at least 2 tests for combination analysis
                has_test2 <- private$.optionSelected(self$options$test2)

                # Check test3 only if provided
                has_test3 <- private$.optionSelected(self$options$test3)
                if (has_test3 && !has_test2) {
                    private$.addNotice(
                        "ERROR",
                        .("Test 2 Required Before Test 3"),
                        .("Test 3 cannot be combined without Test 2. Select Test 2 and its positive level, or remove Test 3.")
                    )
                    return(FALSE)
                }

                selected_vars <- c(
                    self$options$gold,
                    self$options$test1,
                    if (has_test2) self$options$test2,
                    if (has_test3) self$options$test3
                )
                duplicated_vars <- unique(selected_vars[duplicated(selected_vars)])
                if (length(duplicated_vars) > 0) {
                    private$.addNotice(
                        "ERROR",
                        .("Variables Must Be Distinct"),
                        .fmt(
                            .("The reference standard and tests must use different variables. Select a different variable for: {variables}."),
                            variables = paste(duplicated_vars, collapse = ", ")
                        )
                    )
                    return(FALSE)
                }

                # Pending (see above): stop before anything reads a positive level.
                if (pending) {
                    return(FALSE)
                }

                # Minimum data requirement
                if (nrow(self$data) < 4) {
                    private$.addNotice(
                        "ERROR",
                        .("Insufficient Data"),
                        .("At least four cases are required for analysis.")
                    )
                    return(FALSE)
                }

                selected_levels <- list(
                    list(
                        var = self$options$gold,
                        level = self$options$goldPositive,
                        label = .("gold standard")
                    ),
                    list(
                        var = self$options$test1,
                        level = self$options$test1Positive,
                        label = .("Test 1")
                    )
                )
                if (has_test2) {
                    selected_levels <- c(selected_levels, list(list(
                        var = self$options$test2,
                        level = self$options$test2Positive,
                        label = .("Test 2")
                    )))
                }
                if (has_test3) {
                    selected_levels <- c(selected_levels, list(list(
                        var = self$options$test3,
                        level = self$options$test3Positive,
                        label = .("Test 3")
                    )))
                }

                selected_data <- private$.normalizeMissing(
                    self$data[, selected_vars, drop = FALSE])

                # Before the complete-case exits: a design in which a test or the reference
                # standard was obtained only after particular results can leave few or no
                # complete cases, and the exits below would then stop without saying why.
                # Unvalidated levels are harmless here: a positive level that never occurs
                # makes that test's predictor constant, which the screen skips, and a
                # reference level that never occurs only removes the stratification.
                tested <- selected_levels[-1]
                for (i in seq_along(tested)) {
                    tested[[i]]$index <- i
                }
                private$.screenSelectiveMissingness(selected_data, selected_levels[[1]], tested)

                n_complete <- nrow(jmvcore::naOmit(selected_data))
                if (n_complete == 0) {
                    private$.addNotice(
                        "ERROR",
                        .("No Complete Cases"),
                        .("No complete cases remain after removing missing data.")
                    )
                    return(FALSE)
                }

                # Validate selected levels before individual-test tables are populated.
                # Factor levels are checked against their declared levels so that an unused
                # level remains a valid selection in a one-class sample.
                for (sl in selected_levels) {
                    variable <- self$data[[sl$var]]
                    available <- if (is.factor(variable)) {
                        levels(variable)
                    } else {
                        unique(stats::na.omit(as.character(variable)))
                    }
                    if (!sl$level %in% available) {
                        private$.addNotice(
                            "ERROR",
                            .("Missing Level"),
                            .fmt(
                                .('The specified positive level "{level}" is not defined for variable "{variable}" ({label}). Select a level that exists in the data.'),
                                level = sl$level,
                                variable = sl$var,
                                label = sl$label
                            )
                        )
                        return(FALSE)
                    }
                }

                if (n_complete < 4) {
                    private$.addNotice(
                        "ERROR",
                        .("Insufficient Complete Cases"),
                        .fmt(
                            .("At least four complete cases are required for combination analysis; only {used} of {total} cases remain after excluding missing values."),
                            used = n_complete,
                            total = nrow(selected_data)
                        )
                    )
                    return(FALSE)
                }

                return(TRUE) # All validation checks passed
            },
            # Missing results that follow another result. Complete-case analysis assumes the
            # excluded cases are like the rest; in pathology they often are not. A test done only
            # after a positive or equivocal result (reflex: HER2 ISH after IHC 2+), a third test read
            # only when two disagree (referee), or a reference standard obtained only after a
            # positive test (partial verification) or only where the tests disagree (discrepant
            # resolution) leaves the excluded cases concentrated in particular results, so every row
            # is scored on a selected subgroup. Measured: Test 2 recorded only for Test 1-positive
            # patients made "Test 1 alone" read 100% / 0% (truth 89% / 78%), raised Serial
            # sensitivity from 70.9% to 79.7% and crowned "Test 2 alone", under only a generic
            # "Removed 351 case(s)".
            #
            # For the reference standard and each test k, against each other test j and, with two
            # or more other tests, "they disagree" (and, for the reference, "any test positive"): the
            # recording rates of k when the predictor is positive and negative, and rho = the larger
            # over the smaller (Inf when one is 0). If missingness follows the predictor, the
            # complete-case odds of a sensitivity (specificity) are multiplied by that ratio among
            # disease-present (disease-absent) cases, so a proportion can shift by up to
            # (sqrt(rho) - 1) / (sqrt(rho) + 1): 0.06 at rho 1.25, 0.17 at 2. For a test k, rho is
            # therefore the Mantel-Haenszel rate ratio pooled WITHIN the reference groups and p comes
            # from an exact conditional (Mantel-Haenszel) test stratified by the reference standard,
            # so size and significance measure the same thing. Missingness that follows disease
            # status alone leaves sensitivity and specificity unbiased, and a crude ratio both hid a
            # 2.5-fold within-group difference and inflated a 1.2-fold one to a serious warning at
            # large n. The pooled ratio, not the larger of the two group ratios: in a small group
            # that larger ratio is mostly noise (27 disease-present Test 1-negative cases turned a
            # true 1.5-fold difference into a serious warning).
            # The reference standard cannot be stratified on itself: crude ratio, Fisher's test.
            # Flagged at rho >= 1.25 with p < 0.001 and at least 5 missing; STRONG at rho >= 2. The
            # most significant predictor is reported for each k, once (ties, when p underflows on
            # large data, go to the larger test statistic), so a test that merely agrees with the
            # driver is not named as one. Counts are stated as fact, the mechanism only
            # conditionally: the same counts arise when results are withheld because of disease
            # status. Sentences about what the combination table contains are added by
            # .emitMissingnessNotices() only when that table is built and holds both reference
            # outcomes.
            # Simulated on this method (2,000 replicates per cell, n 60 / 150 / 600, two and three
            # tests): MCAR at 10-40% STRONG in at most 0.15% and WARNING in at most 0.55%;
            # missingness that follows disease status never flagged; reflex (also with 3% random
            # loss), partial verification, verification after any positive test, discrepant
            # resolution and referee designs STRONG in 99.9-100%; reflex on an equivocal level STRONG
            # in 70% at n 60 and 100% from n 150; a 2-fold difference within the disease-absent
            # group hidden by a crude ratio of 1 flagged in 98.5% at n 600; a 1.2-fold within-group
            # difference inflated by disease-driven missingness silent at n 20,000.
            .screenSelectiveMissingness = function(data, gold, tests) {
                gold_known <- !is.na(data[[gold$var]])
                disease <- gold_known & as.character(data[[gold$var]]) == gold$level
                positive <- function(item) as.character(data[[item$var]]) == item$level
                percent <- function(x, n) base::format(round(100 * x / n, 1), nsmall = 1)
                rates <- function(rec, value) {
                    list(o0 = sum(rec[!value]), n0 = sum(!value),
                         o1 = sum(rec[value]), n1 = sum(value))
                }
                # The larger recording rate over the smaller (Inf when one is 0, NA when both are or
                # an arm is empty), pooled over the reference groups by Mantel-Haenszel weights; one
                # group gives the plain ratio.
                # The counts are integers (sum() of logicals), so each product is formed in double:
                # an integer product overflows to NA once both factors pass ~46,000, and `if (NA)`
                # then stopped the analysis on ANY sample of about 93,000 cases with five values
                # missing at random. The counts stay integers for the notice text, where jmvcore's
                # format() prints a double 100000 as "1e+05".
                ratio <- function(groups) {
                    num <- 0
                    den <- 0
                    for (r in groups) {
                        total <- r$n0 + r$n1
                        if (r$n0 == 0 || r$n1 == 0) {
                            next
                        }
                        num <- num + as.numeric(r$o1) * r$n0 / total
                        den <- den + as.numeric(r$o0) * r$n1 / total
                    }
                    if (num == 0 && den == 0) NA_real_ else if (num == 0 || den == 0) Inf
                    else max(num / den, den / num)
                }
                # The p, and a test statistic that orders ties when p underflows to 0. Only the p of
                # the exact test is used: it also fits a conditional MLE and its interval with
                # uniroot() on natural-scale densities, which underflow on large, strongly selective
                # data (a pure reflex design of about 1,700 cases stopped the whole analysis with "f()
                # values at end points not of opposite sign"). There the asymptotic test is accurate,
                # so it is the fallback.
                association <- function(rec, value, stratum) {
                    sizes <- table(stratum)
                    keep <- stratum %in% names(sizes)[sizes >= 2L]
                    if (!any(keep)) {
                        return(list(p = 1, stat = 0))
                    }
                    lv <- c(FALSE, TRUE)
                    tab <- table(factor(rec[keep], lv), factor(value[keep], lv),
                                 factor(stratum[keep]))
                    if (dim(tab)[3] == 1L) {
                        p <- stats::fisher.test(tab[, , 1], conf.int = FALSE)$p.value
                        stat <- suppressWarnings(
                            unname(stats::chisq.test(tab[, , 1], correct = FALSE)$statistic))
                    } else {
                        asymptotic <- tryCatch({
                            doubled <- tab
                            storage.mode(doubled) <- "double"
                            stats::mantelhaen.test(doubled, correct = FALSE)
                        }, error = function(e) NULL)
                        stat <- if (is.null(asymptotic)) 0 else unname(asymptotic$statistic)
                        p <- tryCatch(stats::mantelhaen.test(tab, exact = TRUE)$p.value,
                                      error = function(e) NA_real_)
                        if (is.na(p)) {
                            p <- if (is.null(asymptotic)) 1 else asymptotic$p.value
                        }
                    }
                    list(p = if (is.finite(p)) p else 1, stat = if (is.finite(stat)) stat else 0)
                }
                empty <- list(lines = character(), consequences = character(), kinds = character())
                found <- list(STRONG_WARNING = empty, WARNING = empty)
                for (k in c(list(gold), tests)) {
                    k_is_gold <- identical(k$var, gold$var)
                    others <- Filter(function(item) !identical(item$var, k$var), tests)
                    if (length(others) == 0) {
                        next
                    }
                    calls <- lapply(others, positive)
                    predictors <- lapply(seq_along(others), function(i)
                        list(kind = "test", item = others[[i]], value = calls[[i]]))
                    if (length(others) >= 2L) {
                        hits <- Reduce(`+`, lapply(calls, as.integer))   # NA if any is missing
                        if (k_is_gold) {
                            predictors <- c(predictors, list(list(kind = "any", value = hits >= 1L)))
                        }
                        predictors <- c(predictors, list(list(
                            kind = "disagree", value = hits > 0L & hits < length(others))))
                    }
                    recorded <- !is.na(data[[k$var]])
                    best <- NULL
                    for (pr in predictors) {
                        rows <- !is.na(pr$value) & (k_is_gold | gold_known)
                        rec <- recorded[rows]
                        value <- pr$value[rows]
                        if (sum(!rec) < 5L || all(value) || !any(value)) {
                            next
                        }
                        strata <- NULL
                        if (k_is_gold) {
                            rho <- ratio(list(rates(rec, value)))
                            stratum <- rep(1L, length(rec))
                        } else {
                            stratum <- disease[rows]
                            strata <- list(present = rates(rec[stratum], value[stratum]),
                                           absent = rates(rec[!stratum], value[!stratum]))
                            rho <- ratio(strata)
                        }
                        if (is.na(rho) || rho < 1.25) {
                            next
                        }
                        a <- association(rec, value, stratum)
                        if (a$p >= 0.001 || (!is.null(best) &&
                                             (a$p > best$p || (a$p == best$p && a$stat <= best$stat)))) {
                            next
                        }
                        best <- list(pr = pr, p = a$p, stat = a$stat, rho = rho, rows = rows,
                                     crude = rates(rec, value), strata = strata)
                    }
                    if (is.null(best)) {
                        next
                    }
                    kind <- best$pr$kind
                    j <- best$pr$item
                    c0 <- best$crude
                    crude <- list(o0 = c0$o0, n0 = c0$n0, p0 = percent(c0$o0, c0$n0),
                                  o1 = c0$o1, n1 = c0$n1, p1 = percent(c0$o1, c0$n1))
                    two_groups <- !k_is_gold &&
                        all(vapply(best$strata, function(r) r$n0 > 0 && r$n1 > 0, logical(1)))
                    if (k_is_gold) {
                        line <- do.call(.fmt, c(list(switch(kind,
                            test = .("The reference standard was recorded for {o0} of {n0} cases with {test} negative ({p0}%) and {o1} of {n1} with {test} positive ({p1}%)."),
                            any = .("The reference standard was recorded for {o0} of {n0} cases where every test was negative ({p0}%) and {o1} of {n1} where at least one test was positive ({p1}%)."),
                            disagree = .("The reference standard was recorded for {o0} of {n0} cases where the tests agreed ({p0}%) and {o1} of {n1} where they disagreed ({p1}%).")),
                            test = if (is.null(j)) "" else j$label), crude))
                    } else if (two_groups) {
                        pr_ <- best$strata$present
                        ab <- best$strata$absent
                        groups <- list(
                            o0d = pr_$o0, n0d = pr_$n0, p0d = percent(pr_$o0, pr_$n0),
                            o1d = pr_$o1, n1d = pr_$n1, p1d = percent(pr_$o1, pr_$n1),
                            o0h = ab$o0, n0h = ab$n0, p0h = percent(ab$o0, ab$n0),
                            o1h = ab$o1, n1h = ab$n1, p1h = percent(ab$o1, ab$n1))
                        line <- if (kind == "disagree") {
                            do.call(.fmt, c(list(.("{missing} was recorded, among disease-present cases, for {o0d} of {n0d} where {first} and {second} agreed ({p0d}%) and {o1d} of {n1d} where they disagreed ({p1d}%), and among disease-absent cases for {o0h} of {n0h} where they agreed ({p0h}%) and {o1h} of {n1h} where they disagreed ({p1h}%)."),
                                missing = k$label, first = others[[1]]$label,
                                second = others[[2]]$label), groups))
                        } else {
                            do.call(.fmt, c(list(.("{missing} was recorded, among disease-present cases, for {o0d} of {n0d} with {test} negative ({p0d}%) and {o1d} of {n1d} with {test} positive ({p1d}%), and among disease-absent cases for {o0h} of {n0h} with {test} negative ({p0h}%) and {o1h} of {n1h} with {test} positive ({p1h}%)."),
                                missing = k$label, test = j$label), groups))
                        }
                    } else {
                        # One reference group lacks a case in one arm: crude counts.
                        line <- if (kind == "disagree") {
                            do.call(.fmt, c(list(.("Among cases with a reference result, {missing} was recorded for {o0} of {n0} where {first} and {second} agreed ({p0}%) and {o1} of {n1} where they disagreed ({p1}%)."),
                                missing = k$label, first = others[[1]]$label,
                                second = others[[2]]$label), crude))
                        } else {
                            do.call(.fmt, c(list(.("Among cases with a reference result, {missing} was recorded for {o0} of {n0} with {test} negative ({p0}%) and {o1} of {n1} with {test} positive ({p1}%)."),
                                missing = k$label, test = j$label), crude))
                        }
                    }
                    # The raw levels at which k was recorded, when j has more than two: the usual
                    # reflex trigger is an EQUIVOCAL level (IHC 2+), which the dichotomisation folds
                    # into the negative arm.
                    if (!is.null(j)) {
                        raw <- as.character(data[[j$var]][best$rows])
                        by_level <- tapply(recorded[best$rows], raw, mean)
                        if (length(by_level) > 2L && any(by_level == 0) && any(by_level > 0)) {
                            line <- paste(line, .fmt(.("It was recorded only when {test} was {levels}."),
                                test = j$label,
                                levels = paste0("\"", names(by_level)[by_level > 0], "\"",
                                                collapse = ", ")))
                        }
                    }
                    # With one arm never recorded, what that does to the combination table (worded
                    # by .emitMissingnessNotices() only if the table is built).
                    none0 <- c0$o0 == 0
                    none1 <- c0$o1 == 0
                    consequence <- if (!(none0 || none1)) {
                        ""
                    } else if (k_is_gold && kind == "test") {
                        .fmt(if (none0) {
                            .("No case with {test} negative has a reference result, so {test}'s sensitivity reads 100% and its specificity 0% by construction, in the combination table and in its own table, which also needs the reference standard.")
                        } else {
                            .("No case with {test} positive has a reference result, so {test}'s sensitivity reads 0% and its specificity 100% by construction, in the combination table and in its own table, which also needs the reference standard.")
                        }, test = j$label)
                    } else if (k_is_gold && kind == "any") {
                        if (none0) {
                            .("No case where every test was negative has a reference result: every pattern with all tests negative is empty in the combination table, and the Parallel rule reads sensitivity 100% and specificity 0% by construction.")
                        } else {
                            ""
                        }
                    } else if (k_is_gold) {
                        if (none0) {
                            .("The combination table therefore contains only cases where the tests disagreed: every pattern in which they agree is empty, and every row describes the tests in those cases only.")
                        } else {
                            .("The combination table therefore contains only cases where the tests agreed: every pattern in which they disagree is empty, and every row describes the tests in those cases only.")
                        }
                    } else if (kind == "disagree") {
                        .fmt(if (none0) {
                            .("The combination table therefore contains only cases where {first} and {second} disagreed: every pattern in which they agree is empty, and every row describes the tests in those cases only.")
                        } else {
                            .("The combination table therefore contains only cases where {first} and {second} agreed: every pattern in which they disagree is empty, and every row describes the tests in those cases only.")
                        }, first = others[[1]]$label, second = others[[2]]$label)
                    } else {
                        .fmt(if (none0) {
                            .("The combination table therefore contains no case with {test} negative: the \"{alone}\" row reads sensitivity 100% and specificity 0% by construction, every pattern with {test} negative is empty, and the other rows, the ranking included, describe the tests only among cases with {test} positive.")
                        } else {
                            .("The combination table therefore contains no case with {test} positive: the \"{alone}\" row reads sensitivity 0% and specificity 100% by construction, every pattern with {test} positive is empty, and the other rows, the ranking included, describe the tests only among cases with {test} negative.")
                        }, test = j$label, alone = private$.singleTestLabel(j$index))
                    }
                    severity <- if (best$rho >= 2) "STRONG_WARNING" else "WARNING"
                    found[[severity]]$lines <- c(found[[severity]]$lines, line)
                    found[[severity]]$consequences <- c(found[[severity]]$consequences, consequence)
                    found[[severity]]$kinds <- c(found[[severity]]$kinds,
                        if (!k_is_gold) "test" else if (kind == "test") "gold_test" else "gold_joint")
                }
                complete <- stats::complete.cases(data)
                private$.missingness <- list(
                    found = found,
                    both_outcomes = any(disease[complete]) && any(!disease[complete]))
            },
            .emitMissingnessNotices = function(table_built) {
                screen <- private$.missingness
                if (is.null(screen)) {
                    return()
                }
                # "By construction" figures hold only in a table with both reference outcomes: in a
                # one-class sample the cells they name are blank, and that notice says why.
                describe_table <- table_built && isTRUE(screen$both_outcomes)
                for (severity in names(screen$found)) {
                    f <- screen$found[[severity]]
                    if (length(f$lines) == 0) {
                        next
                    }
                    body <- if (describe_table) trimws(paste(f$lines, f$consequences)) else f$lines
                    # TODO (wording): the "gold_joint" sentence says the predictive values can be
                    # biased "for every test and rule"; the PPV of an exact-pattern row is not when
                    # the reference depends only on the pattern. Cautious, so left; a msgid change
                    # means redoing tr.po (release review 2026-10-01).
                    explanation <- c(
                        if ("gold_test" %in% f$kinds) .("If the reference standard was obtained because of these results (partial verification), sensitivity is overstated and specificity understated for a test whose positive results were verified more often, and the reverse when its negative results were, in the combination table and in that test's own table, which also needs the reference standard; its predictive values are not distorted by it. If instead the reference standard was withheld because of disease status itself, sensitivity and specificity are unaffected but the predictive values are distorted. These data cannot tell the two apart. Correcting partial verification (Begg and Greenes 1983) needs the reference standard on a random sample of the cases verified less often."),
                        if ("gold_joint" %in% f$kinds) .("If the reference standard was obtained only after a positive result on some test, or only where the tests disagreed (discrepant resolution), sensitivity, specificity and the predictive values can all be biased, in either direction, for every test and rule in the table. These data cannot tell this from results withheld because of disease status, and a correction needs the reference standard on a random sample of the cases verified less often."),
                        if ("test" %in% f$kinds) .("If a test was done because of another test's result, a strategy that combines them describes the second test only among the cases that triggered it, not the accuracy of testing everyone with both, and the second test's figures here and in its own table equal its accuracy in all patients only if the two tests err independently given disease status. The test that triggered the testing is unaffected in its own table, which does not need the other test. When a test was done only after a positive result, a serial strategy can still be evaluated on all patients: record its final result (positive only when every test is positive) as a separate variable and analyze it as a single test."),
                        if (table_built) {
                            .("Cases missing any of these results are left out of the combination table, so it describes only the cases these results selected, and tests that agree with the one named are affected as well.")
                        } else {
                            .("The analysis stopped before the combination table was computed; the error above says why.")
                        })
                    private$.addNotice(
                        severity,
                        if (severity == "STRONG_WARNING") {
                            .("Missing Results Follow Another Result")
                        } else {
                            .("Missing Results Partly Follow Another Result")
                        },
                        paste(c(body, explanation), collapse = " "),
                        refs = if (any(c("gold_test", "gold_joint") %in% f$kinds)) "Begg1983" else character(0))
                }
            },
            .prepareData = function() {
                # Data preparation following decision.b.R pattern

                # Get variable names
                goldVar <- self$options$gold
                test1Var <- self$options$test1

                # Collect all variables needed
                vars_needed <- c(goldVar, test1Var)

                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    vars_needed <- c(vars_needed, self$options$test2)
                }

                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    vars_needed <- c(vars_needed, self$options$test3)
                }

                required_levels <- list(
                    gold = list(
                        var = goldVar,
                        level = self$options$goldPositive,
                        label = .("gold standard")
                    ),
                    test1 = list(
                        var = test1Var,
                        level = self$options$test1Positive,
                        label = .("Test 1")
                    )
                )
                if (private$.optionSelected(self$options$test2)) {
                    required_levels$test2 <- list(
                        var = self$options$test2,
                        level = self$options$test2Positive,
                        label = .("Test 2")
                    )
                }
                if (private$.optionSelected(self$options$test3)) {
                    required_levels$test3 <- list(
                        var = self$options$test3,
                        level = self$options$test3Positive,
                        label = .("Test 3")
                    )
                }

                # Get subset of data
                subset_data <- private$.normalizeMissing(
                    self$data[, vars_needed, drop = FALSE])

                # .run() calls .validateInputs() first and returns on failure, and that
                # method already runs the "No Complete Cases" and per-variable "Missing
                # Level" checks over exactly these columns with exactly these helpers. The
                # duplicate copies that used to sit here were therefore unreachable: two
                # error paths that could never be taken, forty lines that had to be kept in
                # step with the originals by hand. A defensive guard is only defensive if
                # it can fire.
                n_before <- nrow(subset_data)
                mydata <- jmvcore::naOmit(subset_data)
                n_after <- nrow(mydata)

                # Cases were being dropped with no disclosure at all: every statistic below
                # was computed on the complete cases while the user still saw the dataset's
                # full size. Say how many went and why.
                if (n_after < n_before) {
                    n_removed <- n_before - n_after
                    private$.addNotice(
                        "WARNING",
                        .fmt(
                            .("Removed {n} case(s) with missing values"),
                            n = n_removed
                        ),
                        .fmt(
                            .("Complete-case analysis uses {used} of {total} cases ({percent}%) for the combination analysis. Cases missing the gold standard or any selected test were excluded. Individual-test tables use their own pairwise-complete denominators. If data are not missing completely at random, investigate the missingness pattern."),
                            used = n_after,
                            total = n_before,
                            percent = base::format(round(100 * n_after / n_before, 1), nsmall = 1)
                        )
                    )
                }

                # Convert to factors
                for (var in vars_needed) {
                    mydata[[var]] <- forcats::as_factor(mydata[[var]])
                }

                # A gold standard with only one observed level cannot support specificity
                # or NPV (there are no true negatives, or no true positives). Those came
                # back as a bare NA with nothing to explain them.
                # Decided on the DICHOTOMISED outcome - the same predicate the table blanking
                # in .analyzeSinglePattern uses - not on the raw labels. A reference standard
                # with levels Malignant / Benign / Atypical filtered to a subgroup with no
                # malignant case has two raw labels but no disease-present case: every cell
                # was blanked with no notice, and the only notice left said PPV and NPV "are
                # calculated".
                gold_chr <- as.character(mydata[[goldVar]])
                n_pos_gold <- sum(gold_chr == self$options$goldPositive, na.rm = TRUE)
                n_obs_gold <- sum(!is.na(gold_chr))
                if (n_obs_gold > 0 && (n_pos_gold == 0 || n_pos_gold == n_obs_gold)) {
                    one_class_message <- if (n_pos_gold > 0) {
                        .fmt(
                            .('Every complete case used for the combination analysis has gold standard "{level}", so the combination analysis contains no disease-absent cases. Specificity cannot be estimated; PPV and NPV are fixed at 100% and 0% by the sampling rather than by any test; Accuracy only repeats the sensitivity; and Balanced Accuracy and Youden\'s J, which combine sensitivity with specificity, cannot be formed. All of these are reported as blank in the combination table. So are LR+, LR- and the diagnostic odds ratio: each of those needs a specificity, and computing one from the continuity correction would report a number that reflects no patient. Diagnostic accuracy assessment requires both diseased and non-diseased cases. Individual-test tables use their own pairwise-complete cases and may still contain both outcomes.'),
                            level = self$options$goldPositive
                        )
                    } else {
                        .fmt(
                            .('No complete case used for the combination analysis has gold standard "{level}" (the level chosen as disease present), so the combination analysis contains no disease-present cases. Sensitivity cannot be estimated; PPV and NPV are fixed at 0% and 100% by the sampling rather than by any test; Accuracy only repeats the specificity; and Balanced Accuracy and Youden\'s J, which combine sensitivity with specificity, cannot be formed. All of these are reported as blank in the combination table. So are LR+, LR- and the diagnostic odds ratio: each of those needs a sensitivity, and computing one from the continuity correction would report a number that reflects no patient. Diagnostic accuracy assessment requires both diseased and non-diseased cases. Individual-test tables use their own pairwise-complete cases and may still contain both outcomes.'),
                            level = self$options$goldPositive
                        )
                    }
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Gold Standard Has Only One Outcome"),
                        one_class_message
                    )
                }

                # Anything that is not the chosen positive level becomes "Negative" below.
                # For a variable with more than two levels this silently folds equivocal or
                # third-category results into the negative arm and can bias every diagnostic
                # performance measure.
                for (rl in required_levels) {
                    lv <- unique(stats::na.omit(as.character(subset_data[[rl$var]])))
                    if (length(lv) > 2) {
                        others <- setdiff(lv, rl$level)
                        shown <- if (length(others) <= 5) paste(others, collapse = ", ")
                                 else paste(c(others[1:5], "..."), collapse = ", ")
                        private$.addNotice(
                            "STRONG_WARNING",
                            .fmt(
                                .("{variable} has {n} levels"),
                                variable = rl$var,
                                n = length(lv)
                            ),
                            .fmt(
                                .('Variable "{variable}" ({label}) has {n} levels: {levels}. Only "{positive}" is treated as positive; every other level ({others}) is counted as NEGATIVE. If any of those levels represent equivocal or indeterminate results, this recoding can bias sensitivity, specificity, predictive values, and likelihood ratios and make them difficult to interpret. Recode the variable to two levels and set equivocal results to missing if that is not what you intend.'),
                                variable = rl$var,
                                label = rl$label,
                                n = length(lv),
                                levels = paste(lv, collapse = ", "),
                                positive = rl$level,
                                others = shown
                            )
                        )
                    }
                }

                # The recoded columns live in their OWN frame, keyed by the source row names.
                # They used to be added with dplyr::mutate() to the frame that holds the
                # user's columns, so a test column that happened to be called
                # "goldVariable2" was overwritten by the recoded reference before the test
                # was read: Test 1 silently became the reference standard (a "perfect" test)
                # while the individual-test table, built separately, disagreed with it.
                # mydata has no missing values here (jmvcore::naOmit above).
                as_positive <- function(values, level) {
                    factor(ifelse(as.character(values) == level, "Positive", "Negative"),
                           levels = c("Positive", "Negative"))
                }
                work <- data.frame(
                    goldVariable2 = as_positive(mydata[[goldVar]], self$options$goldPositive),
                    row.names = rownames(mydata))

                # Sample-size guidance, following the graded ladder in R/decision.b.R:851-870.
                # Validation only requires four complete cases, so without this a pathologist
                # could feed in eight cases and receive sensitivity, specificity, PPV, NPV,
                # LR+, LR-, DOR and confidence intervals for eleven rules with nothing
                # signalling how thin the evidence is. The binding constraint for sensitivity,
                # PPV and every likelihood ratio is the number of DISEASED cases, not the
                # total, so both are reported.
                n_used <- nrow(mydata)
                if (n_used < 20) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .fmt(.("Very small sample: n = {n} complete cases"), n = n_used),
                        .("With fewer than 20 complete cases every proportion rests on a handful of patients, so one reclassified case moves sensitivity or specificity by several percentage points and the 95% confidence intervals are very wide. Read the intervals rather than the point estimates. The number of cases a diagnostic accuracy study needs depends on the expected sensitivity and specificity, the precision wanted and the prevalence (Buderer 1996); as a rough rule of thumb the intervals seldom narrow usefully below about 100 cases, and combination analysis splits those cases across four or eight patterns.")
                    )
                } else if (n_used < 50) {
                    private$.addNotice(
                        "WARNING",
                        .fmt(.("Small sample: n = {n} complete cases"), n = n_used),
                        .("Confidence intervals will be wide, and dividing this sample across four or eight result patterns leaves very few cases per pattern. Interpret the pattern rows as exploratory.")
                    )
                } else if (n_used < 100) {
                    private$.addNotice(
                        "INFO",
                        .fmt(.("Sample size: n = {n} complete cases"), n = n_used),
                        .("About 100 cases is a rough rule of thumb, not a requirement: the number actually needed depends on the expected sensitivity and specificity, the precision wanted and the prevalence (Buderer 1996). This sample supports preliminary estimates, particularly once it is divided across the result patterns.")
                    )
                }

                # Counted once here and reused by the prevalence notice below.
                n_gold_pos <- sum(work$goldVariable2 == "Positive", na.rm = TRUE)
                n_gold_obs <- sum(!is.na(work$goldVariable2))
                n_gold_neg <- n_gold_obs - n_gold_pos
                if (min(n_gold_pos, n_gold_neg) > 0 && min(n_gold_pos, n_gold_neg) < 10) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .fmt(
                            .("Only {n} cases in the smaller reference-standard group"),
                            n = min(n_gold_pos, n_gold_neg)
                        ),
                        .fmt(
                            .("This sample has {pos} disease-present and {neg} disease-absent complete cases. Sensitivity and PPV are limited by the disease-present count and specificity and NPV by the disease-absent count, so with fewer than 10 in one group the statistics that depend on it are driven by single patients and their confidence intervals span most of the possible range. Every likelihood ratio and diagnostic odds ratio inherits that instability."),
                            pos = n_gold_pos,
                            neg = n_gold_neg
                        )
                    )
                }

                # Prevalence is the gold-positive rate over the same complete cases every
                # pattern row is scored on, so it is identical in every row of the
                # combination table -- say it once here rather than per pattern. The
                # all-positive / all-negative case already has its own "Gold Standard Has
                # Only One Outcome" notice above and is excluded here.
                # Threshold and severity follow the meddecide house rule (5% / 95%,
                # STRONG_WARNING); see R/decisioncompare.b.R:597 and R/decisioncurve.b.R:1251.
                prevalence <- if (n_gold_obs > 0) n_gold_pos / n_gold_obs else NA_real_
                if (!is.na(prevalence) && prevalence > 0 && prevalence < 1 &&
                    (prevalence < 0.05 || prevalence > 0.95)) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Extreme Disease Prevalence"),
                        .fmt(
                            # "among observed reference results" would overstate the
                            # denominator: mydata is already joint complete cases, so this
                            # is the set the combination table is scored on, which is
                            # smaller than the pairwise denominators the individual-test
                            # tables use. Name it precisely: the notices panel sits above
                            # those tables, so this is read before they are.
                            .("Extreme disease prevalence in the combination analysis: {percent}% ({diseased}/{observed} complete cases). PPV and NPV are highly sensitive to prevalence and may not generalize to populations with different disease rates. Sensitivity and specificity can also vary across settings and case mix, and with so few cases in one arm the likelihood ratios and diagnostic odds ratios for every pattern rest on a very small denominator. Individual-test tables use their own pairwise denominators, so their prevalence may differ."),
                            percent = base::format(round(100 * prevalence, 1), nsmall = 1),
                            diseased = n_gold_pos,
                            observed = n_gold_obs
                        )
                    )
                }

                work$test1Variable2 <- as_positive(mydata[[test1Var]], self$options$test1Positive)
                if (private$.optionSelected(self$options$test2)) {
                    work$test2Variable2 <- as_positive(
                        mydata[[self$options$test2]], self$options$test2Positive)
                }
                if (private$.optionSelected(self$options$test3)) {
                    work$test3Variable2 <- as_positive(
                        mydata[[self$options$test3]], self$options$test3Positive)
                }

                # A test whose every recorded result falls on one side of its positive level
                # separates no patients, and it ran with nothing naming the cause: one test drew
                # "Poor Discrimination", whose advice about the likelihood ratios cannot apply to
                # a test with one result, and two or three tests drew nothing. Judged on ALL its
                # recorded results, not only the complete cases: a test left with one result
                # because missing values removed the other (reflex testing, partial
                # verification) is explained by the missingness notice, whose cause this one
                # would misname. With one reference outcome the notice above explains the blanks.
                if (n_gold_pos > 0 && n_gold_neg > 0) {
                    for (i in seq_len(3L)) {
                        rl <- required_levels[[paste0("test", i)]]
                        if (is.null(rl)) {
                            next
                        }
                        recorded <- stats::na.omit(as.character(subset_data[[rl$var]]))
                        n_positive <- sum(recorded == rl$level)
                        if (n_positive > 0 && n_positive < length(recorded)) {
                            next
                        }
                        private$.constantTests <- c(private$.constantTests, i)
                        message <- .fmt(if (n_positive == 0) {
                            .('No recorded result of {test} ({variable}) is "{level}", the level chosen as positive, so {test} cannot separate disease-present from disease-absent cases in these data: on its own it reads sensitivity 0% and specificity 100% by construction.')
                        } else {
                            .('Every recorded result of {test} ({variable}) is "{level}", the level chosen as positive, so {test} cannot separate disease-present from disease-absent cases in these data: on its own it reads sensitivity 100% and specificity 0% by construction.')
                        }, test = rl$label, variable = rl$var, level = rl$level)
                        if (private$.optionSelected(self$options$test2)) {
                            message <- paste(message, .("Every combination that uses it either reduces to a rule on the other tests alone or calls every case positive, or every case negative."))
                        }
                        private$.addNotice(
                            "STRONG_WARNING",
                            .fmt(if (n_positive == 0) .("{test} Has No Positive Results")
                                 else .("{test} Has Only Positive Results"), test = rl$label),
                            paste(message, .("Check the level chosen as positive and any filter applied to the data.")))
                    }
                }

                return(work)
            },
            .analyzeIndividualTest = function(test_num) {
                # Analyze individual test performance

                test_var <- self$options[[paste0("test", test_num)]]
                test_positive <- self$options[[paste0("test", test_num, "Positive")]]
                if (!private$.optionSelected(test_var) ||
                    !private$.optionSelected(test_positive)) {
                    return()
                }

                pair_data <- private$.normalizeMissing(
                    self$data[, c(self$options$gold, test_var), drop = FALSE])
                keep <- stats::complete.cases(pair_data)
                n_total <- nrow(pair_data)
                n_used <- sum(keep)
                # Defensive only, and carrying no notice for that reason: .run() calls this after
                # .validateInputs() has required four cases with the reference standard and every
                # selected test, and all of them are in this test's pairwise sample.
                if (n_used == 0) {
                    return()
                }
                if (n_used < n_total) {
                    private$.addNotice(
                        "INFO",
                        .fmt(
                            .("Test {test} Pairwise Denominator"),
                            test = test_num
                        ),
                        .fmt(
                            .("Individual Test {test} statistics use {used} of {total} cases with both the test and reference standard observed."),
                            test = test_num,
                            used = n_used,
                            total = n_total
                        )
                    )
                }

                pair_data <- pair_data[keep, , drop = FALSE]
                data_prep <- data.frame(
                    goldVariable2 = factor(
                        ifelse(
                            as.character(pair_data[[self$options$gold]]) ==
                                self$options$goldPositive,
                            "Positive", "Negative"
                        ),
                        levels = c("Positive", "Negative")
                    ),
                    testVariable2 = factor(
                        ifelse(
                            as.character(pair_data[[test_var]]) == test_positive,
                            "Positive", "Negative"
                        ),
                        levels = c("Positive", "Negative")
                    )
                )

                # Create contingency table
                cont_table <- table(data_prep$testVariable2, data_prep$goldVariable2)

                # Validate table structure
                if (!all(dim(cont_table) == c(2, 2))) {
                    return()
                }

                # Extract counts. table() cannot return an NA or negative count, and n_used > 0
                # above, so the counts are never invalid or all zero. The notices that used to
                # guard those cases could not fire; an unreachable message is still sent to
                # translators, so there is none.
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Individual-test statistics are proportions (sens/spec/PPV/NPV) that remain
                # well-defined with zero cells, so point estimates are computed on the raw
                # (unadjusted) contingency table -- mirroring the combination-pattern path and
                # keeping the displayed statistics consistent with the integer table above.
                # (No LR/DOR/CI are reported here, so no continuity correction is required.)

                # Get results tables
                if (test_num == 1) {
                    contTable <- self$results$individualTest1$test1Contingency
                    statsTable <- self$results$individualTest1$test1Stats
                } else if (test_num == 2) {
                    contTable <- self$results$individualTest2$test2Contingency
                    statsTable <- self$results$individualTest2$test2Stats
                } else {
                    contTable <- self$results$individualTest3$test3Contingency
                    statsTable <- self$results$individualTest3$test3Stats
                }

                # Populate contingency table
                contTable$setRow(rowKey = "Positive", values = list(
                    goldPos = tp,
                    goldNeg = fp,
                    total = tp + fp
                ))
                contTable$setRow(rowKey = "Negative", values = list(
                    goldPos = fn,
                    goldNeg = tn,
                    total = fn + tn
                ))
                contTable$setRow(rowKey = "Total", values = list(
                    goldPos = tp + fn,
                    goldNeg = fp + tn,
                    total = tp + fp + fn + tn
                ))

                # These four are the definitional proportions of the 2x2. epiR::epi.tests
                # was called here once per pattern (11-14 times a run, ~7 ms each and
                # constant in n) purely to read the same four numbers back out: verified
                # bit-identical to this computation over 400 random 2x2 tables, max
                # difference 0. Agreement with epiR is still asserted, in the place that
                # belongs -- tests/testthat/test-decisioncombine-release-review.R.
                sens <- private$.safeProp(tp, tp + fn)
                spec <- private$.safeProp(tn, fp + tn)
                ppv <- private$.safeProp(tp, tp + fp)
                npv <- private$.safeProp(tn, fn + tn)
                # Same rule as the combination table: with one reference class in this
                # test's pairwise sample, PPV and NPV are fixed by the sampling, not the test.
                if ((tp + fn) == 0 || (fp + tn) == 0) {
                    ppv <- NA_real_
                    npv <- NA_real_
                }

                # Populate statistics table
                statsTable$setRow(rowKey = "sens", values = private$.blankNA(list(
                    estimate = sens
                )))
                statsTable$setRow(rowKey = "spec", values = private$.blankNA(list(
                    estimate = spec
                )))
                statsTable$setRow(rowKey = "ppv", values = private$.blankNA(list(
                    estimate = ppv
                )))
                statsTable$setRow(rowKey = "npv", values = private$.blankNA(list(
                    estimate = npv
                )))
            },
            .analyzeCombinations = function(data_prep) {
                # Generate and analyze all test combinations

                # Always-visible notes: only what a clinician needs to read these numbers
                # safely. They used to run to 613 words on every run, most of it repeating
                # the opt-in About panel, which now holds the longer explanations (complete
                # cases versus the individual-test tables, why the correction is withheld,
                # joint estimation, Prevalence and Balanced Accuracy).
                #
                # Pattern-row semantics are the misreading this table invites ("+/-" is not
                # a rule's sensitivity), so they stay beside it. The note must quote the
                # labels the table prints: it once implied Serial had no row of its own.
                self$results$combinationTable$setNote(
                    "row_kinds",
                    jmvcore::.("Rows such as \"+/-\" are exact result patterns, each a group of patients with exactly that combination: Sensitivity is the share of diseased patients showing it, Specificity the share of non-diseased patients showing anything else, and LR- refers to \"any other pattern\". Read Youden's J there, not Accuracy or NPV. The named rows are rules applied to every patient; Serial (all pos) equals the all-positive pattern.")
                )
                # The note must state the rule the code ACTUALLY applies, per branch: on an
                # empty TEST margin the determinate ratio is kept (a pattern no patient
                # exhibits prints LR- exactly 1.00), and only an empty DISEASE margin blanks
                # all three. A reader told all three were blank would read that 1.00 as a
                # corrected number, so the per-branch rule stays visible.
                self$results$combinationTable$setNote(
                    "haldane",
                    jmvcore::.("A ratio is corrected only when the cell in its denominator is zero and its numerator is not (FP for LR+, TN for LR-, FP or FN for the odds ratio): 0.5 is then added to all four cells for that ratio and its confidence interval (Haldane-Anscombe for the odds ratio). A zero numerator gives exactly 0, uncorrected; a corrected value of 1 or below is left blank as not estimable. On a row with no test-positive patients LR+ and the odds ratio are blank and its LR- is exactly 1; the mirror case blanks LR- and shows LR+ as exactly 1.")
                )
                # Name the interval method; the ranking's own, different bound for J is
                # explained in a note on the ranking table, beside the sentence that quotes it.
                # Named because the other diagnostic analyses of this module report exact
                # (Clopper-Pearson) intervals for the same proportions (review XA-04).
                self$results$combinationTableCI$setNote(
                    "method",
                    jmvcore::.("These are Wilson score 95% confidence intervals. The decision and decisioncompare analyses report exact (Clopper-Pearson) intervals for the same proportions, which are usually a little wider, so the two sets of limits differ.")
                )
                self$results$combinationTable$setNote(
                    "youden_ci",
                    jmvcore::.("The 95% confidence interval beside Youden's J is the Agresti-Caffo interval for a difference of two independent proportions, sensitivity minus the false-positive rate. No row is adjusted for multiple comparisons or for choosing among rules: treat the best-looking row as a hypothesis to confirm in new patients.")
                )

                # Both tables are filled with addRow(), so they must be emptied first.
                # jamovi re-runs .run() on the SAME analysis object whenever any option
                # changes, and without this the pattern rows accumulated on every re-run
                # (5 -> 10 -> 15) until the duplicated rowKeys made $asDF fail outright
                # with "duplicate 'row.names' are not allowed", taking the recommendation
                # and every plot down with it.
                .deleteTableRows(self$results$combinationTable)
                .deleteTableRows(self$results$combinationTableCI)
                .deleteTableRows(self$results$combinationTableCIRatios)

                # PPV and NPV assume this sample's prevalence. That was an INFO notice on
                # every ordinary run, where a standing caveat about two columns does not
                # belong; it is a footnote quoting the prevalence now. Only when some row can
                # show a PPV or NPV: in a one-class sample every such cell is blank.
                gold_classes <- table(factor(data_prep$goldVariable2,
                                             levels = c("Positive", "Negative")))
                self$results$combinationTable$setNote(
                    "ppv_npv",
                    if (all(gold_classes > 0)) {
                        .fmt(jmvcore::.("PPV and NPV are computed at this sample's prevalence of {percent}% and apply to another population only if its prevalence is similar."),
                             percent = base::format(round(100 * gold_classes[["Positive"]] /
                                                              sum(gold_classes), 1), nsmall = 1))
                    }
                )

                has_test2 <- "test2Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test only - no combinations. The row-kind footnote describes
                    # pattern, "alone" and strategy rows that a single-test table does not
                    # have; setNote(key, NULL) removes it, including one left by a previous
                    # run with more tests.
                    self$results$combinationTable$setNote("row_kinds", NULL)
                    private$.analyzeSinglePattern(
                        data_prep, .("Test 1"),
                        data_prep$test1Variable2 == "Positive",
                        row_type = .("Single test"), components = 1L
                    )
                    private$.emitContinuityNotice()
                    private$.emitNonEstimableCINote()
                    private$.assessSparseCounts()
                    return()
                }

                # Row order: result patterns, then each test alone, then the strategies in
                # .strategyDefinitions() order (with three tests the pairwise rules come
                # before the rules over all the tests). The ranking lists named rows first
                # and keeps this order among them, so on a tie the rule that uses fewer tests
                # is shown.
                private$.analyzePatterns(data_prep)
                private$.addSingleTestRules(data_prep)
                private$.addStrategyRules(data_prep)
                private$.emitContinuityNotice()
                private$.emitNonEstimableCINote()
                private$.assessSparseCounts()
            },
            .calcWilsonCI = function(x, n, conf.level = 0.95) {
                # Wilson score confidence interval
                # More accurate than normal approximation, especially for small samples
                if (is.na(x) || n == 0) {
                    return(c(NA, NA))
                }

                p <- x / n
                z <- qnorm((1 + conf.level) / 2) # 1.96 for 95% CI

                # Wilson score formula
                denominator <- 1 + (z^2 / n)
                centre <- (p + (z^2 / (2 * n))) / denominator
                half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator

                # Return bounds, constrained to [0, 1]
                c(max(0, centre - half_width), min(1, centre + half_width))
            },
            # Youden's J = TP/(TP + FN) - FP/(FP + TN) is a difference of two independent
            # binomial proportions (diseased and non-diseased patients), so its interval is
            # Agresti-Caffo, as in R/decision.b.R and the inversion guards below: one success
            # and one failure added per group, then Wald. DescTools clamps it to [-1, 1]. On
            # the raw counts; NA where a reference group is empty and J is undefined.
            .youdenCI = function(tp, fn, fp, tn) {
                if (tp + fn == 0 || fp + tn == 0) {
                    return(c(lower = NA_real_, upper = NA_real_))
                }
                ci <- as.numeric(DescTools::BinomDiffCI(
                    x1 = tp, n1 = tp + fn, x2 = fp, n2 = fp + tn,
                    conf.level = 0.95, method = "ac"))
                c(lower = ci[2], upper = ci[3])
            },
            .estimableSe = function(se) {
                length(se) == 1L && is.finite(se) && se > 0
            },
            .emitNonEstimableCINote = function() {
                # Always written, never left over: a conditional note set on a previous run
                # would otherwise survive into a run where the condition no longer holds.
                # setNote(key, NULL) removes it.
                ratioTable <- self$results$combinationTableCIRatios
                zero_rows <- unique(private$.zeroRatioPatterns)
                ratioTable$setNote("zero_ratio_ci", if (length(zero_rows) > 0) {
                    .fmt(
                        jmvcore::.("In {n} row(s) ({patterns}) LR+, LR- or the diagnostic odds ratio is exactly 0 because the cell in its numerator is zero; no continuity correction is applied to that estimate. Its confidence interval runs from 0 to the upper limit of the same log-scale interval computed with 0.5 added to every cell (Haldane-Anscombe correction)."),
                        n = length(zero_rows),
                        patterns = paste(zero_rows, collapse = ", ")
                    )
                })
                patterns <- unique(private$.zeroSeIntervalPatterns)
                if (length(patterns) == 0) {
                    ratioTable$setNote("nonestimable_ci", NULL)
                    return()
                }
                ratioTable$setNote(
                    "nonestimable_ci",
                    .fmt(
                        jmvcore::.("The confidence interval is left blank for {n} row(s) ({patterns}) whose ratio is determinate but whose standard error is not estimable: the row has an empty test margin (no patient, or every patient, is positive under it), so the delta-method standard error on the log scale is exactly zero and an interval computed from it would be zero-width. A zero-width interval would assert that the ratio is known exactly on a row carrying no information, so the point estimate is shown without one."),
                        n = length(patterns),
                        patterns = paste(patterns, collapse = ", ")
                    )
                )
            },
            .emitContinuityNotice = function() {
                patterns <- unique(private$.continuityPatterns)
                if (length(patterns) == 0) {
                    return()
                }
                private$.addNotice(
                    "INFO",
                    .("Continuity Correction"),
                    .fmt(
                        .("A zero cell required a continuity correction in {n} row(s) ({patterns}). LR+ is corrected only when FP is zero and TP is not, LR- only when TN is zero and FN is not, and the diagnostic odds ratio only when FP or FN is zero and neither TP nor TN is: 0.5 is then added to all four cells for that ratio and its confidence interval (for the odds ratio, the Haldane-Anscombe correction). A ratio whose numerator cell is zero is reported as exactly 0 instead. A corrected ratio stands in for an infinite one, so when the correction would bring it to 1 or below it is left blank as not estimable. The corrected diagnostic odds ratio need not equal the ratio of the displayed likelihood ratios. Sensitivity, specificity, PPV and NPV use observed counts."),
                        n = length(patterns),
                        patterns = paste(patterns, collapse = ", ")
                    )
                )
            },
            .assessSparseCounts = function() {
                table_df <- self$results$combinationTable$asDF
                if (nrow(table_df) == 0) {
                    return()
                }

                # This used to scan only the named strategy rows, so the 4 or 8 exhaustive
                # pattern rows -- the MAJORITY of the table -- were never checked. Those rows
                # can carry a zero cell just as easily as a strategy row, and their LR+ and
                # diagnostic odds ratio were displayed to a pathologist with no caveat at
                # all. A pattern built on tp = 1, fp = 0 can show LR+ near 5 and read as
                # informative.
                cell_matrix <- as.matrix(
                    table_df[, c("tp", "fp", "fn", "tn"), drop = FALSE]
                )
                if (all(is.na(cell_matrix))) {
                    return()
                }
                row_minima <- suppressWarnings(apply(cell_matrix, 1, min, na.rm = TRUE))
                # A row with an empty margin has zero cells by construction, not by sparse
                # sampling, and .analyzeSinglePattern() corrects none of them. With no
                # disease-present or no disease-absent cases (a one-class sample) its ratios
                # are blank and its own notice says why; flagging it told the reader the
                # (blank) ratios were "unstable" and that its (blank) sensitivity could "still
                # be sound". With no test-positive or no test-negative cases (a pattern nobody
                # shows, a rule positive in every case or in none) its ratios are blank or
                # exactly 1 with no interval, as the "haldane" note says, yet every such row
                # was listed as resting on "very few cases".
                margin_empty <- (table_df$tp + table_df$fn) == 0 |
                    (table_df$fp + table_df$tn) == 0 |
                    (table_df$tp + table_df$fp) == 0 |
                    (table_df$fn + table_df$tn) == 0
                sparse <- is.finite(row_minima) & row_minima < 5 &
                    !(margin_empty %in% TRUE)
                if (!any(sparse)) {
                    return()
                }
                minimum_cell <- min(row_minima[sparse])
                affected <- table_df$pattern[sparse]

                # Deliberately says nothing about the candidate-rule ranking. A small cell
                # destabilises the LR and DOR intervals but does NOT disqualify a rule from
                # the Youden ranking -- a rule is often sparse in fp or fn precisely because
                # it is highly specific or highly sensitive. Ranking admissibility is judged
                # on the two reference-group sizes instead; see .populateRecommendation().
                #
                # WARNING, not STRONG_WARNING: as a serious warning it fired on all eight
                # bundled example datasets (n 50 to 400), because good rules are sparse by
                # construction, and it concerns only the ratio columns, whose intervals
                # already widen. A serious warning that always fires teaches readers to skip
                # the level that carries "Positive Levels May Be Inverted".
                private$.addNotice(
                    "WARNING",
                    .("Sparse Cell Counts"),
                    .fmt(
                        .("These rows have a 2-by-2 cell count below 5 (smallest cell {minimum}): {rows}. Their likelihood ratios, diagnostic odds ratios and confidence intervals rest on very few cases and may be unstable even when they look informative. At such counts the log-scale intervals for these ratios are approximate: their actual coverage can be above or below the nominal 95%, and where the 0.5 continuity correction was applied it can fall below it. Sensitivity, specificity and Youden's J on the same rows can still be sound, because those depend on the size of the two reference groups rather than on the smallest cell. Treat the ratio columns as exploratory and validate them in a larger independent sample."),
                        minimum = minimum_cell,
                        rows = paste(affected, collapse = ", ")
                    )
                )
            },
            .analyzeSinglePattern = function(data_prep, pattern_name, condition,
                                             row_type = NULL, components = NULL) {
                # Analyze a single test pattern
                if (is.null(row_type)) {
                    row_type <- .("Pattern")
                }

                # Create binary variable for this pattern
                data_prep$pattern_result <- ifelse(condition, "Positive", "Negative")
                data_prep$pattern_result <- factor(
                    data_prep$pattern_result,
                    levels = c("Positive", "Negative")
                )

                # Create contingency table
                cont_table <- table(data_prep$pattern_result, data_prep$goldVariable2)

                # Defensive only: both factors are built with an explicit
                # levels = c("Positive", "Negative"), so table() is unconditionally 2x2.
                # The guard stays, but it carries no notice -- an unreachable message is
                # still extracted into catalog.pot and handed to translators to translate
                # something no user can ever see.
                if (!all(dim(cont_table) == c(2, 2))) {
                    return()
                }

                # Extract counts
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Defensive only, same reasoning: table() cannot produce a negative or NA
                # count. No notice, for the same catalogue reason as above.
                if (any(is.na(c(tp, fp, fn, tn))) || any(c(tp, fp, fn, tn) < 0)) {
                    return()
                }

                # Defensive only: .validateInputs() guarantees at least four complete cases, so
                # the four counts never sum to 0 (a pattern nobody shows still has fn + tn > 0).
                # No notice, for the same catalogue reason.
                if (tp + fp + fn + tn == 0) {
                    return()
                }

                # Continuity correction, per ratio, only where a zero cell would make it infinite
                # (rules below). This keeps those ratios and their log-scale CIs finite.
                # A pattern that NO patient exhibits (tp = 0 and fp = 0) is routine whenever
                # two tests agree closely. The all-zero guard above cannot catch it, because
                # fn + tn is then the whole sample. Adding 0.5 to every cell would manufacture
                # a finite LR+ and diagnostic odds ratio out of a row containing no patients,
                # and PPV is undefined there. Report those as blank rather than inventing them.
                # The guard used to protect only the TEST margins. A DISEASE margin can be
                # structurally empty too -- a one-class reference standard makes (fp + tn)
                # or (tp + fn) zero in EVERY row -- and there the 0.5 correction fired and
                # manufactured LR+ / LR- / DOR out of nothing: with tp=40, fp=0, fn=20, tn=0
                # the row printed LR+ 1.33 [0.19, 9.50] and DOR 1.98, which are algebraically
                # just 2 x sens_adj and tp_adj/fn_adj and reflect no non-diseased patient,
                # because there are none. A continuity correction repairs a SAMPLING zero,
                # not a structural one.
                #
                # The 0.5 also used to fire when only a NUMERATOR cell was zero. With TP 0,
                # FP 1, FN 10, TN 99 that printed LR+ 3.06 and DOR 3.16 for a test that found
                # no diseased patient (exact LR+ 0, DOR 0): the correction reversed the
                # direction of the evidence (review 2026-10-21, XA-01). .diagRatioRules():
                # correct a ratio only when its denominator is empty; an empty numerator is
                # an exact 0 and an empty numerator AND denominator (an empty test margin)
                # is 0/0, left blank below. A correction that would land at or below 1 (it
                # replaces an infinite ratio) is "undefined" too, and blank.
                empty_class <- (tp + fn) == 0 || (fp + tn) == 0
                ratio_rules <- if (empty_class) {
                    list(lrp = "undefined", lrn = "undefined", dor = "undefined")
                } else {
                    .diagRatioRules(tp, fp, fn, tn)
                }
                lrp_corrected <- ratio_rules$lrp == "corrected"
                lrn_corrected <- ratio_rules$lrn == "corrected"
                use_continuity <- ratio_rules$dor == "corrected"
                if (any(unlist(ratio_rules) == "zero")) {
                    private$.zeroRatioPatterns <- c(private$.zeroRatioPatterns, pattern_name)
                }
                tp_lrp <- if (lrp_corrected) tp + 0.5 else as.numeric(tp)
                fp_lrp <- if (lrp_corrected) fp + 0.5 else as.numeric(fp)
                fn_lrp <- if (lrp_corrected) fn + 0.5 else as.numeric(fn)
                tn_lrp <- if (lrp_corrected) tn + 0.5 else as.numeric(tn)
                tp_lrn <- if (lrn_corrected) tp + 0.5 else as.numeric(tp)
                fp_lrn <- if (lrn_corrected) fp + 0.5 else as.numeric(fp)
                fn_lrn <- if (lrn_corrected) fn + 0.5 else as.numeric(fn)
                tn_lrn <- if (lrn_corrected) tn + 0.5 else as.numeric(tn)
                if (lrp_corrected || lrn_corrected || use_continuity) {
                    # Collected, not announced here: .emitContinuityNotice() reports every
                    # corrected pattern in a single banner once the sweep is finished.
                    private$.continuityPatterns <- c(
                        private$.continuityPatterns, pattern_name)
                }
                if (use_continuity) {
                    tp_adj <- tp + 0.5
                    fp_adj <- fp + 0.5
                    fn_adj <- fn + 0.5
                    tn_adj <- tn + 0.5
                } else {
                    # Doubles, as in the corrected branch: table() counts are integers, and the
                    # odds ratio's products overflowed to NA once two cells passed ~46,000, so
                    # the diagnostic odds ratio and its interval went blank, with no notice, on
                    # most rows of a 200,000-case analysis.
                    tp_adj <- as.numeric(tp)
                    fp_adj <- as.numeric(fp)
                    fn_adj <- as.numeric(fn)
                    tn_adj <- as.numeric(tn)
                }

                # These four are the definitional proportions of the 2x2. epiR::epi.tests
                # was called here once per pattern (11-14 times a run, ~7 ms each and
                # constant in n) purely to read the same four numbers back out: verified
                # bit-identical to this computation over 400 random 2x2 tables, max
                # difference 0. Agreement with epiR is still asserted, in the place that
                # belongs -- tests/testthat/test-decisioncombine-release-review.R.
                sens <- private$.safeProp(tp, tp + fn)
                spec <- private$.safeProp(tn, fp + tn)
                ppv <- private$.safeProp(tp, tp + fp)
                npv <- private$.safeProp(tn, fn + tn)
                acc <- (tp + tn) / (tp + fp + fn + tn)

                # In a one-class sample (every case disease-present, or every case
                # disease-absent) PPV and NPV are fixed at 100% / 0% by the sampling alone:
                # 0/fn is a real division that says nothing about the test. The one-class
                # notice promises these cells are blank, and they used to print 0% with a
                # Wilson interval beside it. Blank them, and their intervals below.
                one_class <- (tp + fn) == 0 || (fp + tn) == 0
                if (one_class) {
                    ppv <- NA_real_
                    npv <- NA_real_
                    # Accuracy = prev * sens + (1 - prev) * spec collapses to the one
                    # estimable proportion when prevalence is 0 or 1, so it only repeated
                    # the specificity (or sensitivity) under another name - beside a notice
                    # saying diagnostic accuracy cannot be assessed.
                    acc <- NA_real_
                }

                # .calcWilsonCI derives z from the requested level; these bounds used the
                # rounded literal 1.96, so the two interval families in one analysis were
                # built on different critical values. Use the same z everywhere.
                z_crit <- stats::qnorm(0.975)

                # Calculate Wilson CIs for all metrics
                total_pos <- tp + fn
                total_neg <- fp + tn
                total_test_pos <- tp + fp
                total_test_neg <- fn + tn
                total <- tp + fp + fn + tn

                sens_ci <- private$.calcWilsonCI(tp, total_pos)
                spec_ci <- private$.calcWilsonCI(tn, total_neg)
                ppv_ci <- if (one_class) c(NA_real_, NA_real_) else private$.calcWilsonCI(tp, total_test_pos)
                npv_ci <- if (one_class) c(NA_real_, NA_real_) else private$.calcWilsonCI(tn, total_test_neg)
                acc_ci <- if (one_class) c(NA_real_, NA_real_) else private$.calcWilsonCI(tp + tn, total)

                # Calculate additional metrics using adjusted counts for LR/DOR
                n <- tp + fp + fn + tn
                prev <- (tp + fn) / n
                balanced_acc <- (sens + spec) / 2
                youden_j <- sens + spec - 1
                youden_ci <- private$.youdenCI(tp, fn, fp, tn)

                # Likelihood ratios using per-ratio adjusted counts (LR+ corrected iff FP = 0 < TP,
                # LR- corrected iff TN = 0 < FN; an empty numerator gives an exact 0)
                lr_pos <- (tp_lrp / (tp_lrp + fn_lrp)) / (fp_lrp / (fp_lrp + tn_lrp))
                lr_neg <- (fn_lrn / (tp_lrn + fn_lrn)) / (tn_lrn / (fp_lrn + tn_lrn))

                # Diagnostic odds ratio using adjusted counts
                dor <- (tp_adj * tn_adj) / (fp_adj * fn_adj)

                # Blank only what is genuinely 0/0, not everything. With no predicted
                # positives sens = 0 and spec = 1, so LR+ is 0/0 while LR- is exactly 1;
                # with no predicted negatives sens = 1 and spec = 0, so LR- is 0/0 while
                # LR+ is exactly 1. Either way the diagnostic odds ratio is undefined. An
                # LR of 1 is worth showing -- it says the result does not move the odds.
                # An empty DISEASE margin leaves sensitivity (or specificity) undefined, so
                # all three ratios are undefined -- they cannot be read off a sample that
                # contains only diseased or only non-diseased patients. ratio_rules above
                # encodes both cases as "undefined".
                if (ratio_rules$lrp == "undefined") lr_pos <- NA_real_
                if (ratio_rules$lrn == "undefined") lr_neg <- NA_real_
                if (ratio_rules$dor == "undefined") dor <- NA_real_
                # Anything still non-finite is an undefined quantity, not a number: with
                # tp = fp = fn = 0 and tn > 0, sens_adj was 0/0 and lr_neg reached addRow()
                # as a literal "NaN" next to a correctly blank Sensitivity. Undefined cells
                # are NA here and become "" when written (.blankNA): jamovi prints a
                # numeric NA, NaN and Inf verbatim, and only "" is an empty cell.
                if (!is.finite(lr_pos)) lr_pos <- NA_real_
                if (!is.finite(lr_neg)) lr_neg <- NA_real_
                if (!is.finite(dor)) dor <- NA_real_

                # The rule itself, for the paired comparisons and the optimism bootstrap.
                private$.rules[[pattern_name]] <- list(
                    flag = as.logical(condition), type = row_type, components = components)

                # Add to main table
                combTable <- self$results$combinationTable
                combTable$addRow(rowKey = pattern_name, values = private$.blankNA(list(
                    pattern = pattern_name,
                    # A "+/-" row is a mutually exclusive group of patients, not a rule you
                    # can apply: its "Sensitivity" is P(this exact pattern | diseased). The
                    # Strategy rows are the rules. The columns read very differently for the
                    # two, and nothing used to distinguish them.
                    rowType = row_type,
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    prevalence = prev,
                    sens = sens,
                    spec = spec,
                    ppv = ppv,
                    npv = npv,
                    acc = acc,
                    balancedAccuracy = balanced_acc,
                    youden = youden_j,
                    youdenLower = youden_ci[["lower"]],
                    youdenUpper = youden_ci[["upper"]],
                    lrPos = lr_pos,
                    lrNeg = lr_neg,
                    dor = dor
                )))

                # Populate CI table with Wilson score intervals
                ciTable <- self$results$combinationTableCI
                # LR+, LR- and DOR are unbounded ratios, not proportions; they share no
                # sensible column format with sensitivity, so they get their own table.
                ratioTable <- self$results$combinationTableCIRatios

                # Sensitivity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_sens"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("Sensitivity"),
                    estimate = sens,
                    lower = sens_ci[1],
                    upper = sens_ci[2]
                )))

                # Specificity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_spec"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("Specificity"),
                    estimate = spec,
                    lower = spec_ci[1],
                    upper = spec_ci[2]
                )))

                # PPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_ppv"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("PPV"),
                    estimate = ppv,
                    lower = ppv_ci[1],
                    upper = ppv_ci[2]
                )))

                # NPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_npv"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("NPV"),
                    estimate = npv,
                    lower = npv_ci[1],
                    upper = npv_ci[2]
                )))

                # Accuracy with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_acc"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("Accuracy"),
                    estimate = acc,
                    lower = acc_ci[1],
                    upper = acc_ci[2]
                )))

                # LR+ with CI (log-scale transformation for CI, using per-ratio adjusted counts)
                # .estimableSe(): the estimate being legitimate says nothing about the
                # interval. On a structurally empty margin no correction is applied and the
                # delta-method SE collapses to EXACTLY 0 -- tp=30, fp=20, fn=0, tn=0 gave
                # se = sqrt(1/30 - 1/30 + 1/20 - 1/20) = 0 and printed LR+ 1.00 [1.00, 1.00],
                # a claim of perfect precision on a row with no negative-test patients. A
                # zero or non-finite SE means the interval is not estimable; blank it.
                if (!is.na(lr_pos) && lr_pos > 0) {
                    log_lr_pos <- log(lr_pos)
                    # Standard SE for log(LR+) using per-ratio adjusted counts
                    se_log_lr_pos <- sqrt((1 / tp_lrp) - (1 / (tp_lrp + fn_lrp)) +
                        (1 / fp_lrp) - (1 / (fp_lrp + tn_lrp)))
                    if (private$.estimableSe(se_log_lr_pos)) {
                        lr_pos_lower <- exp(log_lr_pos - z_crit * se_log_lr_pos)
                        lr_pos_upper <- exp(log_lr_pos + z_crit * se_log_lr_pos)
                    } else {
                        lr_pos_lower <- NA_real_
                        lr_pos_upper <- NA_real_
                        private$.zeroSeIntervalPatterns <- c(
                            private$.zeroSeIntervalPatterns, pattern_name)
                    }
                } else if (ratio_rules$lrp == "zero") {
                    # Exactly 0 (empty numerator): [0, U], U from the +0.5 table.
                    lr_pos_lower <- 0
                    lr_pos_upper <- .diagZeroRatioUpper("lrp", tp, fp, fn, tn)
                } else {
                    lr_pos_lower <- NA
                    lr_pos_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_lrPos"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("LR+"),
                    estimate = lr_pos,
                    lower = lr_pos_lower,
                    upper = lr_pos_upper
                )))

                # LR- with CI (log-scale transformation for CI, using per-ratio adjusted counts)
                if (!is.na(lr_neg) && lr_neg > 0) {
                    log_lr_neg <- log(lr_neg)
                    # Standard SE for log(LR-) using per-ratio adjusted counts
                    se_log_lr_neg <- sqrt((1 / fn_lrn) - (1 / (tp_lrn + fn_lrn)) +
                        (1 / tn_lrn) - (1 / (fp_lrn + tn_lrn)))
                    if (private$.estimableSe(se_log_lr_neg)) {
                        lr_neg_lower <- exp(log_lr_neg - z_crit * se_log_lr_neg)
                        lr_neg_upper <- exp(log_lr_neg + z_crit * se_log_lr_neg)
                    } else {
                        lr_neg_lower <- NA_real_
                        lr_neg_upper <- NA_real_
                        private$.zeroSeIntervalPatterns <- c(
                            private$.zeroSeIntervalPatterns, pattern_name)
                    }
                } else if (ratio_rules$lrn == "zero") {
                    lr_neg_lower <- 0
                    lr_neg_upper <- .diagZeroRatioUpper("lrn", tp, fp, fn, tn)
                } else {
                    lr_neg_lower <- NA
                    lr_neg_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_lrNeg"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("LR-"),
                    estimate = lr_neg,
                    lower = lr_neg_lower,
                    upper = lr_neg_upper
                )))

                # DOR with CI (log-scale transformation for CI, using adjusted counts)
                if (!is.na(dor) && dor > 0) {
                    log_dor <- log(dor)
                    # Approximate SE for log(DOR) using adjusted counts
                    se_log_dor <- sqrt(1 / tp_adj + 1 / fp_adj + 1 / fn_adj + 1 / tn_adj)
                    if (private$.estimableSe(se_log_dor)) {
                        dor_lower <- exp(log_dor - z_crit * se_log_dor)
                        dor_upper <- exp(log_dor + z_crit * se_log_dor)
                    } else {
                        dor_lower <- NA_real_
                        dor_upper <- NA_real_
                        private$.zeroSeIntervalPatterns <- c(
                            private$.zeroSeIntervalPatterns, pattern_name)
                    }
                } else if (ratio_rules$dor == "zero") {
                    dor_lower <- 0
                    dor_upper <- .diagZeroRatioUpper("dor", tp, fp, fn, tn)
                } else {
                    dor_lower <- NA
                    dor_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_dor"), values = private$.blankNA(list(
                    pattern = pattern_name,
                    statistic = .("DOR"),
                    estimate = dor,
                    lower = dor_lower,
                    upper = dor_upper
                )))
            },
            .patternConditions = function(data_prep) {
                # The 4- and 8-pattern condition lists were once written out verbatim in the
                # two- and three-test pattern builders AND again in
                # .populateFrequencyTables, and .addPatternColumn derived the same labels a
                # third way. Three copies of the definition of what "+/-/+" means is three
                # chances for the combination table, the cross-tabulation and the added
                # data column to disagree about the same patient. Generated once here, in
                # the same order as before so row keys and table ordering are unchanged.
                tests <- c("test1Variable2", "test2Variable2", "test3Variable2")
                tests <- tests[tests %in% names(data_prep)]
                if (length(tests) < 2) {
                    return(list())
                }
                # Row order must stay byte-identical to the hand-written lists this
                # replaced -- it is the table's row order and its rowKeys. That order is
                # binary counting with "+" before "-" and test 1 most significant, so the
                # LAST test varies fastest. expand.grid varies its FIRST argument fastest,
                # hence the reversed input and the reversed column read-back. Verified
                # equal to the originals for both the 4- and the 8-pattern case.
                cols <- rep(list(c("Positive", "Negative")), length(tests))
                signs <- expand.grid(rev(cols), stringsAsFactors = FALSE)
                signs <- signs[, rev(seq_len(ncol(signs))), drop = FALSE]
                out <- list()
                for (i in seq_len(nrow(signs))) {
                    row <- unlist(signs[i, ], use.names = FALSE)
                    label <- paste(ifelse(row == "Positive", "+", "-"), collapse = "/")
                    cond <- rep(TRUE, nrow(data_prep))
                    for (j in seq_along(tests)) {
                        cond <- cond & (data_prep[[tests[j]]] == row[j])
                    }
                    out[[label]] <- cond
                }
                out
            },

            .analyzePatterns = function(data_prep) {
                # The 4 or 8 exact result patterns; the arity comes from which testNVariable2
                # columns exist. Every pattern involves every selected test.
                patterns <- private$.patternConditions(data_prep)
                tests <- seq_len(ncol(private$.positiveMatrix(data_prep)))
                for (pattern_name in names(patterns)) {
                    private$.analyzeSinglePattern(data_prep, pattern_name,
                                                  patterns[[pattern_name]], components = tests)
                }
            },
            # Row label of test i used on its own in a 2- or 3-test analysis. One definition,
            # read by the row builder below and by the inversion guard, which has to map a
            # row back to the test it names.
            .singleTestLabel = function(i) {
                .fmt(.("Test {test} alone"), test = i)
            },
            .addSingleTestRules = function(data_prep) {
                # Each test on its own, scored on the same joint complete cases as the
                # patterns. Without these rows a combination could only be compared with
                # other combinations: with one strong test (sensitivity and specificity 0.90)
                # and one weak one (0.60), the ranking named Parallel and Serial, tied at
                # J = 0.50, while Test 1 alone had J = 0.80 and appeared nowhere.
                for (i in seq_len(3L)) {
                    column <- paste0("test", i, "Variable2")
                    if (!column %in% names(data_prep)) {
                        next
                    }
                    private$.analyzeSinglePattern(
                        data_prep,
                        private$.singleTestLabel(i),
                        data_prep[[column]] == "Positive",
                        row_type = .("Single test"), components = i
                    )
                }
            },
            # The selected tests' results as a logical matrix, one column per test
            # (TRUE = positive). matrix() keeps the shape when there is a single case.
            .positiveMatrix = function(data_prep) {
                columns <- intersect(paste0("test", seq_len(3L), "Variable2"), names(data_prep))
                matrix(vapply(columns, function(column) data_prep[[column]] == "Positive",
                              logical(nrow(data_prep))),
                       nrow = nrow(data_prep))
            },
            # The strategy rows, in table order. With three tests the Serial and Parallel
            # rule for each PAIR of tests comes first (a useless third test makes a two-test
            # rule the best, which no other row can express); then Parallel and Serial over
            # all the tests, and with three tests Majority. This one definition is read by
            # the combination table, by the paired-comparison rows built in .init() and by
            # their values in .run(), so a label or a rule cannot drift between them.
            # Serial (all pos) is numerically identical to the all-positive pattern but has
            # its own named row; .populateRecommendation() drops the pattern twin.
            .strategyDefinitions = function(k) {
                defs <- list()
                if (k == 3L) {
                    for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
                        key <- paste(pair, collapse = "")
                        defs[[length(defs) + 1L]] <- list(
                            key = paste0("serial", key), op = "all", components = pair,
                            label = .fmt(.("Serial {first}+{second} (both pos)"),
                                         first = pair[1], second = pair[2]))
                        defs[[length(defs) + 1L]] <- list(
                            key = paste0("parallel", key), op = "any", components = pair,
                            label = .fmt(.("Parallel {first}+{second} (>=1 pos)"),
                                         first = pair[1], second = pair[2]))
                    }
                }
                tests <- seq_len(k)
                defs[[length(defs) + 1L]] <- list(key = "parallel", op = "any",
                    components = tests, label = .("Parallel (>=1 pos)"))
                defs[[length(defs) + 1L]] <- list(key = "serial", op = "all",
                    components = tests, label = .("Serial (all pos)"))
                if (k == 3L) {
                    defs[[length(defs) + 1L]] <- list(key = "majority", op = "majority",
                        components = tests, label = .("Majority (>=2/3 pos)"))
                }
                defs
            },
            # A strategy's positive call: any (Parallel), all (Serial) or at least two
            # (Majority) of its component tests positive.
            .ruleFlag = function(positive, op, components) {
                hits <- rowSums(positive[, components, drop = FALSE])
                switch(op,
                    any = hits >= 1L,
                    all = hits == length(components),
                    majority = hits >= 2L)
            },
            .addStrategyRules = function(data_prep) {
                positive <- private$.positiveMatrix(data_prep)
                for (def in private$.strategyDefinitions(ncol(positive))) {
                    private$.analyzeSinglePattern(
                        data_prep, def$label,
                        private$.ruleFlag(positive, def$op, def$components),
                        row_type = .("Strategy"), components = def$components)
                }
            },
            # Each strategy against each test it combines, paired within the reference groups
            # on the same joint complete cases. A Parallel rule is positive whenever a test it
            # includes is, and a Serial rule only when that test is, so against its own tests a
            # Parallel rule can only GAIN sensitivity and LOSE specificity, and a Serial rule
            # the reverse: one discordant cell is zero by construction. For those rows
            # McNemar's p would be 2^(1 - b), blind to n, for a "no reclassification" null that
            # any single reclassified case refutes, and the Newcombe paired interval runs past
            # 0 into values the rule cannot take. Their change is the reclassified share, exactly
            # one binomial proportion for nested rules, with its Wilson score interval and no p.
            # Majority can move
            # either way: Newcombe's paired interval (shared with decisioncompare) and the
            # McNemar mid-p test (Fagerland 2013), not adjusted, like every row here.
            .populatePairedComparison = function(data_prep) {
                positive <- private$.positiveMatrix(data_prep)
                k <- ncol(positive)
                if (!isTRUE(self$options$showPairedComparison) || k < 2L) {
                    return()
                }
                paired <- self$results$pairedComparisonTable
                paired$setVisible(TRUE)
                # Newcombe and the McNemar mid-p are used only for Majority (three tests); the
                # keys stay in the .r.yaml so the module updater ships them.
                paired$setRefs(if (k == 3L) {
                    c("newcombe1998paired", "mcnemar1947", "Fagerland2013")
                } else {
                    character(0)
                })
                diseased <- data_prep$goldVariable2 == "Positive"
                for (def in private$.strategyDefinitions(k)) {
                    flag <- private$.ruleFlag(positive, def$op, def$components)
                    # The only direction a rule's sensitivity can move against its own test:
                    # +1 (Parallel), -1 (Serial), 0 for either (Majority). Specificity moves
                    # the other way.
                    way <- switch(def$op, any = 1L, all = -1L, 0L)
                    for (i in def$components) {
                        sens <- private$.pairedChange(flag[diseased], positive[diseased, i], way)
                        spec <- private$.pairedChange(!flag[!diseased], !positive[!diseased, i],
                                                      -way)
                        paired$setRow(rowKey = paste0(def$key, "_", i), values = private$.blankNA(list(
                            sensGained = sens$gained, sensLost = sens$lost,
                            sensChange = sens$change, sensLower = sens$lower,
                            sensUpper = sens$upper, sensP = sens$p,
                            specGained = spec$gained, specLost = spec$lost,
                            specChange = spec$change, specLower = spec$lower,
                            specUpper = spec$upper, specP = spec$p)))
                    }
                }
                paired$setNote("method", .fmt(
                    jmvcore::.("Each rule is compared with each test it combines on the same {n} complete cases: sensitivity among the {npos} disease-present and specificity among the {nneg} disease-absent cases. Gained counts the cases the rule classifies correctly and the test alone does not, Lost the reverse, and Change is the rule minus the test."),
                    n = length(diseased), npos = sum(diseased), nneg = sum(!diseased)))
                paired$setNote("nested", jmvcore::.("A Parallel rule can only gain sensitivity and lose specificity against a test it includes, and a Serial rule the reverse, so one of Gained and Lost is zero by construction and any reclassified case is a real difference. Their p is therefore left blank, and the 95% interval is the Wilson interval of the reclassified share. Weigh the gain against the loss: a gain on one side is not overall superiority."))
                paired$setNote("majority", if (k == 3L) {
                    jmvcore::.("Majority can move either way: its 95% interval is Newcombe's hybrid score interval for paired proportions and its p is from the McNemar mid-p test. Neither is adjusted for multiple comparisons, and when p is close to 0.05 the two can disagree: the interval may exclude 0 while p is above 0.05, or the reverse.")
                })
            },
            # One paired contrast within one reference group. rule and test are the correct
            # calls of each (positive among disease-present cases, negative among
            # disease-absent ones); way is the only direction the rule can move (+1 gain,
            # -1 loss) or 0 when it can move either way. An empty group (a one-class sample)
            # gives blanks, which is not the same as "no difference".
            .pairedChange = function(rule, test, way) {
                n <- length(rule)
                if (n == 0) {
                    return(list(gained = NA_integer_, lost = NA_integer_, change = NA_real_,
                                lower = NA_real_, upper = NA_real_, p = NA_real_))
                }
                gained <- sum(rule & !test)
                lost <- sum(!rule & test)
                change <- (gained - lost) / n
                if (way != 0L) {
                    interval <- way * private$.calcWilsonCI(if (way > 0L) gained else lost, n)
                    return(list(gained = gained, lost = lost, change = change,
                                lower = min(interval), upper = max(interval), p = NA_real_))
                }
                interval <- pairedProportionDifference(rule, test)
                discordant <- gained + lost
                p <- if (discordant == 0) {
                    1
                } else {
                    smaller <- min(gained, lost)
                    min(1, 2 * (stats::pbinom(smaller, discordant, 0.5) -
                                    0.5 * stats::dbinom(smaller, discordant, 0.5)))
                }
                list(gained = gained, lost = lost, change = change,
                     lower = interval$lower, upper = interval$upper, p = p)
            },
            .populateFrequencyTables = function(data_prep) {
                # Emptied for the same reason as the combination tables above.
                .deleteTableRows(self$results$goldFreqTable)
                .deleteTableRows(self$results$crossTabTable)

                # Gold standard frequency
                goldTable <- self$results$goldFreqTable
                gold_freq <- table(data_prep$goldVariable2)
                total <- sum(gold_freq)

                for (level in names(gold_freq)) {
                    level_label <- if (identical(level, "Positive")) {
                        .("Positive")
                    } else {
                        .("Negative")
                    }
                    goldTable$addRow(rowKey = level, values = list(
                        level = level_label,
                        count = as.integer(gold_freq[level]),
                        percent = as.numeric(gold_freq[level]) / total
                    ))
                }

                # Cross-tabulation
                crossTable <- self$results$crossTabTable
                has_test2 <- "test2Variable2" %in% names(data_prep)

                # A cross-tabulation of test PATTERNS needs at least two tests. With only
                # test 1 selected this returned early, leaving a fully empty "Test Results
                # Cross-Tabulation" on screen -- headers, no rows, no explanation. Hide the
                # structurally inapplicable table instead. This is shape-driven, not a
                # failure signal, and it is set on BOTH branches every run so it cannot
                # drift out of sync with the declarative visible: (showFrequency).
                crossTable$setVisible(has_test2 && isTRUE(self$options$showFrequency))
                if (!has_test2) {
                    return()
                }

                patterns <- private$.patternConditions(data_prep)

                for (pattern_name in names(patterns)) {
                    pattern_data <- data_prep[patterns[[pattern_name]], ]
                    gold_pos <- sum(pattern_data$goldVariable2 == "Positive", na.rm = TRUE)
                    gold_neg <- sum(pattern_data$goldVariable2 == "Negative", na.rm = TRUE)

                    crossTable$addRow(rowKey = pattern_name, values = list(
                        testCombo = pattern_name,
                        goldPos = gold_pos,
                        goldNeg = gold_neg,
                        total = gold_pos + gold_neg
                    ))
                }
            },
            # How well the best rule discriminates, on the conventional equivalent-AUC bands: for
            # a binary rule AUC = (1 + J) / 2, and an AUC below 0.70 (J below 0.40) is read as
            # poor (Hosmer, Lemeshow & Sturdivant 2013), as R/decision.b.R does for one test.
            # Checked only when the top-ranked rule's J, as printed, is below 0.40; every rule
            # scored is then below 0.40 as well, as a point estimate. Also used for a single
            # test whose J is at or below 0 within sampling variation, which used to get the
            # ungraded serious "No Rule Performs Better Than Chance".
            #
            # The verdict is graded on the Agresti-Caffo intervals of EVERY rule scored (`scored`:
            # the estimable rules, whatever their J), because the notices speak for all of them
            # and the intervals differ in width. Graded on the top rule alone, "no evidence that
            # any rule beats chance" was printed beside a narrower interval that excluded 0, and
            # "poor" beside a wider one that reached 0.40. Serious only when every interval lies
            # wholly below 0.40, so poor discrimination holds even allowing for sampling error.
            # Intervals that include 0 and reach 0.40 leave the sample inconclusive, which a plain
            # warning says: as a serious warning it fired on about a third of samples of an
            # acceptable test at 10 to 15 patients per group (CHANGELOG 2026-09-29: a guard on a
            # point estimate needs a sampling-error gate). J and the bounds are compared as
            # printed (three decimals), so a printed 0.000 or 0.400 always gets the verdict its
            # text states. Silent when an inversion warning was raised, or when a single test has
            # only one result (.constantTests): each is the specific diagnosis, and this notice's
            # advice about the likelihood ratios cannot apply to a test with one result. One test
            # gets its own wording, since nothing was compared or chosen.
            .assessTopRuleDiscrimination = function(best, scored, inversion_flagged, multi_test) {
                if (inversion_flagged || (!multi_test && length(private$.constantTests) > 0)) {
                    return()
                }
                j3 <- round(best$youden, 3)
                lowers <- round(scored$youdenLower, 3)
                uppers <- round(scored$youdenUpper, 3)
                if (!is.finite(j3) || j3 >= 0.4 - 1e-8 || length(lowers) == 0 ||
                    !all(is.finite(c(lowers, uppers)))) {
                    return()
                }
                f3 <- function(x) base::format(x, nsmall = 3)
                values <- list(
                    youden = f3(j3), rule = best$pattern,
                    lower = f3(round(best$youdenLower, 3)),
                    upper = f3(round(best$youdenUpper, 3)),
                    auc = sub("0$", "", base::format(round((1 + j3) / 2, 4), nsmall = 4)))
                if (all(uppers < 0.4 - 1e-8)) {
                    type <- "STRONG_WARNING"
                    title <- .("Poor Discrimination")
                    template <- if (multi_test) {
                        .("No rule scored here reaches acceptable discrimination. The highest Youden's J is {youden}, for {rule} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}), and the 95% confidence interval of every rule lies wholly below 0.40, the equivalent of an area under the ROC curve of 0.70, below which discrimination is conventionally read as poor (Hosmer, Lemeshow and Sturdivant 2013). This is a statistical convention, not a verdict on clinical usefulness: the likelihood ratios still show how far each result moves the probability of disease.")
                    } else {
                        .("Youden's J for {rule} is {youden} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}). The whole interval lies below 0.40, the equivalent of an area under the ROC curve of 0.70, below which discrimination is conventionally read as poor (Hosmer, Lemeshow and Sturdivant 2013). This is a statistical convention, not a verdict on clinical usefulness: the likelihood ratios still show how far each result moves the probability of disease.")
                    }
                } else if (all(lowers <= 0)) {
                    type <- "WARNING"
                    title <- .("Discrimination Not Established")
                    # "No interval wholly above 0" is what the condition tests. It used to say every
                    # interval INCLUDES 0, which a rule wholly below 0 contradicts: an exact pattern
                    # at J -0.30, interval -0.486 to -0.076, beside a top rule at 0.20. One test
                    # cannot reach this with an interval wholly below 0: the inversion warning fires.
                    template <- if (multi_test) {
                        .("The highest Youden's J is {youden}, for {rule} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}). No rule scored here has a 95% confidence interval wholly above 0, so this sample does not show that any of them separates disease-present from disease-absent cases better than chance; at least one interval also reaches 0.40, the equivalent of an area under the ROC curve of 0.70 (Hosmer, Lemeshow and Sturdivant 2013), so acceptable discrimination is not ruled out either. The intervals are not adjusted for choosing among the rules. Check the levels chosen as positive, and whether the sample gives the precision you need.")
                    } else {
                        .("Youden's J for {rule} is {youden} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}). The interval includes 0, so this sample does not show that the test separates disease-present from disease-absent cases better than chance; it also reaches 0.40, the equivalent of an area under the ROC curve of 0.70 (Hosmer, Lemeshow and Sturdivant 2013), so acceptable discrimination is not ruled out either. Check the level chosen as positive, and whether the sample gives the precision you need.")
                    }
                } else {
                    type <- "WARNING"
                    title <- .("Discrimination May Be Poor")
                    template <- if (multi_test) {
                        .("The highest Youden's J is {youden}, for {rule} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}), below 0.40, the equivalent of an area under the ROC curve of 0.70, under which discrimination is conventionally read as poor (Hosmer, Lemeshow and Sturdivant 2013). The 95% confidence interval of at least one rule reaches 0.40, so this sample cannot rule out acceptable discrimination; a larger sample could settle it.")
                    } else {
                        .("Youden's J for {rule} is {youden} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}), below 0.40, the equivalent of an area under the ROC curve of 0.70, under which discrimination is conventionally read as poor (Hosmer, Lemeshow and Sturdivant 2013). The interval reaches 0.40, so this sample cannot rule out acceptable discrimination; a larger sample could settle it.")
                    }
                }
                private$.addNotice(type, title, do.call(.fmt, c(list(template), values)),
                                   refs = "HosmerLemeshow2013")
            },
            # Bootstrap optimism of the ranking (Harrell 2015) for the WHOLE selection: every
            # scored rule competes in every resample, including rules whose observed J is at or
            # below 0, in the ranking's order and with its tie-break. Every rule is a union of
            # exact result patterns and each patient is described by a pattern and a reference
            # result, so drawing pattern counts with rmultinom() within each reference group is
            # exactly a patient bootstrap stratified by reference status (stratified, so the
            # 10-per-group eligibility holds in every resample).
            # Each resample's optimism uses the ranked rule r as a control variate:
            # [J_b(pick) - J_b(r)] + [J_orig(r) - J_orig(pick)]. J is linear in the counts, so
            # E*[J_b(r)] = J_orig(r) and the expectation is Harrell's, but both brackets compare
            # an argmax, so every term is >= 0. The plain J_b(pick) - J_orig(pick) went negative
            # about half the time when one rule clearly dominated, printing a corrected J above the
            # observed one; there the control variate also cuts the Monte Carlo error sharply. Near
            # ties its per-resample spread can be 1.2-1.8 times the plain one, which the printed
            # Monte Carlo standard error reports honestly.
            .optimismCorrection = function(scored, best, data_prep) {
                diseased <- data_prep$goldVariable2 == "Positive"
                patterns <- names(private$.patternConditions(data_prep))
                in_pattern <- matrix(vapply(patterns, function(p) private$.rules[[p]]$flag,
                                            logical(nrow(data_prep))),
                                     nrow = nrow(data_prep))
                # Which patterns make up each rule, read off one patient per pattern (an empty
                # pattern never receives a resampled case, so its membership is irrelevant).
                first_case <- apply(in_pattern, 2, function(x) match(TRUE, x))
                weight <- 1 * t(vapply(scored$pattern, function(rule) {
                    !is.na(first_case) & private$.rules[[rule]]$flag[first_case]
                }, logical(length(patterns))))
                n_pos <- colSums(in_pattern[diseased, , drop = FALSE])
                n_neg <- colSums(in_pattern[!diseased, , drop = FALSE])
                j_orig <- drop(weight %*% (n_pos / sum(n_pos)) - weight %*% (n_neg / sum(n_neg)))
                r <- match(best$pattern, scored$pattern)
                boots <- as.integer(self$options$nBoot)
                # The kind is pinned too, so "Random seed: N" alone reproduces the numbers whatever
                # RNG kind the session (or a calling script) has selected.
                withr::local_seed(self$options$seed, .rng_kind = "Mersenne-Twister",
                                  .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
                draws_pos <- stats::rmultinom(boots, sum(n_pos), n_pos / sum(n_pos))
                draws_neg <- stats::rmultinom(boots, sum(n_neg), n_neg / sum(n_neg))
                j_boot <- weight %*% (draws_pos / sum(n_pos)) - weight %*% (draws_neg / sum(n_neg))
                pick <- apply(j_boot, 2, function(j) which(j >= max(j) - 1e-9)[1])
                optimism <- (j_boot[cbind(pick, seq_len(boots))] - j_boot[r, ]) +
                    (j_orig[r] - j_orig[pick])
                list(optimism = mean(optimism), corrected = best$youden - mean(optimism),
                     mcse = stats::sd(optimism) / sqrt(boots), stability = mean(pick == r),
                     B = boots, rules = nrow(scored))
            },
            .populateRecommendation = function(data_prep) {
                # Called on every .run(), not only when showRecommendation is ticked: the
                # two inversion STRONG_WARNINGs below are the analysis's only detectors of a
                # reversed positive level, and .assessTopRuleDiscrimination() the only check
                # on how well the best rule discriminates. Only the table is gated, at the
                # setVisible() near the end of this method.
                combTable <- self$results$combinationTable
                # Defensive only, and carrying no notice for that reason: .run() calls
                # .analyzeCombinations() immediately before this, every branch of which
                # adds at least one row, and .validateInputs() has already guaranteed at
                # least four complete cases -- so .analyzeSinglePattern()'s own skip
                # branches (non-2x2 table, negative/NA counts, all-zero counts) are
                # themselves unreachable. An unreachable message would still be extracted
                # into catalog.pot and handed to translators.
                if (combTable$rowCount == 0) {
                    return()
                }

                table_df <- combTable$asDF

                # One test gives one rule, so there is nothing to rank: the table stays hidden
                # (see the return after the discrimination check below), but the guards in
                # this method still run. Say why the ticked ranking is absent.
                multi_test <- private$.optionSelected(self$options$test2)
                if (!multi_test && isTRUE(self$options$showRecommendation)) {
                    private$.addNotice(
                        "INFO",
                        .("Ranking Needs Two or More Tests"),
                        .("With one test there is only one rule to score, the test itself, so no ranking table is shown; the combination table reports that test on its own. Select a second test to rank the combinations.")
                    )
                }

                # Rank every estimable classifier represented by the table, including exact
                # result-pattern rules and named clinical strategies. Only one pair of rows
                # is the same RULE: Serial (all pos) and the all-positive pattern, which
                # classify every patient identically by definition.
                #
                # Eligibility is gated on the two REFERENCE-GROUP sizes, not on the smallest
                # cell. Youden's J = sensitivity + specificity - 1, and the precision of
                # those two components depends on how many diseased and non-diseased cases
                # there are (tp + fn and fp + tn), not on how the rule happens to split them.
                # The previous min(tp, fp, fn, tn) >= 5 gate is the rule of thumb for the
                # log-scale LR and DOR intervals, and applying it here inverted the ranking:
                # a rule is sparse in fp precisely BECAUSE it is highly specific, and sparse
                # in fn precisely BECAUSE it is highly sensitive. On a realistic 2-test table
                # it dropped "+/+" (J = 0.633, specificity 0.98, fp = 3) and Parallel
                # (J = 0.617, sensitivity 1.00, fn = 0) and crowned "+/-" at J = 0.29 -- it
                # excluded the two best rules for being good. Cell sparsity still matters for
                # the ratio estimates, and .assessSparseCounts() reports it separately.
                candidates <- table_df
                candidates <- candidates[is.finite(candidates$youden), , drop = FALSE]

                # Named rows first, so which.max()'s first-max tie-break resolves to a rule
                # a clinician can apply rather than to a pattern label, which describes a
                # group of patients.
                candidates <- candidates[
                    order(!(candidates$rowType %in% c(.("Strategy"), .("Single test"))),
                          method = "radix"),
                    , drop = FALSE
                ]
                # Merge ONLY the Serial / all-positive twin. This used to de-duplicate on
                # the tp|fp|fn|tn string, which also merged DIFFERENT rules that happen to
                # share a 2-by-2 in this sample: "+/-" and "-/+" with identical counts left
                # one of them out of the tie list and the candidate count ("2 rules tie"
                # when 3 did; "None of the 2 candidate rules" when 5 were scored).
                if (.("Serial (all pos)") %in% candidates$pattern) {
                    candidates <- candidates[
                        !grepl("^\\+(/\\+)+$", candidates$pattern), , drop = FALSE]
                }

                # Inverted-positive-level guard, asked of every estimable rule BEFORE the
                # eligibility filter below, so it also fires in the small series that filter
                # discards and when no ranking is produced at all.
                #
                # First each test on its own. A test whose sensitivity and specificity add up
                # to less than 1 is worse than chance, the signature of a positive level chosen
                # the wrong way round. That used to be asked only in a single-test analysis.
                # With two or three tests only the collective signature further down was
                # checked, and flipping ONE test does not produce it: with the weaker of two
                # tests flipped, Parallel and Serial stayed just above chance (J 0.017 and
                # 0.083), nothing fired, and the ranking crowned the pattern "+/-" --
                # "positive when Test 2 is negative" -- as the best rule.
                #
                # Graded by sampling error. A correctly coded test with no diagnostic value
                # has J below 0 about half the time at ANY sample size, so a bare J < 0 put a
                # serious warning on about half of such analyses (measured: 41 of 100). The
                # serious warning now needs the Agresti-Caffo 95% interval for J -- the
                # difference of two independent proportions, P(T+ | D) - P(T+ | N) -- to lie
                # wholly below 0; a J below 0 within sampling variation gets a note instead.
                # Once the serious warning fires the chance notice below is suppressed, so the
                # reader gets one specific warning.
                inversion_flagged <- FALSE
                if (nrow(candidates) > 0) {
                    top_row <- candidates[
                        which(candidates$youden >= max(candidates$youden) - 1e-9)[1], ,
                        drop = FALSE]
                    named_rows <- candidates$rowType %in%
                        c(.("Strategy"), .("Single test"))
                    negative <- candidates$rowType == .("Single test") & candidates$youden < 0
                    if (any(negative)) {
                        tested <- candidates[negative, , drop = FALSE]
                        # Row label -> test name: "Test 2 alone" names Test 2. A single-test
                        # analysis labels its only row "Test 1", which is the name already.
                        test_names <- vapply(tested$pattern, function(label) {
                            for (i in seq_len(3L)) {
                                if (identical(label, private$.singleTestLabel(i))) {
                                    return(c(.("Test 1"), .("Test 2"), .("Test 3"))[i])
                                }
                            }
                            label
                        }, character(1), USE.NAMES = FALSE)
                        # The Agresti-Caffo interval the combination table shows beside J.
                        ci <- cbind(tested$youdenLower, tested$youdenUpper)
                        # Three decimals, except that a J closer to 0 than that keeps its sign
                        # and two significant figures, so "below 0" never sits beside "0.000".
                        fmt_j <- function(x) vapply(x, function(v) {
                            if (v != 0 && abs(v) < 0.0005) base::format(signif(v, 2))
                            else base::format(round(v, 3), nsmall = 3)
                        }, character(1))
                        # One translatable template per test: built with paste0() from English
                        # fragments, "(J = ..., 95% CI ... to ...)" stayed English inside an
                        # otherwise Turkish notice.
                        described <- vapply(seq_len(nrow(tested)), function(i) {
                            .fmt(.("{test} (J = {youden}, 95% CI {lower} to {upper})"),
                                 test = test_names[i], youden = fmt_j(tested$youden[i]),
                                 lower = fmt_j(ci[i, 1]), upper = fmt_j(ci[i, 2]))
                        }, character(1))
                        n_cases <- tested$tp[1] + tested$fp[1] + tested$fn[1] + tested$tn[1]
                        clear <- ci[, 2] < 0
                        if (any(clear)) {
                            message <- if (sum(clear) == 1L) {
                                .fmt(
                                    .("{test} classifies worse than chance in this sample, beyond sampling variation: Youden's J = {youden} (Agresti-Caffo 95% CI {lower} to {upper}) on the {n} complete cases analyzed here. Its sensitivity and specificity add up to less than 1, so calling its other level positive would do better; this is the signature of a reversed positive level. Check the level chosen as positive for {test} and for the reference standard before interpreting any of these results."),
                                    test = test_names[clear],
                                    youden = fmt_j(tested$youden[clear]),
                                    lower = fmt_j(ci[clear, 1]),
                                    upper = fmt_j(ci[clear, 2]),
                                    n = n_cases
                                )
                            } else {
                                .fmt(
                                    .("Several tests classify worse than chance in this sample, beyond sampling variation, on the {n} complete cases analyzed here: {tests}. For each, sensitivity and specificity add up to less than 1, the signature of a reversed positive level. Check the levels chosen as positive for these tests and for the reference standard before interpreting any of these results; when every test is below chance, the reference standard's level is the likelier cause."),
                                    n = n_cases,
                                    tests = paste(described[clear], collapse = "; ")
                                )
                            }
                            if (multi_test) {
                                message <- paste(message, .("Every combination that uses such a test is affected as well."))
                            }
                            private$.addNotice(
                                "STRONG_WARNING",
                                .("Positive Levels May Be Inverted"),
                                message
                            )
                            inversion_flagged <- TRUE
                        }
                        # A J below 0 within sampling variation cannot tell an uninformative
                        # test from a reversed level: say so quietly, for one test as for
                        # several. It used to be withheld from a single test large enough to be
                        # ranked, where the serious "No Rule Performs Better Than Chance" spoke
                        # instead on the bare point estimate (27% of samples of a correctly coded
                        # test with true J 0.15 at 15 per group, telling the user to review a
                        # level that was right). Such a test now gets this note and the graded
                        # discrimination notice (see the J > 0 filter below).
                        if (any(!clear)) {
                            private$.addNotice(
                                "INFO",
                                .("Test Performs at Chance Level"),
                                .fmt(
                                    .("Youden's J is below 0 but within sampling variation of 0 for {tests}, on the {n} complete cases analyzed here. A test with no diagnostic value falls below 0 about half the time at any sample size, so a correctly coded but uninformative test and a test whose positive level is reversed cannot be told apart here. If the level chosen as positive is right, the test adds no information in this sample."),
                                    tests = paste(described[!clear], collapse = "; "),
                                    n = n_cases
                                )
                            )
                        }
                    }
                    # The collective signature, graded like the per-test check: the pattern that
                    # carries it must beat chance beyond sampling variation (its Agresti-Caffo
                    # 95% interval for J wholly above 0). Ungraded, it raised the serious warning
                    # on about 27% of samples of two tests independent of the disease, at every
                    # sample size: with four noisy patterns, "-/-" tops the table, or every named
                    # rule dips below 0, by chance alone.
                    clears_chance <- function(r) isTRUE(r$youdenLower > 0)
                    pattern_idx <- which(candidates$rowType == .("Pattern"))
                    negative_rule_wins <- isTRUE(grepl("^-(/-)*$", top_row$pattern)) &&
                        clears_chance(top_row)
                    # Both halves of what the notice claims: every named rule at or below
                    # chance WHILE an exact pattern clearly does better. Without the second
                    # term it also fires on a no-information sample (every rule at exactly
                    # chance, positive levels perfectly correct), where it contradicts the
                    # "No Rule Performs Better Than Chance" notice shown beside it.
                    named_rules_fail <- any(named_rows) &&
                        all(candidates$youden[named_rows] <= 0) &&
                        any(vapply(pattern_idx, function(i)
                            clears_chance(candidates[i, , drop = FALSE]), logical(1)))
                    # A reversed level reports 1 - sensitivity and 1 - specificity (a reversed
                    # reference: 1 - specificity and 1 - sensitivity), not the two "swapped", as
                    # this text used to say; and "-/-" can be applied, only no clinician would.
                    if (!inversion_flagged && (negative_rule_wins || named_rules_fail)) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Positive Levels May Be Inverted"),
                            .("The rule that separates the two groups best in this sample is one that calls a patient positive when the tests are negative; or every named rule (each test alone and every strategy) performs at or below chance while an exact result pattern does not. Both are the signature of a reversed positive level: the arithmetic still works, but the sensitivity and specificity reported for each rule then describe results read the wrong way round, and the best-performing rule is one no clinician would apply. Check the level chosen as positive for the reference standard and for each test before interpreting any of these results.")
                        )
                        inversion_flagged <- TRUE
                    }
                }

                # Eligibility for the ranking itself is gated on the two REFERENCE-GROUP
                # sizes; see the note above on why the smallest-cell rule of thumb is wrong
                # here.
                candidates$n_diseased <- candidates$tp + candidates$fn
                candidates$n_healthy <- candidates$fp + candidates$tn
                candidates <- candidates[
                    candidates$n_diseased >= 10 & candidates$n_healthy >= 10, , drop = FALSE]

                if (nrow(candidates) == 0) {
                    # Unlike the guard below, this one reports only that the ranking
                    # feature cannot run. A user who did not ask for a ranking does not
                    # need telling it is unavailable, and sample sparsity is already
                    # reported on its own terms by .assessSparseCounts(). With one test the
                    # "Ranking Needs Two or More Tests" note has already said why.
                    if (multi_test && isTRUE(self$options$showRecommendation)) {
                        private$.addNotice(
                            "WARNING",
                            .("Strategy Ranking Unavailable"),
                            .("No candidate rule has an estimable Youden index with at least 10 disease-present and 10 disease-absent cases. Youden's J needs both reference groups to be reasonably sized before a ranking means anything.")
                        )
                    }
                    return()
                }

                # Youden's J <= 0 means the rule classifies no better than a coin toss (and
                # below 0, worse than one -- it is anti-predictive and would have to be
                # inverted to be useful). Naming such a row "Highest-Ranked Rule" and then
                # describing it as a sensitivity/specificity trade-off actively misleads.
                # Rank only rules that beat chance, and say so plainly when none do.
                #
                # With two or three tests this is a guard, not a common path. The exhaustive
                # pattern rows partition the diseased and the non-diseased cases separately, so
                # both conditional distributions sum to 1 and the pattern Youden values sum to
                # EXACTLY zero: some pattern always has J > 0 unless every one is precisely 0.
                # The branch below is therefore reached only when every pattern is EXACTLY zero,
                # i.e. the tests carry no information about the reference standard at all. The
                # eligibility gate above cannot help it fire: tp + fn and fp + tn are the
                # sample's group sizes, identical in every row, so the gate admits all rules
                # or none.
                #
                # With ONE test there are no pattern rows, and a J at or below 0 is common: a
                # weak but correctly coded test falls there by chance (27% of samples at true
                # J 0.15, 15 per group). A reversal beyond sampling variation was flagged above;
                # what is left is within sampling variation of 0 and is graded like any other
                # single test by .assessTopRuleDiscrimination(), next to the chance-level note,
                # instead of a serious warning that calls the test anti-predictive.
                n_estimable <- nrow(candidates)
                # Every estimable rule, before the J > 0 filter: the discrimination check
                # speaks for all of them, and a rule with J <= 0 can still have an interval
                # that reaches 0.40.
                scored <- candidates
                candidates <- candidates[candidates$youden > 0, , drop = FALSE]
                if (nrow(candidates) == 0) {
                    if (!multi_test) {
                        private$.assessTopRuleDiscrimination(
                            scored[1, , drop = FALSE], scored, inversion_flagged, multi_test)
                    } else if (!inversion_flagged) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("No Rule Performs Better Than Chance"),
                            .fmt(
                                .("None of the {n} candidate rule(s) that could be scored has a Youden's J above zero: in this sample no single test, result pattern or combination strategy separates disease-present from disease-absent cases better than chance. A rule with a negative Youden's J is anti-predictive - its result would have to be reversed to carry information. Review the level chosen as positive for the reference standard and for each test, then treat the sensitivity and specificity in the tables as uninformative until that is settled."),
                                n = n_estimable
                            )
                        )
                    }
                    return()
                }

                # One tolerance for both the winner and the tie list. which.max() picked the
                # exact maximum, so two rules equal in exact arithmetic but 1e-16 apart in
                # floating point could crown the later row while the tie list, built with a
                # tolerance, named both: the winner then contradicted the "comes first" rule.
                is_top <- candidates$youden >= max(candidates$youden) - 1e-9
                best_pattern <- candidates[which(is_top)[1], , drop = FALSE]

                # Before the single-test return: a poorly discriminating single test needs
                # saying as much as a poor combination.
                private$.assessTopRuleDiscrimination(best_pattern, scored, inversion_flagged,
                                                     multi_test)
                if (!multi_test) {
                    return()
                }

                # Bootstrap optimism of the whole selection, only when it will be shown: this
                # method runs on every .run(), and a hidden table must not cost 10,000 resamples.
                correction <- if (isTRUE(self$options$showRecommendation) &&
                                  isTRUE(self$options$optimismCorrection)) {
                    private$.optimismCorrection(scored, best_pattern, data_prep)
                }

                # An exact tie was previously broken by whichever row came first, silently.
                tied <- candidates$pattern[is_top]
                rationale_parts <- character()
                if (length(tied) > 1) {
                    rationale_parts <- c(
                        rationale_parts,
                        .fmt(
                            .('{n} rules tie on Youden\'s J ({rules}); "{shown}" is displayed only because it comes first (named rules are listed before exact patterns, and rules that use fewer tests come first).'),
                            n = length(tied),
                            rules = paste(tied, collapse = ", "),
                            shown = best_pattern$pattern
                        )
                    )
                }

                # This is an argmax over every candidate rule with no interval or test for the
                # differences between rules, so on data with no real signal it still names a
                # winner. Say how many rules competed, and whether the winner separates from
                # the next one by more than the width of its own interval.
                n_candidates <- nrow(candidates)
                # The runner-up must have a different 2-by-2 table. A rule with the winner's
                # own counts (Test 1 alone and Parallel when Test 2 is positive only where
                # Test 1 is) has the winner's J, so it is named in the tie sentence above and
                # never used as the "next-best" rule: comparing the winner with its own J
                # reported an exact identity as possible sampling variation. Equal counts do
                # not imply the same patients ("+/-" and "-/+" can share a 2-by-2), which is
                # why the sentence below says "a different 2-by-2 table", not "different
                # patients".
                same_table <- candidates$tp == best_pattern$tp &
                    candidates$fp == best_pattern$fp &
                    candidates$fn == best_pattern$fn &
                    candidates$tn == best_pattern$tn
                runner_up <- if (any(!same_table)) {
                    max(candidates$youden[!same_table])
                } else {
                    NA_real_
                }

                rationale_parts <- c(
                    rationale_parts,
                    .fmt(
                        # The eligibility criteria stated here must match the filters
                        # applied above exactly: the count is meaningless if the reader
                        # cannot reproduce which rules it counts.
                        .("This is a descriptive ranking of {n} candidate rule(s) scored on at least 10 disease-present and 10 disease-absent cases with a Youden's J above zero; no significance test or multiplicity correction was applied."),
                        n = n_candidates
                    )
                )

                bound_quoted <- FALSE
                if (is.finite(runner_up)) {
                    bt <- best_pattern
                    tp <- bt$tp
                    fp <- bt$fp
                    fn <- bt$fn
                    tn <- bt$tn
                    sens_ci <- private$.calcWilsonCI(tp, tp + fn)
                    spec_ci <- private$.calcWilsonCI(tn, tn + fp)
                    # Youden's J = sens + spec - 1; a conservative interval for it is the
                    # sum of the two component intervals shifted by 1.
                    j_lower <- sens_ci[1] + spec_ci[1] - 1
                    if (is.finite(j_lower) && j_lower <= runner_up) {
                        rationale_parts <- c(
                            rationale_parts,
                            .fmt(
                                # .fmt()'s placeholder regex does not match
                                # underscores, so a placeholder named runner_up shipped to the
                                # user as literal braces with no warning -- in the one
                                # sentence that tells a clinician the top-ranked rule's
                                # advantage is not established. Placeholder names here must
                                # stay underscore-free.
                                # The bound is Wilson-lower(sens) + Wilson-lower(spec) - 1. Each
                                # term is an approximate one-sided 97.5% bound, so by Bonferroni
                                # the sum is a one-sided lower bound for J at a level of about 95%
                                # or more (Wilson coverage is itself approximate): conservative,
                                # and lower than the Agresti-Caffo limit in the table (the
                                # youden_ci note says so). Say so, rather than calling it "the
                                # lower bound" with no level.
                                .("Its advantage is not established: a conservative 95% lower bound for this rule's Youden's J ({lower}, from the lower Wilson limits of its sensitivity and specificity) falls at or below the point estimate of the best rule with a different 2-by-2 table ({runnerUp}), so the ranking may reflect sampling variation rather than a real difference."),
                                lower = base::format(round(j_lower, 3), nsmall = 3),
                                runnerUp = base::format(round(runner_up, 3), nsmall = 3)
                            )
                        )
                        bound_quoted <- TRUE
                    }
                }

                rationale_parts <- c(
                    rationale_parts,
                    .fmt(
                        .("The highest observed Youden's J among the eligible candidate rules was {youden}."),
                        youden = base::format(
                            round(best_pattern$youden, 3),
                            nsmall = 3
                        )
                    )
                )

                # Describe the point estimates only; their uncertainty is the sentence above.
                if (best_pattern$sens > 0.8 && best_pattern$spec > 0.8) {
                    rationale_parts <- c(
                        rationale_parts,
                        .("Observed sensitivity and specificity are both above 80%.")
                    )
                } else if (best_pattern$sens > 0.7 && best_pattern$spec > 0.7) {
                    rationale_parts <- c(
                        rationale_parts,
                        .("Observed sensitivity and specificity are both above 70%.")
                    )
                } else {
                    rationale_parts <- c(
                        rationale_parts,
                        .("The observed results involve a trade-off between sensitivity and specificity.")
                    )
                }

                # The listed rules are not every rule. Youden's J is additive over the
                # disjoint result patterns (sensitivity and 1 - specificity of a union are
                # sums), so the highest J any rule built from these tests can reach is the sum
                # of the positive pattern J values, attained by calling positive exactly those
                # patterns. Say so when that beats every listed rule, and flag it as chosen on
                # these data: with 8 patterns it can pick up patterns whose J is noise.
                pattern_rows <- table_df[table_df$rowType == .("Pattern") &
                                             is.finite(table_df$youden), , drop = FALSE]
                gains <- pattern_rows$youden > 1e-9
                if (any(gains)) {
                    # Decided and worded on the values the reader sees: a gain of one patient
                    # could otherwise print "would give J = 0.705" beside a winner shown as
                    # 0.705, and a difference of unrounded values can print as "0.000 above".
                    shown_optimum <- round(sum(pattern_rows$youden[gains]), 3)
                    shown_best <- round(best_pattern$youden, 3)
                    if (shown_optimum > shown_best) {
                        rationale_parts <- c(
                            rationale_parts,
                            .fmt(
                                .("No listed rule reaches the highest Youden's J these results allow: calling positive exactly the patterns {patterns} would give J = {youden}, {gain} above the highest-ranked rule. That rule was chosen by looking at these data, so treat it as a hypothesis to test in new patients, not as a better rule."),
                                patterns = paste(pattern_rows$pattern[gains], collapse = ", "),
                                youden = base::format(shown_optimum, nsmall = 3),
                                gain = base::format(round(shown_optimum - shown_best, 3), nsmall = 3)
                            )
                        )
                    }
                }
                # A corrected J at or below 0 is said in words: a bare negative "Corrected J"
                # beside a "Highest-Ranked Rule" reads as nonsense.
                if (!is.null(correction) && round(correction$corrected, 3) <= 0) {
                    rationale_parts <- c(rationale_parts, .fmt(
                        .("Once the choice among all {n} scored rules, including those with a Youden's J at or below zero, is allowed for, Youden's J is {youden}: there is no evidence that the highest-ranked rule separates disease-present from disease-absent cases better than chance."),
                        n = correction$rules,
                        youden = base::format(round(correction$corrected, 3), nsmall = 3)))
                }
                rationale_parts <- c(
                    rationale_parts,
                    .("This sample-dependent ranking is an analytical summary, not a clinical guide or validated recommendation.")
                )
                rationale <- paste(rationale_parts, collapse = " ")

                recTable <- self$results$recommendationTable
                recTable$setVisible(isTRUE(self$options$showRecommendation))
                recTable$setNote(
                    "scope",
                    jmvcore::.("This is a descriptive, sample-dependent ranking of exact-pattern rules, each test alone and named testing strategies. It is not a clinical guide or validated recommendation.")
                )
                # The bound quoted above differs from the Agresti-Caffo interval the
                # combination table shows for the same J; say why, beside the sentence.
                if (bound_quoted) {
                    recTable$setNote(
                        "bound",
                        jmvcore::.("The lower bound quoted under Interpretation adds the lower Wilson limits of the rule's sensitivity and specificity and subtracts 1. It is usually more conservative than the Agresti-Caffo interval shown beside Youden's J in the combination table, so the two lower limits can differ.")
                    )
                }
                if (!is.null(correction)) {
                    recTable$setNote("optimism", .fmt(
                        jmvcore::.("Corrected J estimates how a rule picked by this ranking would score in new patients like these. The whole ranking was repeated in {nboot} bootstrap samples drawn within each reference group; Expected drop is the average amount by which the rule picked in a sample scored higher there than in the original data (Monte Carlo standard error {mcse}). The highest-ranked rule was picked again in {stability}% of the samples. The correction is approximate: it leaves part of the optimism when rules are nearly tied, and it does not allow for how the tests, levels or patients were chosen."),
                        nboot = correction$B,
                        mcse = base::format(signif(correction$mcse, 2), scientific = FALSE),
                        stability = base::format(round(100 * correction$stability, 1),
                                                 nsmall = 1)))
                    # as.integer(): the value set.seed() used, and jmvcore's format() prints a
                    # double seed of 100000 (as the R syntax passes it) as "1e+05".
                    recTable$setNote("seed", .fmt(jmvcore::.("Random seed: {seed}"),
                                                  seed = as.integer(self$options$seed)))
                    recTable$setRefs("harrell2015")
                }
                recTable$setRow(rowNo = 1, values = private$.blankNA(list(
                    pattern = best_pattern$pattern,
                    method = .("Descriptive Youden ranking"),
                    youden = best_pattern$youden,
                    youdenCorrected = if (is.null(correction)) NA_real_ else correction$corrected,
                    optimism = if (is.null(correction)) NA_real_ else correction$optimism,
                    sens = best_pattern$sens,
                    spec = best_pattern$spec,
                    acc = best_pattern$acc,
                    rationale = rationale
                )))
            },
            .addPatternColumn = function() {
                # A pattern depends on the tests alone, so it is written for every case with
                # every selected test observed -- including cases with no reference-standard
                # result. It used to be built from the combination sample, which also requires
                # the reference, so exactly the unverified patients a rule would be applied
                # to got a blank. Cases missing a test stay blank.
                tests <- c("test1", "test2", "test3")
                tests <- tests[vapply(tests, function(t)
                    private$.optionSelected(self$options[[t]]), logical(1))]
                columns <- vapply(tests, function(t) self$options[[t]], character(1))
                positives <- vapply(tests, function(t)
                    self$options[[paste0(t, "Positive")]], character(1))

                test_data <- jmvcore::naOmit(private$.normalizeMissing(
                    self$data[, columns, drop = FALSE]))
                if (nrow(test_data) == 0) {
                    return()
                }
                signs <- lapply(seq_along(columns), function(j)
                    ifelse(as.character(test_data[[columns[j]]]) == positives[j], "+", "-"))
                pattern_values <- do.call(paste, c(signs, sep = "/"))

                output <- self$results$addedPattern
                # Row names are the source rows' own numbers (jamovi's filter-aware index);
                # jmvcore::naOmit keeps them, so each value lands on its own patient.
                output$setRowNums(rownames(test_data))
                output$setValues(pattern_values)
            },
            .setPlotStates = function() {
                if (self$results$combinationTable$rowCount == 0) {
                    return()
                }

                combination_data <- as.data.frame(
                    self$results$combinationTable$asDF,
                    stringsAsFactors = FALSE
                )
                common_state <- list(
                    valid = TRUE,
                    data = combination_data,
                    filterStatistic = self$options$filterStatistic,
                    filterPattern = self$options$filterPattern
                )

                # .applyPatternFilter keeps only rows whose label is made of +/- tokens, so
                # a filter can select nothing at all -- always for a single-test analysis,
                # whose one row is labelled "Test 1" and contains no "/". The renderers then
                # return FALSE and jamovi draws empty panels with nothing to explain them.
                # Detect it here, where notices can still be emitted.
                filtered_df <- private$.applyPatternFilter(
                    combination_data, self$options$filterPattern)
                filtered_rows <- nrow(filtered_df)
                any_filtered_plot <- isTRUE(self$options$showBarPlot) ||
                    isTRUE(self$options$showHeatmap) || isTRUE(self$options$showForest)
                if (filtered_rows == 0 && any_filtered_plot) {
                    private$.addNotice(
                        "WARNING",
                        .("No Rows Match the Pattern Filter"),
                        .fmt(
                            .("The pattern-type filter \"{filter}\" matches none of the rows in this analysis, so the bar chart, heatmap and forest plot are blank. Pattern filters apply to exact result patterns such as \"+/+\"; a single-test analysis has no such rows, and the single-test and strategy rows are not result patterns. Set the pattern filter back to \"All Patterns\" to see the plots."),
                            filter = private$.patternFilterLabel(self$options$filterPattern)
                        )
                    )
                }

                # One statistic selected, and it is blank in every selected row (a one-class
                # sample, or patterns no patient shows): the bar chart drew nothing and the
                # heatmap one grey tile, with no notice. Say it once for all three plots.
                stat_code <- self$options$filterStatistic
                stat_blank <- filtered_rows > 0 && !identical(stat_code, "all") &&
                    stat_code %in% names(filtered_df) &&
                    !any(is.finite(suppressWarnings(as.numeric(filtered_df[[stat_code]]))))
                if (stat_blank && any_filtered_plot) {
                    private$.addNotice(
                        "WARNING",
                        .("Selected Statistic Is Blank"),
                        .fmt(
                            .('"{statistic}" is blank for every row selected by the pattern filter "{filter}" (for example a sample with only one gold-standard outcome, or patterns that no patient shows), so the selected plots have nothing to show for it. The other notices and the combination-table notes give the reason.'),
                            statistic = private$.metricLabel(stat_code),
                            filter = private$.patternFilterLabel(self$options$filterPattern)
                        )
                    )
                }

                # The decision space places a rule by its sensitivity AND its specificity, and
                # no row has both exactly when one reference group is empty (a one-class
                # sample): it was the only plot left blank without a word. Unfiltered rows,
                # because neither filter applies to this plot.
                if (isTRUE(self$options$showDecisionTree) &&
                    !any(is.finite(combination_data$sens) &
                             is.finite(combination_data$spec))) {
                    private$.addNotice(
                        "WARNING",
                        .("Decision-Space Plot Is Blank"),
                        .("The decision-space plot places each rule by its sensitivity and specificity, and no rule has both: every complete case has the same reference-standard result, so one of the two cannot be estimated. The notice about the reference standard says which.")
                    )
                }

                if (isTRUE(self$options$showBarPlot)) {
                    self$results$barPlot$setState(common_state)
                }
                if (isTRUE(self$options$showHeatmap)) {
                    self$results$heatmapPlot$setState(common_state)
                }
                if (isTRUE(self$options$showDecisionTree)) {
                    self$results$decisionTreePlot$setState(common_state)
                }
                if (isTRUE(self$options$showForest)) {
                    prop_data <- as.data.frame(
                        self$results$combinationTableCI$asDF,
                        stringsAsFactors = FALSE
                    )
                    ratio_table <- self$results$combinationTableCIRatios
                    ratio_data <- if (ratio_table$rowCount > 0) {
                        as.data.frame(ratio_table$asDF, stringsAsFactors = FALSE)
                    } else {
                        data.frame()
                    }
                    forest_supported <- !self$options$filterStatistic %in%
                        c("prevalence", "balancedAccuracy", "youden")
                    # Only when nothing earlier already explains the blank plot: with no
                    # matching rows, or a statistic blank in every row, this note's claim that
                    # "the bar chart and heatmap can still display it" was false beside the
                    # notice that said they could not. Same guard as "Nothing to Draw" below.
                    # Youden's J now has an interval (in the combination table), so it gets its
                    # own reason; prevalence and balanced accuracy still have none here.
                    if (!forest_supported && filtered_rows > 0 && !stat_blank) {
                        reason <- if (identical(self$options$filterStatistic, "youden")) {
                            .('The forest plot is not drawn for "{statistic}": it runs from -1 to 1, unlike the proportions and ratios on the forest plot axes. Its 95% confidence interval is in the combination table, and the bar chart and heatmap can still display it.')
                        } else {
                            .('The forest plot is not drawn for "{statistic}" because this analysis does not calculate a confidence interval for that statistic. The bar chart and heatmap can still display it.')
                        }
                        private$.addNotice(
                            "INFO",
                            .("Forest Plot Not Available for Selected Statistic"),
                            .fmt(
                                reason,
                                statistic = private$.metricLabel(
                                    self$options$filterStatistic
                                )
                            )
                        )
                    }
                    forest_state <- list(
                        valid = forest_supported,
                        proportions = prop_data,
                        ratios = ratio_data,
                        filterStatistic = self$options$filterStatistic,
                        # filterPattern was declared in the forest plot's clearWith but was
                        # never carried into its state, so selecting a pattern type cleared
                        # the plot and redrew an identical image.
                        filterPattern = self$options$filterPattern,
                        # Rows whose LR+/LR-/DOR carry the 0.5 continuity correction, so
                        # the plot can mark them (an infinite observed LR+ is drawn as a finite
                        # corrected value otherwise indistinguishable from an observed one).
                        corrected = unique(private$.continuityPatterns)
                    )
                    self$results$forestPlot$setState(forest_state)
                    # (The image size is set in .init(); see .forestPlotHeight().)

                    # The renderer draws only rows with an estimate AND a confidence
                    # interval, so a statistic that is undefined or has no interval for every
                    # selected row (a pattern no patient shows, a one-class sample) left a
                    # blank plot, or silently dropped facets, with no notice. Ask the
                    # renderer's own builder what it will draw and say what is missing. The
                    # other two blank causes (no matching rows; a statistic with no CI) have
                    # notices of their own above.
                    if (forest_supported && filtered_rows > 0 && !stat_blank) {
                        built <- private$.buildForestPanels(forest_state)
                        stat_code <- self$options$filterStatistic
                        if (is.null(built)) {
                            private$.addNotice(
                                "WARNING",
                                .("Forest Plot Has Nothing to Draw"),
                                .fmt(
                                    .('"{statistic}" has no estimate with a confidence interval for any row selected by the pattern filter "{filter}" (for example a pattern that no patient shows, or a sample with only one gold-standard outcome), so the forest plot is blank. The tables show these cells, with the reason in their notes.'),
                                    statistic = if (identical(stat_code, "all")) {
                                        .("Default Metric Set")
                                    } else {
                                        private$.metricLabel(stat_code)
                                    },
                                    filter = private$.patternFilterLabel(self$options$filterPattern)
                                )
                            )
                        } else {
                            wanted <- if (identical(stat_code, "all")) {
                                vapply(c("sens", "spec", "ppv", "npv", "acc", "lrPos", "lrNeg", "dor"),
                                       private$.metricLabel, character(1))
                            } else {
                                private$.metricLabel(stat_code)
                            }
                            drawn <- unlist(lapply(built$panels, function(p)
                                levels(p$data$statistic)))
                            omitted <- setdiff(wanted, drawn)
                            if (length(omitted) > 0) {
                                private$.addNotice(
                                    "INFO",
                                    .("Statistics Omitted from the Forest Plot"),
                                    .fmt(
                                        .("{statistics}: no selected row has both an estimate and a confidence interval (for example a pattern that no patient shows, where PPV, LR+ and the diagnostic odds ratio are undefined and LR- is exactly 1 with no interval, or a sample with only one gold-standard outcome), so the forest plot omits them. The tables show these cells, with the reason in their notes."),
                                        statistics = paste(unname(omitted), collapse = ", ")
                                    )
                                )
                            }
                            # Rules drawn for some statistics but empty for others kept their
                            # labelled row with nothing on it, which on a log axis reads as a
                            # value off the scale. Name them, per statistic.
                            partial <- built$dropped
                            if (!is.null(partial) && nrow(partial) > 0) {
                                partial <- partial[partial$statistic %in% drawn, , drop = FALSE]
                            }
                            if (!is.null(partial) && nrow(partial) > 0) {
                                # "PPV: +/-, -/+; LR+: ..." - rule labels such as "Parallel (>=1 pos)"
                                # already contain parentheses, so none are added around them.
                                details <- vapply(unique(as.character(partial$statistic)), function(st)
                                    paste0(st, ": ", paste(unique(as.character(
                                        partial$pattern[partial$statistic == st])), collapse = ", ")),
                                    character(1))
                                private$.addNotice(
                                    "INFO",
                                    .("Rules Left Empty in the Forest Plot"),
                                    .fmt(
                                        .("These rules have no estimate with a confidence interval for the statistic named, so their row is left empty in that facet: {details}. This happens for a pattern that no patient shows (PPV, LR+ and the diagnostic odds ratio are undefined there, and LR- is exactly 1 with no interval) and for a rule on which every patient, or no patient, tests positive. The tables show these cells, with the reason in their notes."),
                                        details = paste(unname(details), collapse = "; ")
                                    )
                                )
                            }
                        }
                    }
                }
            },
            # ggtheme/theme added to the signature: they were being passed by jamovi
            # into `...` and dropped, so this plot ignored the global theme/palette.
            .plotBarChart = function(image, ggtheme = NULL, theme = NULL, ...) {
                state <- image$state
                if (!is.list(state) || !isTRUE(state$valid) || is.null(state$data)) {
                    return(FALSE)
                }

                table_df <- as.data.frame(state$data, stringsAsFactors = FALSE)

                # Apply statistic filter
                stat_filter <- state$filterStatistic
                if (stat_filter != "all") {
                    metrics <- stat_filter
                } else {
                    metrics <- c("sens", "spec", "ppv", "npv", "acc")
                }

                # Apply pattern filter
                pattern_filter <- state$filterPattern
                filtered_df <- private$.applyPatternFilter(table_df, pattern_filter)

                if (nrow(filtered_df) == 0) {
                    return(FALSE)
                }

                # Create long format
                plot_data <- data.frame()
                for (metric in metrics) {
                    if (metric %in% names(filtered_df)) {
                        temp <- data.frame(
                            Pattern = filtered_df$pattern,
                            Metric = private$.metricLabel(metric),
                            Value = filtered_df[[metric]],
                            stringsAsFactors = FALSE
                        )
                        plot_data <- rbind(plot_data, temp)
                    }
                }

                # A pattern row with an empty test margin legitimately carries NA for PPV or
                # NPV. geom_bar() drops those rows itself and emits "Removed n rows
                # containing missing values", which jamovi surfaces in Analysis Notes with no
                # context and reads like a fault. Drop them here instead: the omission is
                # correct and is already explained by the table footnote.
                plot_data <- plot_data[is.finite(plot_data$Value), , drop = FALSE]
                if (nrow(plot_data) == 0) {
                    return(FALSE)
                }
                # Table order, not alphabetical: a character x axis sorted "Parallel 1+2"
                # before "Test 1 alone" and scattered each rule away from its kind.
                plot_data$Pattern <- factor(plot_data$Pattern,
                                            levels = unique(filtered_df$pattern))

                # Proportion metrics are bounded to [0, 1] and shown as percentages; unbounded
                # metrics (Youden's J, LR+, LR-, DOR) must use a free auto scale, otherwise the
                # fixed [0, 1] limit clips their bars to blank.
                proportion_metrics <- c("prevalence", "sens", "spec", "ppv", "npv", "acc", "balancedAccuracy")
                all_proportions <- all(metrics %in% proportion_metrics)

                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(
                        x = .data$Pattern, y = .data$Value, fill = .data$Metric)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge") +
                    ggplot2::labs(
                        title = .("Diagnostic Performance Comparison"),
                        x = .("Test Pattern"),
                        y = .("Value"),
                        # Without an explicit label ggplot titles the legend with the mapped
                        # column name, which never passes through .() and so is untranslatable.
                        fill = .("Metric")
                    ) +
                    # ggtheme LAST, then the tweak: a jamovi ggtheme is a complete theme plus
                    # discrete fill/colour scales, so it replaces anything before it.
                    # theme_minimal() was therefore dead and the rotation was being dropped.
                    ggtheme +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

                if (all_proportions) {
                    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
                }

                print(p)
                return(TRUE)
            },
            # ggtheme/theme added to the signature: they were being passed by jamovi
            # into `...` and dropped, so this plot ignored the global theme/palette.
            .plotHeatmap = function(image, ggtheme = NULL, theme = NULL, ...) {
                state <- image$state
                if (!is.list(state) || !isTRUE(state$valid) || is.null(state$data)) {
                    return(FALSE)
                }

                table_df <- as.data.frame(state$data, stringsAsFactors = FALSE)
                pattern_filter <- state$filterPattern
                filtered_df <- private$.applyPatternFilter(table_df, pattern_filter)

                if (nrow(filtered_df) == 0) {
                    return(FALSE)
                }

                # The default panel contains bounded or centered metrics that share a
                # meaningful color scale. A selected ratio is still honored as a single
                # metric with an odds/likelihood-ratio midpoint of one.
                # Youden's J runs -1..1 with chance at 0, while every other metric here is a
                # 0..1 proportion with a natural midpoint of 0.5. They cannot share one
                # diverging colour scale: on a 0.5-centred scale J = 0.30 (useful) paints as
                # "bad" red and J = 0 (useless) paints as neutral white. J stays selectable on
                # its own, where the midpoint below is set to 0 for it.
                metrics <- c("prevalence", "sens", "spec", "ppv", "npv", "acc",
                             "balancedAccuracy")
                stat_filter <- state$filterStatistic
                if (stat_filter != "all") {
                    if (!stat_filter %in% names(filtered_df)) {
                        return(FALSE)
                    }
                    metrics <- stat_filter
                }
                metric_data <- filtered_df[, c("pattern", metrics), drop = FALSE]

                # Reshape to long format
                plot_data <- tidyr::pivot_longer(
                    metric_data,
                    cols = -"pattern",
                    names_to = "Metric",
                    values_to = "Value"
                )
                plot_data$Metric <- vapply(
                    plot_data$Metric,
                    private$.metricLabel,
                    character(1)
                )
                # Rows top to bottom in table order (a discrete y axis puts the first level
                # at the bottom), not alphabetically.
                plot_data$pattern <- factor(plot_data$pattern,
                                            levels = rev(unique(filtered_df$pattern)))

                # LR+, LR- and the diagnostic odds ratio are multiplicative: 0.5 and 2 are
                # equally far from 1. They were coloured on a linear scale centred on 1, so
                # every ratio below 1 was squeezed into the neutral band (an LR+ of 0.02
                # looked like 1) and anything short of the largest value washed out (13.7 was
                # near-neutral beside 60.7). They are now coloured on log10, centred on 0,
                # which is a ratio of 1.
                ratio_metric <- stat_filter %in% c("lrPos", "lrNeg", "dor")
                midpoint <- if (ratio_metric) 0
                            else if (identical(stat_filter, "youden")) 0
                            else 0.5

                # The neutral pole and the "good" pole follow the jamovi theme; the warm pole
                # stays hand-picked, because a diverging ramp needs a designed opposite and a
                # categorical palette cannot supply one. Guarded: `theme` is absent in tests.
                # theme defaults to NULL in the signature above, so these fallbacks are
                # reachable. Without the default, `is.list(theme)` FORCED the missing promise
                # and raised "argument \"theme\" is missing" instead of falling back -- the
                # guard read as defensive but could only ever error.
                #
                # theme$color[[2]] is jmvcore::colorPalette(1, palette, "color") -- the first
                # colour of the user's CATEGORICAL palette -- and a categorical palette has
                # no designed opposite. Verified: it is #E63032 under Set1, #F09571 under
                # RdBu, #FC9869 under Spectral and #F8837B under hadley, all warm, so the
                # "good" pole collapsed onto the hardcoded warm-pink "bad" pole and a
                # sensitivity of 0.05 read as the same colour family as 0.95; under
                # iheartspss theme$color is c("#333333", "#333333"), making every
                # high-performing tile near-black. A diverging scale needs a designed pair,
                # so both poles are now fixed (the red/blue ends of ColorBrewer RdBu) and
                # only the NEUTRAL midpoint follows the theme, where the theme's own panel
                # fill is the right answer and is always light.
                low_col  <- "#ef8a8a"
                mid_col  <- if (is.list(theme) && length(theme$fill) > 0) theme$fill[[1]] else "#f7f7f7"
                high_col <- "#74add1"
                # A SMALL LR- is the good end (a negative result rules disease out), so its
                # poles are swapped. Unswapped, the worst LR- (3.63) took the blue "good" pole.
                if (identical(stat_filter, "lrNeg")) {
                    swapped <- low_col
                    low_col <- high_col
                    high_col <- swapped
                }
                # The value the fill scale sees: log10 for ratios (non-positive or blank
                # values become non-finite and take the grey na.value tile).
                plot_data$Fill <- if (ratio_metric) log10(plot_data$Value) else plot_data$Value
                estimable <- is.finite(plot_data$Fill)
                # Limits: the data's own range, as before, when the tiles differ. When every tile
                # has the same value (a one-test analysis has one tile), ggplot's zero-width range
                # painted the neutral colour whatever the value; such tiles now sit on the metric's
                # natural scale (0 to 1, -1 to 1, or 0.1 to 10 and wider for a ratio), so the colour
                # keeps its direction AND its size: stretching the range to the midpoint instead
                # gave a sensitivity of 0.51 the same full blue as 0.99. Prevalence is identical in
                # every row and is not a performance measure, so it stays neutral.
                values <- plot_data$Fill[estimable]
                fill_limits <- NULL
                if (length(values) > 0 && diff(range(values)) == 0 &&
                    !identical(stat_filter, "prevalence")) {
                    fill_limits <- if (ratio_metric) {
                        c(-1, 1) * max(1, abs(values[1]))
                    } else if (identical(stat_filter, "youden")) {
                        c(-1, 1)
                    } else {
                        c(0, 1)
                    }
                }

                # The tile labels were drawn in a hardcoded near-black whatever was under
                # them. Reproduce the fill scale_fill_gradient2() will build and pick a light
                # or dark label per tile from its relative luminance (Rec. 709), so the value
                # stays readable on any pole, including a future dark render surface.
                tile_fill <- rep("#bebebe", nrow(plot_data))   # blank tiles carry no label
                if (any(estimable)) {
                    tile_fill[estimable] <- scales::div_gradient_pal(
                        low_col, mid_col, high_col
                    )(scales::rescale_mid(values,
                                          from = if (is.null(fill_limits)) range(values) else fill_limits,
                                          mid = midpoint))
                }
                luminance <- colSums(
                    grDevices::col2rgb(tile_fill) / 255 * c(0.2126, 0.7152, 0.0722))
                plot_data$LabelColour <- ifelse(luminance < 0.5, "#f5f5f5", "#1a1a1a")
                # sprintf("%.2f", NA) is the literal "NA", which was printed on the grey tile
                # of every blank cell (empty patterns, one-class PPV/NPV). Labels keep the
                # untransformed value.
                plot_data$Label <- ifelse(is.finite(plot_data$Value),
                                          sprintf("%.2f", plot_data$Value), "")

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(
                        x = .data$Metric, y = .data$pattern, fill = .data$Fill)) +
                    ggplot2::geom_tile() +
                    ggplot2::geom_text(
                        ggplot2::aes(label = .data$Label, color = .data$LabelColour)
                    ) +
                    ggplot2::labs(
                        title = .("Performance Heatmap"),
                        x = "",
                        y = .("Pattern"),
                        fill = if (ratio_metric) .("Value (log scale)") else .("Value")
                    ) +
                    # theme_minimal() replaced by ggtheme. The continuous fill scale must come
                    # AFTER ggtheme: ggtheme carries a DISCRETE fill scale that would replace
                    # this one and then fail with "continuous value supplied to discrete scale".
                    ggtheme +
                    # Ratio legends are labelled in the original units at round ratios
                    # (0.1, 1, 10, ...) rather than in log10 units.
                    ggplot2::scale_fill_gradient2(
                        low = low_col, mid = mid_col, high = high_col,
                        midpoint = midpoint, limits = fill_limits,
                        breaks = if (ratio_metric) {
                            function(lim) log10(scales::breaks_log(n = 5)(10^lim))
                        } else {
                            ggplot2::waiver()
                        },
                        labels = if (ratio_metric) {
                            function(b) base::format(signif(10^b, 2), scientific = FALSE,
                                                     drop0trailing = TRUE, trim = TRUE)
                        } else {
                            ggplot2::waiver()
                        }
                    ) +
                    # After ggtheme for the same reason as the fill scale: ggtheme carries a
                    # discrete colour scale that would otherwise replace this one.
                    ggplot2::scale_color_identity()

                print(p)
                return(TRUE)
            },
            # Builds the forest plot's panels without printing them, so they can be
            # inspected directly. Returns NULL when there is nothing to draw, otherwise
            # list(panels = <1 or 2 ggplots>, heights = <facet counts>).
            #
            # Proportions and ratios need different x axes. A single facet_wrap(scales =
            # "free_x") gave every facet one LINEAR scale: a log-symmetric interval such as
            # DOR 6 [0.53, 67.6] looked lopsided, the null value 1 was unmarked, and a DOR
            # axis running to 250 squeezed every ratio near 1 into its left edge. Ratios are
            # now drawn on a log10 axis with a dashed reference line at 1.
            .buildForestPanels = function(state, ggtheme = NULL) {
                if (!is.list(state) || !isTRUE(state$valid)) {
                    return(NULL)
                }
                select_rows <- function(df) {
                    if (!is.data.frame(df) || nrow(df) == 0) {
                        return(NULL)
                    }
                    df <- as.data.frame(df, stringsAsFactors = FALSE)
                    rownames(df) <- NULL
                    # Same row selection as the bar chart and heatmap; named strategy rows
                    # are not +/- patterns and drop out under a specific pattern type.
                    df <- private$.applyPatternFilter(df, state$filterPattern)
                    # The CI tables store display labels ("Sensitivity"), so map the option
                    # code to its label before subsetting.
                    stat_filter <- state$filterStatistic
                    if (!is.null(stat_filter) && !identical(stat_filter, "all")) {
                        df <- df[df$statistic == private$.metricLabel(stat_filter), ,
                                 drop = FALSE]
                    }
                    df
                }
                props_all <- select_rows(state$proportions)
                ratios_all <- select_rows(state$ratios)
                # One row list for BOTH panels, in table order, taken before any row is
                # dropped: the two panels then line up rule for rule, and a rule with nothing
                # to draw keeps its (empty) row instead of vanishing from one panel only.
                pattern_levels <- rev(unique(c(props_all$pattern, ratios_all$pattern)))
                # Exact result patterns ("+/-") are scored one-vs-rest ("exactly this pattern"
                # against everything else); Parallel, Serial and Majority are strategies. The
                # table says so in a note, but an exported figure travels without the table:
                # "-/-" read as a test that works backwards. Strategies sort to the bottom, so
                # a line above them separates the two kinds, and the caption names them.
                is_pattern_level <- grepl("^[+-](/[+-])+$", pattern_levels)
                n_strategy_rows <- sum(!is_pattern_level)
                separate_kinds <- any(is_pattern_level) && n_strategy_rows > 0
                corrected <- as.character(state$corrected)
                drawable <- function(df, ratio) {
                    if (is.null(df) || nrow(df) == 0) {
                        return(NULL)
                    }
                    # A 95% CI forest plot draws only rows that HAVE an interval. A ratio that
                    # is exactly 1 on a pattern no patient shows has a blank interval in the
                    # table (its log-SE is 0); drawn as a bare point it looked like the most
                    # precise estimate on the plot. Undefined estimates (0/0) go too, and a
                    # log axis needs positive values.
                    keep <- is.finite(df$estimate) & is.finite(df$lower) & is.finite(df$upper)
                    if (ratio) {
                        keep <- keep & df$estimate > 0 & df$lower > 0
                    }
                    df <- df[keep, , drop = FALSE]
                    if (nrow(df) == 0) {
                        return(NULL)
                    }
                    df$pattern <- factor(df$pattern, levels = pattern_levels)
                    df$statistic <- factor(df$statistic, levels = unique(df$statistic))
                    df
                }
                # Tick labels through base format(): scales::label_number() rounds the
                # whole break vector to one shared accuracy (0.001 printed as "0" beside a
                # DOR of 0.0004), adds a space as a thousands mark ("1 000") and ignores
                # jamovi's decimal symbol, which jmvcore passes in as OutDec.
                ratio_labels <- function(x) {
                    base::format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
                }
                # Log breaks. `lim` is the EXPANDED range, and the dashed line at 1 pulls 1
                # into it, so a facet whose intervals all lie on one side of 1 reaches past 1
                # only by the ~5% padding.
                ratio_breaks <- function(lim) {
                    b <- scales::breaks_log(n = 6)(lim)
                    span <- diff(log10(lim))
                    if (!all(is.finite(lim)) || !is.finite(span) || span <= 0) {
                        return(b)
                    }
                    inside <- b[is.finite(b) & b >= lim[1] & b <= lim[2]]
                    # Fall back to plain breaks on a side of 1 that the data really reach
                    # (a fifth of the axis or more) but breaks_log() left unlabelled: a
                    # narrow LR- facet 0.71-1.27 had no label above 1. Firing on the padding
                    # alone crammed 2-4 labels next to 1, which printed as "0.960.981".
                    if (log10(lim[2]) > 0.2 * span && !any(inside > 1)) {
                        b <- c(b, scales::breaks_extended(3)(c(1, lim[2])))
                    }
                    if (-log10(lim[1]) > 0.2 * span && !any(inside < 1)) {
                        b <- c(b, scales::breaks_extended(3)(c(lim[1], 1)))
                    }
                    # Always label the null value when it is on the axis: a DOR facet
                    # spanning 1e-6 to 1e6 was labelled 1e-6, 0.01, 100, 1e6 but not 1.
                    if (lim[1] <= 1 && lim[2] >= 1) {
                        b <- c(b, 1)
                    }
                    b <- sort(unique(b[is.finite(b) & b > 0 & b >= lim[1] & b <= lim[2]]))
                    # Keep 1 first, then each break nearest to 1 that is at least 7% of the
                    # log span from every break already kept, so no two labels overprint.
                    kept <- numeric()
                    for (x in b[order(abs(log10(b)))]) {
                        if (all(abs(log10(x) - log10(kept)) >= 0.07 * span)) {
                            kept <- c(kept, x)
                        }
                    }
                    sort(kept)
                }
                panel <- function(df, ratio) {
                    df$corrected <- ratio & df$pattern %in% corrected
                    p <- ggplot2::ggplot(df, ggplot2::aes(
                        x = .data$estimate, y = .data$pattern, colour = .data$statistic))
                    if (separate_kinds) {
                        p <- p + ggplot2::geom_hline(
                            yintercept = n_strategy_rows + 0.5, colour = "grey70",
                            linewidth = 0.3)
                    }
                    if (ratio) {
                        p <- p + ggplot2::geom_vline(
                            xintercept = 1, linetype = "dashed", colour = "grey50")
                    }
                    p <- p +
                        ggplot2::geom_errorbar(
                            ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                            orientation = "y", width = 0.25
                        ) +
                        # Open points: a continuity-corrected row (an empty denominator cell), so an
                        # infinite observed LR+ no longer looks like an ordinary 14.8.
                        # .data$ names the COLUMN: a bare `corrected` also matched the
                        # character vector of corrected rule labels in the enclosing frame.
                        ggplot2::geom_point(ggplot2::aes(shape = .data$corrected), size = 2.5,
                                            fill = NA) +
                        # free_x in BOTH panels, so every facet draws its own labelled x
                        # axis: with "fixed" only the bottom of five stacked proportion
                        # facets was labelled, 1000+ px below the top one. The proportions
                        # still share one 0-100% range through coord_cartesian() below.
                        ggplot2::facet_wrap(~statistic, ncol = 1, scales = "free_x") +
                        ggplot2::labs(
                            x = if (ratio) {
                                .("Ratio (95% CI), log scale; dashed line = 1 (uninformative)")
                            } else {
                                .("Proportion (95% CI)")
                            },
                            y = .("Pattern")
                        ) +
                        # ggtheme replaces earlier themes and discrete scales, so the legend
                        # tweak and the scales come after it. The facet strip already names
                        # each statistic, so the colour legend is redundant.
                        ggtheme +
                        ggplot2::theme(legend.position = "none") +
                        ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21),
                                                    guide = "none") +
                        # Fixed row order (table order, top to bottom) and the same rows in
                        # both panels, whichever layer trains the scale first.
                        ggplot2::scale_y_discrete(limits = pattern_levels, drop = FALSE)
                    if (ratio) {
                        p + ggplot2::scale_x_log10(breaks = ratio_breaks, labels = ratio_labels)
                    } else {
                        p + ggplot2::scale_x_continuous(
                                labels = scales::percent_format(
                                    decimal.mark = getOption("OutDec", "."), big.mark = "")) +
                            ggplot2::coord_cartesian(xlim = c(0, 1))
                    }
                }
                props <- drawable(props_all, ratio = FALSE)
                ratios <- drawable(ratios_all, ratio = TRUE)
                # Cells selected but not drawable, for the notice that names them.
                left_out <- function(all, kept) {
                    if (is.null(all) || nrow(all) == 0) {
                        return(NULL)
                    }
                    key <- paste(all$pattern, all$statistic, sep = "\r")
                    kept_key <- if (is.null(kept)) character() else
                        paste(as.character(kept$pattern), as.character(kept$statistic), sep = "\r")
                    all[!key %in% kept_key, c("pattern", "statistic"), drop = FALSE]
                }
                dropped <- rbind(left_out(props_all, props), left_out(ratios_all, ratios))
                panels <- list()
                heights <- numeric()
                if (!is.null(props)) {
                    panels$proportions <- panel(props, ratio = FALSE)
                    heights <- c(heights, nlevels(props$statistic))
                }
                if (!is.null(ratios)) {
                    panels$ratios <- panel(ratios, ratio = TRUE)
                    heights <- c(heights, nlevels(ratios$statistic))
                }
                if (length(panels) == 0) {
                    return(NULL)
                }
                panels[[1]] <- panels[[1]] +
                    ggplot2::labs(title = .("Forest Plot - 95% Confidence Intervals"))
                caption <- character()
                if (separate_kinds) {
                    caption <- c(caption, .("Rows such as +/- are exact result patterns, each scored as 'this exact pattern' against all other results; the rows below the line are single tests and testing strategies. Read Youden's J in the table for pattern rows."))
                }
                if (!is.null(ratios) && any(ratios$pattern %in% corrected)) {
                    caption <- c(caption, .("Open points: rows where at least one of LR+, LR- and DOR uses a 0.5 continuity correction, because a cell in its denominator is zero; without it that ratio would be infinite."))
                }
                if (length(caption) > 0) {
                    last <- length(panels)
                    panels[[last]] <- panels[[last]] + ggplot2::labs(
                        caption = paste(strwrap(paste(caption, collapse = " "), width = 115),
                                        collapse = "\n")) +
                        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
                }
                # Layout rows for .plotForest. The image height is budgeted from the options
                # in .init() (8 facets for the default set), because export and reopen never
                # run .run(). When whole statistics cannot be drawn, give the unused budget
                # to a blank bottom row so the drawn facets keep their designed row spacing;
                # otherwise two facets stretched over a 2466 px image with rows ~200 px apart.
                budget <- if (identical(state$filterStatistic, "all")) 8 else 1
                rows <- heights + 0.6
                spare <- budget - sum(heights)
                if (spare > 0) {
                    rows <- c(rows, spare)
                }
                list(panels = panels, heights = heights, rows = rows,
                     dropped = if (is.null(dropped)) NULL else unique(dropped))
            },
            # ggtheme/theme added to the signature: they were being passed by jamovi
            # into `...` and dropped, so this plot ignored the global theme/palette.
            .plotForest = function(image, ggtheme = NULL, theme = NULL, ...) {
                built <- private$.buildForestPanels(image$state, ggtheme)
                if (is.null(built)) {
                    return(FALSE)
                }
                if (length(built$rows) == 1L) {
                    print(built$panels[[1]])
                    return(TRUE)
                }
                # Two ggplots with different x scales cannot share one facet grid. Stack
                # them with grid viewports (grid is already imported) rather than adding a
                # plot-composition package to the module's dependencies; a trailing blank
                # row, when present, holds the unused height budget.
                grid::grid.newpage()
                grid::pushViewport(grid::viewport(layout = grid::grid.layout(
                    nrow = length(built$rows), heights = grid::unit(built$rows, "null"))))
                for (i in seq_along(built$panels)) {
                    print(built$panels[[i]], vp = grid::viewport(layout.pos.row = i))
                }
                return(TRUE)
            },
            # ggtheme/theme added to the signature: they were being passed by jamovi
            # into `...` and dropped, so this plot ignored the global theme/palette.
            .plotDecisionTree = function(image, ggtheme = NULL, theme = NULL, ...) {
                state <- image$state
                if (!is.list(state) || !isTRUE(state$valid) || is.null(state$data)) {
                    return(FALSE)
                }

                table_df <- as.data.frame(state$data, stringsAsFactors = FALSE)
                table_df <- table_df[is.finite(table_df$sens) & is.finite(table_df$spec), ,
                                     drop = FALSE]
                if (nrow(table_df) == 0) {
                    return(FALSE)
                }
                # With up to 20 rules the figure, which travels without the table, printed
                # rules that classify these patients identically on top of each other
                # ("Serial (all pos)" over "+/+/+" read "Serial+/(all+pos)") and coloured 20
                # rules in near-identical shades. One label per point, naming every rule
                # there, and colour by row type (three levels) instead of by rule.
                at <- paste(signif(table_df$sens, 12), signif(table_df$spec, 12))
                first <- !duplicated(at)
                label_df <- table_df[first, c("sens", "spec"), drop = FALSE]
                label_df$label <- vapply(at[first], function(k)
                    paste(table_df$pattern[at == k], collapse = " = "), character(1))
                table_df$rowType <- factor(table_df$rowType,
                                           levels = unique(table_df$rowType))

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = .data$sens, y = .data$spec)) +
                    ggplot2::geom_point(ggplot2::aes(size = .data$youden, colour = .data$rowType)) +
                    ggplot2::geom_text(data = label_df, ggplot2::aes(label = .data$label),
                                       vjust = -1, size = 3.2) +
                    # Room for labels at 100% sensitivity or specificity, and no clipping at
                    # the panel edge (three "Parallel" labels were cut in half).
                    ggplot2::scale_x_continuous(
                        labels = scales::percent_format(),
                        expand = ggplot2::expansion(mult = c(0.08, 0.3))) +
                    ggplot2::scale_y_continuous(
                        labels = scales::percent_format(),
                        expand = ggplot2::expansion(mult = c(0.08, 0.15))) +
                    ggplot2::coord_cartesian(clip = "off") +
                    ggplot2::labs(
                        title = .("Decision Space - Sensitivity vs Specificity"),
                        x = .("Sensitivity"),
                        y = .("Specificity"),
                        size = .("Youden's J"),
                        colour = .("Row type")
                    ) +
                    # theme_minimal() replaced by ggtheme (global theme + palette). The
                    # continuous x/y scales above are position scales and survive it.
                    ggtheme

                print(p)
                return(TRUE)
            },
            .applyPatternFilter = function(data, filter_type) {
                if (filter_type == "all" || is.null(filter_type)) {
                    return(data)
                }

                labels <- as.character(data$pattern)
                # Only the exhaustive result patterns are made of +/- tokens; the named
                # strategy rows ("Parallel (>=1 pos)") are not patterns and never match.
                is_pattern <- grepl("^[+-](/[+-])+$", labels)
                all_pos <- is_pattern & !grepl("-", labels)
                all_neg <- is_pattern & !grepl("\\+", labels)

                keep <- switch(filter_type,
                    allPositive = all_pos,
                    allNegative = all_neg,
                    # "mixed" previously excluded anything STARTING with "+/+" or "-/-",
                    # which threw away genuinely mixed three-test patterns such as "+/+/-"
                    # and "-/-/+". Mixed means: a pattern that is neither all-positive nor
                    # all-negative.
                    mixed = is_pattern & !all_pos & !all_neg,
                    NULL
                )
                if (is.null(keep)) {
                    return(data)
                }

                # Returning the UNFILTERED table when nothing matched meant a user who
                # selected "All Positive" was shown every pattern and had no way to tell.
                # Return the empty selection; the plot callers already decline to draw and
                # jamovi shows an empty plot rather than a misleading full one.
                data[which(keep), , drop = FALSE]
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for decisioncombine analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                gold <- self$options$gold
                test1 <- self$options$test1

                # Emit syntax whenever the analysis can run (gold + test1 present). test2/test3
                # are optional, so single-test and two-test analyses also get reproducible code.
                if (is.null(gold) || is.null(test1)) {
                    return("")
                }

                # Get arguments
                args <- ""
                if (!is.null(private$.asArgs)) {
                    # .asArgs() prefixes its first argument with the same "\n    " this
                    # adds below, which left an empty line after `data = data,`.
                    args <- sub("^\\s+", "", private$.asArgs(incData = FALSE))
                }
                if (args != "") {
                    args <- paste0(",\n    ", args)
                }

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath" # fallback

                # Build complete function call
                paste0(pkg_name, "::decisioncombine(\n    data = data", args, ")")
            }
        ) # End of public list
    )
}
