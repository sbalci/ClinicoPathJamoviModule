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
            # Rows that needed the 0.5 continuity correction, accumulated across
            # .analyzeSinglePattern() calls and reported as ONE notice. Emitting per
            # pattern produced up to eleven near-identical banners in a 3-test analysis.
            .continuityPatterns = character(),
            # Patterns whose LR+/LR- point estimate is determinate but whose delta-method
            # SE collapses to exactly 0 (a structurally empty test margin), so the printed
            # interval would be the point estimate repeated. Collected here and disclosed
            # as ONE table note, for the same reason .continuityPatterns is.
            .zeroSeIntervalPatterns = character(),

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
                    li(esc(.("Prevalence is identical in every row: it is the disease rate in the whole sample, not the rate within that pattern."))),
                    li(esc(.("PPV and NPV depend on prevalence, so they transfer to another population only if its disease rate is similar. Sensitivity, specificity and the likelihood ratios are less prevalence-dependent, though not independent of it. LR+ and LR- are the more useful numbers to carry elsewhere not because they are more stable -- being functions of sensitivity and specificity, they inherit exactly the same dependence -- but because they convert a pre-test probability into a post-test probability at whatever prevalence the new setting has."))),
                    li(esc(.("Balanced Accuracy and Youden's J are the same statistic on two scales: Balanced Accuracy = (J + 1) / 2."))),
                    li(esc(.("LR+, LR- and the diagnostic odds ratio add 0.5 to all four cells when a cell is zero, so they stay finite (for the odds ratio this is the Haldane-Anscombe correction; the likelihood ratios use the same adjustment); the proportions on the same row use the observed counts. A row on which no patient tests positive (a pattern nobody shows, or a rule that is never positive) is left BLANK rather than corrected: with no predicted positives, LR+ and the odds ratio are undefined and are not invented."))),
                    "</ul><h4>", esc(.("What this analysis does NOT assume")), "</h4><p>",
                    esc(.("Every row is estimated directly from the observed joint results, not derived from the individual tests' sensitivity and specificity. These figures therefore do NOT require the tests to be conditionally independent given disease status, and they stay valid when the tests share biology, specimen or technology. That is the main difference from a sequential-testing calculator, which must assume independence and warns about it.")),
                    "</p><p>",
                    esc(.("Two things that assumption-free estimation does not buy. Precision: the sample is split across four or eight pattern cells instead of being pooled into two marginal proportions, so the intervals are wider -- that width is what the assumption was paying for. Transport: these numbers carry to another population only if the whole joint distribution of results given disease status carries over, which is a stronger requirement than each test's sensitivity and specificity carrying over. Parallel and Serial figures here will also generally NOT match the textbook independence formulas computed by hand from the individual tests, and will usually be worse when the tests are correlated.")),
                    "</p><h4>", esc(.("The ranking")), "</h4><p>",
                    esc(.("The candidate-rule ranking is a descriptive argmax over observed Youden's J, taken over every row of the table -- result patterns, single tests and strategies -- with no significance test and no multiplicity correction. On a tie the rule using fewer tests is shown first. On data with no real signal it will still name a winner. It is a hypothesis to confirm in new data, not a validated recommendation.")),
                    "</p><p>", esc(.("The pattern-type and statistic filters affect the plots only. The performance tables always show every row.")), "</p>"
                )

                assumptions <- paste0(
                    "<p>", esc(.("Diagnostic accuracy estimates are shaped by how the study was designed, not only by how the tests perform. The three biases below dominate real pathology accuracy studies and none of them is detectable from the data in front of this analysis.")), "</p><ul>",
                    li("<strong>", esc(.("Verification (work-up) bias.")), "</strong> ",
                       esc(.("If the reference standard was obtained mainly on patients whose test was positive, then test-negative patients of BOTH disease states are under-represented: the missing false negatives inflate sensitivity, and the missing true negatives deflate specificity. This is the usual situation when the reference standard is a biopsy or resection that is only performed after a positive test."))),
                    li("<strong>", esc(.("Spectrum bias.")), "</strong> ",
                       esc(.("Accuracy measured on clearly diseased versus clearly healthy patients is higher than accuracy in the population the test is actually used on, which is full of early, partial and equivocal presentations. A case-control (two-gate) design inflates sensitivity and specificity and makes the Prevalence column an artefact of the sampling ratio rather than of any clinical population, so PPV is inflated while NPV is DEFLATED relative to a low-prevalence setting. At sensitivity and specificity of 0.90, a 50% sampled prevalence gives PPV 0.90 and NPV 0.90, while a true 5% prevalence gives PPV 0.32 and NPV 0.99."))),
                    li("<strong>", esc(.("Incorporation bias.")), "</strong> ",
                       esc(.("If one of the tests being evaluated also contributed to establishing the reference standard, that test is being compared against itself and its apparent accuracy is inflated. This is easy to do accidentally when the reference is a final diagnosis assembled from all available information."))),
                    "</ul><h4>", esc(.("Other requirements")), "</h4><ul>",
                    li("<strong>", esc(.("One row per patient.")), "</strong> ",
                       esc(.("Every row must be a different patient. Several specimens, cores, blocks, lesions or lymph nodes from the same patient are correlated, and every confidence interval here treats the rows as independent, so with such data the intervals are too narrow. Use one result per patient, or a method that accounts for clustering."))),
                    li(esc(.("Complete-case analysis: a patient missing the reference standard or any selected test is excluded from the combination table. When any are excluded, the number is given in the warning \"Removed N case(s) with missing values\" in the notices at the top of the results."))),
                    li(esc(.("Each test is reduced to two levels. Any level that is not the one chosen as positive counts as negative, so equivocal and indeterminate results are folded into the negative arm unless you recode them as missing first."))),
                    li(esc(.("The rows are not independent: every rule is scored on the same patients, so the differences between them are correlated. A formal comparison needs a paired procedure -- McNemar's test applied SEPARATELY within the diseased patients to compare sensitivity and within the non-diseased patients to compare specificity, because a single McNemar over the whole sample compares positivity rates rather than accuracy. Paired PPV and NPV comparisons need a generalised score test instead. This analysis performs neither."))),
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
                    self$results$notices$setContent("")
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

            .optionSelected = function(value) {
                !is.null(value) && length(value) == 1L && nzchar(value)
            },

            # Forest plot height from the options alone, so .init() can set it. The rows are
            # fixed by the number of tests k: 2^k result patterns, each test alone, Parallel
            # and Serial, plus (k = 3) the six pairwise rules and Majority; one row for a
            # single test. About 22 px per row per facet keeps the rule labels from
            # overprinting.
            .forestPlotHeight = function() {
                k <- sum(vapply(c("test1", "test2", "test3"), function(n)
                    private$.optionSelected(self$options[[n]]), logical(1)))
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
                        item$deleteRows()
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
                        stats$setRow(rowKey = key, values = list(estimate = NA_real_))
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

                self$results$recommendationTable$setVisible(FALSE)
                # Set only beside the "advantage is not established" sentence it explains.
                self$results$recommendationTable$setNote("bound", NULL)
                self$results$recommendationTable$setRow(rowNo = 1, values = list(
                    pattern = NA_character_,
                    method = NA_character_,
                    youden = NA_real_,
                    sens = NA_real_,
                    spec = NA_real_,
                    acc = NA_real_,
                    rationale = NA_character_
                ))

                for (name in c("barPlot", "heatmapPlot", "forestPlot",
                               "decisionTreePlot")) {
                    self$results[[name]]$setState(list(valid = FALSE))
                }
            },

            .init = function() {
                private$.noticeList <- list()
                private$.continuityPatterns <- character()
                private$.zeroSeIntervalPatterns <- character()

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
                private$.clearDynamicResults()
                private$.updateIndividualVisibility()

                # Static educational content: render BEFORE the validation early-returns, so
                # a user who ticks "About this analysis" while still choosing variables gets
                # the explanation rather than an empty pane. It depends on no data.
                private$.renderAboutPanels()

                # .run() has two early returns (failed validation and failed data prep --
                # incomplete variable selection is now one of the validation errors, see
                # below). Rendering only at the bottom meant every
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
                if (!validation_result) {
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
                    return()
                }

                # Step 4: Combination analysis
                private$.analyzeCombinations(data_prep)

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
                private$.populateRecommendation()

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
                # None of the four checks below touches self$data.
                #
                # Empty state: nothing chosen at all. A freshly opened analysis must not
                # greet the user with a red ERROR before they have configured anything, so
                # return quietly here and let the Getting Started panel (welcome) speak. The checks
                # below then only fire once the user has STARTED choosing -- a partial
                # selection (a gold standard with no positive level, say) is the case that
                # genuinely needs telling, because nothing on screen says which control is
                # still empty.
                if (!private$.optionSelected(self$options$gold) &&
                    !private$.optionSelected(self$options$test1)) {
                    return(FALSE)
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    private$.addNotice(
                        "ERROR",
                        .("No Gold Standard"),
                        .("A gold standard variable is required. Select a reference test.")
                    )
                    return(FALSE)
                }

                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    private$.addNotice(
                        "ERROR",
                        .("No Gold Positive Level"),
                        .("Select the disease-present level for the gold standard.")
                    )
                    return(FALSE)
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    private$.addNotice(
                        "ERROR",
                        .("No Test 1"),
                        .("Test 1 is required. Select at least one test variable.")
                    )
                    return(FALSE)
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    private$.addNotice(
                        "ERROR",
                        .("No Test 1 Positive Level"),
                        .("Select the positive level for Test 1.")
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

                if (has_test2) {
                    if (is.null(self$options$test2Positive) || self$options$test2Positive == "") {
                        private$.addNotice(
                            "ERROR",
                            .("No Test 2 Positive Level"),
                            .("Select the positive level for Test 2.")
                        )
                        return(FALSE)
                    }
                }

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
                if (has_test3) {
                    if (is.null(self$options$test3Positive) || self$options$test3Positive == "") {
                        private$.addNotice(
                            "ERROR",
                            .("No Test 3 Positive Level"),
                            .("Select the positive level for Test 3.")
                        )
                        return(FALSE)
                    }
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
                if (n_used == 0) {
                    private$.addNotice(
                        "WARNING",
                        .fmt(
                            .("Test {test} Has No Complete Cases"),
                            test = test_num
                        ),
                        .fmt(
                            .("Test {test} cannot be summarized because no case has both the test and reference-standard result."),
                            test = test_num
                        )
                    )
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
                statsTable$setRow(rowKey = "sens", values = list(
                    estimate = sens
                ))
                statsTable$setRow(rowKey = "spec", values = list(
                    estimate = spec
                ))
                statsTable$setRow(rowKey = "ppv", values = list(
                    estimate = ppv
                ))
                statsTable$setRow(rowKey = "npv", values = list(
                    estimate = npv
                ))
            },
            .analyzeCombinations = function(data_prep) {
                # Generate and analyze all test combinations

                # The columns are headed "Sensitivity"/"Specificity"/"PPV"/"NPV" for every
                # row, but an exhaustive pattern row ("+/-") is not a decision rule -- its
                # "sensitivity" is P(this exact pattern | diseased). Only the strategy rows
                # are rules you could actually apply to a patient. Say so.
                self$results$combinationTable$setNote(
                    "row_kinds",
                    # The note must quote the labels the table actually prints. It used to
                    # call Serial "the all-positive pattern, which is the Serial (AND) rule",
                    # implying Serial had no row of its own -- but a named "Serial (all pos)"
                    # row has been emitted since the strategies were split out, so the note
                    # sent the reader looking for a row that was right in front of them.
                    jmvcore::.("A pattern row (e.g. \"+/-\") is a mutually exclusive group of patients. It can still be read as a rule -- \"call positive only when the results are exactly this\" -- and the ranking treats it that way, but every column then refers to that exact pattern: Sensitivity is the proportion of diseased patients showing it, Specificity the proportion of non-diseased patients showing anything else, and LR- the likelihood ratio for \"any other pattern\" rather than for a negative test. Accuracy and NPV on such a row are dominated by the patients who did not show the pattern, so a rare pattern reports both close to 1 minus the prevalence however uninformative it is -- read Youden's J there, not Accuracy. The named rows are rules you would normally apply to a patient: each test alone (\"Test 1 alone\"), Parallel (>=1 pos) and Serial (all pos) and, with three tests, the same two rules for every pair of tests (\"Parallel 1+2 (>=1 pos)\", \"Serial 1+2 (both pos)\") and Majority (>=2/3 pos). Every row is scored on the same complete cases; Serial (all pos) is numerically identical to the all-positive pattern row. The individual-test tables use each test's own pairwise-complete cases, so with missing data they can differ from the \"alone\" rows.")
                )
                self$results$combinationTable$setNote(
                    "haldane",
                    # The note must state the rule the code ACTUALLY applies, per branch.
                    # It used to say the three ratios are "left blank whenever a whole
                    # margin is empty", lumping the test and disease margins together --
                    # but only the DISEASE-margin branch blanks all three. On an empty
                    # TEST margin the code deliberately keeps the determinate ratio: a
                    # pattern no patient exhibits (tp + fp = 0) prints LR- exactly 1.00,
                    # verified on a fixture where test2 copies test1 so "+/-" and "-/+"
                    # are empty. A reader told all three were blank would have read that
                    # 1.00 as a corrected number.
                    jmvcore::.("LR+, LR- and the diagnostic odds ratio are computed with 0.5 added to all four cells when a cell is zero, so they stay finite (for the odds ratio this is the Haldane-Anscombe correction; the likelihood ratios use the same adjustment); sensitivity, specificity, PPV and NPV on the same row use the observed counts. The two therefore need not agree exactly at a zero cell. The correction repairs a sampling zero, not a structural one, so it is withheld whenever a whole margin is empty, and what is then left blank differs between the two kinds of empty margin. On a row with no test-positive patients (a pattern nobody shows, or a rule that is never positive), LR+ and the odds ratio are 0/0 and are left blank, while LR- is exactly 1 and is shown as such: any other result leaves the odds unchanged. The mirror case, a row with no test-negative patients, blanks LR- and the odds ratio and shows LR+ as exactly 1. In a sample where every case is disease-present, or every case is disease-absent, sensitivity or specificity cannot be estimated at all, so all three ratios are left blank in every row, as are PPV and NPV, which the sampling alone fixes at 0% or 100%, and Accuracy, which then only repeats the sensitivity or specificity.")
                )
                # sequentialtests warns about conditional independence in five places because
                # it DERIVES combined performance from marginal sensitivity and specificity.
                # This analysis measures every combined rule directly from the observed joint
                # 2x2, so that assumption is not required -- which is a real advantage, and a
                # pathologist arriving from the sibling analysis has no way to know it applies
                # differently here unless it is said.
                self$results$combinationTable$setNote(
                    "joint_estimation",
                    jmvcore::.("Every row is estimated directly from the observed joint results, not derived from the individual tests' sensitivity and specificity. These figures therefore do not assume the tests are conditionally independent given disease status, and they remain valid when the tests share biology or technology. They do assume this sample's case mix resembles the population the rule would be used in.")
                )
                self$results$combinationTable$setNote(
                    "column_reading",
                    jmvcore::.("Prevalence is the same in every row: it is the disease rate in the whole sample, not the rate within that pattern. Balanced Accuracy and Youden's J are the same statistic on two scales (Balanced Accuracy = (J + 1) / 2), so ranking by one is identical to ranking by the other.")
                )
                # Name the interval method. The ranking's own, different bound for J is
                # explained in a note on the ranking table, beside the sentence that quotes it.
                self$results$combinationTable$setNote(
                    "youden_ci",
                    jmvcore::.("The 95% confidence interval beside Youden's J is the Agresti-Caffo interval for a difference of two independent proportions, sensitivity minus the false-positive rate. It is not adjusted for choosing among rules or for the number of rules shown.")
                )
                self$results$combinationTable$setNote(
                    "multiplicity",
                    jmvcore::.("Every pattern and strategy is reported together with no adjustment for multiple comparisons. Treat the best-looking row as a hypothesis to confirm in new data, not as an established result.")
                )

                # Both tables are filled with addRow(), so they must be emptied first.
                # jamovi re-runs .run() on the SAME analysis object whenever any option
                # changes, and without this the pattern rows accumulated on every re-run
                # (5 -> 10 -> 15) until the duplicated rowKeys made $asDF fail outright
                # with "duplicate 'row.names' are not allowed", taking the recommendation
                # and every plot down with it.
                self$results$combinationTable$deleteRows()
                self$results$combinationTableCI$deleteRows()
                self$results$combinationTableCIRatios$deleteRows()

                # Inform users that PPV/NPV are based on sample prevalence - only when some
                # row can show one. In a one-class sample every PPV/NPV cell is blank and the
                # "Gold Standard Has Only One Outcome" notice says why; this note, which says
                # they "are calculated", then contradicted the table.
                gold_classes <- table(factor(data_prep$goldVariable2,
                                             levels = c("Positive", "Negative")))
                if (all(gold_classes > 0)) {
                    private$.addNotice(
                        "INFO",
                        .("PPV/NPV Interpretation"),
                        .("Positive and negative predictive values are calculated using the sample prevalence. Interpret them cautiously if the sample does not represent the target clinical population.")
                    )
                }

                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test only - no combinations. The row-kind footnote describes
                    # pattern, "alone" and strategy rows that a single-test table does not
                    # have; setNote(key, NULL) removes it, including one left by a previous
                    # run with more tests.
                    self$results$combinationTable$setNote("row_kinds", NULL)
                    private$.analyzeSinglePattern(
                        data_prep, .("Test 1"),
                        data_prep$test1Variable2 == "Positive",
                        row_type = .("Single test")
                    )
                    private$.emitContinuityNotice()
                    private$.emitNonEstimableCINote()
                    private$.assessSparseCounts()
                    return()
                }

                # Row order: result patterns, then each test alone, then (three tests) the
                # pairwise rules, then the strategies over all the tests. The ranking lists
                # named rows first and keeps this order among them, so on a tie the rule that
                # uses fewer tests is shown.
                if (!has_test3) {
                    # Two-test combinations (4 patterns)
                    private$.analyzeTwoTestPatterns(data_prep)
                    private$.addSingleTestRules(data_prep)
                    # Add clinical strategies for 2 tests
                    private$.addTwoTestStrategies(data_prep)
                } else {
                    # Three-test combinations (8 patterns)
                    private$.analyzeThreeTestPatterns(data_prep)
                    private$.addSingleTestRules(data_prep)
                    private$.addPairwiseRules(data_prep)
                    # Add clinical strategies for 3 tests
                    private$.addThreeTestStrategies(data_prep)
                }
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
                        .("A continuity correction of 0.5, added to all four cells, was applied to {n} row(s) with at least one zero cell ({patterns}). For the diagnostic odds ratio this is the Haldane-Anscombe correction; the likelihood ratios use the same adjustment. It affects LR+, LR-, the diagnostic odds ratio and their confidence intervals only; sensitivity, specificity, PPV and NPV on the same rows use the observed counts."),
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
                # A row with no disease-present or no disease-absent cases (a one-class
                # sample) has zero cells by construction, not by sparse sampling: its
                # ratios are already blank and its other notice explains why. Flagging it
                # here told the reader the (blank) ratios were "unstable" and that its
                # (blank) sensitivity could "still be sound".
                disease_margin_empty <- (table_df$tp + table_df$fn) == 0 |
                    (table_df$fp + table_df$tn) == 0
                sparse <- is.finite(row_minima) & row_minima < 5 &
                    !(disease_margin_empty %in% TRUE)
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
                                             row_type = NULL) {
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

                # Apply continuity correction if any cell is zero (except when all are zero)
                # This prevents Inf/NaN in likelihood ratios and allows valid CIs
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
                empty_margin <- (tp + fp) == 0 || (fn + tn) == 0 ||
                    (tp + fn) == 0 || (fp + tn) == 0
                use_continuity <- !empty_margin && any(c(tp, fp, fn, tn) == 0)
                if (use_continuity) {
                    tp_adj <- tp + 0.5
                    fp_adj <- fp + 0.5
                    fn_adj <- fn + 0.5
                    tn_adj <- tn + 0.5
                    # Collected, not announced here: .emitContinuityNotice() reports every
                    # corrected pattern in a single banner once the sweep is finished.
                    private$.continuityPatterns <- c(
                        private$.continuityPatterns, pattern_name)
                } else {
                    tp_adj <- tp
                    fp_adj <- fp
                    fn_adj <- fn
                    tn_adj <- tn
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

                # Calculate sensitivity and specificity from adjusted counts for LR/DOR
                sens_adj <- tp_adj / (tp_adj + fn_adj)
                spec_adj <- tn_adj / (fp_adj + tn_adj)

                # Likelihood ratios using adjusted counts (prevents Inf/NaN)
                lr_pos <- sens_adj / (1 - spec_adj)
                lr_neg <- (1 - sens_adj) / spec_adj

                # Diagnostic odds ratio using adjusted counts
                dor <- (tp_adj * tn_adj) / (fp_adj * fn_adj)

                # Blank only what is genuinely 0/0, not everything. With no predicted
                # positives sens = 0 and spec = 1, so LR+ is 0/0 while LR- is exactly 1;
                # with no predicted negatives sens = 1 and spec = 0, so LR- is 0/0 while
                # LR+ is exactly 1. Either way the diagnostic odds ratio is undefined. An
                # LR of 1 is worth showing -- it says the result does not move the odds.
                if ((tp + fp) == 0) {
                    lr_pos <- NA_real_
                    dor <- NA_real_
                }
                if ((fn + tn) == 0) {
                    lr_neg <- NA_real_
                    dor <- NA_real_
                }
                # An empty DISEASE margin leaves sensitivity (or specificity) undefined, so
                # all three ratios are undefined -- they cannot be read off a sample that
                # contains only diseased or only non-diseased patients.
                if ((tp + fn) == 0 || (fp + tn) == 0) {
                    lr_pos <- NA_real_
                    lr_neg <- NA_real_
                    dor <- NA_real_
                }
                # Anything still non-finite is an undefined quantity, not a number: with
                # tp = fp = fn = 0 and tn > 0, sens_adj was 0/0 and lr_neg reached addRow()
                # as a literal "NaN" next to a correctly blank Sensitivity. jamovi renders
                # NA as an empty cell but prints NaN and Inf verbatim.
                if (!is.finite(lr_pos)) lr_pos <- NA_real_
                if (!is.finite(lr_neg)) lr_neg <- NA_real_
                if (!is.finite(dor)) dor <- NA_real_

                # Add to main table
                combTable <- self$results$combinationTable
                combTable$addRow(rowKey = pattern_name, values = list(
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
                ))

                # Populate CI table with Wilson score intervals
                ciTable <- self$results$combinationTableCI
                # LR+, LR- and DOR are unbounded ratios, not proportions; they share no
                # sensible column format with sensitivity, so they get their own table.
                ratioTable <- self$results$combinationTableCIRatios

                # Sensitivity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_sens"), values = list(
                    pattern = pattern_name,
                    statistic = .("Sensitivity"),
                    estimate = sens,
                    lower = sens_ci[1],
                    upper = sens_ci[2]
                ))

                # Specificity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_spec"), values = list(
                    pattern = pattern_name,
                    statistic = .("Specificity"),
                    estimate = spec,
                    lower = spec_ci[1],
                    upper = spec_ci[2]
                ))

                # PPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_ppv"), values = list(
                    pattern = pattern_name,
                    statistic = .("PPV"),
                    estimate = ppv,
                    lower = ppv_ci[1],
                    upper = ppv_ci[2]
                ))

                # NPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_npv"), values = list(
                    pattern = pattern_name,
                    statistic = .("NPV"),
                    estimate = npv,
                    lower = npv_ci[1],
                    upper = npv_ci[2]
                ))

                # Accuracy with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_acc"), values = list(
                    pattern = pattern_name,
                    statistic = .("Accuracy"),
                    estimate = acc,
                    lower = acc_ci[1],
                    upper = acc_ci[2]
                ))

                # LR+ with CI (log-scale transformation for CI, using adjusted counts)
                # .estimableSe(): the estimate being legitimate says nothing about the
                # interval. On a structurally empty margin no correction is applied and the
                # delta-method SE collapses to EXACTLY 0 -- tp=30, fp=20, fn=0, tn=0 gave
                # se = sqrt(1/30 - 1/30 + 1/20 - 1/20) = 0 and printed LR+ 1.00 [1.00, 1.00],
                # a claim of perfect precision on a row with no negative-test patients. A
                # zero or non-finite SE means the interval is not estimable; blank it.
                if (!is.na(lr_pos) && lr_pos > 0) {
                    log_lr_pos <- log(lr_pos)
                    # Standard SE for log(LR+) using adjusted counts
                    se_log_lr_pos <- sqrt((1 / tp_adj) - (1 / (tp_adj + fn_adj)) +
                        (1 / fp_adj) - (1 / (fp_adj + tn_adj)))
                    if (private$.estimableSe(se_log_lr_pos)) {
                        lr_pos_lower <- exp(log_lr_pos - z_crit * se_log_lr_pos)
                        lr_pos_upper <- exp(log_lr_pos + z_crit * se_log_lr_pos)
                    } else {
                        lr_pos_lower <- NA_real_
                        lr_pos_upper <- NA_real_
                        private$.zeroSeIntervalPatterns <- c(
                            private$.zeroSeIntervalPatterns, pattern_name)
                    }
                } else {
                    lr_pos_lower <- NA
                    lr_pos_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_lrPos"), values = list(
                    pattern = pattern_name,
                    statistic = .("LR+"),
                    estimate = lr_pos,
                    lower = lr_pos_lower,
                    upper = lr_pos_upper
                ))

                # LR- with CI (log-scale transformation for CI, using adjusted counts)
                if (!is.na(lr_neg) && lr_neg > 0) {
                    log_lr_neg <- log(lr_neg)
                    # Standard SE for log(LR-) using adjusted counts
                    se_log_lr_neg <- sqrt((1 / fn_adj) - (1 / (tp_adj + fn_adj)) +
                        (1 / tn_adj) - (1 / (fp_adj + tn_adj)))
                    if (private$.estimableSe(se_log_lr_neg)) {
                        lr_neg_lower <- exp(log_lr_neg - z_crit * se_log_lr_neg)
                        lr_neg_upper <- exp(log_lr_neg + z_crit * se_log_lr_neg)
                    } else {
                        lr_neg_lower <- NA_real_
                        lr_neg_upper <- NA_real_
                        private$.zeroSeIntervalPatterns <- c(
                            private$.zeroSeIntervalPatterns, pattern_name)
                    }
                } else {
                    lr_neg_lower <- NA
                    lr_neg_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_lrNeg"), values = list(
                    pattern = pattern_name,
                    statistic = .("LR-"),
                    estimate = lr_neg,
                    lower = lr_neg_lower,
                    upper = lr_neg_upper
                ))

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
                } else {
                    dor_lower <- NA
                    dor_upper <- NA
                }
                ratioTable$addRow(rowKey = paste0(pattern_name, "_dor"), values = list(
                    pattern = pattern_name,
                    statistic = .("DOR"),
                    estimate = dor,
                    lower = dor_lower,
                    upper = dor_upper
                ))
            },
            .patternConditions = function(data_prep) {
                # The 4- and 8-pattern condition lists were written out verbatim in
                # .analyzeTwoTestPatterns / .analyzeThreeTestPatterns AND again in
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

            .analyzeTwoTestPatterns = function(data_prep) {
                patterns <- private$.patternConditions(data_prep)
                for (pattern_name in names(patterns)) {
                    private$.analyzeSinglePattern(data_prep, pattern_name,
                                                  patterns[[pattern_name]])
                }
            },
            .analyzeThreeTestPatterns = function(data_prep) {
                # Same generator; the arity comes from which testNVariable2 columns exist.
                private$.analyzeTwoTestPatterns(data_prep)
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
                        row_type = .("Single test")
                    )
                }
            },
            .addPairwiseRules = function(data_prep) {
                # With three tests, the Serial and Parallel rules for each PAIR of tests. A
                # useless third test makes the best rule a two-test one ("Test 1 and Test 2
                # both positive"), which no other row can express.
                for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
                    first <- data_prep[[paste0("test", pair[1], "Variable2")]] == "Positive"
                    second <- data_prep[[paste0("test", pair[2], "Variable2")]] == "Positive"
                    private$.analyzeSinglePattern(
                        data_prep,
                        .fmt(.("Serial {first}+{second} (both pos)"),
                             first = pair[1], second = pair[2]),
                        first & second,
                        row_type = .("Strategy")
                    )
                    private$.analyzeSinglePattern(
                        data_prep,
                        .fmt(.("Parallel {first}+{second} (>=1 pos)"),
                             first = pair[1], second = pair[2]),
                        first | second,
                        row_type = .("Strategy")
                    )
                }
            },
            .addTwoTestStrategies = function(data_prep) {
                # Add clinical strategy rows for 2-test combinations

                # Parallel strategy: Positive if ANY test is positive (high sensitivity)
                parallel_condition <- data_prep$test1Variable2 == "Positive" |
                    data_prep$test2Variable2 == "Positive"
                private$.analyzeSinglePattern(
                    data_prep,
                    .("Parallel (>=1 pos)"),
                    parallel_condition,
                    row_type = .("Strategy")
                )

                # Serial (AND) is numerically identical to the all-positive pattern "+/+",
                # but a reader should not have to know that to find it. It gets its own
                # named row; .populateRecommendation drops the all-positive pattern when this
                # row is present, so the twin does not become a spurious tie.
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                    data_prep$test2Variable2 == "Positive"
                private$.analyzeSinglePattern(
                    data_prep,
                    .("Serial (all pos)"),
                    serial_condition,
                    row_type = .("Strategy")
                )
            },
            .addThreeTestStrategies = function(data_prep) {
                # Add clinical strategy rows for 3-test combinations

                # Parallel strategy: Positive if ANY test is positive (high sensitivity)
                parallel_condition <- data_prep$test1Variable2 == "Positive" |
                    data_prep$test2Variable2 == "Positive" |
                    data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(
                    data_prep,
                    .("Parallel (>=1 pos)"),
                    parallel_condition,
                    row_type = .("Strategy")
                )

                # Serial (AND): identical to "+/+/+" but named, for the same reason as above.
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                    data_prep$test2Variable2 == "Positive" &
                    data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(
                    data_prep,
                    .("Serial (all pos)"),
                    serial_condition,
                    row_type = .("Strategy")
                )

                # Majority rule: Positive if at least 2 of 3 tests are positive (balanced)
                t1_pos <- data_prep$test1Variable2 == "Positive"
                t2_pos <- data_prep$test2Variable2 == "Positive"
                t3_pos <- data_prep$test3Variable2 == "Positive"
                majority_condition <- (as.integer(t1_pos) + as.integer(t2_pos) + as.integer(t3_pos)) >= 2
                private$.analyzeSinglePattern(
                    data_prep,
                    .("Majority (>=2/3 pos)"),
                    majority_condition,
                    row_type = .("Strategy")
                )
            },
            .populateFrequencyTables = function(data_prep) {
                # Emptied for the same reason as the combination tables above.
                self$results$goldFreqTable$deleteRows()
                self$results$crossTabTable$deleteRows()

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
                has_test3 <- "test3Variable2" %in% names(data_prep)

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
            # text states. Silent when an inversion warning was raised: that is the specific
            # diagnosis. One test gets its own wording, since nothing was compared or chosen.
            .assessTopRuleDiscrimination = function(best, scored, inversion_flagged, multi_test) {
                if (inversion_flagged) {
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
                    template <- if (multi_test) {
                        .("The highest Youden's J is {youden}, for {rule} (Agresti-Caffo 95% CI {lower} to {upper}; equivalent AUC {auc}). The 95% confidence interval of every rule scored here includes 0, so this sample does not show that any of them separates disease-present from disease-absent cases better than chance; at least one interval also reaches 0.40, the equivalent of an area under the ROC curve of 0.70 (Hosmer, Lemeshow and Sturdivant 2013), so acceptable discrimination is not ruled out either. The intervals are not adjusted for choosing among the rules. Check the levels chosen as positive, and whether the sample gives the precision you need.")
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
            .populateRecommendation = function() {
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
                                    .("{test} classifies worse than chance in this sample, beyond sampling variation: Youden's J = {youden} (Agresti-Caffo 95% CI {lower} to {upper}) on the {n} complete cases analysed here. Its sensitivity and specificity add up to less than 1, so calling its other level positive would do better; this is the signature of a reversed positive level. Check the level chosen as positive for {test} and for the reference standard before interpreting any of these results."),
                                    test = test_names[clear],
                                    youden = fmt_j(tested$youden[clear]),
                                    lower = fmt_j(ci[clear, 1]),
                                    upper = fmt_j(ci[clear, 2]),
                                    n = n_cases
                                )
                            } else {
                                .fmt(
                                    .("Several tests classify worse than chance in this sample, beyond sampling variation, on the {n} complete cases analysed here: {tests}. For each, sensitivity and specificity add up to less than 1, the signature of a reversed positive level. Check the levels chosen as positive for these tests and for the reference standard before interpreting any of these results; when every test is below chance, the reference standard's level is the likelier cause."),
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
                                    .("Youden's J is below 0 but within sampling variation of 0 for {tests}, on the {n} complete cases analysed here. A test with no diagnostic value falls below 0 about half the time at any sample size, so a correctly coded but uninformative test and a test whose positive level is reversed cannot be told apart here. If the level chosen as positive is right, the test adds no information in this sample."),
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
                    if (!inversion_flagged && (negative_rule_wins || named_rules_fail)) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Positive Levels May Be Inverted"),
                            .("The rule that separates the two groups best in this sample is one that calls a patient positive when the tests are negative; or every named rule (each test alone and every strategy) performs at or below chance while an exact result pattern does not. Both are the signature of a reversed positive level: the arithmetic still works, but the sensitivity and specificity reported for each rule are then swapped, and the best-performing rule is one no clinician can apply. Check the level chosen as positive for the reference standard and for each test before interpreting any of these results.")
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
                recTable$setRow(rowNo = 1, values = list(
                    pattern = best_pattern$pattern,
                    method = .("Descriptive Youden ranking"),
                    youden = best_pattern$youden,
                    sens = best_pattern$sens,
                    spec = best_pattern$spec,
                    acc = best_pattern$acc,
                    rationale = rationale
                ))
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
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Pattern, y = Value, fill = Metric)) +
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
                    cols = -pattern,
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

                midpoint <- if (stat_filter %in% c("lrPos", "lrNeg", "dor")) 1
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

                # The tile labels were drawn in a hardcoded near-black whatever was under
                # them. Reproduce the fill scale_fill_gradient2() will build and pick a light
                # or dark label per tile from its relative luminance (Rec. 709), so the value
                # stays readable on any pole, including a future dark render surface.
                label_values <- plot_data$Value
                estimable <- is.finite(label_values)
                tile_fill <- rep("#bebebe", length(label_values))   # ggplot's na.value grey
                if (any(estimable)) {
                    tile_fill[estimable] <- scales::div_gradient_pal(
                        low_col, mid_col, high_col
                    )(scales::rescale_mid(label_values[estimable], mid = midpoint))
                }
                luminance <- colSums(
                    grDevices::col2rgb(tile_fill) / 255 * c(0.2126, 0.7152, 0.0722))
                plot_data$LabelColour <- ifelse(luminance < 0.5, "#f5f5f5", "#1a1a1a")
                # sprintf("%.2f", NA) is the literal "NA", which was printed on the grey tile
                # of every blank cell (empty patterns, one-class PPV/NPV).
                plot_data$Label <- ifelse(estimable, sprintf("%.2f", label_values), "")

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Metric, y = pattern, fill = Value)) +
                    ggplot2::geom_tile() +
                    ggplot2::geom_text(
                        ggplot2::aes(label = Label, color = LabelColour)
                    ) +
                    ggplot2::labs(
                        title = .("Performance Heatmap"),
                        x = "",
                        y = .("Pattern"),
                        fill = .("Value")
                    ) +
                    # theme_minimal() replaced by ggtheme. The continuous fill scale must come
                    # AFTER ggtheme: ggtheme carries a DISCRETE fill scale that would replace
                    # this one and then fail with "continuous value supplied to discrete scale".
                    ggtheme +
                    ggplot2::scale_fill_gradient2(
                        low = low_col, mid = mid_col, high = high_col,
                        midpoint = midpoint
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
                    p <- ggplot2::ggplot(
                        df, ggplot2::aes(x = estimate, y = pattern, colour = statistic))
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
                            ggplot2::aes(xmin = lower, xmax = upper),
                            orientation = "y", width = 0.25
                        ) +
                        # Open points: a continuity-corrected ratio (a zero cell), so an
                        # infinite observed LR+ no longer looks like an ordinary 14.8.
                        ggplot2::geom_point(ggplot2::aes(shape = corrected), size = 2.5,
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
                    caption <- c(caption, .("Open points: rows with a zero cell, whose LR+, LR- and DOR use a 0.5 continuity correction; without it at least one of the three would be 0 or infinite."))
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

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = sens, y = spec)) +
                    ggplot2::geom_point(ggplot2::aes(size = youden, colour = rowType)) +
                    ggplot2::geom_text(data = label_df, ggplot2::aes(label = label),
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
