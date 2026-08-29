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
            # Patterns that needed a Haldane-Anscombe correction, accumulated across
            # .analyzeSinglePattern() calls and reported as ONE notice. Emitting per
            # pattern produced up to eleven near-identical banners in a 3-test analysis.
            .continuityPatterns = character(),
            # A results item that may not exist in the compiled .h.R yet. jmvcore raises
            # "'<name>' does not exist in this results element" rather than returning NULL,
            # so a bare self$results$combinationTableCIRatios would crash every run between
            # the .r.yaml edit and the next jmvtools::prepare().
            .resultsItem = function(name) {
                tryCatch(self$results[[name]], error = function(e) NULL)
            },

            .addNotice = function(type, title, content) {
                notice <- list(
                    type = type,
                    title = title,
                    content = content
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
                    "<h3 style='margin-top: 0; color: #2563eb;'>",
                    htmltools::htmlEscape(title), "</h3>", body, "</div>"
                )
            },

            .renderAboutPanels = function() {
                # jmvcore raises "options$showAbout does not exist" rather than returning
                # NULL, so a bare read throws on EVERY run in the window between adding the
                # option to the .a.yaml and the next jmvtools::prepare(). That window is
                # real -- prepare() is module-wide and the user's to run -- and an
                # unguarded read there kills the whole analysis, not just this panel.
                show <- tryCatch(self$options$showAbout, error = function(e) FALSE)
                if (!isTRUE(show)) {
                    return()
                }

                li <- function(...) paste0("<li style='margin-bottom: 6px;'>", ..., "</li>")
                esc <- function(x) htmltools::htmlEscape(x)

                about <- paste0(
                    "<p>", esc(.("This analysis evaluates every way two or three diagnostic tests can be combined, scoring each against the reference standard on the same patients. With a single test selected it reports that one test alone, and the two row kinds described below do not arise.")), "</p>",
                    "<h4>", esc(.("The two kinds of row")), "</h4><ul>",
                    li(esc(.("A PATTERN row (\"+/-\") is a mutually exclusive group of patients: those whose results were exactly that. It can be read as a rule -- \"call positive only when the results are exactly this\" -- and the ranking treats it that way, but then every column refers to that exact pattern. Sensitivity is the share of diseased patients showing it; Specificity the share of non-diseased patients showing anything else; LR- the likelihood ratio for \"any other pattern\" rather than for a negative test."))),
                    li(esc(.("Accuracy and NPV on a pattern row are dominated by the patients who did NOT show that pattern, so a rare pattern reports both close to 1 minus the prevalence however uninformative it is. A pattern seen in 5 of 50 diseased and 10 of 150 non-diseased patients reports Accuracy 0.73 and NPV 0.76 against a prevalence of 0.25, while its Youden's J is 0.03. Read Youden's J or Balanced Accuracy on these rows, not Accuracy. PPV and LR+ are the two columns that read cleanly: PPV is the probability of disease given exactly that result combination, and LR+ is the likelihood ratio for that combination."))),
                    li(esc(.("A STRATEGY row is a rule you would normally apply to a patient. Parallel (>=1 pos) calls positive if any test is positive, which raises sensitivity and lowers specificity. Serial (all pos) requires every test to be positive, which does the reverse. Majority (>=2/3 pos) needs two of three."))),
                    "</ul><h4>", esc(.("Reading the columns")), "</h4><ul>",
                    li(esc(.("Prevalence is identical in every row: it is the disease rate in the whole sample, not the rate within that pattern."))),
                    li(esc(.("PPV and NPV depend on prevalence, so they transfer to another population only if its disease rate is similar. Sensitivity, specificity and the likelihood ratios are less prevalence-dependent, though not independent of it. LR+ and LR- are the more useful numbers to carry elsewhere not because they are more stable -- being functions of sensitivity and specificity, they inherit exactly the same dependence -- but because they convert a pre-test probability into a post-test probability at whatever prevalence the new setting has."))),
                    li(esc(.("Balanced Accuracy and Youden's J are the same statistic on two scales: Balanced Accuracy = (J + 1) / 2."))),
                    li(esc(.("LR+, LR- and the diagnostic odds ratio use a Haldane-Anscombe 0.5 correction when a cell is zero, so they stay finite; the proportions on the same row use the observed counts. A pattern that no patient exhibits is left BLANK rather than corrected: with no predicted positives, LR+ and the odds ratio are undefined and are not invented."))),
                    "</ul><h4>", esc(.("What this analysis does NOT assume")), "</h4><p>",
                    esc(.("Every row is estimated directly from the observed joint results, not derived from the individual tests' sensitivity and specificity. These figures therefore do NOT require the tests to be conditionally independent given disease status, and they stay valid when the tests share biology, specimen or technology. That is the main difference from a sequential-testing calculator, which must assume independence and warns about it.")),
                    "</p><p>",
                    esc(.("Two things that assumption-free estimation does not buy. Precision: the sample is split across four or eight pattern cells instead of being pooled into two marginal proportions, so the intervals are wider -- that width is what the assumption was paying for. Transport: these numbers carry to another population only if the whole joint distribution of results given disease status carries over, which is a stronger requirement than each test's sensitivity and specificity carrying over. Parallel and Serial figures here will also generally NOT match the textbook independence formulas computed by hand from the individual tests, and will usually be worse when the tests are correlated.")),
                    "</p><h4>", esc(.("The ranking")), "</h4><p>",
                    esc(.("The candidate-rule ranking is a descriptive argmax over observed Youden's J with no significance test and no multiplicity correction. On data with no real signal it will still name a winner. It is a hypothesis to confirm in new data, not a validated recommendation.")),
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
                    li(esc(.("Complete-case analysis: a patient missing the reference standard or any selected test is excluded from the combination table. The number excluded is reported above when it is not zero."))),
                    li(esc(.("Each test is reduced to two levels. Any level that is not the one chosen as positive counts as negative, so equivocal and indeterminate results are folded into the negative arm unless you recode them as missing first."))),
                    li(esc(.("The rows are not independent: every rule is scored on the same patients, so the differences between them are correlated. A formal comparison needs a paired procedure -- McNemar's test applied SEPARATELY within the diseased patients to compare sensitivity and within the non-diseased patients to compare specificity, because a single McNemar over the whole sample compares positivity rates rather than accuracy. Paired PPV and NPV comparisons need a generalised score test instead. This analysis performs neither."))),
                    li(esc(.("Sample size and cell counts are reported as notices when they fall below the usual thresholds. Read the confidence intervals rather than the point estimates whenever they do."))),
                    "</ul>"
                )

                self$results$about$setContent(
                    private$.panelHtml(.("What this analysis does"), about))
                self$results$assumptions$setContent(
                    private$.panelHtml(.("Assumptions, biases and requirements"), assumptions))
            },

            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Notices were rendered in the order they happened to be emitted, which put
                # the STRONG_WARNING about sparse strategy counts BELOW four routine INFO
                # banners -- the reader had to scroll past the reassuring notes to reach the
                # reason to distrust the numbers. Sort by severity, stably, so equally
                # severe notices keep the order in which the analysis produced them.
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
                type_styles <- list(
                    ERROR = list(
                        color = "#dc2626", bgcolor = "rgba(220, 38, 38, 0.10)",
                        border = "#fca5a5", icon = "\u{26D4}"),      # no-entry sign
                    STRONG_WARNING = list(
                        color = "#ea580c", bgcolor = "rgba(234, 88, 12, 0.10)",
                        border = "#fdba74", icon = "\u{26A0}"),      # warning sign
                    WARNING = list(
                        color = "#ca8a04", bgcolor = "rgba(202, 138, 4, 0.12)",
                        border = "#fde047", icon = "\u{26A0}"),
                    INFO = list(
                        color = "#2563eb", bgcolor = "rgba(37, 99, 235, 0.08)",
                        border = "#93c5fd", icon = "\u{2139}")       # info sign
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
                        "<strong style='color: ", style$color, ";'>",
                        style$icon, " ", htmltools::htmlEscape(notice$title),
                        "</strong><br>",
                        "<span style='color: inherit;'>",
                        htmltools::htmlEscape(notice$content),
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
                    item <- private$.resultsItem(name)
                    if (!is.null(item) && item$rowCount > 0) {
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
                self$results$recommendationTable$setVisible(FALSE)
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
                    item <- private$.resultsItem(name)
                    if (!is.null(item)) {
                        item$setState(list(valid = FALSE))
                    }
                }
            },

            .init = function() {
                private$.noticeList <- list()
                private$.continuityPatterns <- character()

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
            },
            .run = function() {
                private$.noticeList <- list()
                private$.continuityPatterns <- character()
                private$.clearDynamicResults()
                private$.updateIndividualVisibility()

                # Static educational content: render BEFORE the validation early-returns, so
                # a user who ticks "About this analysis" while still choosing variables gets
                # the explanation rather than an empty pane. It depends on no data.
                private$.renderAboutPanels()

                # .run() has three early returns (failed validation, failed data prep, and
                # incomplete variable selection). Rendering only at the bottom meant every
                # notice explaining WHY the analysis stopped -- "Missing Level",
                # "No Complete Cases", every validation error -- was collected and then
                # discarded, leaving the user with a blank analysis and no message. on.exit
                # covers every exit path, including ones added later.
                on.exit(private$.renderNotices(), add = TRUE)

                # Check if we have minimum required variables
                if (!private$.hasRequiredVars()) {
                    return()
                }

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

                # Step 6: Populate recommendation (if requested)
                if (self$options$showRecommendation) {
                    private$.populateRecommendation()
                }

                # Step 7: Add pattern to data (if requested)
                # House idiom for a pure Output item (R/ctdnadynamics.b.R:139): gate on
                # isNotFilled() alone. A `type: Output` option is NOT an argument of the
                # generated wrapper, so gating on its value would make this column
                # unreachable from the R API; and in the GUI jamovi materialises the column
                # only when the Output control is enabled, so writing unconditionally here
                # is both harmless and simpler.
                if (self$results$addedPattern$isNotFilled()) {
                    private$.addPatternColumn(data_prep)
                }

                private$.setPlotStates()

                # Notices are rendered by the on.exit handler registered above.
            },
            .hasRequiredVars = function() {
                # Check if minimum required variables are selected
                # Returns FALSE to silently skip analysis, not throw error

                if (is.null(self$data) || nrow(self$data) == 0) {
                    return(FALSE)
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    return(FALSE)
                }

                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    return(FALSE)
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    return(FALSE)
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    return(FALSE)
                }

                return(TRUE)
            },
            .validateInputs = function() {
                # Strict validation with clear error messages using HTML notices
                # Returns TRUE if validation passes, FALSE otherwise

                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        "ERROR",
                        .("No Data"),
                        .("No data are available. Load data before running the analysis.")
                    )
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
                gold_levels_present <- unique(stats::na.omit(as.character(mydata[[goldVar]])))
                if (length(gold_levels_present) < 2) {
                    one_class_message <- if (identical(
                        gold_levels_present[1], self$options$goldPositive)) {
                        .fmt(
                            .('Every complete case has gold standard "{level}", so this sample contains no disease-absent cases. Specificity and NPV cannot be estimated and are reported as blank. Diagnostic accuracy assessment requires both diseased and non-diseased cases.'),
                            level = gold_levels_present[1]
                        )
                    } else {
                        .fmt(
                            .('Every complete case has gold standard "{level}", so this sample contains no disease-present cases. Sensitivity and PPV cannot be estimated and are reported as blank. Diagnostic accuracy assessment requires both diseased and non-diseased cases.'),
                            level = gold_levels_present[1]
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

                # Recode gold standard
                mydata <- mydata %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVar]]) ~ NA_character_,
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive",
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        goldVariable2 = factor(goldVariable2, levels = c("Positive", "Negative"))
                    )

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
                        .("With fewer than 20 complete cases every proportion rests on a handful of patients, so one reclassified case moves sensitivity or specificity by several percentage points and the 95% confidence intervals are very wide. Read the intervals rather than the point estimates. Diagnostic accuracy studies usually need on the order of 100 cases before the intervals narrow usefully, and combination analysis splits those cases across four or eight patterns.")
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
                        .("Around 100 cases or more is the usual target for a stable diagnostic accuracy estimate; this sample supports preliminary estimates, particularly once it is divided across the result patterns.")
                    )
                }

                # Counted once here and reused by the prevalence notice below.
                n_gold_pos <- sum(mydata$goldVariable2 == "Positive", na.rm = TRUE)
                n_gold_obs <- sum(!is.na(mydata$goldVariable2))
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
                            # tables above use. Name it precisely -- the severity sort now
                            # lifts this notice above those tables.
                            .("Extreme disease prevalence in the combination analysis: {percent}% ({diseased}/{observed} complete cases). PPV and NPV are highly sensitive to prevalence and may not generalize to populations with different disease rates. Sensitivity and specificity can also vary across settings and case mix, and with so few cases in one arm the likelihood ratios and diagnostic odds ratios for every pattern rest on a very small denominator. Individual-test tables use their own pairwise denominators, so their prevalence may differ."),
                            percent = base::format(round(100 * prevalence, 1), nsmall = 1),
                            diseased = n_gold_pos,
                            observed = n_gold_obs
                        )
                    )
                }

                # Recode Test 1
                mydata <- mydata %>%
                    dplyr::mutate(
                        test1Variable2 = dplyr::case_when(
                            is.na(.data[[test1Var]]) ~ NA_character_,
                            .data[[test1Var]] == self$options$test1Positive ~ "Positive",
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        test1Variable2 = factor(test1Variable2, levels = c("Positive", "Negative"))
                    )

                # Recode Test 2 if present
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    test2Var <- self$options$test2
                    mydata <- mydata %>%
                        dplyr::mutate(
                            test2Variable2 = dplyr::case_when(
                                is.na(.data[[test2Var]]) ~ NA_character_,
                                .data[[test2Var]] == self$options$test2Positive ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        ) %>%
                        dplyr::mutate(
                            test2Variable2 = factor(test2Variable2, levels = c("Positive", "Negative"))
                        )
                }

                # Recode Test 3 if present
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    test3Var <- self$options$test3
                    mydata <- mydata %>%
                        dplyr::mutate(
                            test3Variable2 = dplyr::case_when(
                                is.na(.data[[test3Var]]) ~ NA_character_,
                                .data[[test3Var]] == self$options$test3Positive ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        ) %>%
                        dplyr::mutate(
                            test3Variable2 = factor(test3Variable2, levels = c("Positive", "Negative"))
                        )
                }

                return(mydata)
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

                # Extract counts
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Validate counts
                if (any(is.na(c(tp, fp, fn, tn))) || any(c(tp, fp, fn, tn) < 0)) {
                    private$.addNotice(
                        "WARNING",
                        .("Invalid Counts"),
                        .fmt(
                            .("Invalid counts were detected for Test {test}. The individual analysis was skipped."),
                            test = test_num
                        )
                    )
                    return()
                }

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice(
                        "WARNING",
                        .("All Zero Counts"),
                        .fmt(
                            .("No valid observations were found for Test {test}. The individual analysis was skipped."),
                            test = test_num
                        )
                    )
                    return()
                }

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
                    jmvcore::.("A pattern row (e.g. \"+/-\") is a mutually exclusive group of patients. It can still be read as a rule -- \"call positive only when the results are exactly this\" -- and the ranking treats it that way, but every column then refers to that exact pattern: Sensitivity is the proportion of diseased patients showing it, Specificity the proportion of non-diseased patients showing anything else, and LR- the likelihood ratio for \"any other pattern\" rather than for a negative test. Accuracy and NPV on such a row are dominated by the patients who did not show the pattern, so a rare pattern reports both close to 1 minus the prevalence however uninformative it is -- read Youden's J there, not Accuracy. The named rows -- Parallel (>=1 pos), Serial (all pos) and, with three tests, Majority (>=2/3 pos) -- are the strategies you would normally apply to a patient; Serial (all pos) is numerically identical to the all-positive pattern row.")
                )
                self$results$combinationTable$setNote(
                    "haldane",
                    jmvcore::.("LR+, LR- and the diagnostic odds ratio are computed with a Haldane-Anscombe 0.5 continuity correction when a cell is zero, so they stay finite; sensitivity, specificity, PPV and NPV on the same row use the observed counts. The two therefore need not agree exactly at a zero cell. A pattern that no patient exhibits is left blank instead of corrected, because a likelihood ratio for a row containing no patients is undefined.")
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
                ratios_tbl <- private$.resultsItem("combinationTableCIRatios")
                if (!is.null(ratios_tbl)) ratios_tbl$deleteRows()

                # Inform users that PPV/NPV are based on sample prevalence
                private$.addNotice(
                    "INFO",
                    .("PPV/NPV Interpretation"),
                    .("Positive and negative predictive values are calculated using the sample prevalence. Interpret them cautiously if the sample does not represent the target clinical population.")
                )

                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test only - no combinations
                    private$.analyzeSinglePattern(
                        data_prep, .("Test 1"),
                        data_prep$test1Variable2 == "Positive",
                        row_type = .("Single test")
                    )
                    private$.emitContinuityNotice()
                    private$.assessSparseCounts()
                    return()
                }

                if (!has_test3) {
                    # Two-test combinations (4 patterns)
                    private$.analyzeTwoTestPatterns(data_prep)
                    # Add clinical strategies for 2 tests
                    private$.addTwoTestStrategies(data_prep)
                } else {
                    # Three-test combinations (8 patterns)
                    private$.analyzeThreeTestPatterns(data_prep)
                    # Add clinical strategies for 3 tests
                    private$.addThreeTestStrategies(data_prep)
                }
                private$.emitContinuityNotice()
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
            .emitContinuityNotice = function() {
                patterns <- unique(private$.continuityPatterns)
                if (length(patterns) == 0) {
                    return()
                }
                private$.addNotice(
                    "INFO",
                    .("Continuity Correction"),
                    .fmt(
                        .("A Haldane-Anscombe continuity correction of 0.5 was applied to {n} pattern(s) with at least one zero cell ({patterns}). The correction affects LR+, LR-, the diagnostic odds ratio and their confidence intervals only; sensitivity, specificity, PPV and NPV on the same rows use the observed counts."),
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
                sparse <- is.finite(row_minima) & row_minima < 5
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
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Sparse Cell Counts"),
                    .fmt(
                        .("These rows have a 2-by-2 cell count below 5 (smallest cell {minimum}): {rows}. Their likelihood ratios, diagnostic odds ratios and confidence intervals rest on very few cases and may be unstable even when they look informative. Sensitivity, specificity and Youden's J on the same rows can still be sound, because those depend on the size of the two reference groups rather than on the smallest cell. Treat the ratio columns as exploratory and validate them in a larger independent sample."),
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

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice(
                        "WARNING",
                        .("All Zero Counts"),
                        .fmt(
                            .('No observations were found for pattern "{pattern}". This combination was skipped.'),
                            pattern = pattern_name
                        )
                    )
                    return()
                }

                # Apply continuity correction if any cell is zero (except when all are zero)
                # This prevents Inf/NaN in likelihood ratios and allows valid CIs
                # A pattern that NO patient exhibits (tp = 0 and fp = 0) is routine whenever
                # two tests agree closely. The all-zero guard above cannot catch it, because
                # fn + tn is then the whole sample. Adding 0.5 to every cell would manufacture
                # a finite LR+ and diagnostic odds ratio out of a row containing no patients,
                # and PPV is undefined there. Report those as blank rather than inventing them.
                empty_margin <- (tp + fp) == 0 || (fn + tn) == 0
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
                ppv_ci <- private$.calcWilsonCI(tp, total_test_pos)
                npv_ci <- private$.calcWilsonCI(tn, total_test_neg)
                acc_ci <- private$.calcWilsonCI(tp + tn, total)

                # Calculate additional metrics using adjusted counts for LR/DOR
                n <- tp + fp + fn + tn
                prev <- (tp + fn) / n
                balanced_acc <- (sens + spec) / 2
                youden_j <- sens + spec - 1

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
                    lrPos = lr_pos,
                    lrNeg = lr_neg,
                    dor = dor
                ))

                # Populate CI table with Wilson score intervals
                ciTable <- self$results$combinationTableCI
                # LR+, LR- and DOR are unbounded ratios, not proportions; they share no
                # sensible column format with sensitivity, so they get their own table.
                ratioTable <- private$.resultsItem("combinationTableCIRatios")

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
                if (!is.na(lr_pos) && lr_pos > 0) {
                    log_lr_pos <- log(lr_pos)
                    # Standard SE for log(LR+) using adjusted counts
                    se_log_lr_pos <- sqrt((1 / tp_adj) - (1 / (tp_adj + fn_adj)) +
                        (1 / fp_adj) - (1 / (fp_adj + tn_adj)))
                    lr_pos_lower <- exp(log_lr_pos - z_crit * se_log_lr_pos)
                    lr_pos_upper <- exp(log_lr_pos + z_crit * se_log_lr_pos)
                } else {
                    lr_pos_lower <- NA
                    lr_pos_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_lrPos"), values = list(
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
                    lr_neg_lower <- exp(log_lr_neg - z_crit * se_log_lr_neg)
                    lr_neg_upper <- exp(log_lr_neg + z_crit * se_log_lr_neg)
                } else {
                    lr_neg_lower <- NA
                    lr_neg_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_lrNeg"), values = list(
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
                    dor_lower <- exp(log_dor - z_crit * se_log_dor)
                    dor_upper <- exp(log_dor + z_crit * se_log_dor)
                } else {
                    dor_lower <- NA
                    dor_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_dor"), values = list(
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
                # named row; .populateRecommendation de-duplicates candidates by their 2x2
                # counts so the twin does not become a spurious tie.
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
            .populateRecommendation = function() {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return()
                }

                table_df <- combTable$asDF

                # Rank every estimable classifier represented by the table, including exact
                # result-pattern rules and named clinical strategies, de-duplicating
                # algebraically identical 2-by-2 tables (for example, Serial and the
                # all-positive pattern).
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
                candidates$n_diseased <- candidates$tp + candidates$fn
                candidates$n_healthy <- candidates$fp + candidates$tn
                candidates <- candidates[
                    candidates$n_diseased >= 10 & candidates$n_healthy >= 10, , drop = FALSE]

                # De-duplication keeps whichever row comes first, and the exhaustive
                # patterns are added to the table before the named strategies. That made
                # the winner display as "+/+/+" rather than the algebraically identical
                # "Serial (all pos)" -- but a pattern label describes a group of patients,
                # while the strategy label names a rule a clinician can actually apply.
                # Sorting the named rows to the front makes both the de-duplication below
                # and which.max()'s first-max tie-break resolve to the applicable rule.
                candidates <- candidates[
                    order(!(candidates$rowType %in% c(.("Strategy"), .("Single test"))),
                          method = "radix"),
                    , drop = FALSE
                ]
                candidates <- candidates[
                    !duplicated(paste(candidates$tp, candidates$fp,
                                      candidates$fn, candidates$tn, sep = "|")),
                    , drop = FALSE
                ]
                if (nrow(candidates) == 0) {
                    private$.addNotice(
                        "WARNING",
                        .("Strategy Ranking Unavailable"),
                        .("No candidate rule has an estimable Youden index with at least 10 disease-present and 10 disease-absent cases. Youden's J needs both reference groups to be reasonably sized before a ranking means anything.")
                    )
                    return()
                }

                # Youden's J <= 0 means the rule classifies no better than a coin toss (and
                # below 0, worse than one -- it is anti-predictive and would have to be
                # inverted to be useful). Naming such a row "Highest-Ranked Rule" and then
                # describing it as a sensitivity/specificity trade-off actively misleads.
                # Rank only rules that beat chance, and say so plainly when none do.
                #
                # This is a guard, not a common path. The exhaustive pattern rows partition
                # the diseased and the non-diseased cases separately, so both conditional
                # distributions sum to 1 and the pattern Youden values sum to EXACTLY zero:
                # some pattern always has J > 0 unless every one is precisely 0. The branch
                # below is therefore reached only when every pattern is EXACTLY zero, i.e.
                # the tests carry no information about the reference standard at all. The
                # eligibility gate above cannot help it fire: tp + fn and fp + tn are the
                # sample's group sizes, identical in every row, so the gate admits all rules
                # or none. This is a guard against a degenerate sample, not a common path.
                n_estimable <- nrow(candidates)
                candidates <- candidates[candidates$youden > 0, , drop = FALSE]
                if (nrow(candidates) == 0) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("No Rule Performs Better Than Chance"),
                        .fmt(
                            .("None of the {n} eligible candidate rules has a Youden's J above zero, so none discriminates better than chance in this sample and no rule is ranked. A rule with a negative Youden's J is anti-predictive: its result would have to be reversed to carry information. Review the positive-level assignments for the reference standard and each test before interpreting these results."),
                            n = n_estimable
                        )
                    )
                    return()
                }

                max_youden_idx <- which.max(candidates$youden)
                best_pattern <- candidates[max_youden_idx, , drop = FALSE]

                # An exact tie was previously broken by whichever row came first, silently.
                tied <- candidates$pattern[
                    abs(candidates$youden - best_pattern$youden) < 1e-9]
                rationale_parts <- character()
                if (length(tied) > 1) {
                    rationale_parts <- c(
                        rationale_parts,
                        .fmt(
                            .('{n} rules tie on Youden\'s J ({rules}); "{shown}" is displayed only because it comes first.'),
                            n = length(tied),
                            rules = paste(tied, collapse = ", "),
                            shown = best_pattern$pattern
                        )
                    )
                }

                # This is an argmax over every candidate rule with no confidence interval
                # and no test, so on data with no real signal it still names a winner. Say
                # how many rules competed, and whether the winner separates from the next
                # one by more than the width of its own interval.
                n_candidates <- nrow(candidates)
                runner_up <- if (n_candidates > 1) {
                    sort(candidates$youden, decreasing = TRUE)[2]
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
                                .("Its advantage is not established: the lower bound of this rule's Youden's J ({lower}) falls at or below the next-best rule's point estimate ({runnerUp}), so the ranking may reflect sampling variation rather than a real difference."),
                                lower = base::format(round(j_lower, 3), nsmall = 3),
                                runnerUp = base::format(round(runner_up, 3), nsmall = 3)
                            )
                        )
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

                # Grade the estimate, not the certainty -- and only when it is not already
                # flagged as unseparated.
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
                # Relabelling symmetry: flip every test's positive level and the same 2x2s
                # reappear with the labels reversed, so an all-negative pattern wins while
                # Parallel, Serial and Majority all go negative and are stripped by the
                # youden > 0 filter. The result is a headline naming a rule no clinician can
                # apply -- "call it positive when every test is negative" -- with nothing to
                # flag it. The existing "no rule beats chance" notice names this failure mode
                # but fires only when every J is exactly zero, so it cannot catch this.
                winner_all_negative <- grepl("^-(/-)*$", best_pattern$pattern)
                named_rows <- candidates$rowType %in% c(.("Strategy"), .("Single test"))
                if (isTRUE(winner_all_negative) ||
                    (any(named_rows) && all(candidates$youden[named_rows] <= 0))) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Positive Levels May Be Inverted"),
                        .("The highest-ranked rule is an all-negative result pattern, or every named strategy performs at or below chance while an exact pattern does not. Both are the signature of a reversed positive level: if the level chosen as positive for one or more tests is actually the negative result, the arithmetic still works but the winning rule reads as \"call the patient positive when the tests are negative\", which is not a rule anyone can apply. Check the positive level selected for the reference standard and for each test before interpreting this ranking.")
                    )
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
                    jmvcore::.("This is a descriptive, sample-dependent ranking of exact-pattern rules and named testing strategies. It is not a clinical guide or validated recommendation.")
                )
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
            .addPatternColumn = function(data_prep) {
                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test pattern
                    pattern_values <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                } else if (!has_test3) {
                    # Two-test pattern
                    t1 <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                    t2 <- ifelse(data_prep$test2Variable2 == "Positive", "+", "-")
                    pattern_values <- paste0(t1, "/", t2)
                } else {
                    # Three-test pattern
                    t1 <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                    t2 <- ifelse(data_prep$test2Variable2 == "Positive", "+", "-")
                    t3 <- ifelse(data_prep$test3Variable2 == "Positive", "+", "-")
                    pattern_values <- paste0(t1, "/", t2, "/", t3)
                }

                output <- self$results$addedPattern
                output$setRowNums(rownames(data_prep))
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
                filtered_rows <- nrow(private$.applyPatternFilter(
                    combination_data, self$options$filterPattern))
                any_filtered_plot <- isTRUE(self$options$showBarPlot) ||
                    isTRUE(self$options$showHeatmap) || isTRUE(self$options$showForest)
                if (filtered_rows == 0 && any_filtered_plot) {
                    private$.addNotice(
                        "WARNING",
                        .("No Rows Match the Pattern Filter"),
                        .fmt(
                            .("The pattern-type filter \"{filter}\" matches none of the rows in this analysis, so the bar chart, heatmap and forest plot are blank. Pattern filters apply to exact result patterns such as \"+/+\"; a single-test analysis has no such rows, and the named strategies are not result patterns. Set the pattern filter back to \"All Patterns\" to see the plots."),
                            filter = private$.patternFilterLabel(self$options$filterPattern)
                        )
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
                    ratio_table <- private$.resultsItem("combinationTableCIRatios")
                    ratio_data <- if (!is.null(ratio_table) && ratio_table$rowCount > 0) {
                        as.data.frame(ratio_table$asDF, stringsAsFactors = FALSE)
                    } else {
                        data.frame()
                    }
                    forest_supported <- !self$options$filterStatistic %in%
                        c("prevalence", "balancedAccuracy", "youden")
                    if (!forest_supported) {
                        private$.addNotice(
                            "INFO",
                            .("Forest Plot Not Available for Selected Statistic"),
                            .fmt(
                                .('The forest plot is not drawn for "{statistic}" because this analysis does not calculate a confidence interval for that statistic. The bar chart and heatmap can still display it.'),
                                statistic = private$.metricLabel(
                                    self$options$filterStatistic
                                )
                            )
                        )
                    }
                    self$results$forestPlot$setState(list(
                        valid = forest_supported,
                        proportions = prop_data,
                        ratios = ratio_data,
                        filterStatistic = self$options$filterStatistic,
                        # filterPattern was declared in the forest plot's clearWith but was
                        # never carried into its state, so selecting a pattern type cleared
                        # the plot and redrew an identical image.
                        filterPattern = self$options$filterPattern
                    ))
                }
            },
            .plotBarChart = function(image, ...) {
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
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

                if (all_proportions) {
                    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
                }

                print(p)
                return(TRUE)
            },
            .plotHeatmap = function(image, ...) {
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

                midpoint <- if (stat_filter %in% c("lrPos", "lrNeg", "dor")) 1
                            else if (identical(stat_filter, "youden")) 0
                            else 0.5
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Metric, y = pattern, fill = Value)) +
                    ggplot2::geom_tile() +
                    ggplot2::geom_text(
                        ggplot2::aes(label = sprintf("%.2f", Value)),
                        color = "#1a1a1a"
                    ) +
                    ggplot2::scale_fill_gradient2(
                        low = "#ef8a8a", mid = "#f7f7f7", high = "#74add1",
                        midpoint = midpoint
                    ) +
                    ggplot2::labs(
                        title = .("Performance Heatmap"),
                        x = "",
                        y = .("Pattern"),
                        fill = .("Value")
                    ) +
                    ggplot2::theme_minimal()

                print(p)
                return(TRUE)
            },
            .plotForest = function(image, ...) {
                state <- image$state
                if (is.null(state)) {
                    return(FALSE)
                }
                if (!is.list(state) || !isTRUE(state$valid)) {
                    return(FALSE)
                }

                parts <- list()
                if (is.data.frame(state$proportions) && nrow(state$proportions) > 0) {
                    parts[[length(parts) + 1]] <- state$proportions
                }
                if (is.data.frame(state$ratios) && nrow(state$ratios) > 0) {
                    parts[[length(parts) + 1]] <- state$ratios
                }
                if (length(parts) == 0) {
                    return(FALSE)
                }
                table_df <- do.call(rbind, parts)
                rownames(table_df) <- NULL

                # Same row selection as the bar chart and heatmap. The CI tables store the
                # same pattern labels as the combination table, so .applyPatternFilter works
                # unchanged here; named strategy rows are not +/- patterns and drop out
                # whenever a specific pattern type is selected.
                table_df <- private$.applyPatternFilter(table_df, state$filterPattern)
                if (nrow(table_df) == 0) {
                    return(FALSE)
                }

                # Filter by statistic. The CI table stores display labels (e.g. "Sensitivity"),
                # so the option code must be mapped to its label before subsetting -- comparing
                # the code directly to the label always yielded an empty plot. Statistics not
                # present in the CI table (prevalence/balancedAccuracy/youden) leave the plot
                # unfiltered rather than blanking it.
                stat_filter <- state$filterStatistic
                if (stat_filter != "all") {
                    target_label <- private$.metricLabel(stat_filter)
                    if (!is.null(target_label) && target_label %in% table_df$statistic) {
                        table_df <- table_df[table_df$statistic == target_label, ]
                    }
                }

                if (nrow(table_df) == 0) {
                    return(FALSE)
                }

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = estimate, y = pattern, color = statistic)) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_errorbar(
                        ggplot2::aes(xmin = lower, xmax = upper),
                        orientation = "y", width = 0.2
                    ) +
                    ggplot2::labs(
                        title = .("Forest Plot - 95% Confidence Intervals"),
                        x = .("Estimate (95% CI)"),
                        y = .("Pattern"),
                        colour = .("Statistic")
                    ) +
                    ggplot2::theme_minimal() +
                    # Sensitivity lives on 0-1 while a diagnostic odds ratio can reach the
                    # hundreds. On the shared x-axis that facet_wrap() defaults to, every
                    # proportion facet collapsed into a sliver at the left edge.
                    ggplot2::facet_wrap(~statistic, ncol = 1, scales = "free_x")

                print(p)
                return(TRUE)
            },
            .plotDecisionTree = function(image, ...) {
                state <- image$state
                if (!is.list(state) || !isTRUE(state$valid) || is.null(state$data)) {
                    return(FALSE)
                }

                table_df <- as.data.frame(state$data, stringsAsFactors = FALSE)

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = sens, y = spec)) +
                    ggplot2::geom_point(ggplot2::aes(size = youden, color = pattern)) +
                    ggplot2::geom_text(ggplot2::aes(label = pattern), vjust = -1) +
                    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
                    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
                    ggplot2::labs(
                        title = .("Decision Space - Sensitivity vs Specificity"),
                        x = .("Sensitivity"),
                        y = .("Specificity"),
                        size = .("Youden's J"),
                        colour = .("Pattern")
                    ) +
                    ggplot2::theme_minimal()

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
                    args <- private$.asArgs(incData = FALSE)
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
